module Layout.LayInterpret where

import Common.CommonTypes
import Common.CommonUtils
import Layout.LayLayerTypes
import Layout.LayLayerUtils hiding (cast)

import Layout.Scanner

import Layout.TreeEditPres
import Arrangement.ArrTypes
import Rendering.RenTypes
import Layout.LayTypes
import Proxima.Wrap
import Evaluation.DocTypes

--interpretIO :: state -> low -> high -> editLow -> IO (editHigh, state, low)
interpretIO :: (DocNode node, Eq token, Show doc, Show enr, Show token, Show node) => ScannerSheet doc enr node clip token -> LayerStateLay doc enr node clip token -> LayoutLevel doc enr node clip token -> PresentationLevel doc enr node clip token -> [EditLayout doc enr node clip token]
            -> IO ([EditPresentation doc enr node clip token], LayerStateLay doc enr node clip token, LayoutLevel doc enr node clip token)
interpretIO scannerSheet state low high = castRemainingEditOps $ \editLow ->
  do { (editsHigh, state', low') <- parseIO scannerSheet state low high editLow
     ; debugLnIO Lay $ "Edit Layout: "++show editLow
     ; return (editsHigh, state', low')
     }


-- one extra indirection because separate cases make it hard to do debugging on result



--------------- Parser


-- split in monadic and non-monadic part
parseIO :: (Eq token, Show token, DocNode node, Show doc, Show enr) => ScannerSheet doc enr node clip token -> LayerStateLay doc enr node clip token -> LayoutLevel doc enr node clip token -> PresentationLevel doc enr node clip token -> EditLayout doc enr node clip token -> IO ([EditPresentation doc enr node clip token], LayerStateLay doc enr node clip token, LayoutLevel doc enr node clip token)
--parseIO _ state layLvl prsLvl (OpenFileLay str) = openFile str state layLvl prsLvl
--parseIO _ state layLvl prsLvl (SaveFileLay str) = setUpd NothingUpdated $ saveFile state layLvl prsLvl str 
parseIO _ state layLvl prsLvl (OpenFileLay str) = return ([OpenFilePres str], state, layLvl)
parseIO _ state layLvl prsLvl (SaveFileLay str) = return ([SaveFilePres str], state, layLvl)
parseIO scannerSheet state layLvl@(LayoutLevel pres _ dt) prsLvl (SetFocusLay focus) = 
  return ([SkipPres 0], state, LayoutLevel pres focus dt)
{- auto doc focus turned off
  let pathDoc = pathDocFromFocusPres focus pres
      (editHigh1, state', LayoutLevel pres' focus' dt' ) = tokenizeLay scannerSheet state 
                                                             (LayoutLevel pres focus dt) prsLvl
  in  debug Lay ("\n\n\nDocument focus: "++show pathDoc) $
      return ( [ editHigh1
               , castDoc' $ NavPathDoc' pathDoc
               ]
             , state', LayoutLevel pres' focus' dt')
-}
parseIO scannerSheet state layLvl prsLvl event = let (editsHigh, state', low') = parse scannerSheet state layLvl prsLvl event
                                                 in  return (editsHigh, state', low')

parse :: (DocNode node, Show token, Eq token, Show doc, Show enr) => ScannerSheet doc enr node clip token -> LayerStateLay doc enr node clip token -> 
         LayoutLevel doc enr node clip token -> PresentationLevel doc enr node clip token -> EditLayout doc enr node clip token ->
         ([EditPresentation doc enr node clip token], LayerStateLay doc enr node clip token, LayoutLevel doc enr node clip token)
parse _ state layLvl prsLvl (SkipLay i)   = ([SkipPres (i+1)], state, layLvl)
parse _ state layLvl prsLvl InitLay       = ([InitPres], state, layLvl)
parse _ state layLvl prsLvl (InsertLay c) = editLay (editInsert c) state layLvl prsLvl
parse _ state layLvl prsLvl CutLay   = editLay editCut state layLvl prsLvl
parse _ state layLvl prsLvl CopyLay   = editCopy state layLvl prsLvl
parse _ state layLvl prsLvl PasteLay  = editLay editPaste state layLvl prsLvl
parse _ state layLvl prsLvl DeleteLay = editLay editDelete state layLvl prsLvl 
parse _ state layLvl prsLvl (MoveLay src dst) = ([SkipPres 0], state, layLvl) 
-- requires tree algorithms that work regardles of structural/parsing and on paths instead of focus

parse _ state layLvl prsLvl SplitLay  = editLay editSplit state layLvl prsLvl
parse scannerSheet state layLvl@(LayoutLevel pres f _) prsLvl LeftDeleteLay = 
  if focusIsOnGraph f pres -- if the from path is in a graph, this is a graph edit
  then graphEdit scannerSheet state layLvl prsLvl deleteInGraph
  else editLay editLeftDelete state layLvl prsLvl
parse scannerSheet state layLvl@(LayoutLevel pres f _) prsLvl RightDeleteLay =
  if focusIsOnGraph f pres -- if the from path is in a graph, this is a graph edit
  then graphEdit scannerSheet state layLvl prsLvl deleteInGraph
  else editLay editRightDelete state layLvl prsLvl

parse scannerSheet state layLvl prsLvl (EditStyleLay style) = editLayParse (editEditStyle style) scannerSheet state layLvl prsLvl 

parse _ state layLvl prsLvl LeftLay   = navigateLeft state layLvl prsLvl 
parse _ state layLvl prsLvl RightLay  = navigateRight state layLvl prsLvl

parse _ state layLvl prsLvl EnlargeLeftLay   = enlargeLeft state layLvl prsLvl
parse _ state layLvl prsLvl EnlargeRightLay  = enlargeRight state layLvl prsLvl

parse scannerSheet state layLvl prsLvl (AddVertexLay pth pos)  = graphEdit scannerSheet state layLvl prsLvl (addVertex pth pos) 
parse scannerSheet state layLvl prsLvl (AddEdgeLay pth)        = graphEdit scannerSheet state layLvl prsLvl (addEdge pth)
parse scannerSheet state layLvl@(LayoutLevel pres f dt) prsLvl (MoveVertexLay pth pos) = 
  let (state', layLvl') = moveVertex pth pos state layLvl
  in  ([SkipPres 0], state', layLvl')
parse scannerSheet state layLvl prsLvl NormalizeLay       = editLay editNormalize state layLvl prsLvl

parse scannerSheet state layLvl prsLvl ParseLay = let (editOps, state', layLvl') = tokenizeLay scannerSheet state layLvl prsLvl
                                                  in  (editOps {- ++ [ castArr GuaranteeFocusInViewArr ] -}, state', layLvl')
parse  scannerSheet state layLvl@( LayoutLevel pres focus dt) prsLvl (FindLay mStr) = 
  let str = case mStr of
              Just str -> str
              Nothing  -> case getLastSearchTerm state of 
                            Just str' -> str'
                            Nothing -> error "no previous search term"
  in  debug Prs ("\n\n\n\nFinding "++str) $
      case findLay focus str pres of
        Nothing     -> ([castRen' $ AlertRen' $ "Search term \"" ++ str ++ "\" not found."], state,  layLvl)
        Just focus' -> debug Prs (show focus') $
                         ( [SkipPres 0, castArr $ GuaranteeFocusInViewArr]
                         , state { getLastSearchTerm = Just str },  LayoutLevel pres focus' dt)


parse _ state layLvl prsLvl Test2Lay           = ([Test2Pres], state, layLvl)
-- We want to be able to set the presentation here and probably do a Layout to Presentation mapping.
-- Not possible with just single edit commands. The problem is that the parser must not always be called. This
-- does not readily fit in the current model.
-- We can fix it by not setting the higher pres level if we don't want a parse.


{-parse _ state layLvl prsLvl Test2Lay   = setUpd AllUpdated $editReadFile state layLvl prsLvl focus 
--parse _ state layLvl prsLvl (MouseDownLay path ms i) = setUpd AllUpdated $ editMouseDown state layLvl prsLvl path -- Helium
-- to allow presenter mouse handle: change GestureInterpreter, so the event is handled there
-}
parse _ state layLvl prsLvl (WrapLay wrapped) = ([unwrap wrapped],        state, layLvl)
parse _ state layLvl prsLvl cmd            = debug Err ("LayInterpret.parse: unhandled command "++show cmd) $ ([SkipPres 0], state, layLvl)


-- edit ops need to be consistent, when navigating with non-empty focus, collapse focus
-- when inserting with non-empty focus, first delete

-- edit ops that actually change the presentation tree should be a separate type because now we have multiple 
-- functions or lose type safety  (?)


-- doc and/or presentation need some way to say whether document parts are parsed. Now With nodes pile up on the 
-- unparsed presentation.



-- if focus is valid, apply editF to the presentation, and try to reparse the presentation 
--editLay :: 
--            Layout doc enr node clip token -> Layout doc enr node clip token -> LayoutLevel doc enr node clip token -> FocusPres -> (EditPresentation doc enr node clip token, Layout doc enr node clip token, Layout doc enr node clip token)

editLay editF state layLvl@(LayoutLevel pres NoFocusP dt) presLvl = ([SkipPres 0], state, layLvl)
editLay editF state (LayoutLevel pres focus dt) (PresentationLevel _ (layout, idCounter)) = 
 let (ll@(LayoutLevel pres' focus' dt), clip') = editF (getClipboard state) (LayoutLevel pres focus dt) -- this will be layLvl's own focus
     diffTree = diffPres pres' pres
 in ([SkipPres 0], state { getClipboard = clip'}, LayoutLevel pres' focus' diffTree)

editLayParse editF scannerSheet state layLvl@(LayoutLevel pres NoFocusP dt) presLvl = ([SkipPres 0], state, layLvl)
editLayParse editF scannerSheet state layLvl@(LayoutLevel pres focus dt) prsLvl@(PresentationLevel _ (layout, idCounter)) = 
 let (ll@(LayoutLevel pres' focus' dt), clip') = editF (getClipboard state) (LayoutLevel pres focus dt) -- this will be layLvl's own focus
     diffTree = diffPres pres' pres
 in  tokenizeLay scannerSheet state ll prsLvl
     
-- should we make a similar function for edit ops that do not alter the presentation? This function would not do
-- much, except setting the update region, getting rid of the document argument and returning a SkipPres 0,
-- Also some edit ops change the focus, whereas others only change the clip, or do IO.  Different functions?
-- They differ also in handling an empty focus. copy and navigate can't handle empty focus, but save would work fine
-- What is a nice abstraction here?

-- replace the current layout by pres, focus is reset


-- replace the current layout by pres, focus is reset
editSet :: Layout doc enr node clip token -> Layout doc enr node clip token -> LayoutLevel doc enr node clip token -> (LayoutLevel doc enr node clip token, Layout doc enr node clip token)
editSet pres' clip (LayoutLevel pres focus@(FocusP f t) dt) = (LayoutLevel pres' NoFocusP dt, clip)

openFile :: (DocNode node, Show token, Eq token) => String -> LayerStateLay doc enr node clip token -> LayoutLevel doc enr node clip token -> PresentationLevel doc enr node clip token -> 
            IO ([EditPresentation doc enr node clip token], LayerStateLay doc enr node clip token, LayoutLevel doc enr node clip token)
openFile filePath clip layLvl prsLvl =
 do { debugLnIO Lay $ "Opening file: "++filePath
    ; str <- readFile filePath
    ; let pres' = StringP NoIDP filePath
    ; return $ editLay (editSet pres') clip layLvl prsLvl
    }
    
editInsert :: (DocNode node, Show token) => Char -> Layout doc enr node clip token -> LayoutLevel doc enr node clip token -> (LayoutLevel doc enr node clip token, Layout doc enr node clip token)
editInsert c clip (LayoutLevel pres focus@(FocusP f t) dt) = 
  let (pres', focus')  = if f==t then (pres,focus) else deleteTree focus pres
      (pres'',focus'') = pasteTree (fromP focus') (text [c]) pres'
  in  (LayoutLevel pres'' focus'' dt, clip)

editCut :: (DocNode node, Show token) => Layout doc enr node clip token -> LayoutLevel doc enr node clip token -> (LayoutLevel doc enr node clip token, Layout doc enr node clip token)
editCut clip (LayoutLevel pres focus dt) = 
  let clip' = copyTree focus clip pres                                                                                              
      (pres', focus') = deleteTree focus pres
  in  (LayoutLevel pres' focus' dt, clip')

editCopy :: (DocNode node, Show token) => LayerStateLay doc enr node clip token -> LayoutLevel doc enr node clip token -> PresentationLevel doc enr node clip token -> 
            ([EditPresentation doc enr node clip token], LayerStateLay doc enr node clip token, LayoutLevel doc enr node clip token)
editCopy state layLvl@(LayoutLevel pres NoFocusP dt) doc = ([SkipPres 0], state, layLvl)
editCopy state layLvl@(LayoutLevel pres focus dt)    doc = 
  let clip' = copyTree focus (getClipboard state) pres                                                                     
  in  ([SkipPres 0], state { getClipboard = clip'}, (LayoutLevel pres focus dt))   -- set the pres focus to the one coming from the arranger, see focus discussion

editPaste clip (LayoutLevel pres focus@(FocusP f t) dt) = 
  let (pres', focus') = if f==t then (pres,focus) else deleteTree focus pres
      (pres'', focus'') = pasteTree (fromP focus') clip pres'                                                                                          
  in  (LayoutLevel pres'' focus'' dt, clip)

editDelete clip (LayoutLevel pres focus dt) = 
  let (pres', focus') = deleteTree focus pres
  in  (LayoutLevel pres' focus' dt, clip)

editSplit clip (LayoutLevel pres focus dt) = 
  let (pres', focus')   = deleteTree focus pres
      (pres'', focus'') = splitRowTree (fromP focus') pres'
  in (LayoutLevel pres'' focus'' dt, clip)



-- only for column of rows:
editNormalize :: (DocNode node, Show token) => Layout doc enr node clip token -> LayoutLevel doc enr node clip token -> (LayoutLevel doc enr node clip token, Layout doc enr node clip token)
editNormalize clip (LayoutLevel pres focus dt) = 
 let (pres', focus') = normalizePresentation pres focus
 in  (LayoutLevel pres' focus' dt, clip)



-- unlike paste split and insert, left and right delete do not perform their edit command when the focus was non-empty
-- ie. in that case, they are interpreted as a regular delete
editLeftDelete clip layLvl@(LayoutLevel pres focus@(FocusP f t) dt) =
  if f /= t then editDelete clip layLvl else
    let focus'          = navigateLeftTreePres (toP focus) pres
        focus''         = FocusP (toP focus) (toP focus')
        (pres', focus''') = deleteTree focus'' pres
    in (LayoutLevel pres' focus''' dt, clip)

editRightDelete clip layLvl@(LayoutLevel pres focus@(FocusP f t) dt) =
  if f /= t then editDelete clip layLvl else
    let focus'          = navigateRightTreePres (toP focus) pres
        focus''         = FocusP (toP focus) (toP focus')
        (pres', focus''') = deleteTree focus'' pres
    in  (LayoutLevel pres' focus''' dt, clip)


editEditStyle :: (DocNode node, Show token) => StyleEdit -> Layout doc enr node clip token -> LayoutLevel doc enr node clip token ->
                (LayoutLevel doc enr node clip token, Layout doc enr node clip token)
editEditStyle style clip (LayoutLevel pres focus dt) = 
 let pres' = editStylePres style focus pres
     focus' = editStylePresF focus
 in  (LayoutLevel pres' focus' dt, clip)


navigateLeft :: (DocNode node, Show token) =>  LayerStateLay doc enr node clip token -> LayoutLevel doc enr node clip token -> PresentationLevel doc enr node clip token -> 
                ([EditPresentation doc enr node clip token], LayerStateLay doc enr node clip token, LayoutLevel doc enr node clip token)
navigateLeft state layLvl@(LayoutLevel pres NoFocusP dt) doc = ([SkipPres 0], state, layLvl)
navigateLeft state (LayoutLevel pres focus dt) doc =
  let  focus' = navigateLeftTreePres (toP focus) pres
  in  ([SkipPres 0], state, LayoutLevel pres focus' dt)

navigateRight :: (DocNode node, Show token) => LayerStateLay doc enr node clip token -> LayoutLevel doc enr node clip token -> PresentationLevel doc enr node clip token -> 
                 ([EditPresentation doc enr node clip token], LayerStateLay doc enr node clip token, LayoutLevel doc enr node clip token)
navigateRight state layLvl@(LayoutLevel pres NoFocusP dt) doc = ([SkipPres 0], state, layLvl)
navigateRight state (LayoutLevel pres focus dt) doc = 
  let  focus' = navigateRightTreePres (toP focus) pres
  in  ([SkipPres 0], state, LayoutLevel pres focus' dt)

enlargeLeft :: (DocNode node, Show token) => LayerStateLay doc enr node clip token -> LayoutLevel doc enr node clip token -> PresentationLevel doc enr node clip token -> 
               ([EditPresentation doc enr node clip token], LayerStateLay doc enr node clip token, LayoutLevel doc enr node clip token)
enlargeLeft state layLvl@(LayoutLevel pres NoFocusP dt) doc = ([SkipPres 0], state, layLvl)
enlargeLeft state (LayoutLevel pres focus dt) doc =
  let  focus' = navigateLeftTreePres (toP focus) pres
       focus'' = FocusP (fromP focus) (fromP focus')
  in  ([SkipPres 0], state, LayoutLevel pres focus'' dt)

enlargeRight :: (DocNode node, Show token) => LayerStateLay doc enr node clip token -> LayoutLevel doc enr node clip token -> PresentationLevel doc enr node clip token -> 
                ([EditPresentation doc enr node clip token], LayerStateLay doc enr node clip token, LayoutLevel doc enr node clip token)
enlargeRight state layLvl@(LayoutLevel pres NoFocusP dt) doc = ([SkipPres 0], state, layLvl)
enlargeRight state (LayoutLevel pres focus dt) doc = 
  let  focus' = navigateRightTreePres (toP focus) pres
       focus'' = FocusP (fromP focus) (fromP focus')
  in  ([SkipPres 0], state, LayoutLevel pres focus'' dt)


-- Graph editing (deletion is in editLeft/RightDelete functions)
-- Move vertex does not make the graph dirty
--continueWithParse (_, state, LayoutLevel pres focus ) = tokenizeLay


-- | graphEdit performs a graph edit operation on the layout level and continues by parsing
--   the layout
graphEdit :: (Show token, DocNode node) => ScannerSheet doc enr node clip token ->
             LayerStateLay doc enr node clip token -> LayoutLevel doc enr node clip token ->
             PresentationLevel doc enr node clip token ->
             (LayerStateLay doc enr node clip token -> LayoutLevel doc enr node clip token -> (LayerStateLay doc enr node clip token, LayoutLevel doc enr node clip token))->
             ([EditPresentation doc enr node clip token], LayerStateLay doc enr node clip token, LayoutLevel doc enr node clip token) 
graphEdit scannerSheet state layLvl presLvl editFn =
  let (state', layLvl') = editFn state layLvl 
      
  in  tokenizeLay scannerSheet state' layLvl' presLvl



addVertex :: (DocNode node, Show token) => [Int] -> (Int, Int) ->  LayerStateLay doc enr node clip token -> LayoutLevel doc enr node clip token ->
             (LayerStateLay doc enr node clip token, LayoutLevel doc enr node clip token)
addVertex pth (x,y) state (LayoutLevel pres focus dt)  =
  let vertexIDs = getVertexIDs pth pres
      freshID = head' "LayInterpret.addVertex" $ dropWhile (`elem` vertexIDs) [0..]
      pres' = addVertexPres (PathP pth 0) (loc noNode $ structural $ VertexP NoIDP freshID x y outline vertex) pres
  in  (state, LayoutLevel pres' focus dt)     -- 0 in path is ignored
 where vertex = empty
       outline = const (0,0)

addEdge toPth state layLvl@(LayoutLevel pres focus@(FocusP (PathP fromPth _) _) dt) =
  let pres' = case selectTree fromPth pres of
                (VertexP _ _ _ _ _ _) -> addEdgePres (PathP fromPth 0) (PathP toPth 0) pres -- 0 in path is ignored
                _                     -> pres
  in  (state, LayoutLevel pres' focus dt)
addEdge _ state layLvl = (state, layLvl)
 
moveVertex pth pt state layLvl@(LayoutLevel pres focus dt) =
  let pres' = moveVertexPres pth pt pres
  in  (state, LayoutLevel pres' focus dt)

deleteInGraph state layLvl@(LayoutLevel pres focus@(FocusP f t) dt) =
  let pres' = deleteGraphPres f pres
  in  (state, LayoutLevel pres' NoFocusP dt)
{-
openFile :: Layout doc node clip -> LayoutLevel doc enr node clip token -> PresentationLevel doc enr node clip token -> FilePath -> IO (EditPresentation doc enr node clip token, Layout doc node clip, LayoutLevel doc enr node clip token) 
openFile clip layLvl doc filePath =
 do { debugLnIO Prs "Opening file"
    ; str <- readFile filePath
    ; let (doc', layLvl', clip') = read str
    ; return (SetPres doc', clip', layLvl')
    }

saveFile :: Layout doc node clip -> LayoutLevel doc enr node clip token -> PresentationLevel doc enr node clip token -> FilePath -> IO (EditPresentation doc enr node clip token, Layout doc node clip, LayoutLevel doc enr node clip token)
saveFile clip layLvl doc filePath =
 do { debugLnIO Prs "Saving file"
   -- ; let (src, errsStr) =  span (\l -> not (isPrefixOf "#######" l)) . lines . stringFromPres $ pres
   -- ; let str = unlines src
    ; writeFile filePath $ show (doc, layLvl, clip)  -- can't show pres, because of with nodes
    ; return (SkipPres 0, clip, layLvl)
    }

-}
{-

Focus:

Focus is not completely right yet. Instead of passing the focus from gest int. to presenter for every edit, all levels
should keep a focus. This will allow the nice up/down navigation.

Also, a skip doc, operation will reinstall the old pres focus, which is not right if an up down has been performed on
arrangement focus. There is a choice here. Either every level has correct focus, or a skip does lead to a focus update
on the lower level. In the last case, the focus moves up and down only when required. This might lead to some difficult
administration though.

Probably everything will be ok, once we have Level types on all levels, then the passed datastructure will always contain
the current focus, and it is passed only when the lower level cannot handel it.

When we stay at lower levels, the focus will not be correct at the higher levels, this is similar edit ops that are 
short cut at lower levels. Maybe we need a distinction though. An edit on the pres tree without a reparse should signal
that the document is not consistent with the presentation yet, but if only the focus has changed, we don't want that signal.

The difference we have here might be that the focus updates are LS updates.

For now, we fix it by having edit ops that do a skip doc, also set the presentation focus.


edit, compute focus after edit, compute focus in terms of position in string repr.

present: recomputes focus in updated tree from string position except for skip, because then focus is ok.

arrange: interpret focusP to focusA

structural problem: if presentation before focus changes in size, the focus is incorrect.
-- this will be solved by having a document focus.


focus on presentation requires rearrange after each focus move. This does not seem to be what we want
will we allow the presentation to be influenced by the focus? This will be even more expensive

mouse handling stuff seems to call for a backtrack in edit levels, try highest level, if fail try lower.
This is not part of the model yet



BUG copy depends on direction!!
-}






