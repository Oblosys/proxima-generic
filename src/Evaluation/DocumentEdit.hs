{-# LANGUAGE RankNTypes, MonoLocalBinds #-}
module Evaluation.DocumentEdit where

{-

Defines document edit (structure edit) functions

-}

import Common.CommonTypes
import Evaluation.DocTypes
import Evaluation.DocUtils
import Presentation.PresTypes

import Data.List
import Debug.Trace

class Editable a doc enr node clip token | a -> doc enr node clip token where
  select :: Path -> a -> clip
  paste :: Path -> clip -> a -> a
  alternatives :: a -> [ (String, clip) ]
  arity :: a -> Int
  toClip :: a -> clip
  fromClip :: clip -> Maybe a
  parseErr :: ParseError doc enr node clip token -> a
  hole :: a
  holeNodeConstr :: (a -> Path -> node) -- for automatic hole parsing in PresentationParsing.pStr
  isList :: a -> Bool
  insertList :: Int -> clip -> a -> clip
  removeList :: Int -> a -> clip

class Clip clip where
  arityClip :: clip -> Int
  alternativesClip :: clip -> [ (String, clip) ]
  holeClip :: clip -> clip
  isListClip :: clip -> Bool
  insertListClip :: Int -> clip -> clip -> clip
  removeListClip :: Int -> clip -> clip
  
navigateUpD :: FocusDoc -> document -> FocusDoc
navigateUpD NoPathD         _  = NoPathD
navigateUpD (PathD [])      _  = NoPathD
navigateUpD (PathD p@(_:_)) _  = PathD $ init p
navigateUpD pd              _ = pd

navigateDownD :: (Editable doc doc enr node clip token, Clip clip) => FocusDoc -> doc -> FocusDoc
navigateDownD NoPathD   d = PathD []
navigateDownD (PathD p) d = PathD $ if arityD p d > 0 then p++[0] else p
 
navigateLeftD :: Editable doc doc enr node clip token => FocusDoc -> doc -> FocusDoc
navigateLeftD NoPathD         _ = NoPathD
navigateLeftD (PathD p@(_:_)) d = PathD $ let i = last p
                                          in  if i > 0 then init p ++ [i-1] else p
navigateLeftD pd              _ = pd

navigateRightD :: (Editable doc doc enr node clip token, Clip clip) => FocusDoc -> doc -> FocusDoc
navigateRightD NoPathD         _ = NoPathD
navigateRightD (PathD p@(_:_)) d = PathD $ let i = last p
                                               a = arityD (init p) d
                                           in  if i < a-1 then init p ++ [i+1] else p
navigateRightD pd              _ = pd




editCopyD :: Editable doc doc enr node clip token => DocumentLevel doc clip -> DocumentLevel doc clip
editCopyD (DocumentLevel doc NoPathD clip)        = DocumentLevel doc NoPathD clip
editCopyD (DocumentLevel doc pd@(PathD pth) clip) = DocumentLevel doc pd (selectD pth doc)

editCutD :: (Editable doc doc enr node clip token, Clip clip) => DocumentLevel doc clip -> DocumentLevel doc clip
editCutD  (DocumentLevel doc NoPathD clip)           = DocumentLevel doc NoPathD clip
editCutD  (DocumentLevel doc pd@(PathD pth) clip)    = let (doc', pd') = deleteD pth doc
                                                       in  DocumentLevel doc' pd' (selectD pth doc)

editDeleteD :: (Editable doc doc enr node clip token, Clip clip) => DocumentLevel doc clip -> DocumentLevel doc clip
editDeleteD (DocumentLevel doc NoPathD clip)        = DocumentLevel doc NoPathD clip
editDeleteD (DocumentLevel doc pd@(PathD pth) clip) =  let (doc', pd') = deleteD pth doc
                                                       in  DocumentLevel doc' pd' clip

editPasteD :: Editable doc doc enr node clip token => DocumentLevel doc clip -> DocumentLevel doc clip
editPasteD (DocumentLevel doc NoPathD clip)         = DocumentLevel doc NoPathD clip
editPasteD (DocumentLevel doc pd@(PathD pth) clip)  = DocumentLevel (pasteD pth clip doc) pd clip


-- menuD is probably not a good name
menuD :: (Editable doc doc enr node clip token, Clip clip) => PathDoc -> doc -> [ (String, DocumentLevel doc clip -> DocumentLevel doc clip) ]
menuD NoPathD _              = []
menuD path@(PathD p) d =
  let alts = alternativesD p d
      mkItem (s,c) = (s, \(DocumentLevel _ pth clip) -> DocumentLevel (pasteD p c d) pth clip)
  in  [ ("Cut", \(DocumentLevel d _ clip) -> let (d',p') = deleteD p d 
                                               in  DocumentLevel d' p' (selectD p d) ) 
      , ("Copy", \(DocumentLevel d _ clip) -> DocumentLevel d path (selectD p d) ) 
      , ("Paste", \(DocumentLevel d _ clip) -> DocumentLevel (pasteD p clip d) path clip )
      , ("Select", \(DocumentLevel d _ clip) -> DocumentLevel d path clip ) ]
{-      ++ map mkItem alts   -- use this one or the one in the argument? They should be the same
      ++ if null p then [] else
           let parent = (selectD (init p) d)
           in if not (isListClip parent) then [] else
                let alts2 = [ ("Add "++s, insertListClip (last p+1) c parent) | (s,c) <- alts]
                    mkItem2 (s,c) = (s, \(DocumentLevel _ pth clip) -> DocumentLevel (pasteD (init p) c d) pth clip)
                    pasteBefore = ("Paste before", \(DocumentLevel _ pth clip) -> 
                                                     DocumentLevel (pasteD (init p) (insertListClip (last p) clip parent) d) pth clip )
                    pasteAfter = ("Paste after", \(DocumentLevel _ pth clip) -> 
                                                     DocumentLevel (pasteD (init p) (insertListClip (last p+1) clip parent) d) pth clip )
                in  map mkItem2 alts2 ++ [pasteBefore,pasteAfter]
-}

selectD :: Editable doc doc enr node clip token => Path -> doc -> clip
selectD p doc = select p doc

pasteD :: Editable doc doc enr node clip token => Path -> clip -> doc -> doc
pasteD p c doc = paste p c doc

insertListD :: (Show clip,Clip clip, Editable doc doc enr node clip token) => Path -> Int -> clip -> doc -> doc
insertListD path index clip doc = 
  let list = selectD path doc
  in  if isListClip list
      then if index <= arityClip list 
           then pasteD path (insertListClip index clip list) doc
           else debug Err ("DocumentEdit.insertBeforeD beyond end of list "++show path) $ doc
      else debug Err ("DocumentEdit.insertBeforeD on non-list at "++show path++show clip) $ doc

-- ugly mix of levels, find out how to do it nicely
deleteD :: (Editable doc doc enr node clip token, Clip clip) => Path -> doc -> (doc, PathDoc)
deleteD p d = if not (null p) && isListClip (selectD (init p) d) -- if parent is list, then delete is remove from list
              then (pasteD (init p) (removeListClip (last p) (selectD (init p) d)) d, NoPathD)
              else let newhole = holeClip (selectD p d)
                   in  (pasteD p newhole d, PathD p) 

arityD :: (Editable doc doc enr node clip token, Clip clip) => Path -> doc -> Int
arityD p d = arityClip (select p d)

alternativesD :: (Editable doc doc enr node clip token, Clip clip) => Path -> doc -> [ (String, clip) ]
alternativesD p d = alternativesClip (select p d)

 
moveDocPathD :: (Editable doc doc enr node clip token, Clip clip, Show clip, Show doc) => Path -> Path -> Int -> doc -> doc
moveDocPathD [] targetListPath index doc = error "Move with empty source path."
moveDocPathD sourcePath targetListPath index doc =
  let (tgtPath,tgtIx) =
        if not $ init sourcePath `isPrefixOf` targetListPath 
        then (targetListPath, index)
        else if length sourcePath - 1 == length targetListPath -- src & tgt in same list
             then (targetListPath, if last sourcePath < index then index-1 else index) 
             else -- we now have: length targetListPath > length sourcePath - 1
                  let (pref, p: suffix)  = splitAt (length sourcePath - 1) targetListPath 
                  in  debug Arr (show (pref, p, suffix) ++ show sourcePath) $
                      if last sourcePath == p then error "Cyclic move."          
                      else if last sourcePath < p then (pref ++ [p-1] ++ suffix, index)
                                                  else (pref ++ [p]   ++ suffix, index)
      
      source = selectD sourcePath doc
      (doc',_) = deleteD sourcePath doc
      (doc'') = debug Arr ("move to " ++ show (tgtPath, tgtIx)) $
                insertListD tgtPath tgtIx source doc'
  in  --debug Eva ("Document move\n"++show doc ++ "\n\n" ++ show doc'') $
      doc''