{-# LANGUAGE CPP, FlexibleContexts #-} 
-- CPP is enabled only for this module, since it slows the build process down quite a bit
module Rendering.RendererServer (render, renderFocus, mkPopupMenuXY) where

import Common.CommonTypes hiding (Rectangle)
import qualified Common.CommonTypes as CommonTypes
import Common.CommonUtils

import Rendering.RenLayerTypes
import Rendering.RenLayerUtils
import Proxima.Wrap

import Arrangement.ArrLayerUtils (point, popupMenuItemsPres, pathPFromPathA')  -- for context menu hack
--import Presentation.PresTypes hiding (font) -- For Locations
import Layout.LayTypes hiding (Point)

import Evaluation.DocTypes (DocumentLevel)
import Arrangement.FontLib

import System.IO.Unsafe
import Data.IORef
import System.IO
import Control.Monad.Writer hiding (when)

render :: Show node => Scale -> Bool -> DiffTreeArr -> Arrangement node -> (Point, Size) -> Writer String ()
render scale arrDb diffTree arrangement viewedArea =
 do { -- seq (walk arrangement) $ return ()        -- maybe this is not necessary anymore, now the datastructure is strict
    --; putStrLn $ "Rendering on viewedArea " ++ show viewedArea
    -- debugLnIO Err ("The updated rectangle is: "++show (updatedRectArr diffTree arrangement))
    --; clipRegion <- regionRectangle $ Rectangle (xA arrangement) (yA arrangement) (widthA arrangement) (heightA arrangement)
    -- cannot use these IO regions anymore
    
    ; --debug Ren ("DiffTree is " ++ show diffTree) $
     -- debug Ren ("Arrangement is "++showTreeArr arrangement) $
      renderArr arrDb scale origin viewedArea Nothing (Just [0]) diffTree arrangement
    }                 -- 0 is for the rendering (1 is focus)


-- old comment: debugged rendering also displays overlay for focus adding, but this has not been processed by debugArrangement
-- this makes it tricky to move the debuggedArrangement, since the Gest.Int. will not know about it
-- however, we don't want to debug the focus
    
renderFocus :: Show node => Scale -> Bool -> FocusArr -> Arrangement node -> (Point, Size) -> Writer String ()
renderFocus scale arrDb focus arrangement viewedArea =
 do { -- clipRegion <- regionRectangle $ Rectangle (xA arrangement) (yA arrangement) (widthA arrangement) (heightA arrangement)

    ; let focusArrList = arrangeFocus focus arrangement
    ; case focus of
        FocusA (PathA p i) (PathA p' i') | p == p' && i == i' ->
          case  selectTreeA p arrangement of
            (_, _, (StringA arrId _ _ _ _ _ _ _ _ _ _ _)) -> 
              let pathR = 0:pathRFromPathA arrangement p++[0]
              in  --debug Ren ("Focused id "++ show arrId ++ ", index " ++show i
                  --           ++ "HTML path: "++ show pathR ++" pathNodes:" ++ showPathNodesA p arrangement ) $
                   do { tell $ "<div id='setFocusedId' op='setFocusedId' "++
                                   " focusedId='"++show arrId++"' index='"++show i++"'>" ++
                                   htmlPath pathR ++ "</div>"
                      }
            _ -> return ()
        _ -> return ()
    ; debug Ren ("Focus: "++show focus) $
                                          -- ++ "\nFocus arrangement:\n"++show focusArrList) $
               
        renderArr arrDb scale origin viewedArea Nothing
                        (Just [1])  -- 1 is for the focus (0 is rendering)
                        (DiffLeafArr False Nothing)
                        (OverlayA (IDA (-2)) (xA arrangement) (yA arrangement)  
                                        (widthA arrangement) (heightA arrangement) 
                                        0 0 transparent
                                  HeadInFront
                                  focusArrList) 
   }

-- Note that the type differs from RendererServer.mkPopupMenuXY because the type GUICommand.GUICommand defined differently based on whether we compile for gtk or server
mkPopupMenuXY :: (DocNode node, Show token) => Settings ->
                 Layout doc enr node clip token -> Scale -> Arrangement node ->
                 Int -> Int -> [PopupMenuItem doc enr node clip token]
mkPopupMenuXY settings prs scale arr x' y' =
  let (x,y) = (descaleInt scale x',descaleInt scale y')
      ctxtItems = case point x y arr of
                        Nothing -> []
                        Just pthA -> popupMenuItemsPres (pathPFromPathA' arr prs pthA) prs
  in [ (toHTML str,upd) | (str,upd) <- ctxtItems ]
   




{-
When a node is self dirty, all children are also dirty. Hence, once we end up in the last
case of renderArr and start generating code, we stay there.

cleanParentId contains Just the parent if it was self clean. On rendering, Nothing is passed on.

Hence, we can emit a replace command if the parent is clean but the child is self dirty
-}

-- This is not very nice, makeReplaceUpdate is called also on descendents, in which case it doesn't really make an update
-- arrangement is passed only for debugging
makeReplaceUpdate Nothing    arrangement mkArrangement = mkArrangement
makeReplaceUpdate (Just pth) arrangement mkArrangement = 
 do { --debug Ren ("mkReplace " ++ shallowShowArr arrangement ++ " on "++ show pth) $ return ()
    ; tell $ "<div id='replace' op='replace'>"++htmlPath pth
    --; putStrLn $ "\n\n*********REPLACE "++show pth
    --; putStrLn $ "by:\n" ++ showTreeArr arrangement
    ; mkArrangement
    ; tell $ "</div>" 
    }

makeMoveUpdate Nothing    coords = debug Err ("RendererServer.makeMoveUpdate: no path.") $ return ()
makeMoveUpdate (Just pth) dim@((x,y),(w,h)) = 
 do { --debug Ren ("mkMove "++show dim++" on "++ show pth) $ return ()
    ; tell $ "<div id='move' op='move' x='"++show x++"' y='"++show y++"' w='"++show w++"' h='"++show h++"'>"++htmlPath pth
    ; tell $ "</div>" 
    }

makeInsertDeleteUpdate _ Nothing    insdel = debug Err ("RendererServer.makeInsertDeleteUpdate: no path.") $ return ()
makeInsertDeleteUpdate arr (Just pth) (InsertChildrenRen pos nr) = 
 do { --debug Ren ("mkInsert "++show pos++" "++show nr++" on "++ show pth ++ " " ++ shallowShowArr arr) $ return ()
    ; tell $ "<div id='insert' op='insert' pos='"++show pos++"' nr='"++show nr++"'>"++htmlPath pth
    ; tell $ "</div>" 
    }
makeInsertDeleteUpdate arr (Just pth) (DeleteChildrenRen pos nr) = 
 do { --debug Ren ("mkDelete "++show pos++" "++show nr++" on "++ show pth ++ " " ++ shallowShowArr arr) $ return ()
    ; tell $ "<div id='delete' op='delete' pos='"++show pos++"' nr='"++show nr++"'>"++htmlPath pth
    ; tell $ "</div>" 
    }

htmlPath pth = "<div id='path'>"++stepsHTML++"</div>"
 where stepsHTML = concat [ "<div id='step' childNr='"++show p++"'></div>" | p <- pth ]

pathRFromPathA _                   []       = []
pathRFromPathA (StructuralA _ arr) (0:path) = pathRFromPathA arr path
pathRFromPathA (ParsingA _ arr)    (0:path) = pathRFromPathA arr path
pathRFromPathA (LocatorA _ arr)    (0:path) = pathRFromPathA arr path
pathRFromPathA (TagA _ arr)        (0:path) = pathRFromPathA arr path
pathRFromPathA arr                 (p:path) = p : pathRFromPathA (index "pathRFromPathA" (getChildrenA arr) p) path

{- inUpdate is True when renderArr is inside a replace update -}

-- first we handle the arrangements which are not rendered and hence should not cause changes to the rootPath (Maybe Path)
-- take care that these are stripped from the arrangementPaths in pathRFromPathA
renderArr :: Show node => Bool -> Scale -> (Int,Int) ->
                           (Point, Size) -> Maybe Tags -> Maybe Path -> DiffTreeArr -> Arrangement node ->
                           Writer String ()    
renderArr o s (lux, luy) v mt m dt (StructuralA _ arr) =
           renderArr o s (lux, luy) v mt m dt arr
renderArr o s (lux, luy) v mt m dt (ParsingA _ arr) =
           renderArr o s (lux, luy) v mt m dt arr
renderArr o s (lux, luy) v mt m dt (LocatorA _ arr) =
           renderArr o s (lux, luy) v mt m dt arr
renderArr o s (lux, luy) v mt m dt (TagA tags arr) =
           renderArr o s (lux, luy) v (Just tags) m dt arr

renderArr arrDb scale (lux, luy) viewedArea mt mPth diffTree arrangement =
 do { --debug Ren (shallowShowArr arrangement ++"renderArr on: "++ show (isSelfCleanDT diffTree)++":"++ show (isCleanDT diffTree)) $
        return ()
     --if True then return () else    -- uncomment this line to skip rendering

    ; case getMove diffTree of 
        (Just m) -> makeMoveUpdate mPth m
        _ -> return ()                       
    ; case getInsertDelete diffTree of 
        (Just insdel) -> makeInsertDeleteUpdate arrangement mPth insdel
        _ -> return ()                       
                                       
    ; if (isSelfCleanDTArr diffTree)  -- if self is clean, only render its children (if present)
     then if (isCleanDTArr diffTree)
          then do { --putStrLn "renderArr: self clean, children clean";
                   return ()
                  }
          else let renderChildren x' y' mtags arrs =
                    do { -- putStrLn "renderArr: self clean, children not clean"
                       ; let (x,y)=(lux+scaleInt scale x', luy+scaleInt scale y')
                       ; let childDiffTrees = case diffTree of
                                                DiffLeafArr c _   -> repeat $ DiffLeafArr c Nothing
                                                DiffNodeArr c c' _ _ dts -> dts ++ repeat (DiffLeafArr False Nothing)
                       ; sequence_ $ zipWith3 (renderArr arrDb scale (x, y) viewedArea mtags ) 
                                       (case mPth of
                                          Nothing -> repeat Nothing
                                          Just pth -> [ Just $ pth++[i] | i <- [0..] ])
                                       childDiffTrees 
                                       arrs 
                       }
               in case arrangement of
                    RowA     _ x' y' _ _ _ _ _ arrs -> renderChildren x' y' Nothing arrs
                    ColA     _ x' y' _ _ _ _ _ _ arrs -> renderChildren x' y' Nothing arrs
                    OverlayA _ x' y' _ _ _ _ _ _ arrs -> renderChildren x' y' Nothing arrs
                    -- bug: overlay children do not take into account direction

                    GraphA   _ x' y' _ _ _ _ _ _ arrs -> renderChildren x' y' Nothing arrs
                    VertexA  _ x' y' _ _ _ _ _ _ arr  -> renderChildren x' y' Nothing [arr]
                    StructuralA _ arr           -> renderChildren 0 0 mt [arr]
                    ParsingA _ arr              -> renderChildren 0 0 mt [arr]
                    LocatorA _ arr              -> renderChildren 0 0 mt [arr]
                    TagA tags arr              -> renderChildren 0 0 (Just tags) [arr]
                    _ -> return ()
     else -- in this case, all children are also dirty (as enforced by ArrUtils.diffArr)
          --when (overlap ((lux+xA arrangement, luy+yA arrangement),
          --               (widthA arrangement, heightA arrangement)) viewedArea) $
          -- only render when the arrangement is in the viewed area   
          makeReplaceUpdate mPth arrangement $
--          (\mkArr -> do {putStrLn "self dirty"; mkArr}) $
          
  case arrangement of 

    (EmptyA  id x' y' w' h' _ _ bColor) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; divOpen id x' y' w' h' bColor (mkClass mt)
        ; divClose
        }
      
    (StringA id x' y' w' h' _ vRef' str fColor bColor fnt _) ->
     do { let (x,y,w,h, vRef)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h', scaleInt scale vRef')
        ; stringHTML id str x' y' w' h' fnt fColor bColor (mkClass mt)
        }

    (ImageA id x' y' w' h' _ _ src style lColor bColor) ->
     do { imageHTML id src x' y' w' h' lColor bColor (mkClass mt)
        }

    (RectangleA id x' y' w' h' _ _ lw' style lColor fColor bColor) ->
     do { let pts = [(0,0),(w',0),(w',h'),(0,h')]
        ; polyHTML id x' y' w' h' pts (scaleInt scale lw' `max` 1) lColor fColor (mkClass mt)
        }

    (EllipseA id x' y' w' h' _ _ lw' style lColor fColor bColor) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')       
        ; -- todo: take style into account
        ; ellipseHTML id x' y' w h (scaleInt scale lw' `max` 1) lColor fColor (mkClass mt)
        }

    (PolyA id x' y' w' h' _ _ pts' lw' style lColor fColor bColor) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
       
        
       
        
        ; -- todo: take style into account & clip
        ; polyHTML id x' y' w' h' pts' (scaleInt scale lw' `max` 1) lColor fColor (mkClass mt)
        
        }


    (RowA id x' y' w' h' _ _ bColor arrs) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let childDiffTrees = case diffTree of
                                 DiffLeafArr c _   -> repeat $ DiffLeafArr c Nothing
                                 DiffNodeArr c c' _ _ dts -> dts ++ repeat (DiffLeafArr False Nothing) -- in case there are too few dts

        ; divOpen id x' y' w' h' bColor (mkClass mt)
        ; sequence_ $ zipWith (renderArr arrDb scale (x, y) viewedArea Nothing Nothing) childDiffTrees arrs
        ; divClose
        }

    (ColA id x' y' w' h' _ _ bColor _ arrs) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let childDiffTrees = case diffTree of
                                 DiffLeafArr c _    -> repeat $ DiffLeafArr c Nothing
                                 DiffNodeArr c c' _ _ dts -> dts ++ repeat (DiffLeafArr False Nothing)

        ; divOpen id x' y' w' h' bColor (mkClass mt ++ ["Column"])
        ; sequence_ $ zipWith (renderArr arrDb scale (x, y) viewedArea Nothing Nothing) childDiffTrees arrs
        ; divClose
        }

    (OverlayA id x' y' w' h' _ _ bColor direction arrs) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let childDiffTrees = case diffTree of
                                 DiffLeafArr c _    -> repeat $ DiffLeafArr c Nothing
                                 DiffNodeArr c c' _ _ dts -> dts ++ repeat (DiffLeafArr False Nothing)

        ; let order = case direction of
                        HeadInFront -> reverse
                        HeadAtBack  -> Prelude.id
              
        ; divOpen id x' y' w' h' bColor (mkClass mt)
        ; sequence_ $ order $
            zipWith (renderArr arrDb scale (x, y) viewedArea Nothing Nothing) childDiffTrees arrs
        ; divClose
        
        }

    (GraphA id x' y' w' h' _ _ bColor nrOfVertices arrs) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let childDiffTrees = case diffTree of
                                 DiffLeafArr c _    -> repeat $ DiffLeafArr c Nothing
                                 DiffNodeArr c c' _ _ dts -> dts ++ repeat (DiffLeafArr False Nothing)

        
        
        ; let (vertexDiffTrees, edgeDiffTrees) = splitAt nrOfVertices childDiffTrees
        ; let (vertexArrs, edgeArrs) = splitAt nrOfVertices arrs
        
        
        ; divOpen id x' y' w' h' bColor (mkClass mt)
        ; sequence_ $ reverse $ zipWith (renderArr arrDb scale (x, y) viewedArea Nothing Nothing) vertexDiffTrees vertexArrs -- reverse so first is drawn in front
        
        ; svgStart
        ; sequence_ $ reverse $ zipWith (renderArr arrDb scale (x, y) viewedArea Nothing Nothing) edgeDiffTrees edgeArrs -- reverse so first is drawn in front
        ; svgEnd
        ; divClose
        }

    (VertexA id x' y' w' h' _ _ bColor _ arr) ->
     do { let (x,y,w,h)=(lux+scaleInt scale x', luy+scaleInt scale y', scaleInt scale w', scaleInt scale h')
        ; let childDiffTrees = case diffTree of
                                 DiffLeafArr c _    -> repeat $ DiffLeafArr c Nothing
                                 DiffNodeArr c c' _ _ dts -> dts ++ repeat (DiffLeafArr False Nothing)
        
        ; divOpen id x' y' w' h' bColor (mkClass mt)
        ; renderArr arrDb scale (x, y) viewedArea Nothing Nothing (head' "Renderer.renderArr" childDiffTrees) arr
        ; divClose
        }

    (EdgeA id lux' luy' rlx' rly' _ _ lw' lColor) ->
     do { let (fromx, fromy, tox, toy)=(lux+scaleInt scale lux', luy+scaleInt scale luy', lux+scaleInt scale rlx', luy+scaleInt scale rly')
        ; let angleFromEnd = atan (fromIntegral (tox-fromx) / fromIntegral (toy-fromy)) -- atan works okay for pos and neg infinity
                             + if fromy > toy then pi  else 0
              
        
        
              ptHTML1 = (rlx' - round (arrowHeadSize * sin (angleFromEnd + arrowHeadHalfAngle)), rly' - round (arrowHeadSize * cos (angleFromEnd + arrowHeadHalfAngle))) 
              ptHTML2 = (rlx' - round (arrowHeadSize * sin (angleFromEnd - arrowHeadHalfAngle)), rly' - round (arrowHeadSize * cos (angleFromEnd - arrowHeadHalfAngle))) 
        -- edge will never have tag draggable
        ; edgeHTML id (lux',luy') (rlx',rly') (scaleInt scale lw' `max` 1) lColor
        ; polyHTML' id 0 0 0 0 [ptHTML1, ptHTML2, (rlx', rly')] (scaleInt scale lw' `max` 1) lColor lColor
        }

--    _ ->  return () --dcDrawText dc ("unimplemented arrangement: "++shallowShowArr arrangement) (pt lux luy)
-- rather get an exception during development
{-
  ; when arrDb $
      renderID scale (lux+xA arrangement) (luy+yA arrangement) (idA arrangement)      
-}


  }


showIDNr (IDA nr) = show nr
showIDNr NoIDA    = {- debug Err "Renderer.showIDNr: NoIDA " $ -} show (-1)
-- -2 is the rendering of the focus


mkClass (Just DragSourceTag) = ["Draggable"]
mkClass _                    = []

showClasses [] = ""
showClasses cs = " class='"++concat (intersperse " " cs)++"'"

divOpen id x y w h (r,g,b) classes = tell $ 
  "<div id='"++showIDNr id++"' style='position: absolute; left:"++show x++"px; top:"++show y++"px;"++
                "width:"++show w++"px;height:"++show h++"px;"++
                (if r /= -1 then "background-color:rgb("++show (r::Int)++","++show (g::Int)++","++show (b::Int)++");"
                           else "")++"'"++
                showClasses classes ++ 
                ">" 
divClose = tell "</div>"

 
stringHTML id str x y w h (Font fFam fSiz fBld fUnderln fItlc fStrkt) (r,g,b) (br,bg,bb) classes = tell $ 
  "<div id='"++showIDNr id++"' style='position:absolute;left:"++show x++"px;top:"++show (y)++"px;"++
                "width:"++show w++"px;height:"++show h++"px;"++
                 (if br /= -1 then "background-color:rgb("++show (br::Int)++","++show (bg::Int)++","++show (bb::Int)++");"
                           else "") ++ "'" ++ showClasses classes ++ ">"++
                                
  "<div style='position:absolute;left:0px;top:"++show (h `div` 2)++"px;"++
                --"width:"++show (w*2)++"px;"++ -- no need to set width, which also makes it easier to compute
                                                -- new width for predictive character rendering by client
                "font-family:"++show fFam++";"++
                "font-size:"++show ((fSiz *1334) `div`1000)++"px;"++
                (if fBld then "font-weight: bold;" else "")++
                (if fItlc then "font-style: italic;" else "")++
                "color:rgb("++show (r::Int)++","++show (g::Int)++","++show (b::Int)++");'>"++
                toHTML str ++ "</div></div>"

toHTML str = concatMap htmlChar str
 where --htmlChar '\n' = "<br/>"
       --htmlChar ' '  = "&#8194;"
       --htmlChar ' '  = "&nbsp;"
       htmlChar '&'  = "&amp;"
       htmlChar '<'  = "&lt;"
       htmlChar '>'  = "&gt;"
       htmlChar c    = [c]

imageHTML id src x y w h lColor (br,bg,bb) classes = tell $
  "<div id='"++showIDNr id++"' style='position:absolute;left:"++show x++"px;top:"++show (y)++"px;"++
                "width:"++show w++"px;height:"++show h++"px;"++
                 (if br /= -1 then "background-color:rgb("++show (br::Int)++","++show (bg::Int)++","++show (bb::Int)++");"
                           else "") ++
                 "background-image:url(\"/"++src++"\");'"++
                 showClasses classes ++                  
                 ">"++
  "</div>"                           

svgStart = tell $ 
  "<svg width='100%' height='100%' version='1.1' xmlns='http://www.w3.org/2000/svg'>"
svgEnd = tell $ 
  "</svg>"
  
edgeHTML id (fromX,fromY) (toX, toY) lw (lr,lg,lb) = tell $
  "<line x1='"++show fromX++"' y1='"++show fromY++"' x2='"++show toX++"' y2='"++show toY++"' "++
  "style='stroke:rgb("++show lr++","++show lg++","++show lb++");stroke-width:"++show lw++"'/>"
  
  
ellipseHTML id x y w h lw (lr,lg,lb) (fr,fg,fb) classes = tell $
  "<div id='"++showIDNr id++"' style='position: absolute; left:"++show (x-1)++"px; top:"++show (y-1)++"px;"++
                "width:"++show (w+2)++"px;height:"++show (h+2)++"px;"++
                "'"++ showClasses classes ++ ">" ++
  "<svg width='100%' height='100%' version='1.1' xmlns='http://www.w3.org/2000/svg'>" ++
  "<ellipse cx='"++show ((w `div` 2)+1)++"' cy='"++show ((h `div` 2)+1)++"' rx='"++show (w `div` 2)++"' ry='"++show (h `div` 2)++"' "++
  "style='fill:rgb("++show fr++","++show fg++","++show fb++");"++
  "stroke:rgb("++show lr++","++show lg++","++show lb++");stroke-width:"++show lw++"'/>" ++
  "</svg></div>"
-- TODO: why this max 4?

polyHTML id x y w h pts lw (lr,lg,lb) (fr,fg,fb) classes = tell $  
  "<div id='"++showIDNr id++"' style='position: absolute; left:"++show (x-1)++"px; top:"++show (y-1)++"px;"++
                "width:"++show (w+2)++"px;height:"++show ((h+2)`max` 4)++"px;"++
                "'"++ showClasses classes ++ ">" ++
  "<svg width='100%' height='100%' version='1.1' xmlns='http://www.w3.org/2000/svg'>" ++
  "<polygon points='"++pointsStr++"' "++
  "style='fill:"++(if fr == -1 then "none; "
                               else "rgb("++show fr++","++show fg++","++show fb++");")++
  "stroke:rgb("++show lr++","++show lg++","++show lb++");stroke-width:"++show lw++"'/>" ++
  "</svg></div>"
 where pointsStr = concat $ intersperse " " $ [show (x) ++ "," ++ show (y) | (x,y) <- pts ]
-- don't correct for x-1 and y-1, since poly's seems to be renderered +1 already


-- TODO: somehow the above does not work for arrowheads in Safari, this is just a quick fix
polyHTML' id x y w h pts lw (lr,lg,lb) (fr,fg,fb) = tell $  
  "<svg width='100%' height='100%' version='1.1' xmlns='http://www.w3.org/2000/svg'>" ++
  "<polygon points='"++pointsStr++"' "++
  "style='fill:"++(if fr == -1 then "none; "
                               else "rgb("++show fr++","++show fg++","++show fb++");")++
  "stroke:rgb("++show lr++","++show lg++","++show lb++");stroke-width:"++show lw++"'/>" ++
  "</svg>"
 where pointsStr = concat $ intersperse " " $ [show (x) ++ "," ++ show (y) | (x,y) <- pts ]
-- don't correct for x-1 and y-1, since poly's seems to be renderered +1 already
