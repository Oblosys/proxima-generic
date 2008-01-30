-----------------------------------------------------------------------------------------
{-| Module      : Scanner
    Copyright   : (c) 2007 Martijn Schrage
    License     : All Rights Reserved

    Maintainer  : martijn@cs.uu.nl
    Stability   : experimental
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Scanner where

import CommonTypes
import LayLayerTypes
import LayLayerUtils
import qualified Data.Map as Map

import ScannerAG


{-
Challenges:

passing several Alex scanners
handling tokenization of structural presentations in the parsing presentation

Optimizations:
- use . for string concatenation
-}





tokenizeLay :: (Show token) =>
               ScannerSheet doc node clip token -> state -> LayoutLevel doc node clip ->
               PresentationLevel doc node clip token -> (EditPresentation docLvl doc node clip token , state, LayoutLevel doc node clip)
tokenizeLay scannerSheet state layLvl@(LayoutLevel lay focus dt) (PresentationLevel _ (layout, idCounter)) = 
 let (pres, layout', idCounter') = scannerSheet idCounter lay
     (dummyPres,whitespaceMap) = scanStructural LexHaskell Nothing lay 
     forceTypes = [dummyPres, pres]
     presLvl' = PresentationLevel pres (layout',idCounter')
 in  (length $ show dummyPres) `seq`
     (SetPres presLvl', state, layLvl)

{-
tokenize traverses the structural parts of the tree, calling scanPresentation on Parsing subtrees.
It is a simple AG:

inherited attribute   lx: the lexer of                   (simply copied down)
                      loc: the nearest parent locator    (assigned at LocatorP case)
synthesized attribute pres: the tokenized presentation   (constructed at every case because of cast from Layout to Presentation)
                      wm: the whitespace map             (copied except in List, where the union is taken)
-}
scanStructural :: Lexer -> Maybe node -> Layout doc node clip -> (Presentation doc node clip token, WhitespaceMap)
scanStructural lx loc prez =
  case prez of
    ParsingP idP lx' pres'      -> scanPresentation lx loc idP lx' pres'
    EmptyP idd                  -> (EmptyP idd,               Map.empty)
    StringP idd str             -> (StringP idd str,          Map.empty)
    ImageP idd istr st          -> (ImageP idd istr st,       Map.empty)
    PolyP idd pts w st          -> (PolyP idd pts w st,       Map.empty)
    RectangleP idd w h lw st    -> (RectangleP idd w h lw st, Map.empty)
    EllipseP idd w h lw st      -> (EllipseP idd w h lw st,   Map.empty)
    RowP id rf press            -> let (press', wm) = scanStructuralList lx loc press
                                   in  (RowP id rf press',     wm)
    ColP id rf f press          -> let (press', wm) = scanStructuralList lx loc press
                                   in  (ColP id rf f press',   wm)
    OverlayP id press           -> let (press', wm) = scanStructuralList lx loc press
                                   in  (OverlayP id press',    wm)
    GraphP id d w h edges press -> let (press', wm) = scanStructuralList lx loc press
                                   in  (GraphP id d w h edges press', wm)
    FormatterP id press         -> let (press', wm) = scanStructuralList lx loc press
                                   in  (FormatterP id press', wm)
    WithP ar pres               -> let (pres', wm) = scanStructural lx loc pres
                                   in  (WithP ar pres',        wm)
    StructuralP id pres         -> let (pres', wm) = scanStructural lx loc pres
                                   in  (StructuralP id pres',  wm)
    VertexP id v x y o pres     -> let (pres', wm) = scanStructural lx loc pres
                                   in  (VertexP id v x y o pres', wm)
    LocatorP newLoc pres        -> let (pres', wm) = scanStructural lx (Just newLoc) pres
                                   in  (LocatorP newLoc pres', wm)
    pr -> debug Err ("Scanner.scanStructural: can't handle "++ show pr) (castLayToPres pr, Map.empty)


scanStructuralList :: Lexer -> Maybe node -> [Layout doc node clip] ->
                      ([Presentation doc node clip token], WhitespaceMap)
scanStructuralList lx loc []           = ([], Map.empty)
scanStructuralList lx loc (pres:press) = let (pres',  lm0) = scanStructural lx loc pres
                                             (press', lm1) = scanStructuralList lx loc press
                                         in  (pres':press', lm1 `Map.union` lm0)


scanPresentation inheritedLex loc idP presentationLex pres =
 let lex = case  presentationLex of
             LexInherited -> inheritedLex
             _            -> presentationLex
     (self,str)    = sem_Layout pres loc
     tokenizedPres = undefined
     whitespaceMap     = undefined
 in  debug Lay (str) $
     (ParsingP idP presentationLex $ StringP NoIDP str, whitespaceMap)
-- in  (ParsingP idP presentationLex $ empty, whitespaceMap)