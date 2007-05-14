module ArrTypes where

import CommonTypes
import DocTypes (DocumentLevel) -- for Locations

import PresTypes
                  
data IDA = NoIDA | IDA Int deriving (Show, Read, Eq, Ord)
               
                                                              -- ugly hack for popups, need pres to get items
data ArrangementLevel doc node clip = ArrangementLevel (Arrangement node) FocusArr (Presentation doc node clip) deriving Show

data EditArrangement' doc node clip =
    SetArr' (ArrangementLevel doc node clip)
  | SkipArr' Int deriving Show

data EditArrangement documentLevel =
    SkipArr Int
  | SetFocusArr FocusArr
  | InitArr
  | CloseArr
  | CutArr
  | CopyArr
  | PasteArr
  | DeleteArr -- probably don't need delete because right delete can take its function
  | SplitArr
  | LeftDeleteArr
  | RightDeleteArr
  | LeftArr
  | RightArr
  | EnlargeLeftArr
  | EnlargeRightArr
  | NormalizeArr
  | TestArr
  | Test2Arr
  | KeyCharArr Char
  | KeySpecialArr SpecialKey Modifiers
  | MouseDownArr PathArr Modifiers Int
  | MouseDragArr PathArr Modifiers 
  | MouseUpArr PathArr Modifiers
  | OpenFileArr String
  | SaveFileArr String
  | UpdateDocArr (documentLevel -> documentLevel) -- should encapsulate these so they automatically go to doc level
  | NavUpDocArr
  | NavDownDocArr
  | NavLeftDocArr
  | NavRightDocArr
  | CutDocArr
  | CopyDocArr
  | PasteDocArr
  | DeleteDocArr                                  --
  | MouseDownDocArr PathArr Modifiers Int -- bit hacky, will disappear
  | DocumentLoadedArr String deriving Show


-- node is parameter for Node type
data Arrangement node =
    EmptyA      !IDA  !XCoord !YCoord !Width !Height !HRef !VRef
  | StringA     !IDA !XCoord !YCoord !Width !Height !HRef !VRef !String !Color !Font [Int]
  | ImageA      !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !String !ImgStyle !Color !Color
  | PolyA       !IDA  !XCoord !YCoord !Width !Height !HRef !VRef ![(XCoord, YCoord)] !Int !Color !Color
  | RectangleA  !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !Int !Style !Color !Color
  | EllipseA    !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !Int !Style !Color !Color
  | LineA       !IDA  !XCoord !YCoord !XCoord !YCoord !HRef !VRef !Int !Color
  | RowA        !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !Color ![Arrangement node]
  | ColA        !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !Color ![Arrangement node]
  | OverlayA    !IDA  !XCoord !YCoord !Width !Height !HRef !VRef !Color ![Arrangement node]
  | StructuralA !IDA  !(Arrangement node)
  | ParsingA    !IDA  !(Arrangement node)
  | LocatorA    node !(Arrangement node) deriving (Show, Read) -- do we want a ! for location  ?  
  -- make some choices here: lower right/or width height, w h/(w,h)
  -- | matrix is different from col of rows, even in arrangement (e.g. selection)


-- TODO put dirty info in Id child
--type Info = (Bool, Bool)   -- True is Clean, False is Dirty       --(selfandChildren, self)
type XCoord = Int
type YCoord = Int
type Width = Int
type Height = Int
type HRef = Int
type VRef = Int
data Style = Solid | Transparent deriving (Show, Eq, Read)


-- empty will have size when it is stretched. But maybe empty should be ignored in the arrangement,
-- although it might have a background color. Check out what desired focus behaviour is when navigating
-- over empty's

-- Strings have no background color! this is illogical
-- need a transparent color

-- *** having a WithA makes the arrangement a lot smaller and rendering faster. Are there objections?

-- *** a more logical data structure might be one in which rows and columns contain the positions 
-- of the children, and elements have no position of their own.
-- How does that affect dirty bits? And locators/with nodes?


-- Lines are different from all other arrangement elements with their corner coordinates. 
-- Note:  Line from x to x+w includes both x and x+w whereas box line from x to x+w does not include points on x+w

-- should Empty have x y w and h
-- Can empties have nonzero size? Background color? Focus? If they can they're not really neutral
-- elements anymore. This is also a presentation question.
-- maybe Empties should not even be part of the arrangement. Still, in the presentation, we want to have something
-- like stretching glue. using a filled rectangle is not what we want.
-- locators need coordinates! so we can show them in debug arrangements nicely


-- overlay problem needs to be solved. Sometimes front object is not the parsed one
-- Workaround hack: if last elt is empty, the overlay is presented in reverse order, but parsed normally
-- so overlay [Exp, squiggly, empty] is presented with squiggly in front


-- last argument of StringA is a list of offsets of the characters in the string. 
-- TODO: don't want cumulative character widths           (? why not?)
-- TODO: background for StringA, RectangleA, and EllipseA




-- do we really need that index?
data PathArr = PathA [Int] Int 
              | NoPathA deriving (Show, Eq, Ord)


-- focus? (from, to)? yes, seems ok, now pres/formatting difference is not very big. even formatter follows normal
-- directions, so from to in arr is from to in pres. Maybe for more complicated presentations we can keep this property


data FocusArr = FocusA PathArr PathArr
              | NoFocusA deriving Show 

-- smart constructor

focusA from@(PathA _ _) to@(PathA _ _) = FocusA from to
focusA _ _                             = NoFocusA

-- selectors

focusAL (ArrangementLevel _ f _) = f


fromA (FocusA from _) = from
fromA NoFocusA        = NoPathA

toA (FocusA _ to) = to
toA NoFocusA        = NoPathA

xA (EmptyA _ x y w h _ _)             = x
xA (StringA _ x y w h _ _ _ _ _ _)    = x
xA (ImageA _ x y w h _ _ _ _ _ _)     = x
xA (PolyA _ x y w h _ _ _ _ _ _)      = x
xA (RectangleA _ x y w h _ _ _ _ _ _) = x
xA (EllipseA _ x y w h _ _ _ _ _ _)   = x
xA (LineA _ x y x' y' _ _ _ _)        = x
xA (RowA _ x y w h _ _ _ _)           = x
xA (ColA _ x y w h _ _ _ _)           = x
xA (OverlayA _ x y w h _ _ _ _)       = x
xA (StructuralA _ child)          = xA child -- inefficient, but there probably won't be many chains
xA (ParsingA _ child)             = xA child --
xA (LocatorA location child)      = xA child -- 
xA arr                            = debug Err ("ArrTypes.xA: unhandled arrangement "++show arr) 0

yA (EmptyA _ x y w h _ _)             = y
yA (StringA _ x y w h _ _ _ _ _ _)    = y
yA (ImageA _ x y w h _ _ _ _ _ _)     = y
yA (PolyA _ x y w h _ _ _ _ _ _)      = y
yA (RectangleA _ x y w h _ _ _ _ _ _) = y
yA (EllipseA _ x y w h _ _ _ _ _ _)   = y
yA (LineA _ x y x' y' _ _ _ _)        = y
yA (RowA _ x y w h _ _ _ _)           = y
yA (ColA _ x y w h _ _ _ _)           = y
yA (OverlayA _ x y w h _ _ _ _)       = y
yA (StructuralA _ child)          = yA child
yA (ParsingA _ child)             = yA child
yA (LocatorA location child)      = yA child
yA arr                            = debug Err ("ArrTypes.yA: unhandled arrangement "++show arr) 0

widthA (EmptyA _ x y w h _ _)             = w
widthA (StringA _ x y w h _ _ _ _ _ _)    = w
widthA (ImageA _ x y w h _ _ _ _ _ _)     = w
widthA (PolyA _ x y w h _ _ _ _ _ _)      = w
widthA (RectangleA _ x y w h _ _ _ _ _ _) = w
widthA (EllipseA _ x y w h _ _ _ _ _ _)   = w
widthA (LineA _ x y x' y' _ _ _ _)        = x'-x
widthA (RowA _ x y w h _ _ _ _)           = w
widthA (ColA _ x y w h _ _ _ _)           = w
widthA (OverlayA _ x y w h _ _ _ _)       = w
widthA (StructuralA _ child)          = widthA child
widthA (ParsingA _ child)             = widthA child
widthA (LocatorA location child)      = widthA child
widthA arr                            = debug Err ("ArrTypes.widthA: unhandled arrangement "++show arr) 0

heightA (EmptyA _ x y w h _ _)             = h
heightA (StringA _ x y w h _ _ _ _ _ _)    = h
heightA (ImageA _ x y w h _ _ _ _ _ _)     = h
heightA (PolyA _ x y w h _ _ _ _ _ _)      = h
heightA (RectangleA _ x y w h _ _ _ _ _ _) = h
heightA (EllipseA _ x y w h _ _ _ _ _ _)   = h
heightA (LineA _ x y x' y' _ _ _ _)        = y'-y
heightA (RowA _ x y w h _ _ _ _)           = h
heightA (ColA _ x y w h _ _ _ _)           = h
heightA (OverlayA _ x y w h _ _ _ _)       = h
heightA (StructuralA _ child)          = heightA child
heightA (ParsingA _ child)             = heightA child
heightA (LocatorA location child)      = heightA child
heightA arr                            = debug Err ("ArrTypes.heightA: unhandled arrangement "++show arr) 0

hRefA (EmptyA _ x y w h hr vr)             = hr
hRefA (StringA _ x y w h hr vr _ _ _ _)    = hr
hRefA (ImageA _ x y w h hr vr _ _ _ _)     = hr
hRefA (PolyA _ x y w h hr vr _ _ _ _)      = hr
hRefA (RectangleA _ x y w h hr vr _ _ _ _) = hr
hRefA (EllipseA _ x y w h hr vr _ _ _ _)   = hr
hRefA (LineA _ x y x' y' hr vr _ _)        = hr
hRefA (RowA _ x y w h hr vr _ _)           = hr
hRefA (ColA _ x y w h hr vr _ _)           = hr
hRefA (OverlayA _ x y w h hr vr _ _)       = hr
hRefA (StructuralA _ child)          = hRefA child
hRefA (ParsingA _ child)             = hRefA child
hRefA (LocatorA location child)      = hRefA child
hRefA arr                            = debug Err ("ArrTypes.hRefA: unhandled arrangement "++show arr) 0
 
vRefA (EmptyA _ x y w h hr vr)             = vr
vRefA (StringA _ x y w h hr vr _ _ _ _)    = vr
vRefA (ImageA _ x y w h hr vr _ _ _ _)     = vr
vRefA (PolyA _ x y w h hr vr _ _ _ _)      = vr
vRefA (RectangleA _ x y w h hr vr _ _ _ _) = vr
vRefA (EllipseA _ x y w h hr vr _ _ _ _)   = vr
vRefA (LineA _ x y x' y' hr vr _ _)        = vr
vRefA (RowA _ x y w h hr vr _ _)           = vr
vRefA (ColA _ x y w h hr vr _ _)           = vr
vRefA (OverlayA _ x y w h hr vr _ _)       = vr
vRefA (StructuralA _ child)          = vRefA child
vRefA (ParsingA _ child)             = vRefA child
vRefA (LocatorA location child)      = vRefA child
vRefA arr                            = debug Err ("ArrTypes.vRefA: unhandled arrangement "++show arr) 0

-- use named fields?



idA (EmptyA id x y w h _ _)             = id
idA (StringA id x y w h _ _ _ _ _ _)    = id
idA (ImageA id x y w h _ _ _ _ _ _)     = id
idA (PolyA id x y w h _ _ _ _ _ _)      = id
idA (RectangleA id x y w h _ _ _ _ _ _) = id
idA (EllipseA id x y w h _ _ _ _ _ _)   = id
idA (LineA id x y x' y' _ _ _ _)        = id
idA (RowA id x y w h _ _ _ _)           = id
idA (ColA id x y w h _ _ _ _)           = id
idA (OverlayA id x y w h _ _ _ _)       = id
idA (StructuralA _ child)          = idA child
idA (ParsingA _ child)             = idA child
idA (LocatorA location child)      = idA child
idA arr                            = debug Err ("ArrTypes.idA: unhandled arrangement "++show arr) NoIDA




setXYWHA x y w h (EmptyA id _ _ _ _ hr vr)                        = EmptyA id x y w h  hr vr          
setXYWHA x y w h (StringA id _ _ _ _ hr vr str c f cxs)          = StringA id x y w h hr vr str c f cxs
setXYWHA x y w h (ImageA id _ _ _ _ hr vr src style lc bc)    = ImageA id x y w h hr vr src style lc bc        
setXYWHA x y w h (PolyA id _ _ _ _ hr vr  pts lw lc bc)        = PolyA id x y w h hr vr pts lw lc bc            
setXYWHA x y w h (RectangleA id _ _ _ _ hr vr  lw style lc fc) = RectangleA id x y w h hr vr lw style lc fc     
setXYWHA x y w h (EllipseA id _ _ _ _ hr vr  lw style lc fc)   = EllipseA id x y w h hr vr lw style lc fc     
setXYWHA x y w h (RowA id _ _ _ _ hr vr  c arrs)               = RowA id x y w h hr vr c arrs                   
setXYWHA x y w h (ColA id _ _ _ _ hr vr  c arrs)               = ColA id x y w h hr vr c arrs                   
setXYWHA x y w h (OverlayA id _ _ _ _ hr vr  c arrs)           = OverlayA id x y w h hr vr c arrs               
setXYWHA x y w h (LocatorA location arr)                = LocatorA location $ setXYWHA x y w h arr
setXYWHA x y w h (StructuralA id arr)                   = StructuralA id $ setXYWHA x y w h arr
setXYWHA x y w h (ParsingA id arr)                      = ParsingA id    $ setXYWHA x y w h arr                         
setXYWHA _ _ _ _ arr                                    = debug Err ("ArrTypes.setXYWHA: unimplemented arrangement: "++show arr) arr
