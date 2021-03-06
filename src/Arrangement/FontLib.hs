{-# LANGUAGE CPP #-}
module Arrangement.FontLib where

import Common.CommonTypes
import Common.CommonUtils
#ifndef SERVER   
import Graphics.UI.Gtk hiding (FontMetrics, Font, Settings)
#endif

import Settings

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Array

import Data.Char
import Data.IORef
import System.IO
import Data.Maybe
import GHC.Float



-- use different structure to make lookup more efficient? Or is this a waste of time
type FontMetrics = Map Font (Int, Int, Array Int Float)

type FontMetricsRef = IORef FontMetrics

newFontMetricsRef :: IO FontMetricsRef
newFontMetricsRef = newIORef Map.empty 

initFontMetrics :: IO FontMetricsRef
initFontMetrics = newFontMetricsRef



#ifdef SERVER   
-- Server font querying

-- first time, fonts are not present and are put in request.txt
-- second time, fonts will be absent again, and
mkFontMetrics :: Settings -> [Font] -> IO FontMetrics
mkFontMetrics settings fonts =
 do { --putStrLn "Before reading queriedMetrics.txt"
    ; fh <- openFile "queriedMetrics.txt" ReadMode -- readFile and seq gives problems when clearing it in GUI.hs
    ; queriedFontsTxt <- hGetContents fh 
    ; seq (length queriedFontsTxt) $ return ()
    ; hClose fh
    ; let queriedFonts :: [((String, Int,Bool,Bool),(Int,Int,[Int]))]
          queriedFonts = map read $ lines queriedFontsTxt
    ; let alreadyQueried = catMaybes $ map (lookupFont queriedFonts) fonts
--    ; putStrLn "check 1"
    ; pendingQueriesTxt <-  readFile "metricsQueries.txt"
--    ; putStrLn "check 2"
    ; seq (length pendingQueriesTxt) $ return ()
    ; let pendingQueries = map read $ lines pendingQueriesTxt 
          newQueries  = fonts \\ (map fst alreadyQueried)
          queryTuples = [ (fFamily font, fSize font, fBold font, fItalic font) | font <- newQueries ]
          newQueryTuples = queryTuples \\ pendingQueries
          
--    ; putStrLn "check 3"
    ; fh' <- openFile "metricsQueries.txt" AppendMode
--    ; putStrLn "check 4"
    ; hPutStr fh' $ unlines (map show newQueryTuples)
--    ; putStrLn "check 5"
    ; hClose fh'
--    ; putStrLn "check 6"

    ; return $ Map.fromList $ map mkFontMetric alreadyQueried
    }
-- Because Underline and strikeOut have no influence on the metrics, all
-- fonts are stored in the Map with these attributes set to False.
 where mkFontMetric (f,(h,b,ws)) = 
         (f {fUnderline = False, fStrikeOut = False}, (h, b, listArray (0,223) [ fromIntegral w / 1000 | w <- ws])) 
       lookupFont queries font = case lookup (fFamily font, fSize font, fBold font, fItalic font) queries of
                                   Nothing -> Nothing
                                   Just metrics -> Just (font, metrics)

#else
-- Gtk font querying

mkFontMetrics :: Settings -> [Font] -> IO FontMetrics
mkFontMetrics settings fonts =
  fmap Map.fromList $ mapM mkFontMetric fonts
-- Because Underline and strikeOut have no influence on the metrics, all
-- fonts are stored in the Map with these attributes set to False.
 where mkFontMetric font = 
        do { (f,(h, b, ws)) <- queryFont font
           ; return $ (f {fUnderline = False, fStrikeOut = False}, (h, b, listArray (0,223) ws)) 
           }


--- query the metrics for font. 
queryFont :: Font -> IO (Font,(Int, Int, [Float]))
queryFont font =
 do { --debugLnIO Arr $ "Querying: " ++ show (fSize font) ++ " " ++ (fFamily font)
    ; context <- cairoCreateContext Nothing
    ; language <- contextGetLanguage context
    ; fontDescription <- fontDescriptionFromProximaFont font
    
    ; let allChars = map chr [32..255]
    ; widths <- mapM (\c -> do { pangoItems <- pangoItemize context [c] [ AttrFontDescription 0 255 fontDescription]
                               ; glyphItem <- pangoShape (head' "Fontlib.queryFont" pangoItems)
                               ; widths <- glyphItemGetLogicalWidths glyphItem (Just False)
                               ; return (double2Float $ head' "Fontlib.queryFont" widths)
                               })
                     allChars
    
    ; metrics <- contextGetMetrics context fontDescription language
      
    ; let ascnt = round $ ascent metrics    
          dscnt = round $ descent metrics
          hght = ascnt + dscnt
    
{-
    ; debugLnIO Arr $      "Metrics for: "++show font 
                        ++ "\nascent:   " ++ show ascnt
                        ++ "\ndescent:  " ++ show dscnt
                        ++ "\nheight: " ++ show hght
    ; debugLnIO Arr $    "\nwidths:   " ++ show widths  
-} 

    ; return (font, (hght,ascnt,widths))
    }

fontDescriptionFromProximaFont :: Font -> IO FontDescription
fontDescriptionFromProximaFont (Font fFam fSiz fBld fUnderln fItlc fStrkt) =
 do { fontDescription <- fontDescriptionNew    
    ; fontDescriptionSetFamily fontDescription fFam
    ; fontDescriptionSetStyle fontDescription (if fItlc then StyleItalic else StyleNormal) -- check if the font has italic or oblique?
    ; fontDescriptionSetVariant fontDescription VariantNormal
    ; fontDescriptionSetWeight fontDescription (if fBld then WeightBold else WeightNormal)
    ; fontDescriptionSetStretch fontDescription StretchNormal
    ; fontDescriptionSetSize fontDescription (fromIntegral fSiz)
    ; return fontDescription
    }


#endif



-- | Lookup the metrics for font. Because Underline and strikeOut have no influence on the metrics, all 
-- fonts are stored in the Map with these attributes set to False.
metricsLookup :: Font -> FontMetrics -> (Int, Int, Array Int Float)
metricsLookup font fontMetrics = 
  -- debug Err ("looking up: " ++ show (fSize font) ++ " " ++ (fFamily font)) $
  case Map.lookup (font {fUnderline = False, fStrikeOut = False}) fontMetrics  of
            Just metrics -> metrics
            Nothing      -> {- debug Err "metrics for font not queried" $ -} (20,15, listArray (0,223) (repeat 10))

    
forceEval :: Show a => a -> IO ()
forceEval a = seq (last (show a)) (return ())


textWidth :: FontMetrics -> Font -> String -> Int
textWidth fms f str = let (h,b,ws) = metricsLookup f fms
                          toWidth c = let i = ord c 
                                      in  if i < 32 then 0 else ws ! (ord c - 32)
                      in round $ sum (map toWidth str)
-- round (fromInt (length str) * charWidth fs)

-- Is it accurate enough to add the widhts of the characters? The width of the string might
-- be different due to rounding errors. We could use a stringwidth function, but this results 
-- in more communication with the renderer.
cumulativeCharWidths :: FontMetrics -> Font -> String -> [Int]
cumulativeCharWidths fms f str = let (h,b,ws) = metricsLookup f fms
                                     toWidth c = let i = ord c 
                                                 in  if i < 32 then 0 else ws ! (ord c - 32)
                                 in  map round $ scanl (+) 0 (map toWidth str)

charHeight :: FontMetrics -> Font -> Int
charHeight fms f  = let (h,b,ws) = metricsLookup f fms
                    in  (h)

baseLine :: FontMetrics -> Font -> Int
baseLine fms f = let (h,b,ws) = metricsLookup f fms
                 in  (b)
