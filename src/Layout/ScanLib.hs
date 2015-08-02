-----------------------------------------------------------------------------------------
{-| Module      : Layout.ScanLib
    Copyright   : (c) 2007 Martijn Schrage
    License     : All Rights Reserved

    Maintainer  : martijn@cs.uu.nl
    Stability   : experimental
    Portability : 

This module is imported by the Alex scanner. Because of the module exports, the scanner
only needs to import this module (and DocTypes_Generated for the token data type).

Some scanner functionality could not be factorized and can be found in AlexTemplate-ghc

-}
-----------------------------------------------------------------------------------------

module Layout.ScanLib ( module Layout.ScanLib
                      , module Data.Maybe
                      , module Common.DebugLevels
                      , module Layout.LayLayerTypes
                      , module Layout.LayLayerUtils
                      )  where

import Common.CommonTypes
import Common.DebugLevels
import Layout.LayLayerTypes
import Layout.LayLayerUtils hiding (empty)
import qualified Data.Map as Map hiding (mapMaybe, (!))
import Data.Maybe
import Data.Word (Word8)
import Data.Bits


type AlexInputAbs doc enr node clip token = (Char, [Byte], [ScanChar doc enr node clip token])

-- Code copied from .cabal/share/x86_64-osx-ghc-7.8.3/alex-3.1.4/AlexWrapper-basic, and
-- modified to encode structural tokens as \255 characters.

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

type Byte = Word8

alexGetByte :: AlexInputAbs doc enr node clip token -> Maybe (Byte, AlexInputAbs doc enr node clip token)
alexGetByte (c,(b:bs),s) = Just (b,(c,bs,s))
alexGetByte (c,[],[])    = Nothing
alexGetByte (_,[],(c:s)) = 
  let c' = encodeStructural c
  in  case utf8Encode c' of
        (b:bs) -> Just (b, (c', bs, s))
        [] -> Nothing
  where encodeStructural (Char _ _ _ _ c)          = c
        encodeStructural (Structural _ _ _ _ _ _) ='\255'

alexInputPrevChar (c,_,_) = c

-- end of AlexWrapper-basic code

mkToken = mkTokenEx id

-- the first strf is for manipulating the string that is stored in the token
mkTokenEx :: (String->String) -> (String -> userToken) -> ScannerState -> [ScanChar doc enr node clip userToken] -> 
           (ScannedToken doc enr node clip userToken, ScannerState)
mkTokenEx strf tokf (startCode,tokenPos) scs = 
  let str = strf $ stringFromScanChars scs
      idp = idPFromScanChars scs
      loc = locFromScanChars scs
      userToken = tokf str
                                                   
  in  ( ScannedToken (getFocusStartEnd scs) $ UserTk tokenPos userToken str loc idp
      , (startCode,tokenPos + 1)
      )

collectWhitespace :: ScannerState -> [ScanChar doc enr node clip userToken] -> 
                     (ScannedToken doc enr node clip userToken, ScannerState)
collectWhitespace tokenPos scs =
  let whitespaceChars = stringFromScanChars scs
      scannedWhitespace = ( length (filter (=='\n') whitespaceChars)
                          , length (takeWhile (==' ') (reverse whitespaceChars))
                          )
  in  ( ScannedWhitespace (getFocusStartEnd scs) scannedWhitespace
      , tokenPos
      )
