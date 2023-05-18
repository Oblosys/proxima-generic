-- -----------------------------------------------------------------------------
-- Proxima wrapper (src/proxima/src/Layout/AlexWrappers.hs)
--
-- Different wrappers are selected with #ifdef's, so the wrapper name does not matter and we can include this file with:
--
--   %wrapper "basic"
-- 

type ScanChar_ = ScanChar Document EnrichedDoc Node ClipDoc UserToken
-- ScanChar_ is ScanChar applied to its parameter types. This is necessary, because Alex
-- cannot handle parameters in the AlexInput type.

type AlexInput = AlexInputAbs Document EnrichedDoc Node ClipDoc UserToken

-- the scanner does not yet return the lexerState (or start code). Thus, style or structural tokens will break stateful scanning
-- such as in strings

scanner :: ScannerState -> [ScanChar_] ->
           ([ScannedToken Document EnrichedDoc Node ClipDoc UserToken], ScannerState)
scanner initState scs = -- debug Lay ("scanning on "++stringFromScanChars scs++" state: "++show initState) $ 
                        go initState ('\n',[],scs)
  where go :: ScannerState -> AlexInput -> ([ScannedToken Document EnrichedDoc Node ClipDoc UserToken], ScannerState)
        go state@(startCode,pos) inp@(_, _, str) =
          case alexScan inp startCode of
            AlexEOF                   -> ([], state)
            AlexError (_,_,remaining) -> ( [ScannedToken (getFocusStartEnd remaining)
                                             (ErrorTk pos (stringFromScanChars str) NoIDP) ]
                                         , state)
            AlexSkip  inp' len        -> go state inp'
            AlexToken inp' len act    -> let (scannedToken, state') = act state (take len str)
                                             (scannedTokens, state'') = go state' inp'
                                         in  (scannedToken : scannedTokens, state'')  

-- End of Proxima wrapper
-- -----------------------------------------------------------------------------
