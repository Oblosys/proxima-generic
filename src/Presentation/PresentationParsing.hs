{-# LANGUAGE GADTs, ScopedTypeVariables, NoMonoLocalBinds, FlexibleContexts #-}
module Presentation.PresentationParsing ( module Presentation.PresentationParsing
                                        , module UU.Parsing 
                                        ) where

import Common.CommonTypes
import Presentation.PresTypes
import Presentation.PresLayerTypes
import Evaluation.DocumentEdit
import UU.Parsing

import Data.Char
import Data.Maybe

import Debug.Trace

reuse = Nothing
set = Just

parsePres :: (Show enr, DocNode node, Ord token, Show token) => 
             ListParser doc enr node clip token enr -> PresentationBase doc enr node clip token level -> Maybe enr
parsePres recognizeEnrichedDoc (TokenP _ (StructuralTk _ _ _ tokens _)) = 
  let (enr,errs) = runParser recognizeEnrichedDoc tokens
  in --debug Err ("Parsing:\n"++concatMap (deepShowTks 0) (tokens) 
     --             ++"\nhas result:"++show enr ) $
     if null errs then Just enr else Nothing

parsePres _ _    = error "parsePres: scanned presentation has wrong format"

pMaybe parser = Just <$> parser `opt` Nothing

pStructuralTk nd = pSym (StructuralTk 0 (Just $ nd (error "This should not have happened") []) (EmptyP NoIDP) [] NoIDP)


applyDummyParameters nd = nd (error "This should not have happened") [] 


-- continues parsing on the children inside the structural token. the structural token is put in front
-- of the children, so reuse can be used on it just like in the normal parsers
--pStr ::  (Editable a doc enr node clip token, DocNode node, Ord token, Show token) =>
--         ListParser doc enr node clip token a -> ListParser doc enr node clip token a
pStr = pStr' (EmptyP NoIDP)

pStrVerbose str = pStr' (StringP NoIDP str)

pStr' prs p = unfoldStructure  
     <$> pSym (StructuralTk 0 Nothing prs [] NoIDP)
 where unfoldStructure structTk@(StructuralTk _ nd pr tokens _) = 
         let (res, errs) = runParser (addHoleParser p) (structTk : tokens) {- (p <|> hole/parseErr parser)-}
         in  if null errs then res else debug Err ("ERROR: Parse error in structural parser:"++(show errs)) parseErr (StructuralParseErr pr)
       unfoldStructure _ = error "NewParser.pStr structural parser returned non structural token.."

-- The scoped type variable is necessary to get hole and holeNodeConstr of the same type a.
addHoleParser :: forall a doc enr node clip token . (DocNode node, Ord token, Show token, Editable a doc enr node clip token) => ListParser doc enr node clip token a -> ListParser doc enr node clip token a 
addHoleParser p =
  p <|> hole <$ pStructuralTk (holeNodeConstr :: a -> Path -> node)
  

pStr'' :: (DocNode node, Ord token, Show token, Editable a doc enr node clip token) => 
          (a -> Path -> node) -> a -> ListParser doc enr node clip token a ->
          ListParser doc enr node clip token a
pStr'' nodeC hole p = unfoldStructure  
     <$> pSym (StructuralTk 0 Nothing (EmptyP NoIDP) [] NoIDP)
 where unfoldStructure structTk@(StructuralTk _ nd pr tokens _) = 
         let pOrHole = p <|> hole <$ pStructuralTk nodeC
             (res, errs) = runParser pOrHole (structTk : tokens) {- (p <|> hole/parseErr parser)-}
         in  if null errs then res else debug Err ("ERROR: Parse error in structural parser:"++(show errs)) parseErr (StructuralParseErr pr)
       unfoldStructure _ = error "NewParser.pStr structural parser returned non structural token.."


-- version of pStr that gets a constructor from the data type Node, that specifies
-- the type and constructor it should succeed on.
pStrAlt ndf p = unfoldStructure  
     <$> pSym (StructuralTk 0 (Just nd) (StringP NoIDP $ show nd) [] NoIDP)
 where unfoldStructure structTk@(StructuralTk _ nd pr tokens _) = 
         let (res, errs) = runParser p (structTk : tokens) {- (p <|> hole/parseErr parser)-}
          in if null errs then res else debug Err ("ERROR: Parse error in structural parser:"++(show errs)) parseErr (StructuralParseErr pr)
       unfoldStructure _ = error "NewParser.pStr structural parser returned non structural token.."
       
       nd = applyDummyParameters ndf



-- in case of a parse error, the repaired result is used in the tree and an error message
-- is sent to the prompt.
-- ? parse error is tricky, since the structural parent of the parsing subtree should know
-- when an error occurred. Instead of Maybe, we need something like Reuse|(Set x)|(ParseErr [Err])
-- for structurals, the presentation is lost on a parse error, but structural parse errors
-- are an editor design error and will not arise during document
-- editing, so it's not a problem    parseErr node (row children) errs

-- maybe it will work when there is a separate Parsing token, that contains the old value.
-- but for now, just don't set the node. Do we ever use it?

-- what about presenting parse errors in another presentation than the one with the error?
-- maybe we do want the old value for that one? Right now the parse error presentation is presented
-- so a tree can contain source text (which fails on parsing)

f <@> p = undefined

-- is this right?
pInc :: (DocNode node, Ord token, Show token) => 
        ListParser doc enr node clip token a  -> ListParser doc enr node clip token a
pInc p = pWrap f f' p
 where f'     state@(tk:_) steps k = (state, steps, k)
       f  brr state@(tk:_) steps k = 
         case getValIfUnchanged tk of
           Nothing -> (state, val (uncurry brr) steps, k)
           Just (v,nrOfToks) ->  (state, val (uncurry brr) (NoMoreSteps v), (\_ -> k (drop nrOfToks state)))
           
getValIfUnchanged :: token -> Maybe (a, Int)
getValIfUnchanged = undefined
           
pSkip :: (DocNode node, Ord token, Show token) => Int -> ListParser doc enr node clip token ()
pSkip n = pMap f f' (pSucceed ())
 where f  brr state steps = (drop n state, val (uncurry brr) steps)
       f' state steps     = (drop n state, steps)

{-
           => (forall r  r'' .   (b -> r -> r'') 
                                    -> state
                                    -> Steps (a, r) s p 
                                    -> (state -> Steps r s p) 
                                    -> (state, Steps r'' s p, state -> Steps r s p))
           -> (forall r        .   state  
                                -> Steps r s p 
                                -> (state -> Steps r s p) 
                                -> (state, Steps r s p, state -> Steps r s p)) 
           -> AnaParser state result s p a -> AnaParser state result s p b

-}

pStrDirty ::  (Editable a doc enr node clip token, DocNode node, Ord token, Show token) => ListParser doc enr node clip token (a, Dirty) -> ListParser doc enr node clip token (a, Dirty)
pStrDirty p = pStrExtra Dirty p


-- pStrExtra is a variant of pStr that allows an extra parser result to be returned in a tuple.
-- extraDefault is a default value for this type in case of a parse error.
pStrExtra ::  (Editable a doc enr node clip token, DocNode node, Ord token, Show token) =>
              b -> ListParser doc enr node clip token (a, b) -> ListParser doc enr node clip token (a, b)
pStrExtra extraDefault p = unfoldStructure  
     <$> pSym (StructuralTk 0 Nothing (EmptyP NoIDP) [] NoIDP)
 where unfoldStructure structTk@(StructuralTk _ nd pr tokens _) = 
         let (res, errs) = runParser p (structTk : tokens) {- (p <|> hole/parseErr parser)-}
         in  if null errs 
             then res 
             else debug Err ("ERROR: Parse error in structural parser:"++(show errs))
                        (parseErr (StructuralParseErr pr),extraDefault)
       unfoldStructure _ = error "NewParser.pStr structural parser returned non structural token.."

-- TODO: why do we need the 's in Editable?
pPrs ::  (Editable a doc enr node clip token, DocNode node, Ord token, Show token) => ListParser doc enr node clip token a -> ListParser doc enr node clip token a
pPrs p = unfoldStructure  
     <$> pSym (ParsingTk Nothing Nothing [] NoIDP)
 where unfoldStructure presTk@(ParsingTk _ _ tokens idP) = 
         let (res, errs) = runParser p tokens
         in  if null errs 
             then res 
             else debug Err ("ERROR: Parse error"++(show errs)) $
                  parseErr (ParsingParseErr idP (mkErrs errs) tokens 
                           (debug Err "pPrs: lexer not specified in parse error, better use parsingWithParserLexer method" LexInherited) (mkClipParser LexInherited p))
             -- the lexer is not available here. But pPrs is kind of obsolete anyway
       unfoldStructure _ = error "NewParser.pStr structural parser returned non structural token.."

{-
{-
Experimental function that adds inserted tokens.
This is quite tricky. The whitespace usually gets wrong, and we need a way to ignore the
inserted tokens on scanning. With a -666 idp, the scanner can ignore the token, but
what happens when it is edited? We don't want to ignore it then anymore, so the -666
tokens should get normal idps as soon as they are edited. This is not easy at the moment.
-}

tricky: we have to use
addInserted :: (DocNode node, Ord token, Show token) =>
               [Message (Token doc enr node clip token) (Maybe (Token doc enr node clip token)) ] ->
               [Token doc enr node clip token] -> [Token doc enr node clip token]
addInserted messages tokens = foldl  insertAt tokens inserts
 where inserts = catMaybes [ case act of 
                               Insert t -> 
                                 let insertedT = setTokenIDP (IDP (-666)) t
                                 in  case pos of
                                       Just postoken -> Just (getTokenPosition postoken, insertedT)
                                       Nothing       -> Just (Nothing, insertedT)
                               _        -> Nothing
                               
                           | (Msg _ pos act) <- messages ]
       insertAt ts     (Nothing,token) = ts ++ [token]
       insertAt []     (Just p,token)  = debug Err ("PresentationParsing.addInserted: incorrect position in token "++show token) $ []
       insertAt (t:ts) (Just p,token)  = if getTokenPosition t == Just p 
                                         then token : t : ts 
                                         else t : insertAt ts (Just p,token)
-}
                                       
mkErrs :: (DocNode node, Ord token, Show token) =>
         [Message (Token doc enr node clip token) (Maybe (Token doc enr node clip token)) ] -> 
         [ParseErrorMessage]
mkErrs []                        = []
mkErrs (msg@(Msg _ pos _):msgs)  = 
  case pos of
    Just t@(ErrorTk _ _ _) ->  [(getTokenPosition t, showMessage msg)]
    Just t ->                  (getTokenPosition t, showMessage msg) : mkErrs msgs
    Nothing ->                 (Nothing, showMessage msg)            : mkErrs msgs
     
   
getTokenPosition (UserTk p _ _ _ _)       = Just p
getTokenPosition (StructuralTk p _ _ _ _) = Just p
getTokenPosition (ErrorTk p _ _)          = Just p
getTokenPosition _                        = Nothing

-- since show for message uses show on symbol, we define a special showMessage
-- (we don't want full tokens in the error message, and a user-friendly Show Token is awkward
-- for development)
showMessage :: (DocNode node, Ord token, Show token) =>
                 Message (Token doc enr node clip token) (Maybe (Token doc enr node clip token)) -> String
showMessage (Msg expecting (Just (ErrorTk _ (c:_) _)) action) = "Lexical error at character "++show c
    -- there will always be at least one offending character
showMessage (Msg expecting position action)  
   =  "Parse error at " ++ showPosition position ++ "\n" ++
      "Expecting:   " ++ showExpecting expecting ++ "\n" ++
      "Repaired by: "  ++ showAction action ++"\n"


showPosition :: (DocNode node, Ord token, Show token) =>
                Maybe (Token doc enr node clip token) -> String
showPosition Nothing = "end of input"
showPosition (Just t) = tokenString t

showExpecting :: (DocNode node, Ord token, Show token) =>
                 Expecting (Token doc enr node clip token) -> String
showExpecting (ESym t)      = showRange t
showExpecting (EStr str)    = str
showExpecting (EOr  [])     = "Nothing expected "
showExpecting (EOr  [t])    = showExpecting t
showExpecting (EOr  (t:ts)) = showExpecting t ++ " or " ++ showExpecting (EOr ts)
showExpecting (ESeq exps)   = concat (map showExpecting exps)

showRange EmptyR      = "the empty range"
showRange (Range a b) = if a == b then tokenString a else tokenString a ++ ".." ++ tokenString b

showAction :: (DocNode node, Ord token, Show token) =>
              Action (Token doc enr node clip token) -> String
showAction (Insert t) = "inserting: " ++ tokenString t 
showAction (Delete t) = "deleting: "  ++ tokenString t 
showAction (Other t)  = t 


retrieveTokenPosition errStr messageText =
  case drop' errStr messageText of
    Just str -> Just $ read $ takeWhile isDigit str
    Nothing  -> Nothing

drop' :: Eq a => [a] -> [a] -> Maybe [a]
drop' [] ys = Just ys
drop' xs [] = Nothing
drop' xs (y:ys) = if xs `isPrefixOf` (y:ys)
                  then Just $ drop (length xs) $ y:ys
                  else drop' xs ys 
-- Does parseErr need a location? It used to be NoNode anyway.

-- hole parser
{-
       p
   <|>  (\_ -> DeclHole)
        pSym (StructuralTk (Just $ DeclHoleNode hole []) [] NoIDP)
 if we put holeNode and in Editable (maybe better in separate class Parseable)
 then
       (\_ -> hole) -- or reuse
   <$> pSym (StructuralTk (Just holeNode) [] NoIDP)


maybe just one HoleNode?

       (\_ -> hole) -- or reuse

parseErrs are not in the presentation, so we won't need ParseErrNodes

so Div (Parse Err (IntExp 1) "1_") (IntExp 2) is presented as  (StructuralTk "1_" "2")
and the node for the first child is (IntExp 1) There is never a ParseErrNode
-}















newtype ParsePres doc enr node clip token a b c = ParsePres (Presentation doc enr node clip token) deriving Show

-- parsing bits




  




instance (DocNode node, Ord token, Show token) => Symbol (Token doc enr node clip token) where

runParser (pp) inp =
      let res = parse pp inp
          (Pair v final) = evalSteps (res) 
          errs = getMsgs (res) 
      in  (v, errs)



-- parser for token
pToken :: (DocNode node, Ord token, Show token) =>
          token -> ListParser doc enr node clip token (Token doc enr node clip token)
pToken token = pSym $ UserTk 0 token (show token) Nothing (IDP (-1))



-- pCostSym expects the parser twice
pCSym c p = pCostSym c p p




strucTk   = StructuralTk 0 Nothing (EmptyP NoIDP) [] (IDP (-1))
parsingTk = (ParsingTk Nothing Nothing [] NoIDP)
graphTk   = GraphTk Dirty [] Nothing (IDP (-1)) -- probably a graph will never be inserted by
vertexTk  = VertexTk (-1) (0,0) Nothing  (IDP (-1))  -- the parser, but if it is, it should be dirty




--- Automatic structure recognizer

{-

TODO
- what do we do with non-Proxima types?
- what happens when we edit inside a structural that is in a parsing presentation? Will recognize handle this
  right? The path in the structural token will not be correct.
- Automatic structure recognizing does not work for unboxed strings in structural presentations.
   can we do something about that?
- using the press attribute for lists also causes problems if no loc and structural/parsing is added  
   maybe we should not support this attribute? It also does not handle parse errors well.
Design:
- maybe pStructural should fail if wrong type is present?
- maybe we should enforce structural and parsing in AG by using a local attribute presType?
    This way, we could also automatically supply the @self attribute for guaranteeing correct parser type.


Longer term
- When we have dirty bits in the presentation, duplicates can be handled automatically.
-}




class Construct doc enr node clip token where
  construct :: node -> (Token doc enr node clip token) -> [Maybe clip] -> clip


-- used by Xprez.parsingWithParser. Converts a parser to a ClipParser
-- does not take into account idP of parsingTk yet (for leading whitespace)
mkClipParser :: (Editable a doc enr node clip token, DocNode node, Ord token, Show token) =>
                Lexer -> ListParser doc enr node clip token a -> ClipParser doc enr node clip token
mkClipParser lexer parser = 
 let clipParser = 
       \tokens ->
         let (res, errs) = runParser parser tokens
         in  toClip $ if null errs then res 
                      else debug Prs ("Presentation parser:\n"++(show errs)) $ 
                             parseErr (ParsingParseErr NoIDP (mkErrs errs) tokens lexer clipParser)
 in  clipParser

{- recognize parses a structural token and recognizes its structure. The parser will succeed
   on any structural token.
-}
pStructural :: (Editable a doc enr node clip token, Clip clip, Construct doc enr node clip token,
              DocNode node, Ord token, Show token, Show clip) =>
             ListParser doc enr node clip token a
pStructural = pStructuralEx Nothing

{- pStructuralConstr only succeeds on a structural node with the constructor specified by nodeConstr
-}
pStructuralConstr nodeConstr = pStructuralEx (Just nodeConstr)

{- recognizeEx parses a structural token and reconizes its structure. The argument can be
   Nothing, in which case any structural token is matched, or it can be a Just node constructor,
   in which case the parser only succeeds on a structural token with that constructor.
-}   
pStructuralEx :: (Editable a doc enr node clip token, Clip clip, Construct doc enr node clip token,
                DocNode node, Ord token, Show token, Show clip) =>
                Maybe (a -> Path -> node) -> ListParser doc enr node clip token a
pStructuralEx mNodeConstr =  
          (\structuralToken -> --debug Prs ("pStructural on\n"++deepShowTks 0 structuralToken) $
                   let clip = recognizeClip structuralToken
                   in  case fromClip clip of 
                         Just enr -> enr
                         Nothing  -> error $ "Error"++show clip )
      <$> case mNodeConstr of
          Just nodeConstr -> pSym (StructuralTk 0 (Just $ nodeConstr (error "pStructural: Node argument evaluated") []) (EmptyP NoIDP) [] NoIDP)
          Nothing   -> pSym (StructuralTk 0 Nothing (EmptyP NoIDP) [] NoIDP)
          -- During parsing, only the constructor of node is taken into account. The error argument
          -- is never evaluated.

recognizeClip :: (Clip clip, Construct doc enr node clip token, DocNode node, Show token, Ord token) =>
             Token doc enr node clip token -> clip
recognizeClip strTk@(StructuralTk _ (Just node) _ childTokens _) = 
  --debug Prs ("Recognize on "++show node++" with children"++show childTokens) $
  if isListClip (construct node strTk []) 
  then 
  let thisPath = case pathNode node of
                   PathD path -> path
                   NoPathD    -> error $ "recognize: Encountered StructuralTk that has node without path:" ++ show strTk
      eltTokens = map (snd . tokenPath thisPath) childTokens -- we do the checks, but discard the numbers
      eltClips = map (Just. recognizeClip) eltTokens
  in  construct node strTk eltClips -- for lists construct does not call reuse
  else    
  let thisPath = case pathNode node of
                   PathD path -> path
                   NoPathD    -> error $ "recognize: Encountered StructuralTk that has node without path:" ++ show strTk
      numberedChildTokens = map (tokenPath thisPath) childTokens
      constructorArity = arityClip result -- no problem, since the constructor can be evaluated lazily
      initChildTokenGroups = replicate constructorArity []
      childTokenGroups = addChildTokens initChildTokenGroups numberedChildTokens 
      clipGroups       = map (map recognizeClip) childTokenGroups
      reuseArgs = [ case group of
                      (clip:_) -> Just clip -- cannot handle multiple occurrences yet. Now we just take
                                            -- the first, in the future, use dirty bits to take the updated one
                      []       -> Nothing
                  | group <- clipGroups
                  ]
      parsedChildren = map recognizeClip childTokens
      result = construct node strTk reuseArgs
  in  -- debug Prs ("\nThis path"++ show thisPath ++"\nChildren (max "++show constructorArity++"):\n"++show numberedChildTokens++"\n" ++ show childTokenGroups) $
      result
recognizeClip tk@(StructuralTk _ Nothing _ childTokens _) =
  error $ "recognize: Encountered StructuralTk without node: " ++ show tk
recognizeClip tk@(ParsingTk (Just parser) _ childTokens _) = 
  parser childTokens
recognizeClip tk@(ParsingTk Nothing _ childTokens _) = 
  error $ "recognize: Encountered ParsingTk without parser: " ++ show tk
recognizeClip tk =
  error $ "recognize: Encountered token other than StructuralTk or ParsingTk: " ++ show tk

tokenPath :: (Construct doc enr node clip token, DocNode node, Show token) => Path -> Token doc enr node clip token -> (Int, Token doc enr node clip token)
tokenPath parentPath tk =
  let node = case tk of 
               (StructuralTk _ (Just node) _ _ _) -> node
               (StructuralTk _ Nothing _ _ _)     -> error $ "tokenPath: childToken without node" ++ show tk
               (ParsingTk _ (Just node) _ _) -> node
               (ParsingTk _ Nothing _ _)     -> error $ "tokenPath: childToken without node" ++ show tk
  in case pathNode node of
       PathD path -> if parentPath `isPrefixOf` path 
                     then case drop (length parentPath) path of
                            [childNr] -> (childNr, tk)
                            _ -> error $ "encountered token that is not a child: tokenPath=" ++show path ++ 
                                         " parentPath=" ++ show parentPath ++ " token=" ++ show tk
                     else error $ "encountered token that is not a child: tokenPath=" ++show path ++ 
                                  " parentPath=" ++ show parentPath ++ " token=" ++ show tk
       NoPathD    -> error $ "tokenPath: childToken has node without path: " ++ show tk

addChildTokens childTokenGroups []  = childTokenGroups
addChildTokens childTokenGroups (childToken: childTokens) =
  addChildTokens (addChildToken childTokenGroups childToken) childTokens
 where addChildToken childTokenGroups (nr, childToken) =
         case splitAt nr childTokenGroups of
           (left, group:right) -> left ++ (group ++ [childToken]) : right
           _                   -> error $ "addChildToken: encountered child with number larger than parent's arity: nr="++show nr ++ " token="++show childToken 

     
-- If the argument is nothing, return Nothing (for reuse), otherwise apply fromClip to the
-- argument.
retrieveArg :: (Editable a doc enr node clip token, Show clip) => 
               String -> String -> Maybe clip -> Maybe a
retrieveArg parentCnstrName expectedTypeName (Just clip) =
  case fromClip clip of
    Just x  -> Just x
    Nothing -> debug Err ("\n\n\nCritical structural parse error: retrieveArg: Type error in "++parentCnstrName++
                          ". Encountered " ++ show clip ++ " while expecting "++expectedTypeName) $
                 Nothing
retrieveArg _ _ Nothing     = Nothing
-- retrieveArg fails if the ClipParser for a child produced a clip of the wrong type.
-- Because parsingWithParser @self guarantees the parser has the correct type, this
-- error should not occur. (For structural presentations, the type is taken from the node
-- that is put in the presentation automatically with the loc application in lhs.pres


genericConstruct_List typeName toList clips =
  toClip $ toList $ catMaybes $ map (retrieveArg ("List_"++typeName) typeName) clips
