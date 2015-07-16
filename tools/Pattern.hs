-- hat-observe permits matching of candidate equations against a pattern.
-- This requires a parser to convert a pattern from a string into an SExp,
-- and a comparison function on SExps.
module Pattern
  ( topMatchPat
  , matchPat
  , lexPat
  , parsePat
  ) where

import Prelude hiding (lex)
import Data.List	(isPrefixOf)
import Data.Char	(isDigit,isAlphaNum,isSpace,isUpper,isLower)
import SExp             (SExp(..),SFixity(..),QName(..),showQN)
import Text.ParserCombinators.Poly



-- Comparison of an expression against a pattern.
-- All SExp labels are ignored.
-- First arg is candidate, second is pattern.
matchPat :: SExp a -> SExp b -> Bool
matchPat _                 (SUnevaluated _)  = True
matchPat (SApp _ as)       (SApp _ bs)       = and (zipWith matchPat as bs)
matchPat (SId _ a _)       (SId _ b _)       = a==b
matchPat (SApp _ (a:_))  b@(SId _ _ _)       = matchPat a b
matchPat (SLiteral _ a)    (SLiteral _ b)    = a==b
matchPat (SString _ a _)    b                = matchString a b
matchPat (SLambda _)       (SLambda _)       = True
matchPat (SBottom _)       (SBottom _)       = True
matchPat (SEquation _ a b) (SEquation _ c d) = matchPat a c && matchPat b d
matchPat (SEquation _ a b) p                 = matchPat a p
matchPat (SFieldExpr _ e fs vs) (SFieldExpr _ e' f's v's) = matchPat e e'
                                                && matchFields (zip fs vs)
                                                               (zip f's v's)
matchPat (SFieldExpr _ e fs vs) (SApp _ (a:as)) = matchPat e a
                                                && and (zipWith matchPat vs as)
	-- above is only correct if applied field order matches pattern order
matchPat (SFieldExpr _ e fs vs) b@(SId _ _ _)= matchPat e b
matchPat _                 _                 = False

-- No matches for SWithin, SIf, SCase, SGuard, SCut, because
-- you can't specify such a pattern.

matchString :: String -> SExp a -> Bool
matchString []     (SId _ (Qualified _ "[]") _) = True
matchString (c:cs) (SApp _ [SId _ (Qualified _ ":") _, SLiteral _ c', tl]) =
        c' == show c  &&  matchString cs tl
matchString _      _ = False

matchFields :: [(String, SExp a)] -> [(String, SExp b)] -> Bool
matchFields candidate pattern =
    and (map (\ (f,e)-> case lookup f candidate of
                          Nothing -> False
                          Just e' -> matchPat e' e )
             pattern)

-- A toplevel match must not compare the fun position (to allow for
-- oversaturated CAF apps).
topMatchPat :: SExp a -> SExp b -> Bool
topMatchPat (SApp _ (a:as)) (SApp _ (b:bs))     = and (zipWith matchPat as bs)
topMatchPat (SEquation _ a b) (SEquation _ c d) = topMatchPat a c && matchPat b d
topMatchPat (SEquation _ a b) p                 = topMatchPat a p
topMatchPat a b                                 = matchPat a b


-- Simple FSM lexer for patterns.
data Token
  = Variable QName
  | Constructor QName
  | InfixVariable QName
  | InfixConstructor QName
  | Numeric String
  | String String
  | Char String
  | OpenParen
  | CloseParen
  | OpenBracket
  | CloseBracket
  | Comma
  | Underscore
  | Bottom
  | Equal
  | In
  | OpenBrace
  | CloseBrace
  | End
  | Error String
  deriving (Eq, Show)

gather :: (Char->Bool) -> (String->Token) -> (String->[Token])
                   -> String -> (String->[Token])
gather pred tok k acc [] = tok (reverse acc): k []
gather pred tok k acc (c:cs)
    | pred c    = tok (reverse acc): k (c:cs)
    | otherwise = gather pred tok k (c:acc) cs

lexPat :: String -> [Token]
lexPat [] = [End]
lexPat (x:xs)
  | x=='('     = OpenParen:    lexPat xs
  | x==')'     = CloseParen:   lexPat xs
  | x=='['     = OpenBracket:  lexPat xs
  | x==']'     = CloseBracket: lexPat xs
  | x=='{'     = OpenBrace:    lexPat xs
  | x=='}'     = CloseBrace:   lexPat xs
  | x==','     = Comma:        lexPat xs
  | x=='_' && "|_" `isPrefixOf` xs
               = Bottom:       lexPat (drop 2 xs)
  | x=='_' && (null xs || not (isVariable (head xs)))
               = Underscore:   lexPat xs
  | x=='=' && (null xs || not (isSymbol (head xs)))
               = Equal:        lexPat xs
  | x=='i' && "n " `isPrefixOf` xs
               = In:           lexPat (drop 2 xs)
  | isSpace x  = lexPat xs
  | isDigit x  = gather (not.isNumeric) Numeric lexPat [x] xs
  | isLower x || x=='_'		-- already checked for wildcard above
               = gather (not.isVariable) (plain Variable) lexPat [x] xs
  | isUpper x  = gather (not.isQVariable) qname lexPat [x] xs
  | isSymbol x = gather (not.isSymbol) (plain InfixVariable) lexPat [x] xs
  | x==':'     = gather (not.isSymbol) (plain InfixConstructor) lexPat [x] xs
  | x=='`'     = gather (=='`') (plain InfixVariable) (lexPat.tail) [] xs
  | x=='\''    = gather (=='\'') Char (lexPat.tail) [] xs
  | x=='"'     = gather (=='"') String (lexPat.tail) [] xs
  | otherwise  = [Error (x:xs)]

isSymbol, isNumeric, isVariable, isQVariable :: Char -> Bool
isSymbol x    = x `elem` "!#$%&*+./<=>?@\\^|-~"
isNumeric x   = isDigit x    || x `elem` ".eE"
isVariable x  = isAlphaNum x || x `elem` "_'"
isQVariable x = isVariable x || isSymbol x

plain :: (QName->Token) -> String -> Token
plain typ s = typ (Plain s)

qname :: String -> Token
qname str
  | null n'    = Constructor (Plain q)
  | isUpper c  = Constructor (Qualified q n)
  | isLower c || c=='_'
               = Variable (Qualified q n)
  | isSymbol c = InfixVariable (Qualified q n)
  | c==':'     = InfixConstructor (Qualified q n)
  | otherwise  = Error str
  where (q,n') = break (=='.') str
        n = tail n'
        c = head n


-- The Parser part:

-- Derived combinator used frequently
tok   :: Token -> Parser Token Token
tok t = do 
  x <- next
  if t==x then return t 
          else fail ("Parsing failed. Expected " ++ show t ++ " but found " ++ show x)


-- Parser from [Token] to SExp.
parsePat :: [Token] -> (Either String (SExp ()), Maybe QName)
parsePat tokens = case runParser context tokens of
  (Left err, restTokens) -> (Left err, Nothing)
  (Right (exp,ctx), [])  -> (Right exp, ctx)
  _                      -> (Left "tokens left for parsing", Nothing)
 
{-
parsePat tokens = case papply context tokens of
                    [((exp,ctx),[End])] -> (Right exp, ctx)
                    [(_,    [Error s])] -> (Left s,  Nothing)
                    _                   -> (Left "ambiguous", Nothing)
-}

context  :: Parser Token (SExp (), Maybe QName)
context =
    do eqn <- equation
       ctx <- ( do tok In
                   Variable ctx  <- next
                   return (Just ctx) ) <|>
              ( do return Nothing )
       return (eqn,ctx)
            
equation :: Parser Token (SExp ())
equation =
    do lhs <- pattern
       ( ( do tok Equal
              rhs <- pattern
              return (SEquation () lhs rhs) ) <|>
         ( do return lhs ) )

pattern :: Parser Token (SExp ())
pattern =
  ( do tok OpenParen
       tok CloseParen
       return (SId () (Plain "()") SInfixDefault) ) <|>
  ( do tok OpenParen
       (SId () v (SInfix 0)) <- atom
       tok CloseParen
       return (SId () v SInfixDefault) ) <|>
  ( do tok OpenParen
       p <- pattern
       tok CloseParen
       return p ) <|>
  ( do ps <- many1 atom
       ( let reorder xs =
                 case xs of
                     (p0:p1@(SId _ _ (SInfix _)):rs)
                           -> SApp () [p1,p0,reorder rs]
                     [p]   -> p
                     _     -> SApp () xs
         in return (reorder ps) ) )
  
atom :: Parser Token (SExp ())
atom =
  ( do String s <- next
       return (mkList (SLiteral () . show) s) ) <|>
  ( do tok Underscore
       return (SUnevaluated ()) ) <|>
  ( do Char c <- next
       return (SLiteral () ('\'':c++"'")) ) <|>
  ( do Numeric n <- next
       return (SLiteral () n) ) <|>
  ( do Constructor c <- next
       tok OpenBrace
       fields <- (do Variable name <- next
                     tok Equal
                     pat <- pattern
                     return (showQN False name,pat)) `sepBy1` (tok Comma)
       tok CloseBrace
       let (names,exps) = unzip fields
       return (SFieldExpr () (SId () c SInfixDefault) names exps) ) <|>
  ( do Constructor c <- next
       many ( do { tok OpenBrace; tok CloseBrace } )
       return (SId () c SInfixDefault) ) <|>
  ( do Variable v <- next
       return (SId () v SInfixDefault) ) <|>
  ( do InfixConstructor v <- next
       return (SId () v (SInfix 0)) ) <|>
  ( do InfixVariable v <- next
       return (SId () v (SInfix 0)) ) <|>
  ( do tok OpenParen
       ps <- pattern `sepBy1` (tok Comma)
       tok CloseParen
       case length ps of
         1 -> return (head ps)
         _ -> let tuple = Plain (replicate (length ps - 1) ',')
              in return (SApp () (SId () tuple SInfixDefault: ps)) ) <|>
  ( do tok OpenParen
       ps <- many1 atom
       tok CloseParen
       return (SApp () ps) ) <|>
  ( do tok OpenBracket
       elems <- pattern `sepBy` (tok Comma)
       tok CloseBracket
       return (mkList id elems) )

mkList :: (a->SExp ()) -> [a] -> SExp ()
mkList one []     = SId () (Plain "[]") SInfixDefault
mkList one (x:xs) = SApp () [ SId () (Plain ":") SInfixDefault 
                            , one x
                            , mkList one xs ]

