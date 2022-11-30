
-- | Earley Parser Combinators
module ParE (Par,parse,word,key,int,ws0,ws1,sp,nl,lit,sat,char,alts,opt,separated,terminated,many,some,digit) where

import Control.Applicative (Alternative,empty,(<|>),many,some)
import Control.Monad (ap,liftM)
import qualified Data.Char as Char

import qualified EarleyM as EM (
  Gram,Lang,fail,alts,getToken,parseAmb,
  Parsing(..),ParseError(..),SyntaxError(..),Ambiguity(..),Pos)

instance Functor Par where fmap = liftM
instance Applicative Par where pure = Ret; (<*>) = ap
instance Alternative Par where empty = Fail; (<|>) = Alt
instance Monad Par where (>>=) = Bind

separated :: Par () -> Par a -> Par [a]
terminated :: Par () -> Par a -> Par [a]
opt :: Par a -> Par (Maybe a)
alts :: [Par a] -> Par a
word :: Par String
key :: String -> Par ()
int :: Par Int
ws1 :: Par ()
ws0 :: Par ()
digit :: Par Int
sp :: Par ()
nl :: Par ()
lit :: Char -> Par ()
sat :: (Char -> Bool) -> Par Char
char :: Par Char

separated sep p = do x <- p; alts [ pure [x], do sep; xs <- separated sep p; pure (x:xs) ]
terminated term p = alts [ pure [], do x <- p; term; xs <- terminated term p; pure (x:xs) ]
opt p = alts [ pure Nothing, fmap Just p ]
alts = foldl Alt Fail
word = some $ sat Char.isAlpha
key cs = mapM_ lit cs
int = foldl (\acc d -> 10*acc + d) 0 <$> some digit
ws1 = do sp; ws0
ws0 = do _ <- many sp; return ()
digit = digitOfChar <$> sat Char.isDigit
sp = lit ' '
nl = lit '\n'
lit x = do _ <- sat (== x); pure ()

sat pred = do c <- Token; if pred c then return c else Fail
char = Token

digitOfChar :: Char -> Int
digitOfChar c = Char.ord c - ord0 where ord0 = Char.ord '0'

data Par a where
  Ret :: a -> Par a
  Bind :: Par a -> (a -> Par b) -> Par b
  Fail :: Par a
  Token :: Par Char
  Alt :: Par a -> Par a -> Par a

parse :: Show a => Par a -> String -> a
parse par string = runLG string $ langOfPar par

withTok :: EM.Gram Char -> Par a -> EM.Gram a
withTok tok = conv
  where
    conv :: Par a -> EM.Gram a
    conv = \case
      Ret a -> return a
      Bind p f -> conv p >>= conv . f
      Fail -> EM.fail
      Token -> tok
      Alt p1 p2 -> EM.alts [conv p1, conv p2]

langOfPar :: Par a -> EM.Lang Char (EM.Gram a)
langOfPar par = do
  tok <- EM.getToken
  return $ withTok tok par

runLG :: Show a => String -> EM.Lang Char (EM.Gram a) -> a
runLG s lang =
  case EM.parseAmb lang s of
    EM.Parsing{EM.outcome} -> case outcome of
      Left pe -> error $ prettySE pe
      Right [a] -> a
      Right xs -> error $ show ("ambiguity", length xs)
  where

    _prettyPE :: EM.ParseError -> String
    _prettyPE = \case
      EM.AmbiguityError (EM.Ambiguity tag p1 p2) -> show ("Ambiguity",tag,p1,p2)
      EM.SyntaxError se -> prettySE se

    prettySE :: EM.SyntaxError -> String
    prettySE = \case
      EM.UnexpectedTokenAt pos -> "unexpected " ++ show (s!!(pos-1)) ++ " at " ++ lc (pos-1)
      EM.UnexpectedEOF pos -> "unexpected EOF at " ++ lc (pos-1)
      EM.ExpectedEOF pos -> "expected EOF at " ++ lc (pos-1)

    lc :: EM.Pos -> String
    lc p = "line: " ++ show line ++ ", col: " ++ show col
      where
        line :: Int = 1 + length [ () | c <- take p s, c == '\n' ]
        col :: Int = length (takeWhile (/= '\n') (reverse (take p s)))
