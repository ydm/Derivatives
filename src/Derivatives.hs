module Derivatives
where
    -- ( someFunc
    -- ) where

import Control.Monad ( join )
import Data.List ( delete, group, intercalate, isSuffixOf, sort )


newtype Prod_ = Prod_ [Expr] deriving (Eq, Show)
-- newtype Sum_ = Sum_ [Expr] deriving (Eq, Show)

data Expr = Const Rational
          | Neg Expr
          -- | Pow Expr Expr
          -- | Prod Prod_
          | Sum [Expr]
          | Var
          deriving (Eq, Show)


-- Predicates
sump :: Expr -> Bool
sump (Sum _) = True
sump _ = False



-- instance Monoid Sum_ where
--   mempty = Sum_ [Const 0]
--   (Sum_ xs) `mappend` (Sum_ ys) = Sum_ $ xs ++ ys


-- sumex :: [Expr] -> Expr
-- sumex = Sum . Sum_


-- prodex


---------------------------
-- Derive
---------------------------

derive :: Expr -> Expr
derive (Const _) = Const 0
derive (Neg x) = Neg $ derive x
derive (Sum xs) = Sum $ map derive xs
-- derive (Sum (Sum_ xs)) = Sum $ Sum_ $ map derive xsp
derive Var = Const 1

select :: (Expr -> Bool) -> [Expr] -> ([Expr], [Expr])
select p = foldl f ([], [])
  where f (first, second) x
          | p x       = (x:first,   second)
          | otherwise = (  first, x:second)
  --   f (consts, other) c@(Const _) = (c:consts, other)
  --       f (consts, other) x = (consts, x:other)

  -- foldl f [Const 0] xs
  -- where f (Const a : ys) (Const b) = Const (a + b) : ys
  --       f (y:ys) x = y:x:ys


simplify :: Expr -> Expr
-- simplify (Sum xs) = 
simplify (Sum xs) = g $ Sum $ foldl f [Const 0] xs
  where f (Const a : ys) (Const b) = Const (a + b) : ys
        f (y:ys) x = y:x:ys
        g (Sum [x]) = x
        g x = x

-- derive (Pow Var a) = Prod [ a, Pow Var (Sum [ a, (Neg (Const 1)) ] ) ]
-- derive (Prod (x:[])) = derive x
-- derive (Prod (x:xs)) = Sum [ lhs, rhs ]
--   where lhs = Prod $ (derive x) : xs
--         rhs = Prod $         x  : [derive $ Prod xs]

-- base (Prod a b)  = Sum [(Prod (derive a) b), (Prod a (derive b))]


{-

derive :: Expr -> Expr
derive = simplify . base


---------------------------
-- Simplify
---------------------------

prep :: Expr -> [Expr] -> [[Expr]]
prep z xs = group $ sort $ delete z $ map simplify xs

f :: (Expr -> Bool) -> (Expr -> [Expr]) -> [Expr] -> [Expr]
f p extract xs = ans2
  where ans  = map simplify xs
        ans2 = foldl g [] ans
        g memo x
          | p x       = concat [extract x, memo]
          | otherwise = x : memo

sumpls :: [Expr] -> Expr
sumpls xs = Const $ foldl g (0 :: Rational) xs
  where g memo (Const x) = memo + x


-- TODO!
-- accum (Sum xs) = Sum $ append (filter (not . p) xs) [ sumpls (filter p xs) ]

simplify :: Expr -> Expr
simplify (Sum (x:[])) = simplify x
-- simplify (Sum xs) = Sum ans
--   where clean = prep (Const 0) xs
--         ans   = foldl f [] clean
--         f memo exprs
--           | len <= 1  = (head exprs) : memo
--           | otherwise = (Prod [ Const (toRational len), head exprs ] ) : memo
--           where len = length exprs
simplify (Sum xs) = Sum $ f p extract xs
  where p (Sum _) = True
        p      _  = False
        extract (Sum xs) = xs

-- simplify (Sum  (Const a) (Const b)) = Const $ a + b
-- simplify (Sum  (Const 0)        a ) = simplify a
-- simplify (Sum         a  (Const 0)) = simplify a
-- simplify (Diff (Const a) (Const b)) = Const $ a - b
-- simplify (Diff        a  (Const 0)) = simplify a
simplify (Prod (x:[])) = simplify x
simplify (Prod xs) = Prod $ delete (Const 1) $ map simplify xs

-- simplify (Prod (Const a) (Const b)) = Const (a * b)
-- simplify (Prod (Const 0)        _ ) = Const 0
-- simplify (Prod        _  (Const 0)) = Const 0
-- simplify (Prod (Const 1)        a ) = simplify a
-- simplify (Prod        a  (Const 1)) = simplify a
simplify (Pow         a  (Const 1)) = simplify a
simplify (Pow         _  (Const 0)) = Const 1

-- simplify e@(Sum  a b) = rec2 e Sum  a b
-- simplify e@(Diff a b) = rec2 e Diff a b
-- simplify e@(Prod a b) = rec2 e Prod a b
simplify e@(Pow a b)  = rec2 e Pow  a b

simplify e = e


-- |Recursively simplify both the left and right operands of the
-- expression.
rec2 :: Expr -> (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr
rec2 expr con left right
  | ans == expr = ans
  | otherwise   = simplify ans
  where ans = con (simplify left) (simplify right)


---------------------------
-- toString
---------------------------

wrap :: String -> String
wrap s = "(" ++ s ++ ")"


toString :: Expr -> String
toString (Const a)  = ugly a
toString (E)        = "E"
-- toString (Diff a b) = wrap $ toString a ++ " - " ++ toString b
toString (Pow  a b) = wrap $ toString a ++ "^" ++ toString b
-- toString (Prod a b) = wrap $ toString a ++ " * " ++ toString b
toString (Prod xs)   = wrap $ intercalate " * " (map toString xs)
toString (Sum xs)    = wrap $ intercalate " + " (map toString xs)
-- toString (Sum  a b) = wrap $ toString a ++ " + " ++ toString b
toString (Var)      = "x"


ugly :: Rational -> String
ugly a = if isSuffixOf ".0" s then d else show a
  where s = show (fromRational a :: Float)
        d = (reverse . drop 2 . reverse) s

-}
