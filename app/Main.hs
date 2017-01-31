module Main where

import Derivatives


-- exs :: [Expr]
-- exs = [
--       -- (Pow Var $ Const 10)
--       -- , Prod [Const 10, Pow Var (Diff (Const 10) (Const 1))]
--       -- , (Pow Var (Diff (Diff (Const 10) (Const 1)) (Const 1)))
--       -- , (Pow Var (Const 2))
--       -- , Prod [Const 10, (Pow Var (Const 2))]
--       -- , Sum [ Prod [Const 10, Var], Prod [Const 20, Var], Prod [Const 30, Var]]
--   Prod [Var, Prod [Var, Var]]
--   , Sum [ Prod [ (Const 10), Var ], Prod [ (Const 20), Var ],
--           Sum [ Prod [ (Const 10), Var ], Prod [ (Const 20), Var ] ] ]
--       ]

main :: IO ()
main = return ()
  -- mapM_ f exs
  -- where f e = putStrLn $ toString e ++ " d/dx=" ++ (toString) e
