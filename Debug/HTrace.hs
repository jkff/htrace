{-# LANGUAGE BangPatterns #-}
-- | Like Debug.Trace.trace, but with indentation corresponding to the
-- level of nesting in the evaluation tree of expressions under htrace.
-- WARNING: Currently only works in single-threaded programs.
-- 
-- Example:
-- 
-- 
-- > xs = map (\x -> htrace (show x) x) [1..10]
-- >
-- > s = foldl (\a b -> htrace "+" (a+b)) 0 xs
-- > s2 = foldl' (\a b -> htrace "+" (a+b)) 0 xs
-- >
-- > b = htrace "b" 2
-- > c = htrace "c" 3
-- > a = htrace "a" $ b + c
-- > x = htrace "x" $ b + c
-- 
--
-- >>> a
-- a
--     b
--     c
-- 5
-- 
-- >>> x
-- x
-- 5
-- 
-- >>> s
-- +
--     +
--         +
--             +
--                 +
--                     +
--                         +
--                             +
--                                 +
--                                     +
--                                         1
--                                     2
--                                 3
--                             4
--                         5
--                     6
--                 7
--             8
--         9
--     10
-- 55
-- 
-- (reload)
-- 
-- >>> s2
-- +
--     1
-- +
--     2
-- +
--     3
-- +
--     4
-- +
--     5
-- +
--     6
-- +
--     7
-- +
--     8
-- +
--     9
-- +
--     10
-- 55
-- 
module Debug.HTrace (htrace) where

import Data.List (foldl')
import Data.IORef
import System.IO.Unsafe

level = unsafePerformIO $ newIORef 0

-- | Trace "str" on a separate line, and increase indentation of subsequent
--   traces while x is being evaluated. 
htrace str x = unsafePerformIO $ do
  lvl <- readIORef level
  putStrLn (replicate (2*lvl) ' ' ++ str)
  writeIORef level (lvl+1)
  let !vx = x
  writeIORef level lvl
  return vx
