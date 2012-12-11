module Stack (
    Stack(MkStk), 
    insert, enumerate, isNull, focus, swap, focusPrev, focusNext
) where

import Test.QuickCheck
import System.Random
import Control.Monad
import Control.Monad.Writer

data Stack w = MkStk [w] [w] -- left and right resp
-- Focus is head of „right‟ list
-- Left list is *reversed*
-- INVARIANT: if „right‟ is empty, so is „left‟

instance (Eq w) => Eq (Stack w) where
    (==) (MkStk l1 r1) (MkStk l2 r2) = (l1 == l2) && (r1 == r2)

instance (Show w) => Show (Stack w) where
    show (MkStk l r) = "List : " ++ show (enumerate (MkStk l r)) ++ "\nInner Rep : "++show l ++ "-" ++ show r

insert :: Stack w -> w -> Stack w
-- Insert new windows at the end of the layout
insert (MkStk ls rs) w = MkStk ls (rs ++ [w])

insert' :: (Stack Char) -> Char -> Writer [String] (Stack Char)
-- Insert version with debug string, only on stacks of chars
insert' (MkStk ls rs) w = do { tell ["Just inserted " ++ [w]]; return $ MkStk ls (rs ++ [w]) }

enumerate :: Stack w -> [w]
-- Enumerate the windows in layout order
enumerate (MkStk ls rs) = reverse ls ++ rs

right :: Stack w -> [w]
-- For Internal use only
-- return the right arm of the Stack
right (MkStk ls rs) = rs

left :: Stack w -> [w]
-- For Internal use only
-- return the left arm of the Stack
left (MkStk ls rs) = ls

isNull :: Stack w -> Bool
-- Return True if the stack is empty (right arm empty)
-- return False otherwise
isNull (MkStk _ []) = True
isNull s = False

focus :: Stack w -> Maybe w
-- Returns the focused window of the stack
-- or Nothing if the stack is empty
focus (MkStk _ []) = Nothing
focus (MkStk _ (w:_)) = Just w

swap :: Stack w -> Stack w
-- Swap topmost pair
swap (MkStk ls []) = MkStk [] []
swap (MkStk [] [r]) = MkStk [] [r]
swap (MkStk [] (r1 : r2 : rs)) = MkStk [] (r2 : r1 :rs)
swap (MkStk [l] (r : rs)) = MkStk [r] (l :rs)
swap (MkStk ls rs) = MkStk ((init . init $ ls) ++ [last ls] ++ [last . init $ ls]) rs

focusPrev :: Stack w -> Stack w
-- Switch the focus to the previous windows
focusPrev (MkStk ls []) = MkStk [] []
focusPrev (MkStk [] rs) = MkStk (reverse . init $ rs) [last rs]
focusPrev (MkStk (l:ls) rs) = MkStk ls (l:rs)

focusNext :: Stack w -> Stack w
-- Switch the focus to the next windows
focusNext (MkStk ls []) = MkStk [] []
focusNext (MkStk ls [r]) = MkStk [] ((reverse ls) ++ [r])
focusNext (MkStk ls (r:rs)) = MkStk (r:ls) rs

-- Write properties in Haskell
type TS = Stack Int -- Test at this type

instance (Arbitrary a) => Arbitrary (Stack a) where
      arbitrary = liftM2 MkStk arbitrary arbitrary

prop_focusNP :: TS -> Bool
-- Execute a focus switch forward then backward and check if still equals to the initial stack
prop_focusNP s = focusNext (focusPrev s) == result
    where result | isNull s = MkStk [] []
                 | otherwise = s

prop_swap :: TS -> Bool
-- Execute two swap and check if still equals to the initial stack
prop_swap s = swap (swap s) == result
    where result | isNull s = MkStk [] []
                 | otherwise = s

prop_focusNfocus :: TS -> Bool
-- Execute a focus switch forward and check if focused element is the expected one
prop_focusNfocus (MkStk l r) = focus (focusNext (MkStk l r)) == result
    where result | null r = Nothing
                 | (length r == 1) && null l = Just . head $ r
                 | (length r == 1) && (not . null $l) = Just . last $ l
                 | otherwise = Just . head . tail $ r 

prop_focusPfocus :: TS -> Bool
-- Execute a focus switch backward and check if focused element is the expected one
prop_focusPfocus (MkStk l r) = focus (focusPrev (MkStk l r)) == result
    where result | null r = Nothing
                 | null l = Just . last $ r
                 | otherwise = Just . head $ l
