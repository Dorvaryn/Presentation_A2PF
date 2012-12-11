data Stack w = MkStk [w] [w] -- left and right resp
-- Focus is head of „right‟ list
-- Left list is *reversed*
-- INVARIANT: if „right‟ is empty, so is „left‟

insert :: Stack w -> w -> Stack w
-- Insert new windows at the end of the layout
insert (MkStk ls rs) w = MkStk ls (rs ++ [w])

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

swap :: Stack w -> Stack w
-- Swap topmost pair
swap (MkStk [] []) = MkStk [] []
swap (MkStk ls []) = MkStk [] []
swap (MkStk [] [r]) = MkStk [] [r]
swap (MkStk [] (r1 : r2 : rs)) = MkStk [] (r2 : r1 :rs)
swap (MkStk [l] (r : rs)) = MkStk [r] (l :rs)
swap (MkStk ls rs) = MkStk ((init . init $ ls) ++ [last ls] ++ [last . init $ ls]) rs

{-
-- Write properties in Haskell
type TS = Stack Int -- Test at this type

prop_swap :: TS -> Bool
-- Execute two swap and check if still equals to the initial stack
prop_swap s = swap (swap s) == result
    where result | isNull s = MkStk [] []
                 | otherwise = s
-}
