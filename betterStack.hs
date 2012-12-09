data Stack w = MkStk [w] [w] -- left and right resp
-- Focus is head of „right‟ list
-- Left list is *reversed*
-- INVARIANT: if „right‟ is empty, so is „left‟

enumerate :: Stack w -> [w]
enumerate (MkStk ls rs) = reverse ls ++ rs

right :: Stack w -> [w]
right (MkStk ls rs) = rs

left :: Stack w -> [w]
left (MkStk ls rs) = ls

focusPrev :: Stack w -> Stack w
focusPrev (MkStk (l:ls) rs) = MkStk ls (l:rs)
focusPrev (MkStk [] (r:rs)) = MkStk (reverse rs) [r]
focusPrev (MkStk [] []) = MkStk [] [] 

-- Write properties in Haskell
type TS = Stack Int -- Test at this type

--prop_focusNP :: TS -> Bool
--prop_focusNP s = focusNext (focusPrev s) == s
--    where types = s::Stack Int

--prop_swap :: TS -> Bool
--prop_swap s = swap (swap s) == s
--    where types = s::Stack Int