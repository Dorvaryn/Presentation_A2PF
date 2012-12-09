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

focus :: Stack w -> Maybe w
-- Returns the focused window of the stack
-- or Nothing if the stack is empty
focus (MkStk _ []) = Nothing
focus (MkStk _ (w:_)) = Just w

swap :: Stack w -> Stack w
swap (MkStk [] []) = MkStk [] []
swap (MkStk [] (r1 : r2 : rs)) = MkStk [] (r2 : r1 :rs)
swap (MkStk [l] (r : rs)) = MkStk [r] (l :rs)
swap (MkStk ls rs) = MkStk ((init . init $ ls) ++ [last ls] ++ [last . init $ ls]) rs

focusPrev :: Stack w -> Stack w
focusPrev (MkStk [] []) = MkStk [] [] 
focusPrev (MkStk [] rs) = MkStk (reverse . init $ rs) [last rs]
focusPrev (MkStk (l:ls) rs) = MkStk ls (l:rs)

focusNext :: Stack w -> Stack w
focusNext (MkStk [] []) = MkStk [] [] 
focusNext (MkStk ls [r]) = MkStk [] ((reverse ls) ++ [r])
focusNext (MkStk ls (r:rs)) = MkStk (r:ls) rs

-- Write properties in Haskell
--type TS = Stack Int -- Test at this type

--prop_focusNP :: TS -> Bool
--prop_focusNP s = focusNext (focusPrev s) == s
--    where types = s::Stack Int

--prop_swap :: TS -> Bool
--prop_swap s = swap (swap s) == s
--    where types = s::Stack Int
