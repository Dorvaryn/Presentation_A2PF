type Stack w = [w]
-- Focus is first element of list,
-- rest follow clockwise

insert :: Stack w -> w -> Stack w
-- Newly inserted window has focus
insert s w = s ++ [w]

swap :: Stack w -> Stack w
-- Swap topmost pair
swap (w1 : w2 : ws) = w2 : w1 : ws
swap ws = ws

focusNext :: Stack w -> Stack w
focusNext (w:ws) = ws ++ [w]
focusNext [] = []

focusPrev :: Stack w -> Stack w
focusPrev = reverse . focusNext . reverse

enumerate:: Stack w -> [w]
-- Enumerate the windows in layout order
enumerate s = s


-- Write properties in Haskell
type TS = Stack Int -- Test at this type

prop_focusNP :: TS -> Bool
prop_focusNP s = focusNext (focusPrev s) == s
--    where types = s::Stack Int

prop_swap :: TS -> Bool
prop_swap s = swap (swap s) == s
--    where types = s::Stack Int
