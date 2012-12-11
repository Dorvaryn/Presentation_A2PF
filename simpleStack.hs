type Stack w = [w]
-- Focus is first element of list,
-- rest follow clockwise

insert :: Stack w -> w -> Stack w
-- Insert new windows at the end of the layout
insert s w = s ++ [w]

focus :: Stack w -> Maybe w
-- Returns the focused window of the stack
-- or Nothing if the stack is empty
focus [] = Nothing
focus s = Just (head s)

swap :: Stack w -> Stack w
-- Swap topmost pair
swap (w1 : w2 : ws) = w2 : w1 : ws
swap ws = ws

focusNext :: Stack w -> Stack w
-- Switch the focus to the next windows
focusNext (w:ws) = ws ++ [w]
focusNext [] = []

focusPrev :: Stack w -> Stack w
-- Switch the focus to the previous windows
focusPrev = reverse . focusNext . reverse

enumerate:: Stack w -> [w]
-- Enumerate the windows in layout order
enumerate s = s


-- Write properties in Haskell
type TS = Stack Int -- Test at this type

prop_focusNP :: TS -> Bool
-- Execute a focus switch forward then backward and check if still equals to the initial stack
prop_focusNP s = focusNext (focusPrev s) == s

prop_swap :: TS -> Bool
-- Execute two swap and check if still equals to the initial stack
prop_swap s = swap (swap s) == s

prop_focusNfocus :: TS -> Bool
-- Execute a focus switch forward and check if focused element is the expected one
prop_focusNfocus s = focus (focusNext s) == result
    where result | null s = Nothing
                 | length s == 1 = Just (head s)
                 | otherwise = Just (head (tail s))

prop_focusPfocus :: TS -> Bool
-- Execute a focus switch backward and check if focused element is the expected one
prop_focusPfocus s = focus (focusPrev s) == result
    where result | null s = Nothing
                 | otherwise = Just (last s)
