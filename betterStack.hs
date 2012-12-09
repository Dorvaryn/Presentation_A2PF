import Test.QuickCheck
import System.Random
import Control.Monad

data Stack w = MkStk [w] [w] -- left and right resp
-- Focus is head of „right‟ list
-- Left list is *reversed*
-- INVARIANT: if „right‟ is empty, so is „left‟

instance (Eq w) => Eq (Stack w) where
    MkStk l1 r1 == MkStk l2 r2 = (l1 == l2) && (r1 == r2)

instance (Show w) => Show (Stack w) where
  show (MkStk l r) = "List : " ++ show (enumerate (MkStk l r)) ++ "\nInner Rep : "++show l ++ "-" ++ show r

insert :: Stack w -> w -> Stack w
insert (MkStk ls rs) w = MkStk (ls ++ [w]) rs

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
swap (MkStk [l] []) = MkStk [] [l]
swap (MkStk [] [r]) = MkStk [] [r]
swap (MkStk [] (r1 : r2 : rs)) = MkStk [] (r2 : r1 :rs)
swap (MkStk [l] (r : rs)) = MkStk [r] (l :rs)
swap (MkStk ls rs) = MkStk ((init . init $ ls) ++ [last ls] ++ [last . init $ ls]) rs

focusPrev :: Stack w -> Stack w
focusPrev (MkStk [] []) = MkStk [] [] 
focusPrev (MkStk [] rs) = MkStk (reverse . init $ rs) [last rs]
focusPrev (MkStk (l:ls) rs) = MkStk ls (l:rs)

focusNext :: Stack w -> Stack w
focusNext (MkStk [] []) = MkStk [] []
focusNext (MkStk ls []) = MkStk [] ls
focusNext (MkStk ls [r]) = MkStk [] ((reverse ls) ++ [r])
focusNext (MkStk ls (r:rs)) = MkStk (r:ls) rs

-- Write properties in Haskell
type TS = Stack Int -- Test at this type

instance (Arbitrary a) => Arbitrary (Stack a) where
--    arbitrary = liftM2 MkStk arbitrary arbitrary
      arbitrary = do
        l <- arbitrary
        r <- arbitrary
        return(MkStk l r)

prop_focusNP :: TS -> Bool
-- Ugly workaround for non authorized values
prop_focusNP (MkStk ls []) = focusNext (focusPrev (MkStk ls [])) == MkStk [] (reverse ls)
prop_focusNP s = focusNext (focusPrev s) == s

prop_swap :: TS -> Bool
-- Ugly workaround for non authorized values
prop_swap (MkStk [l] []) = focusNext (focusPrev (MkStk [l] [])) == MkStk [] [l]
prop_swap s = swap (swap s) == s
