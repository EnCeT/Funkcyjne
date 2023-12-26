compref :: Eq a => [a] ->[a]-> [a]
compref list1 [] = []
compref [] list2 = []
compref (l1:list1) (l2:list2) = if l1 == l2 then l1 : compref list1 list2 else []