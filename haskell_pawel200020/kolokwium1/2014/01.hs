howMany:: Eq a =>[a]->a->Int 
howMany array elem = length (filter(\x->(x==elem)) array)

numocc :: Eq a =>a->[[a]]->[Int] 
numocc a (l:list) = foldl(\acc x -> acc++[(howMany x a)]) [(howMany l a)] list