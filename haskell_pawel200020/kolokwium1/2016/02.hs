prefix:: [a] ->[[a]]
prefix (w:word) = foldl(\acc x -> acc++[(last acc)++[x]]) [[w]] word

sufix:: [a] ->[[a]]
sufix (word)= tail (reverse ( foldl(\acc x -> acc++[[x]++(last acc)]) [[last word]] (tail (reverse word))))

ps :: [a] ->[[a]]
ps word = (prefix word)++(sufix word)