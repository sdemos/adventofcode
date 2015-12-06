part1 :: [Char] -> Integer
part1 = sum . map translate

translate :: Char -> Integer
translate '(' = 1
translate ')' = -1
translate _   = 0

-- continuation passing style
part2 :: [Char] -> Integer
part2 = p2 id 0

p2 :: (Integer -> Integer) -> Integer -> [Char] -> Integer
p2 k s ('(':ps) = p2 ((+ 1) . k) (s + 1) ps
p2 k s (')':ps) = if s == 0 then k 1 else p2 ((+ 1) . k) (s - 1) ps
p2 _ _ []       = -1

-- passing index number
part3 :: [Char] -> Integer
part3 = p3 1 0

p3 :: Integer -> Integer -> [Char] -> Integer
p3 i s ('(':ps) = p3 (i + 1) (s + 1) ps
p3 i s (')':ps) = if s == 0 then i else p3 (i + 1) (s - 1) ps
p3 _ _ []       = -1
