-- 1. Find the last element of a list.
--    myLast [1,2,3] -}
myLast l = head $ reverse l -- same as head (reverse l)
myLast' [] = error "myLast': empty list"
myLast' [x] = x
myLast' (_:xs) = myLast' xs

-- 2. Find the last but one element of a list
--    myButLast [1,2,3,4]
myButLast [] = error "myButLast: empty list"
myButLast [_] = error "myButLast: single element list"
myButLast [x,_] = x
myButLast (x:xs) = myButLast xs

-- 3. Find the kth element of a list. First element is indexed as 1
--    elementAt [1,2,3] 2
elementAt l k = if k <= 0 then error "elementAt: k must be greater than 0"
                else go l k
                where go [] _ = error "elementAt: out of bounds"
                      go (x:xs) 1 = x
                      go (x:xs) k = go xs (k-1)

-- 4. Find the number of elements in a list
--    myLength [123, 456, 789]
myLength l = go l 0
             where go [] n = n
                   go (x:xs) n = go xs (n+1)

-- 5. Reverse a list
--    myReverse [1,2,3,4]; Test with strings, too! :p
myReverse:: [a] -> [a]
myReverse l = go l []
              where go [] r = r
                    go (x:xs) r = go xs (x:r)
