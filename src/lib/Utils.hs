module Utils (makeTuples) where

-- |makeTuples takes as input a list xs.
-- each element x of xs represents a list of possibilities.
-- makeTuples returns lists whose elements are chosen, in order, from each x
-- for example makeTuples [[1, 2], [3, 4]] will return [[1, 3], [1, 4], [2, 3], [2, 4]]
-- you can view the results as the possible "paths" through xs.
--
-- > makeTuples [[a,b,c],[d,e,f],[g,h,i]]
--
-- @
-- a    /- d -\\      g
--     /       \\
-- b -/    e    \\    h
--               \\
-- c       f      \\- i -> [b, d, i]
-- @
makeTuples :: [[a]] -> [[a]]
makeTuples = sequence
