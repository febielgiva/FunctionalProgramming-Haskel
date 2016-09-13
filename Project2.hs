oddsFrom3 = [3, 5 .. ]
primeDivisors n = [d | d <- takeWhile ((<= n) . (^2)) primes, n `mod` d == 0]
primes = 2 : [p | p <- oddsFrom3, null (primeDivisors p)]
isPrime g = g == head (dropWhile (< g) primes)
iSqrt n = floor (sqrt (fromIntegral n))
isSquare n = (iSqrt n) ^ 2 == n 

goldbachPairs g = [(p, k) | k <- takeWhile ((< g) . (*2) . (^2)) [1 .. ], 
                            p <- [g - 2 * k * k], isPrime p]
goldbachPairs' g = [(p, iSqrt kSqr) | p <- takeWhile (< g) (tail primes),  
                                      kSqr <- [(g - p) `div` 2], 
                                      isSquare kSqr]
-- This function returns a boolean value--
-- Second constraint will always compute false --
goldbachDiffs = [g | g <- takeWhile (< 6000) oddsFrom3, not (isPrime g),  goldbachPairs g /= reverse (goldbachPairs' g)]
goldbachPairsTest :: Int -> Bool
goldbachPairsTest g = (goldbachPairs g /= reverse (goldbachPairs' g))

-- This function computes the values of those odd non-prime numbers that does not satisfy the goldbachpair -- 
goldbachFails' = [g | g <- takeWhile (< 6000) oddsFrom3, not (isPrime g), 
                      null (goldbachPairs' g) ]