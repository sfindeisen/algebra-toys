-- import Data.List.Ordered (minus, union, unionAll)

-- prime sequence (infinite)
primes = 2 : 3 : ([x | x <- [5,7..], (all (\z -> ((x `mod` z) /= 0)) (takeWhile (\y -> y*y<=x) primes))])
-- primes_sieve = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+2*p..] | p <- tail primes])

-- prime divisors in non-decreasing order (with repetitions) (inefficient)
prime_divisors 0 = []
prime_divisors 1 = []
prime_divisors n = reverse $ divi n 2 []
    where
        divi n k xs =
            if (k > n) then
                xs
            else
                if (n `mod` k == 0) then
                    divi (n `div` k) k (k:xs)
                else
                    divi n (1+k) xs

-- Fibonacci number n
fibo 0 = 0
fibo 1 = 1
fibo n = snd (fib_pair n 0 1)
    where
        fib_pair k f1 f2 =
            if (1 == k) then
                (f1, f2)
            else
                fib_pair (k-1) f2 (f1+f2)

-- Fibonacci sequence (infinite)
fibs = 0:1:(zipWith (+) fibs (tail fibs))

gen f start g n = reverse (xcalc start [])
    where
        xcalc a t =
            let
                b=(f g a) `mod` n
            in
                if b `elem` t then
                    t
                else
                    xcalc b (b:t)

genmul = gen (*) 1
genadd = gen (+) 0
