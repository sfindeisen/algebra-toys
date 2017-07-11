fibo 0 = 0
fibo 1 = 1
fibo n = snd (fib_pair n 0 1)
    where
        fib_pair k f1 f2 =
            if (1 == k) then
                (f1, f2)
            else
                fib_pair (k-1) f2 (f1+f2)

-- map fibo [0..15]

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
