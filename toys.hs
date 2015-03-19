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
