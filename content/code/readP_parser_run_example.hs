readP_to_S my_parser "axczzz" =
-- definition of my_parser + do de-sugaring
readP_to_S (get >>= \c1 -> do {char 'x'; c2 <- get; return [c1, c2]}) "axczzz" =
-- definition of get
readP_to_S ((R Get) >>= \c1 -> do {char 'x'; c2 <- get; return [c1, c2]}) "axczzz" =
-- definition of (>>=) for ReadP
readP_to_S (R (\k -> Get (\a -> let R m' = (\c1 -> do {char 'x'; c2 <- get; return [c1, c2]}) a
                                in m' k
                         )
              )
           ) "axczzz" =
-- definition of readP_to_S
run ((\k -> Get (\a -> let R m' = (\c1 -> do {char 'x'; c2 <- get; return [c1, c2]}) a
                       in m' k
                )
     ) return) "axczzz" =
-- definition of return (= pure) for P
run ((\k -> Get (\a -> let R m' = (\c1 -> do {char 'x'; c2 <- get; return [c1, c2]}) a
                       in m' k
                )
     ) (\x -> Result x Fail)) "axczzz" =
-- apply lambda function
run (Get (\a -> let R m' = (\c1 -> do {char 'x'; c2 <- get; return [c1, c2]}) a
                in m' (\x -> Result x Fail)
         )
    ) "axczzz" =
-- apply the definition of run for the pattern match Get f
run ((\a -> let R m' = (\c1 -> do {char 'x'; c2 <- get; return [c1, c2]}) a
            in m' (\x -> Result x Fail)
     ) 'a'
    ) "xczzz" =
-- apply lambda function
run (let R m' = (\c1 -> do {char 'x'; c2 <- get; return [c1, c2]}) 'a'
     in m' (\x -> Result x Fail)
    ) "xczzz" =
-- apply lambda function
run (let R m' = do {char 'x'; c2 <- get; return ['a', c2]}
     in m' (\x -> Result x Fail)
    ) "xczzz" =
-- do de-sugaring
run (let R m' = char 'x' >>= (\_ -> do {c2 <- get; return ['a', c2]})
     in m' (\x -> Result x Fail)
    ) "xczzz" =
-- definition of char x
run (let R m' = (satisfy ('x' ==)) >>= (\_ -> do {c2 <- get; return ['a', c2]})
     in m' (\x -> Result x Fail)
    ) "xczzz" =
-- definition of satisfy p
run (let R m' = do {c <- get; if ('x' ==) c then return c else pfail} >>= \_ ->
                do {c2 <- get; return ['a', c2]})
     in m' (\x -> Result x Fail)
    ) "xczzz" =
-- definition of (>>=) for ReadP
run (let R m' = (get >>= (\u -> do {if ('x' ==) u then return u else pfail})) >>= \_ ->
                 do {c2 <- get; return ['a', c2]})
     in m' (\x -> Result x Fail)
    ) "xczzz" =
-- definition of get
run (let R m' = ((R Get) >>= (\u -> do {if ('x' ==) u then return u else pfail})) >>= \_ ->
                 do {c2 <- get; return ['a', c2]})
     in m' (\x -> Result x Fail)
    ) "xczzz" =
-- definition of (>>=) for ReadP
run (let R m' = (R (\k2 -> Get (\a2 -> let R m'' = (\u -> do {if ('x' ==) u then return u else pfail}) a2
                                       in m'' k2))
                 >>= \_ ->
                 do {c2 <- get; return ['a', c2]}
                )
     in m' (\x -> Result x Fail)
    ) "xczzz" =
-- definition of (>>=) for ReadP
run (let R m' = R (\k3 -> (\k2 -> Get (\a2 -> let R m'' = (\u -> do {if ('x' ==) u then return u else pfail}) a2
                                              in m'' k2))
                  (\a3 -> let R m''' = (\_ -> do {c2 <- get; return ['a', c2]}) a3
                          in m''' k3))
     in m' (\x -> Result x Fail)
    ) "xczzz" =
-- pattern match the let expression
run ((\k3 -> (\k2 -> Get (\a2 -> let R m'' = (\u -> do {if ('x' ==) u then return u else pfail}) a2
                                 in m'' k2))
     (\a3 -> let R m''' = (\_ -> do {c2 <- get; return ['a', c2]}) a3
             in m''' k3)) (\x -> Result x Fail)
    ) "xczzz" =
-- function application
run ((\k2 -> Get (\a2 -> let R m'' = (\u -> do {if ('x' ==) u then return u else pfail}) a2
                         in m'' k2))
     (\a3 -> let R m''' = (\_ -> do {c2 <- get; return ['a', c2]}) a3
             in m''' (\x -> Result x Fail))
    ) "xczzz" =
-- function application
run (Get (\a2 -> let R m'' = (\u -> do {if ('x' ==) u then return u else pfail}) a2
                 in m'' (\a3 -> let R m''' = (\_ -> do {c2 <- get; return ['a', c2]}) a3
                                in m''' (\x -> Result x Fail)))
    ) "xczzz" =
-- apply the definition of run for the pattern match Get f
run ((\a2 -> let R m'' = (\u -> do {if ('x' ==) u then return u else pfail}) a2
             in m'' (\a3 -> let R m''' = (\_ -> do {c2 <- get; return ['a', c2]}) a3
                            in m''' (\x -> Result x Fail))) 'x'
    ) "czzz" =
-- function application
run (let R m'' = (\u -> do {if ('x' ==) u then return u else pfail}) 'x'
     in m'' (\a3 -> let R m''' = (\_ -> do {c2 <- get; return ['a', c2]}) a3
                    in m''' (\x -> Result x Fail))
    ) "czzz" =
-- function application
run (let R m'' = do {if ('x' ==) 'x' then return 'x' else pfail}
     in m'' (\a3 -> let R m''' = (\_ -> do {c2 <- get; return ['a', c2]}) a3
                    in m''' (\x -> Result x Fail))
    ) "czzz" =
-- function application
run (let R m'' = do {return 'x'}
     in m'' (\a3 -> let R m''' = (\_ -> do {c2 <- get; return ['a', c2]}) a3
                    in m''' (\x -> Result x Fail))
    ) "czzz" =
-- definition of return
run (let R m'' = R (\k4 -> k4 'x')
     in m'' (\a3 -> let R m''' = (\_ -> do {c2 <- get; return ['a', c2]}) a3
                    in m''' (\x -> Result x Fail))
    ) "czzz" =
-- pattern match the let expression
run ((\k4 -> k4 'x') (\a3 -> let R m''' = (\_ -> do {c2 <- get; return ['a', c2]}) a3
                             in m''' (\x -> Result x Fail))
    ) "czzz" =
-- function application
run ((\a3 -> let R m''' = (\_ -> do {c2 <- get; return ['a', c2]}) a3
             in m''' (\x -> Result x Fail)) 'x'
    ) "czzz" =
-- function application
run (let R m''' = (\_ -> do {c2 <- get; return ['a', c2]}) 'x'
     in m''' (\x -> Result x Fail)
    ) "czzz" =
-- function application
run (let R m''' = do {c2 <- get; return ['a', c2]}
     in m''' (\x -> Result x Fail)
    ) "czzz" =
-- do de-sugaring
run (let R m''' = get >>= (\c2 -> do {return ['a', c2]})
     in m''' (\x -> Result x Fail)
    ) "czzz" =
-- definition of get
run (let R m''' = (R Get) >>= (\c2 -> do {return ['a', c2]})
     in m''' (\x -> Result x Fail)
    ) "czzz" =
-- definition of (>>=) for ReadP
run (let R m''' = R (\k5 -> Get (\a4 -> let R m4 = (\c2 -> do {return ['a', c2]}) a4
                                        in m4 k5
                                )
                    )
     in m''' (\x -> Result x Fail)
    ) "czzz" =
-- pattern match the let expression
run ((\k5 -> Get (\a4 -> let R m4 = (\c2 -> do {return ['a', c2]}) a4
                         in m4 k5
                 )
     ) (\x -> Result x Fail)
    ) "czzz" =
-- function application
run (Get (\a4 -> let R m4 = (\c2 -> do {return ['a', c2]}) a4
                 in m4 (\x -> Result x Fail))
    ) "czzz" =
-- apply the definition of run for the pattern match Get f
run ((\a4 -> let R m4 = (\c2 -> do {return ['a', c2]}) a4
                 in m4 (\x -> Result x Fail)) 'c'
    ) "zzz" =
-- function application
run (let R m4 = (\c2 -> do {return ['a', c2]}) 'c'
     in m4 (\x -> Result x Fail)
    ) "zzz" =
-- function application
run (let R m4 = do {return ['a', 'c']}
     in m4 (\x -> Result x Fail)
    ) "zzz" =
-- definition of return
run (let R m4 = R (\k6 -> k6 ['a', 'c'])
     in m4 (\x -> Result x Fail)
    ) "zzz" =
-- pattern match the let expression
run ((\k6 -> k6 ['a', 'c']) (\x -> Result x Fail)) "zzz" =
-- function application
run ((\x -> Result x Fail) ['a', 'c']) "zzz" =
-- function application
run (Result ['a', 'c'] Fail) "zzz" =
-- apply the definition of run for the pattern match Result x p
(['a', 'c'], "zzz") : run Fail "zzz" =
-- apply the definition of run for the pattern match Fail (_)
(['a', 'c'], "zzz") : [] =
-- use the definition of (:)
[['a', 'c'], "zzz"]
