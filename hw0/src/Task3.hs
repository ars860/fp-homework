module Task3
  ( composition
  , contraction
  , identity
  , permutation
  ) where

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- | id function in SK calculus
identity :: a -> a
identity = s const const

-- | (.) in SK calculus
composition :: (b -> c) -> (a -> b) -> a -> c
composition =
  s
    (s (const s) (s (const const) (s (const s) (s (const const) identity))))
    (const (s (s (const s) (s (const const) identity)) (const identity)))


-- | function that applies the argument twice to the function
contraction :: (a -> a -> b) -> a -> b
contraction =
  s
    (s (const s) (s (s (const s) (s (const const) identity)) (const identity)))
    (const identity)


-- | function that rearranges the arguments of the function
permutation :: (a -> b -> c) -> b -> a -> c
permutation =
  s
    (s
      (const s)
      (s
        (s (const s) (s (const const) (const s)))
        (s
          (s
            (const s)
            (s
              (const (s (const s)))
              (s (const (s (const const))) (s (const const) identity))
            )
          )
          (s (const const) (const identity))
        )
      )
    )
    (const (s (const const) identity))
