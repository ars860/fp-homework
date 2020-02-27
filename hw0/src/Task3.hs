module Task3
  ( composition
  , contraction
  , identity
  , permutation
  ) where

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- guessed
identity :: a -> a
identity = s const const

-- T[\x.\y.\a.x (y a)] =>
-- T[\x.T[\y.\a.x (y a)]] =>
-- T[\x.T[\y.\a.x (y a)]] =>
-- T[\x.T[\y.T[\a.x (y a)]]] =>
-- T[\x.T[\y.(S T[\a.x] T[\a.y a])]] =>
-- T[\x.T[\y.(S (K x) (S T[\a.y] T[\a.a]))]] =>
-- T[\x.T[\y.(S (K x) (S (K y) I))]] =>
-- T[\x.(S T[\y.S (K x)] T[\y.S (K y) I])] =>
-- T[\x.(S (K (S (K x))) (S T[\y.S (K y)] T[\y.I]))] =>
-- T[\x.(S (K (S (K x))) (S (S T[\y.S] T[\y.K y]) (K I)))] =>
-- T[\x.(S (K (S (K x))) (S (S (K S) (S T[\y.K] T[\y.y])) (K I)))] =>
-- T[\x.(S (K (S (K x))) (S (S (K S) (S (K K) I)) (K I)))] =>
-- (S T[\x.S (K (S (K x)))] T[\x.(S (S (K S) (S (K K) I)) (K I))]) =>
-- (S (S T[\x.S] T[\x.(K (S (K x)))]) (K (S (S (K S) (S (K K) I)) (K I)))) =>
-- (S (S (K S) (S T[\x.K] T[\x.(S (K x))])) (K (S (S (K S) (S (K K) I)) (K I)))) =>
-- (S (S (K S) (S (K K) (S T[\x.S] T[\x.(K x)]))) (K (S (S (K S) (S (K K) I)) (K I)))) =>
-- (S (S (K S) (S (K K) (S (K S) (S T[\x.K] T[\x.x])))) (K (S (S (K S) (S (K K) I)) (K I)))) =>
-- (S (S (K S) (S (K K) (S (K S) (S (K K) I)))) (K (S (S (K S) (S (K K) I)) (K I))))
composition :: (b -> c) -> (a -> b) -> a -> c
composition =
  s
    (s (const s) (s (const const) (s (const s) (s (const const) identity))))
    (const (s (s (const s) (s (const const) identity)) (const identity)))


-- T[\f.\x.(f x) x] =>
-- T[\f.T[\x.(f x) x]] =>
-- T[\f.(S T[\x.(f x)] T[\x.x])] =>
-- T[\f.(S (S T[\x.f] T[\x.x]) I)] =>
-- T[\f.(S (S (K f) I) I)] =>
-- (S T[\f.S (S (K f) I)] T[\f.I]) =>
-- (S (S T[\f.S] T[\f.(S (K f) I)]) (K I)) =>
-- (S (S (K S) (S T[\f.S (K f)] T[\f.I])) (K I)) =>
-- (S (S (K S) (S (S T[\f.S] T[\f.K f]) (K I))) (K I)) =>
-- (S (S (K S) (S (S (K S) (S T[\f.K] T[\f.f])) (K I))) (K I)) =>
-- (S (S (K S) (S (S (K S) (S (K K) I)) (K I))) (K I))
contraction :: (a -> a -> b) -> a -> b
contraction =
  s
    (s (const s) (s (s (const s) (s (const const) identity)) (const identity)))
    (const identity)


-- It is larger then 80-100 symbols, but it is kinda inseparable
-- T[\f.\x.\y.f y x] =>
-- T[\f.T[\x.\y.f y x]] =>
-- T[\f.T[\x.T[\y.f y x]]] =>
-- T[\f.T[\x.(S T[\y.f y] T[\y.x])]] =>
-- T[\f.T[\x.(S (S T[\y.f] T[\y.y]) (K x))]] =>
-- T[\f.T[\x.(S (S (K f) I) (K x))]] =>
-- T[\f.(S T[\x.S (S (K f) I)] T[\x.K x])] =>
-- T[\f.(S (S T[\x.S] T[\x.S (K f) I]) (S T[\x.K] T[\x.x]))] =>
-- T[\f.(S (S (K S) (S T[\x.S (K f)] T[\x.I])) (S (K K) I))] =>
-- T[\f.(S (S (K S) (S (S T[\x.S] T[\x.K f]) (K I))) (S (K K) I))] =>
-- T[\f.(S (S (K S) (S (S (K S) (S T[\x.K] T[\x.f])) (K I))) (S (K K) I))] =>
-- T[\f.(S (S (K S) (S (S (K S) (S (K K) (K f))) (K I))) (S (K K) I))] =>
-- S T[\f.S (S (K S) (S (S (K S) (S (K K) (K f))) (K I)))] T[\f.(S (K K) I)] =>
-- S (S T[\f.S] T[\f.(S (K S) (S (S (K S) (S (K K) (K f))) (K I)))]) (K (S (K K) I)) =>
-- S (S (K S) (S T[\f.S (K S)] T[\f.S (S (K S) (S (K K) (K f))) (K I)])) (K (S (K K) I)) =>
-- S (S (K S) (S (S T[\f.S] T[\f.K S]) (S T[\f.S (S (K S) (S (K K) (K f)))] T[\f.K I]))) (K (S (K K) I)) =>
-- S (S (K S) (S (S (K S) (S T[\f.K] T[\f.S])) (S (S T[\f.S] T[\f.S (K S) (S (K K) (K f))]) (S T[\f.K] T[\f.I])))) (K (S (K K) I)) =>
-- S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S T[\f.S (K S)] T[\f.S (K K) (K f)])) (S (K K) (K I))))) (K (S (K K) I)) =>
-- S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K (S (K S))) (S T[\f.S (K K)] T[\f.K f]))) (S (K K) (K I))))) (K (S (K K) I)) =>
-- S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K (S (K S))) (S (K (S (K K))) (S T[\f.K] T[\f.f])))) (S (K K) (K I))))) (K (S (K K) I)) =>
-- S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K (S (K S))) (S (K (S (K K))) (S (K K) I)))) (S (K K) (K I))))) (K (S (K K) I))
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
