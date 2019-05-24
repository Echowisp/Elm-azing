module FastQueue exposing
  (Queue, empty, isEmpty, enqueue, dequeue, peek)


{-

  Note that this module is taken from the work we did in class
  on Queues

-}

type Queue a =
  Q { front : List a, back : List a }

empty : Queue a
empty = Q {front = [], back = []}

isEmpty : Queue a -> Bool
isEmpty q =
  q == empty

enqueue : a -> Queue a -> Queue a      -- O(1)
enqueue x (Q {front, back}) =
  case front of
    [] -> Q {front = [x], back = []}
    _  -> Q {front = front, back = x::back }

dequeue : Queue a -> Maybe (Queue a)   -- O(n)
dequeue (Q {front, back}) =
  case front of
    []    -> Nothing
    _::[] -> Just (Q {front = List.reverse back, back = []})
    _::f_ -> Just (Q {front = f_, back = back})

peek : Queue a -> Maybe a              -- O(1)
peek (Q {front, back}) =
  List.head front

------------------------------------------------------------------------------

checkFront : List a -> List a -> Queue a
checkFront f b =
  case f of
    [] -> Q {front = List.reverse b, back = []}
    _  -> Q {front = f, back = b}

enqueue_ : a -> Queue a -> Queue a      -- O(1)
enqueue_ x (Q {front, back}) =
  checkFront front (x::back)

dequeue_ : Queue a -> Maybe (Queue a)   -- O(n)
dequeue_ (Q {front, back}) =
  case front of
    []    -> Nothing
    _::f_ -> Just (checkFront f_ back)