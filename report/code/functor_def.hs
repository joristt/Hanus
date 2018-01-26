(=$$) :: Functor f => Operator (f a) (Operator a b, b)
(=$$) = Operator forward backward
  where
    forward  f (Operator fwd _, x) = fmap (`fwd` x) f
    backward f (Operator _ bwd, x) = fmap (`bwd` x) f
