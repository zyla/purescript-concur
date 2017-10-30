  | Effect (Exists (EffectF eff k))

data EffectF eff k a = EffectF (Eff eff a) (a -> k)

--------------------------------------------------------------------------------

  | NotifyOp (Exists (NotifyOpF a))

data NotifyOpF a x =
    NewNotify (Notify x -> a)
  | Listen (Notify x) (x -> a)
  | Notify (Notify x) x a
