// data Ref :: Type -> Type

// newRef :: forall eff a. a -> Eff eff (Ref a)
exports.newRef = function(initialValue) {
  return function() {
    return {
      // value :: a
      value: initialValue
    };
  };
};

// readRef :: forall eff a. Ref a -> Eff eff a
exports.readRef = function(ref) {
  return function() {
    return ref.value;
  };
};

// writeRef :: forall eff a. Ref a -> a -> -> Eff eff Unit
exports.writeRef = function(ref) {
  return function(newValue) {
    return function() {
      ref.value = newValue;
      return null;
    };
  };
};
