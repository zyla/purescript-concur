// data Channel :: Type -> Type
// newtype AsyncEff eff a = AsyncEff ((a -> Eff eff Unit) -> Eff eff Unit)

// newChannel :: forall eff a. AsyncEff eff (Channel a)
exports.newChannel = function(cont) {
  return function() {
    return cont({
      // waiters :: Array (a -> Eff eff Unit)
      waiters: [],
      // items :: Array a
      items: []
    })();
  };
};

// await :: forall eff a. Channel a -> AsyncEff eff a
exports.await = function(channel) {
  return function(cont) {
    return function() {
      if(channel.items.length > 0) {
        cont(channel.items.shift())();
      } else {
        channel.waiters.push(cont);
      }
    };
  };
};

// yield :: forall eff a. Channel a -> a -> AsyncEff eff Unit
exports.yield = function(channel) {
  return function(item) {
    return function(cont) {
      return function() {
        if(channel.waiters.length > 0) {
          channel.waiters.shift()(item)();
        } else {
          channel.items.push(item);
        }
        cont(null)();
      };
    };
  };
};

// race :: forall eff a. AsyncEff eff a -> AsyncEff eff a -> AsyncEff eff a
exports.race = function(a) {
  return function(b) {
    return function(cont) {
      return function() {
        var finished = false;
        var onceCont = function(x) {
          return function() {
            if(!finished) {
              cont(x)();
              finished = true;
            }
          };
        };
        a(onceCont)();
        b(onceCont)();
      };
    };
  };
};

// race2 :: forall eff a
//   . AsyncEff eff a
//  -> AsyncEff eff a
//  -> AsyncEff eff { left :: Boolean, winning :: a, losing: AsyncEff eff a }
exports.race2 = function(a) {
  return function(b) {
    return function(cont) {
      return function() {
        var losingCont = null;
        var losingFinished = false;
        var losingFinishedValue = null;

        var onceCont = function(leftWasFirst, winningValue) {
          return function() {
            onceCont = function(_, losingValue) {
              return function() {
                if(losingCont) {
                  losingCont(losingValue)();
                } else {
                  losingFinished = true;
                  losingFinishedValue = losingValue;
                }
              };
            };
            cont({
              left: leftWasFirst,
              winning: winningValue,
              losing: function(cont2) {
                return function() {
                  if(losingFinished) {
                    cont2(losingFinishedValue)();
                  } else {
                    losingCont = cont2;
                  }
                };
              }
            })();
          };
        };
        a(function(v) {
          return function() {
            onceCont(true, v)();
          };
        })();
        b(function(v) {
          return function() {
            onceCont(false, v)();
          };
        })();
      };
    };
  };
};
