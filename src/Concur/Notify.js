// data Channel :: Type -> Type
// newtype AsyncEff eff a = AsyncEff ((a -> Eff eff Unit) -> Eff eff Unit)

// newChannel :: forall eff a. AsyncEff eff (Channel a)
exports.newChannel = function(cont) {
  return function() {
    console.log('new channel');
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
        console.log('await: available');
        cont(channel.items.shift())();
      } else {
        console.log('await: wait');
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
          console.log('yield: available');
          channel.waiters.shift()(item)();
        } else {
          console.log('yield: wait');
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
        var onceCont = function(x) {
          return function() {
            cont(x)();
            onceCont = function(_x) {
              return function() {};
            };
          };
        };
        a(onceCont)();
        b(onceCont)();
      };
    };
  };
};
