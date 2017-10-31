
// setEventHandler :: forall eff. Node -> String -> Eff eff Unit -> Eff eff Unit
exports.setEventHandler = function(node) {
  return function(eventName) {
    return function(handler) {
      return function() {
        node[eventName] = function() {
          handler();
        };
      };
    };
  };
};

// getValue :: forall eff. Node -> Eff eff String
exports.getValue = function(node) {
  return function() {
    return node.value;
  };
};
