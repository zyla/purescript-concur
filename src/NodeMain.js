exports.onStdin = function(handler) {
  return function() {
//    process.stdin.setRawMode(true);
    process.stdin.on('data', function(chunk) {
      if(chunk !== null) {
        handler(chunk.toString())();
      }
    });
  };
};
