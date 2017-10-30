exports.onStdin = function(handler) {
  return function() {
    console.log('onStdin');
    process.stdin.on('readable', function() {
      var chunk = process.stdin.read();
      if(chunk !== null) {
        console.log('stdin available');
        handler(chunk)();
      }
    });
  };
};
