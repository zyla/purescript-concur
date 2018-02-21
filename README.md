# purescript-concur

An attempt to port the [concur](https://github.com/ajnsit/concur) library to PureScript.

## State

**Note**: I'm not developing this anymore (focusing instead on [purescript-specular](https://github.com/restaumatic/purescript-specular)), but the author or Concur for Haskell has started his own PureScript port: <https://github.com/ajnsit/purescript-concur>.

It somewhat works, you can make a UI with it. See `examples/todomvc`.

Needs documentation on semantics of `Widget`.

Has severe performance problems. Rendering is probably quadratic in the number
of DOM elements placed sequentially. This should be fixable.
