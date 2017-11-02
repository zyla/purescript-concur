# purescript-concur

An attempt to port the [concur](https://github.com/ajnsit/concur) library to PureScript.

## State

It somewhat works, you can make a UI with it. See `examples/todomvc`.

Needs documentation on semantics of `Widget`.

Has severe performance problems. Rendering is probably quadratic in the number
of DOM elements placed sequentially. This should be fixable.
