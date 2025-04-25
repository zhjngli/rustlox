# rustlox

following: https://craftinginterpreters.com/

## changes to Lox
1. allow for nested block comments: `/* ... /* ... */ ... */`
1. variables must be initialized before accessed (they don't default to `nil`)
1. disallow division by 0
1. when the left operand of `+` is a string, cast right operands into strings
1. add `break` statements. didn't add `continue`, it's a little more troublesome since it requires desugaring the for loop and performing the increment when the loop hits a `continue`.
