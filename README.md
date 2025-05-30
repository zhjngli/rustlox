# rustlox

following: https://craftinginterpreters.com/

## proof of concepts
Implemented a [hashmap](./lox_scripts/hashmap.lox)!

## changes to (tree-walk interpreted) Lox
1. add support for lists ([usage](./lox_scripts/lists_get_and_set.lox))
1. add static methods to classes using metaclasses ([usage](./lox_scripts/classes_static_method.lox))
1. add getter methods to classes ([usage](./lox_scripts/classes_getter_method.lox))
1. add `hash` native function ([usage](./lox_scripts/native_function_hash.lox))
1. support modulus `%` operator
1. allow for nested block comments: `/* ... /* ... */ ... */`
1. variables must be initialized before accessed (they don't default to `nil`)
1. disallow division by 0
1. when the left operand of `+` is a string, cast right operands into strings
1. add `break` statements ([usage](./lox_scripts/control_flow_break_early_fibonacci.lox)). didn't add `continue`, it's a little more troublesome since it requires desugaring the for loop and performing the increment when the loop hits a `continue`.
