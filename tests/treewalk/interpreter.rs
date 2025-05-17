use std::process::{Command, Output};

enum ErrorType {
    ParseError,
    StaticError,
    RuntimeError,
    None,
}

fn run_interpreter(lox_file: &str) -> Output {
    Command::new("./target/debug/rlox")
        .args(&[lox_file])
        .output()
        .expect("Failed to execute interpreter")
}

fn test_lox_script(lox_script: &str, err: ErrorType, expected_out: &str, expected_err: &str) {
    let output = run_interpreter(&format!("lox_scripts/{}", lox_script));
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert_eq!(
        match err {
            ErrorType::ParseError => 65,
            ErrorType::StaticError => 65,
            ErrorType::RuntimeError => 70,
            ErrorType::None => 0,
        },
        output.status.code().unwrap()
    );
    assert_eq!(expected_out, stdout);
    assert_eq!(expected_err, stderr);
}

mod scripts {
    use crate::test_lox_script;
    use crate::ErrorType;

    #[test]
    fn test_hashmap() {
        test_lox_script(
            "hashmap.lox",
            ErrorType::None,
            "map: [[], [], [], []]\nsize: 6\nbuckets length: 16\nget 1: 2\nold value at 1: 2\nget 1: 42\nget 10: null\nremove 1: 42\nremove 2: 4\nremove 20: null\nsize: 4\nfinal hashmap:\nbucket[0]\n    (4, 8)\n    (5, 10)\nbucket[1]\nbucket[2]\nbucket[3]\n    (0, 0)\nbucket[4]\n    (3, 6)\nbucket[5]\nbucket[6]\nbucket[7]\nbucket[8]\nbucket[9]\nbucket[10]\nbucket[11]\nbucket[12]\nbucket[13]\nbucket[14]\nbucket[15]\n",
            "",
        );
    }

    #[test]
    fn test_linked_list() {
        test_lox_script(
            "linked_list.lox",
            ErrorType::None,
            "1\n2\n3\n4\n2\n4\n6\n8\n",
            "",
        );
    }
}

mod classes {
    use crate::test_lox_script;
    use crate::ErrorType;

    #[test]
    fn test_bound_fields() {
        test_lox_script(
            "classes_bound_fields.lox",
            ErrorType::None,
            "The German chocolate cake is delicious!\n",
            "",
        );
    }

    #[test]
    fn test_bound_methods() {
        test_lox_script("classes_bound_methods.lox", ErrorType::None, "Jane\n", "");
    }

    #[test]
    fn test_getter_method() {
        test_lox_script(
            "classes_getter_method.lox",
            ErrorType::None,
            "50.265482448\n",
            "",
        );
    }

    #[test]
    fn test_initializer() {
        test_lox_script(
            "classes_initializer.lox",
            ErrorType::StaticError,
            "",
            "[line 3] Error at 'return': Can't return a value from an initializer.\nStaticError\n",
        );
    }

    #[test]
    fn test_initializer_return_early() {
        test_lox_script(
            "classes_initializer_return_early.lox",
            ErrorType::None,
            "truthy val passed in\nno truthy val passed in\n",
            "",
        );
    }

    #[test]
    fn test_static_initializer() {
        test_lox_script(
            "classes_static_initializer.lox",
            ErrorType::StaticError,
            "",
            "[line 2] Error at 'init': Initializers can't be class methods.\nStaticError\n",
        );
    }

    #[test]
    fn test_static_method() {
        test_lox_script("classes_static_method.lox", ErrorType::None, "9\n2\n", "");
    }

    #[test]
    fn test_static_no_super() {
        test_lox_script(
            "classes_static_no_super.lox",
            ErrorType::StaticError,
            "",
            "[line 13] Error at 'super': Can't use 'super' in a class with no superclass.\nStaticError\n",
        );
    }

    #[test]
    fn test_static_no_this() {
        test_lox_script(
            "classes_static_no_this.lox",
            ErrorType::RuntimeError,
            "",
            "Runtime Error: [line 3] Error at 'this': Undefined variable 'this'.\n",
        );
    }

    #[test]
    fn test_this_callback() {
        test_lox_script(
            "classes_this_callback.lox",
            ErrorType::None,
            "<instance Thing>\n",
            "",
        );
    }

    #[test]
    fn test_this_not_in_method() {
        test_lox_script(
            "classes_this_not_in_method.lox",
            ErrorType::StaticError,
            "",
            "[line 1] Error at 'this': Can't use 'this' outside of a class.\nStaticError\n",
        );
    }

    #[test]
    fn test_this_setter_method() {
        test_lox_script(
            "classes_this_setter_method.lox",
            ErrorType::None,
            "1\n2\n",
            "",
        );
    }
}

mod comments {
    use crate::test_lox_script;
    use crate::ErrorType;

    #[test]
    fn test_comments() {
        test_lox_script("comments.lox", ErrorType::None, "comments!\n", "");
    }
}

mod control_flow {
    use crate::test_lox_script;
    use crate::ErrorType;

    #[test]
    fn test_break_early_fibonacci() {
        test_lox_script(
            "control_flow_break_early_fibonacci.lox",
            ErrorType::None,
            "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n89\n",
            "",
        );
    }

    #[test]
    fn test_break_outside_loop() {
        test_lox_script(
            "control_flow_break_outside_loop.lox",
            ErrorType::StaticError,
            "",
            "[line 3] Error at ';': 'break' statement not inside a loop.\nParseError\n",
        );
    }

    #[test]
    fn test_for_loop_fibonacci() {
        test_lox_script(
            "control_flow_for_loop_fibonacci.lox",
            ErrorType::None,
            "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n89\n144\n233\n377\n610\n987\n1597\n2584\n4181\n",
            "",
        );
    }

    #[test]
    fn test_nested_loop_break() {
        test_lox_script(
            "control_flow_nested_loop_break.lox",
            ErrorType::None,
            "11\n55\n",
            "",
        );
    }

    #[test]
    fn test_print_truthy() {
        test_lox_script(
            "control_flow_print_truthy.lox",
            ErrorType::None,
            "hi\nyes\n",
            "",
        );
    }

    #[test]
    fn test_while_and_if() {
        test_lox_script(
            "control_flow_while_and_if.lox",
            ErrorType::None,
            "0\n1\n2\n3\n4\nfive\n6\n7\n8\n9\n",
            "",
        );
    }
}

mod functions {
    use crate::test_lox_script;
    use crate::ErrorType;

    #[test]
    fn test_arity() {
        test_lox_script(
            "function_arity.lox",
            ErrorType::RuntimeError,
            "",
            "Runtime Error: [line 5] Error at ')': Expected 3 arguments but got 4.\n",
        );
    }

    #[test]
    fn test_closure() {
        test_lox_script("function_closure.lox", ErrorType::None, "1\n2\n", "");
    }

    #[test]
    fn test_hi() {
        test_lox_script(
            "function_hi.lox",
            ErrorType::None,
            "Hi, integration tester!\n",
            "",
        );
    }

    #[test]
    fn test_pass_by_value_or_reference() {
        test_lox_script(
            "function_pass_by_value_or_reference.lox",
            ErrorType::None,
            "1\n1\n2\n1\n1\n",
            "",
        );
    }

    #[test]
    fn test_recursive_fibonacci() {
        test_lox_script(
            "function_recursive_fibonacci.lox",
            ErrorType::None,
            "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n55\n89\n144\n233\n377\n610\n987\n1597\n2584\n4181\n",
            "",
        );
    }

    #[test]
    fn test_return() {
        test_lox_script("function_return_early.lox", ErrorType::None, "1\n2\n", "");
    }

    #[test]
    fn test_string() {
        test_lox_script(
            "function_string.lox",
            ErrorType::RuntimeError,
            "",
            "Runtime Error: [line 1] Error at ')': Can only call functions and classes\n",
        );
    }
}

mod inheritance {
    use crate::test_lox_script;
    use crate::ErrorType;

    #[test]
    fn test_grandchild_method() {
        test_lox_script(
            "inheritance_grandchild_method.lox",
            ErrorType::None,
            "A method\n",
            "",
        );
    }

    #[test]
    fn test_not_a_class() {
        test_lox_script(
            "inheritance_not_a_class.lox",
            ErrorType::RuntimeError,
            "",
            "Runtime Error: [line 3] Error at 'NotAClass': Superclass must be a class.\n",
        );
    }

    #[test]
    fn test_self() {
        test_lox_script(
            "inheritance_self.lox",
            ErrorType::StaticError,
            "",
            "[line 1] Error at 'Oops': A class can't inherit from itself.\nStaticError\n",
        );
    }

    #[test]
    fn test_super_methods() {
        test_lox_script(
            "inheritance_super_methods.lox",
            ErrorType::None,
            "Fry until golden brown.\nPipe full of custard and coat with chocolate.\n",
            "",
        );
    }

    #[test]
    fn test_super_no_superclass() {
        test_lox_script(
            "inheritance_super_no_superclass.lox",
            ErrorType::StaticError,
            "",
            "[line 3] Error at 'super': Can't use 'super' in a class with no superclass.\nStaticError\n",
        );
    }

    #[test]
    fn test_super_outside_class() {
        test_lox_script(
            "inheritance_super_outside_class.lox",
            ErrorType::StaticError,
            "",
            "[line 1] Error at 'super': Can't use 'super' outside of a class.\nStaticError\n",
        );
    }
}

mod lists {
    use crate::test_lox_script;
    use crate::ErrorType;

    #[test]
    fn test_get_and_set() {
        test_lox_script(
            "lists_get_and_set.lox",
            ErrorType::None,
            "[1, 2, 3]\n4\n[1, 4, 3]\n[1, 4, 3]\n2\n",
            "",
        );
    }

    #[test]
    fn test_index_non_list() {
        test_lox_script(
            "lists_index_non_list.lox",
            ErrorType::RuntimeError,
            "",
            "Runtime Error: [line 1] Error at '[': Only lists can be indexed.\n",
        );
    }

    #[test]
    fn test_index_not_a_number() {
        test_lox_script(
            "lists_index_not_a_number.lox",
            ErrorType::RuntimeError,
            "",
            "Runtime Error: [line 1] Error at '[': Index must be a number.\n",
        );
    }

    #[test]
    fn test_index_out_of_bounds() {
        test_lox_script(
            "lists_index_out_of_bounds.lox",
            ErrorType::RuntimeError,
            "",
            "Runtime Error: [line 1] Error at '[': Index out of bounds: 3\n",
        );
    }

    #[test]
    fn test_index_round_down() {
        test_lox_script("lists_index_round_down.lox", ErrorType::None, "3\n", "");
    }

    #[test]
    fn test_pop_and_append() {
        test_lox_script(
            "lists_pop_and_append.lox",
            ErrorType::None,
            "3\n2\n4\n5\n[1, 4, 5]\n",
            "",
        );
    }
}

mod native_function {
    use crate::test_lox_script;
    use crate::ErrorType;

    #[test]
    fn test_hash() {
        test_lox_script(
            "native_function_hash.lox",
            ErrorType::RuntimeError,
            "6754655388054642\n3997180422823453\n2418384539218088\n3682412656705596\n",
            "Runtime Error: [line 8] Error at ')': Native function error: Can only hash bools, nums, and strings.\n",
        );
    }
}

mod numbers {
    use crate::test_lox_script;
    use crate::ErrorType;

    #[test]
    fn test_arithmetic() {
        test_lox_script(
            "numbers_arithmetic.lox",
            ErrorType::None,
            "50.24\n50.285714285714285\n2\n",
            "",
        );
    }

    #[test]
    fn test_parse_error() {
        test_lox_script("numbers_parse_error.lox", ErrorType::ParseError, "", "[line 1] Error at ';': Expect property name after '.'.\n[line 1] Error at ';': Expect expression.\n[line 2] Error at '.': Expect expression.\n[line 2] Error at '.': Expect expression.\nParseError\n");
    }
}

mod scope {
    use crate::test_lox_script;
    use crate::ErrorType;

    #[test]
    fn test_function_closure() {
        test_lox_script(
            "scope_function_closure.lox",
            ErrorType::None,
            "global\nglobal\n",
            "",
        );
    }

    #[test]
    fn test_initializer() {
        test_lox_script(
            "scope_initializer.lox",
            ErrorType::StaticError,
            "",
            "[line 3] Error at 'a': Can't read local variable in its own initializer.\nStaticError\n",
        );
    }

    #[test]
    fn test_shadowing() {
        test_lox_script("scope_shadowing.lox", ErrorType::None, "outer\ninner\n", "");
    }

    #[test]
    fn test_return_top_level() {
        test_lox_script(
            "scope_return_top_level.lox",
            ErrorType::StaticError,
            "",
            "[line 1] Error at 'return': Can't return from top-level code.\nStaticError\n",
        );
    }

    #[test]
    fn test_variable_exists() {
        test_lox_script(
            "scope_variable_exists.lox",
            ErrorType::StaticError,
            "",
            "[line 3] Error at 'a': Already a variable with this name in this scope.\nStaticError\n",
        );
    }
}

mod strings {
    use crate::test_lox_script;
    use crate::ErrorType;

    #[test]
    fn test_coercion() {
        test_lox_script(
            "strings_coercion.lox",
            ErrorType::None,
            "abctruefalse1\n",
            "",
        );
    }

    #[test]
    fn test_concatenation() {
        test_lox_script("strings_concatenation.lox", ErrorType::None, "abc\n", "");
    }
}

mod variables {
    use crate::test_lox_script;
    use crate::ErrorType;

    #[test]
    fn test_undefined() {
        test_lox_script(
            "variables_undefined.lox",
            ErrorType::RuntimeError,
            "",
            "Runtime Error: [line 1] Error at 'a': Undefined variable 'a'.\n",
        );
    }

    #[test]
    fn test_unitialized() {
        test_lox_script(
            "variables_uninitialized.lox",
            ErrorType::RuntimeError,
            "",
            "Runtime Error: [line 2] Error at 'a': Variable defined but not initialized 'a'.\n",
        );
    }
}
