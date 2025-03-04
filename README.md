# Pop Language

> This is a toy!

Pop is a simple functional programming language that compiles to BEAM (Erlang Virtual Machine) bytecode.

## Features

- Function definitions with parameters
- Basic arithmetic operations (+, -, *, /)
- Comparison operators (==, >, <, >=, <=)
- Variable assignments
- If-else statements
- Return statements for explicit control flow
- String literals
- Boolean literals (true, false)
- Print and println functions for easy output
- Function calls
- Recursive approach by default
- Elixir-powered CLI interface
- Compiles to BEAM bytecode

## Requirements

- Erlang/OTP 24 or later
- Elixir 1.12 or later

## Installation

```bash
# Clone the repository
git clone https://github.com/yourusername/pop.git
cd pop

# Build the compiler
make

# Install globally (optional)
sudo make install
```

## Usage

```bash
# Show help
./pop --help

# Compile and run a Pop file
./pop examples/hello.pop

# Compile a Pop file
./pop compile examples/factorial.pop

# Run a compiled Pop file
./pop run examples/factorial.pop

# Show version
./pop --version
```

## Language Syntax

### Hello World

```
fun main() {
    io_format("Hello, World!\n");
}
```

### Functions

```
fun add(x, y) {
    x + y;
}

fun main() {
    result = add(5, 3);
    io_format("5 + 3 = ~p\n", result);
}
```

### Return Statements

```
fun max(a, b) {
    if (a > b) {
        return a;
    }
    return b;
}

fun early_return(n) {
    if (n < 0) {
        io_format("Error: Cannot process negative numbers\n");
        return;  // Early return with no value
    }
    
    return n * 2;  // Return with a value
}
```

### If-Else Statements

```
fun check_value(x) {
    if (x == 0) {
        println("Value is zero");
    } else {
        if (x > 0) {
            println("Value is positive");
        } else {
            println("Value is negative");
        }
    }
}
```

### Print and Println Functions

```
fun main() {
    println("Hello, World!");  // Prints with a newline
    
    print("Enter your name: "); // Prints without a newline
    
    a = 42;
    println("The answer is", a);
    
    println(); // Just prints a newline
    
    is_even = a % 2 == 0;
    print("Is a even? ");
    println(is_even);
}
```

### Recursive Fibonacci Example

```
fun fibonacci(n) {
    if (n == 0) {
        return 0;
    } else if (n == 1) {
        return 1;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

fun main() {
    println("fibonacci(7) =", fibonacci(7));
}
```

### Fibonacci Sequence with Limit

```
fun fibonacci(n) {
    if (n == 0) {
        return 0;
    } else if (n == 1) {
        return 1;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

fun print_fibonacci_sequence(n, limit) {
    if (n == 0) {
        result = fibonacci(n);
        println("fibonacci(", n, ") =", result);
        print_fibonacci_sequence(n + 1, limit);
    } else {
        result = fibonacci(n);
        if (result <= limit) {
            println("fibonacci(", n, ") =", result);
            print_fibonacci_sequence(n + 1, limit);
        } else {
            println("Reached limit:", limit);
        }
    }
}

fun main() {
    println("Fibonacci sequence up to 100:");
    print_fibonacci_sequence(0, 100);
}
```

### Tail-Recursive Fibonacci

```
fun fibonacci_tail(n) {
    fibonacci_helper(n, 0, 1);
}

fun fibonacci_helper(n, a, b) {
    if (n == 0) {
        return a;
    }
    return fibonacci_helper(n - 1, b, a + b);
}

fun main() {
    println("fibonacci(20) =", fibonacci_tail(20));
}
```

## Development

```bash
# Clean build artifacts
make clean

# Run tests
make test

# Compile examples
make compile_examples

# Show help
make help
```

## Architecture

The Pop language consists of:

1. **Lexer** (src/pop_lexer.xrl) - Tokenizes the source code
2. **Parser** (src/pop_parser.yrl) - Parses tokens into an AST
3. **Compiler** (src/pop_compiler.erl) - Compiles AST to BEAM bytecode
4. **CLI Interface** (lib/pop_cli.ex) - Elixir-powered command-line interface

## License

See the [LICENSE](LICENSE.md) file for details.