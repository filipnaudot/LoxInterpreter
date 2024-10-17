# Lox Interpreter

This project is an implementation of a [Lox](https://craftinginterpreters.com/the-lox-language.html) interpreter in Haskell.

## How to Run
To run the Lox interpreter with a .lox file, use the following command:
```bash
runhaskell lox.hs <file_name.lox>
```

## Running Tests
Python scripts are provided to run tests for scanner, parser, and interpreter.

To run the test suite:
   ```bash
   python3 test_lox.py
   ```
This script will execute the tests located in the `tests/interpreter/` folder.
