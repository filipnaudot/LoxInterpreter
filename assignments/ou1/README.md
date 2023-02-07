# Assagnment 1 - Lox lexer

This is a Lox lexer, a module that takes in a stream of characters and converts it into a stream of tokens. The lexer/scanner is responsible for breaking down the input code into a sequence of tokens that can be more easily parsed and understood by the next phase of the compiler or interpreter.

The main function of this module is `scanTokens`, which takes a list of characters and returns a list of `Token`s.

## How to build and run
The first step is to start `ghci` by running the following command: 
```bash
ghci
```

Now load the Scanner module with:
```bash
:l Scanner.hs
```

The Scanner should now be ready to use. For example:
```bash
scanTokens "let i = 0"
```
should produce the following output:

```bash
[TOKEN IDENTIFIER "let" (ID "let") 1,TOKEN IDENTIFIER "i" (ID "i") 1,
TOKEN EQUAL "=" NONE 1,
TOKEN NUMBER "0" (NUM 0.0) 1,
TOKEN EOF "" NONE 1]
```
The formating might look diffrent in your teminal but the values should be the same.

## TokenType

The `TokenType` data type is used to assign a type to each token that is created by the lexer. The `TokenType` data type is defined as an enumeration of all the different types of tokens that can be found in the input code. These types include:

- `LEFT_PAREN`: Represents the `(` character.
- `RIGHT_PAREN`: Represents the `)` character.
- `LEFT_BRACE`: Represents the `{` character.
- `RIGHT_BRACE`: Represents the `}` character.
- `COMMA`: Represents the `,` character.
- `DOT`: Represents the `.` character.
- `MINUS`: Represents the `-` character.
- `PLUS`: Represents the `+` character.
- `SEMICOLON`: Represents the `;` character.
- `SLASH`: Represents the `/` character.
- `STAR`: Represents the `*` character.
- `BANG`: Represents the `!` character.
- `BANG_EQUAL`: Represents the `!=` sequence of characters.
- `EQUAL`: Represents the `=` character.
- `EQUAL_EQUAL`: Represents the `==` sequence of characters.
- `GREATER`: Represents the `>` character.
- `GREATER_EQUAL`: Represents the `>=` sequence of characters.
- `LESS`: Represents the `<` character.
- `LESS_EQUAL`: Represents the `<=` sequence of characters.
- `IDENTIFIER`: Represents identifier tokens, such as variable names.
- `STRING`: Represents string literals.
- `NUMBER`: Represents numerical literals.
- `AND`: Represents the `and` keyword.
- `CLASS`: Represents the `class` keyword.
- `ELSE`: Represents the `else` keyword.



## Literal

The `Literal` data type is used for storing the actual values of string literals, identifiers, and numbers. It is defined as an enumeration of all the different types of literals that can be found in the input code. These types include:

- `NONE`: Represents the absence of a value.
- `STR String`: Represents a string literal.
- `ID String`: Represents an identifier.
- `NUM Float`: Represents a numerical literal.
- `FALSE_LIT`: Represents the `false` keyword.
- `TRUE_LIT`: Represents the `true` keyword.
- `NIL_LIT`: Represents the `nil` keyword.

## Token

The `Token` data type represents the tokens that the scanner produces and which are used as input for the parser. It contains the following fields:

- `TokenType`: The type of the token.
- `String`: The string representation of the token.
- `Literal`: The actual value of the token, if it has one (e.g. the value of a string literal or a numerical literal).
- `Int` : The line number where the token is found


<br/>
<br/>
<br/>

## Program overview
The `scanTokens` function uses the `scan` function to scan the input string character by character, and uses a case statement to identify and handle different types of characters. For example, when it encounters an open parenthesis, it creates a new `Token` with `TokenType` `LEFT_PAREN`, `String` representation `"("`, `Literal` value `NONE`, and the current line number.
