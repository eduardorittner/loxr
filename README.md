An implementation of the lox bytecode interpreter and compiler in rust from [Crafting Interpreters](https://craftinginterpreters.com/).

# Lox

Lox is a high-level language with C-like syntax, dynamic typing, automatic memory management, classes and methods (functions).

# Loxr

Loxr is a compiler + interpreter in one, it supports reading programs from files (.lox) and a REPL. The input is first tokenized into lexemes, then parsed and compiled directly to lox bytecode, which is then executed. There is no IR or AST stage, it goes directly from parsing to bytecode, which means there are no optimizations at compile time.

# Some major goals:
- Complete implementation of the language
- Have an AST IR
- Implement both an AST vm and a bytecode vm
- Some small optimization passes

# Some not so major goals:
- Debug trace of program execution with conditional compilation (rust's cfg feature)
- Smart handling of strings (avoid clones and Strings everywhere), which would require a bunch of more lifetimes and stuff.
- Smart handling of global variable names. For now everytime a variable is encountered its name is added to the constants table, even if it was already defined and is already in the constants table
