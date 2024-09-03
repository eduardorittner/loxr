A implementation of the lox bytecode interpreter and compiler in rust.

Some major goals:
- Complete implementation of the language
- Nice error reporting with miette

Some not so major goals:
- Debug trace with cfg feature
- Three byte operands allowing for 2^24 constants in a program
