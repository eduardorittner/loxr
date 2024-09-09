A implementation of the lox bytecode interpreter and compiler in rust.

Some major goals:
- Complete implementation of the language
- Nice error reporting with miette

Some not so major goals:
- Debug trace with cfg feature
- Three byte operands allowing for 2^24 constants in a program
- Smart handling of strings (avoid clones and Strings everywhere), which would require a bunch of more lifetimes and stuff.
- Smart handling of global variable names. For now everytime a variable is encountered its name is added to the constants table, even if it was already defined and is already in the constants table
