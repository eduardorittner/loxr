use lex::*;

macro_rules! assert_tokens {
    ($tokens:ident, [$($kind:expr,)*]) => {
        {
            let mut it = $tokens.iter();
            $(
                let token = it.next().expect("not enough tokens");
                assert_eq!(token.kind, $kind);
            )*
        }
    };
}

#[test]
fn single_char_tokens() {
    let input = "+-(.)";
    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize().unwrap();
    assert_tokens!(tokens, [T![+], T![-], T!['('], T![.], T![')'],]);
}

#[test]
fn keywords() {
    let input = "if var = else fun";
    let mut lexer = Lexer::new(input);
    let tokens: Vec<_> = lexer.tokenize().unwrap();
    assert_tokens!(tokens, [T![if], T![var], T![=], T![else], T![fun],]);
}

// TODO add some more lexer tests
