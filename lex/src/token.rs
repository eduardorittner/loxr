use std::range::Range;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Range<usize>,
}

impl Token {
    pub fn len(&self) -> usize {
        self.span.end - self.span.start
    }

    pub fn text<'input>(&self, input: &'input str) -> &'input str {
        &input[self.span]
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} - <{}, {}>",
            self.kind, self.span.start, self.span.end
        )
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    Slash,
    Bang,
    Eq,
    Less,
    Greater,
    BangEq,
    EqEq,
    LessEq,
    GreaterEq,
    TString,
    Number,
    Ident,
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Return,
    Print,
    Super,
    This,
    True,
    Var,
    While,
}

#[macro_export]
macro_rules! T {
    ['('] => {
        $crate::token::TokenKind::LeftParen
    };
    [')'] => {
        $crate::token::TokenKind::RightParen
    };
    ['{'] => {
        $crate::token::TokenKind::LeftBracket
    };
    ['}'] => {
        $crate::token::TokenKind::RightBracket
    };
    [+] => {
        $crate::token::TokenKind::Plus
    };
    [-] => {
        $crate::token::TokenKind::Minus
    };
    [*] => {
        $crate::token::TokenKind::Star
    };
    [/] => {
        $crate::token::TokenKind::Slash
    };
    [.] => {
        $crate::token::TokenKind::Dot
    };
    [,] => {
        $crate::token::TokenKind::Comma
    };
    [;] => {
        $crate::token::TokenKind::Semicolon
    };
    [!] => {
        $crate::token::TokenKind::Bang
    };
    [=] => {
        $crate::token::TokenKind::Eq
    };
    [<] => {
        $crate::token::TokenKind::Less
    };
    [>] => {
        $crate::token::TokenKind::Greater
    };
    [!=] => {
        $crate::token::TokenKind::BangEq
    };
    [==] => {
        $crate::token::TokenKind::EqEq
    };
    [<=] => {
        $crate::token::TokenKind::LessEq
    };
    [>=] => {
        $crate::token::TokenKind::GreaterEq
    };
    [and] => {
        $crate::token::TokenKind::And
    };
    [class] => {
        $crate::token::TokenKind::Class
    };
    [else] => {
        $crate::token::TokenKind::Else
    };
    [false] => {
        $crate::token::TokenKind::False
    };
    [for] => {
        $crate::token::TokenKind::For
    };
    [fun] => {
        $crate::token::TokenKind::Fun
    };
    [if] => {
        $crate::token::TokenKind::If
    };
    [nil] => {
        $crate::token::TokenKind::Nil
    };
    [or] => {
        $crate::token::TokenKind::Or
    };
    [return] => {
        $crate::token::TokenKind::Return
    };
    [print] => {
        $crate::token::TokenKind::Print
    };
    [super] => {
        $crate::token::TokenKind::Super
    };
    [this] => {
        $crate::token::TokenKind::This
    };
    [true] => {
        $crate::token::TokenKind::True
    };
    [var] => {
        $crate::token::TokenKind::Var
    };
    [while] => {
        $crate::token::TokenKind::While
    };
    [num] => {
        $crate::token::TokenKind::Number
    };
    [string] => {
        $crate::token::TokenKind::TString
    };
    [ident] => {
        $crate::token::TokenKind::Ident
    };
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                T![+] => "+",
                T![-] => "-",
                T![*] => "*",
                T![/] => "/",
                T!['('] => "(",
                T![')'] => ")",
                T!['{'] => "{",
                T!['}'] => "}",
                T![num] => "number",
                T![fun] => "fun",
                T![var] => "var",
                T![;] => ";",
                _ => todo!(),
            }
        )
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn token_kind_display() {
        assert_eq!(T![+].to_string(), "+");
        assert_eq!(T![-].to_string(), "-");
        assert_eq!(T![*].to_string(), "*");
        assert_eq!(T![/].to_string(), "/");
    }
}
