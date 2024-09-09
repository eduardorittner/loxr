use miette::{Diagnostic, Error, LabeledSpan};
use std::fmt;
use std::hash::*;
use std::mem;
use thiserror::Error;
use TokenKind::*;

#[derive(Diagnostic, Error, Debug)]
#[error("Unexpected Eof")]
pub struct Eof;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'de> {
    pub source: &'de str,
    pub offset: usize,
    pub kind: TokenKind,
}

#[derive(Debug, Clone, Copy)]
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
    Number(f64),
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

impl PartialEq for TokenKind {
    fn eq(&self, other: &Self) -> bool {
        // mem::discriminant takes only the enum variant, so
        // that we only compare the tokenkind, and not the
        // inner value (in the case of number)
        // So to check if two Number enums are the same,
        // the "==" operator won't work, the contained
        // values have to be checked manually
        mem::discriminant(self) == mem::discriminant(other)
    }
}

impl std::cmp::Eq for TokenKind {}

impl Hash for TokenKind {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            // We implement hash manually to be able to
            // ignore the value in the Number variant
            LeftParen => state.write_u8(0),
            RightParen => state.write_u8(1),
            LeftBracket => state.write_u8(2),
            RightBracket => state.write_u8(3),
            Comma => state.write_u8(4),
            Dot => state.write_u8(5),
            Minus => state.write_u8(6),
            Plus => state.write_u8(7),
            Semicolon => state.write_u8(8),
            Star => state.write_u8(9),
            Slash => state.write_u8(10),
            Bang => state.write_u8(11),
            Eq => state.write_u8(12),
            Less => state.write_u8(13),
            Greater => state.write_u8(14),
            BangEq => state.write_u8(15),
            EqEq => state.write_u8(16),
            LessEq => state.write_u8(17),
            GreaterEq => state.write_u8(18),
            TString => state.write_u8(19),
            Number(_) => state.write_u8(20),
            Ident => state.write_u8(21),
            And => state.write_u8(22),
            Class => state.write_u8(23),
            Else => state.write_u8(24),
            False => state.write_u8(25),
            For => state.write_u8(26),
            Fun => state.write_u8(27),
            If => state.write_u8(28),
            Nil => state.write_u8(29),
            Or => state.write_u8(30),
            Return => state.write_u8(31),
            Super => state.write_u8(32),
            This => state.write_u8(33),
            True => state.write_u8(34),
            Var => state.write_u8(35),
            While => state.write_u8(36),
            Print => state.write_u8(37),
        }
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let source = self.source;
        match self.kind {
            LeftParen => write!(f, "{source}"),
            RightParen => write!(f, "{source}"),
            LeftBracket => write!(f, "{source}"),
            RightBracket => write!(f, "{source}"),
            Comma => write!(f, "{source}"),
            Dot => write!(f, "{source}"),
            Minus => write!(f, "{source}"),
            Plus => write!(f, "{source}"),
            Semicolon => write!(f, "{source}"),
            Star => write!(f, "{source}"),
            Slash => write!(f, "{source}"),
            Bang => write!(f, "{source}"),
            Eq => write!(f, "{source}"),
            Less => write!(f, "{source}"),
            Greater => write!(f, "{source}"),
            BangEq => write!(f, "{source}"),
            EqEq => write!(f, "{source}"),
            LessEq => write!(f, "{source}"),
            GreaterEq => write!(f, "{source}"),
            TString => write!(f, "{source}"),
            Number(_) => write!(f, "{source}"),
            Ident => write!(f, "{source}"),
            TokenKind::And => write!(f, "{source}"),
            TokenKind::Class => write!(f, "{source}"),
            TokenKind::Else => write!(f, "{source}"),
            TokenKind::False => write!(f, "{source}"),
            TokenKind::For => write!(f, "{source}"),
            TokenKind::Fun => write!(f, "{source}"),
            TokenKind::If => write!(f, "{source}"),
            TokenKind::Nil => write!(f, "{source}"),
            TokenKind::Or => write!(f, "{source}"),
            TokenKind::Return => write!(f, "{source}"),
            TokenKind::Super => write!(f, "{source}"),
            TokenKind::This => write!(f, "{source}"),
            TokenKind::True => write!(f, "{source}"),
            TokenKind::Var => write!(f, "{source}"),
            TokenKind::While => write!(f, "{source}"),
            TokenKind::Print => write!(f, "{source}"),
        }
    }
}

pub struct Lexer<'de> {
    whole: &'de str,
    rest: &'de str,
    offset: usize,
    peeked: Option<Result<Token<'de>, miette::Error>>,
}

impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            rest: input,
            offset: 0,
            peeked: None,
        }
    }

    pub fn expect_number(&mut self, unexpected: &str) -> miette::Result<Token<'de>> {
        self.expect_where(|t| matches!(t.kind, TokenKind::Number(_)), unexpected)
    }

    pub fn expect(&mut self, expected: TokenKind, unexpected: &str) -> miette::Result<Token<'de>> {
        self.expect_where(|t| t.kind == expected, unexpected)
    }

    pub fn expect_where(
        &mut self,
        mut check: impl FnMut(&Token<'de>) -> bool,
        unexpected: &str,
    ) -> miette::Result<Token<'de>> {
        match self.next() {
            Some(Ok(token)) if check(&token) => Ok(token),
            Some(Ok(token)) => Err(miette::miette! {
                labels = vec![LabeledSpan::at(token.offset..token.offset + token.source.len(), "here")],
                help = format!("Expected {token:?}"),
                "{unexpected}"
            }.with_source_code(self.whole.to_string())),
            Some(Err(e)) => Err(e),
            None => Err(miette::miette!{
                labels = vec![LabeledSpan::at(self.offset - 1..self.offset, "here")], 
                "{unexpected}",
            }.with_source_code(self.whole.to_string())),
        }
    }

    // TODO: this is wrong
    pub fn peek(&mut self) -> Option<&Result<Token<'de>, miette::Error>> {
        if self.peeked.is_some() {
            return self.peeked.as_ref();
        }

        self.peeked = self.next();
        self.peeked.as_ref()
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.peeked.take() {
            return Some(next);
        }
        loop {
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let c_offset = self.offset;
            let c_str = &self.rest[..c.len_utf8()];
            let c_onwards = self.rest;

            self.rest = chars.as_str();
            self.offset += c.len_utf8();

            let just = move |kind: TokenKind| {
                Some(Ok(Token {
                    kind,
                    offset: c_offset,
                    source: c_str,
                }))
            };

            enum Started {
                TString,
                Ident,
                Number,
                IfEqualElse(TokenKind, TokenKind),
            }

            let started = match c {
            '(' => return just(LeftParen),
            ')' => return just(RightParen),
            '{' => return just(LeftBracket),
            '}' => return just(RightBracket),
            ',' => return just(Comma),
            '.' => return just(Dot),
            '-' => return just(Minus),
            '+' => return just(Plus),
            ';' => return just(Semicolon),
            '*' => return just(Star),
            '/' => return just(Slash),
            '<' => Started::IfEqualElse(LessEq, Less),
            '>' => Started::IfEqualElse(GreaterEq, Greater),
            '!' => Started::IfEqualElse(BangEq, Bang),
            '=' => Started::IfEqualElse(EqEq, Eq),
            '"' => Started::TString,
            '0'..='9' => Started::Number,
            'a'..='z' => Started::Ident,
            c if c.is_whitespace() => continue,
            c => {
                return Some(Err(miette::miette! {
                    labels = vec![LabeledSpan::at(self.offset - c.len_utf8()..self.offset, "this character")],
                    "Unexpected token '{c}'",
                }
                .with_source_code(self.whole.to_string())))
            }
        };
            break match started {
                Started::TString => {
                    let Some(end_of_str) = c_onwards[1..].find(|c| matches!(c, '"')) else {
                        return Some(Err(miette::miette! {
                            labels = vec![LabeledSpan::at(self.offset - c.len_utf8()..self.offset, "this string")],
                            "Unterminated string",
                        }.with_source_code(self.whole.to_string())));
                    };

                    let lit = &c_onwards[1..end_of_str + 1];
                    let lit_offset = lit.len() - c.len_utf8();

                    // +2 for opening and closing '"'
                    self.offset += lit_offset + 2;
                    self.rest = &self.rest[lit_offset + 2..];

                    return Some(Ok(Token {
                        kind: TString,
                        offset: c_offset,
                        source: lit,
                    }));
                }
                Started::Ident => {
                    let first_non_ident = c_onwards
                        .find(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
                        .unwrap_or(c_onwards.len());

                    let lit = &c_onwards[..first_non_ident];
                    let lit_offset = lit.len() - c.len_utf8();
                    self.offset += lit_offset;
                    self.rest = &self.rest[lit_offset..];

                    let kind = match lit {
                        "and" => And,
                        "class" => Class,
                        "else" => Else,
                        "false" => False,
                        "for" => For,
                        "fun" => Fun,
                        "if" => If,
                        "nil" => Nil,
                        "or" => Or,
                        "return" => Return,
                        "super" => Super,
                        "this" => This,
                        "true" => True,
                        "var" => Var,
                        "while" => While,
                        "print" => Print,
                        _ => Ident,
                    };

                    return Some(Ok(Token {
                        kind,
                        offset: c_offset,
                        source: lit,
                    }));
                }
                Started::Number => {
                    let first_non_num = c_onwards
                        .find(|c: char| !c.is_ascii_digit())
                        .unwrap_or(c_onwards.len());

                    let lit = if c_onwards[first_non_num..].starts_with('.')
                        && c_onwards[first_non_num + 1..].starts_with(|c: char| c.is_ascii_digit())
                    {
                        let second_non_num = c_onwards[first_non_num + 1..]
                            .find(|c: char| !c.is_ascii_digit())
                            .unwrap_or(c_onwards[first_non_num + 1..].len());
                        &c_onwards[..first_non_num + second_non_num + 1]
                    } else {
                        &c_onwards[..first_non_num]
                    };

                    let lit_offset = lit.len() - c.len_utf8();
                    self.rest = &self.rest[lit_offset..];
                    self.offset += lit_offset;

                    let num = match lit.parse() {
                        Ok(n) => n,
                        Err(e) => {
                            return Some(Err(miette::miette! {
                            labels = vec![LabeledSpan::at(self.offset - lit_offset..self.offset, "this numeric literal")],
                            "{e}",
                                    }.with_source_code(self.whole.to_string())))
                        }
                    };

                    return Some(Ok(Token {
                        kind: Number(num),
                        offset: c_offset,
                        source: lit,
                    }));
                }
                Started::IfEqualElse(yes, no) => {
                    if self.rest.starts_with('=') {
                        let span = &c_onwards[..c.len_utf8() + 1];
                        self.rest = &self.rest[1..];
                        self.offset += 1;
                        Some(Ok(Token {
                            kind: yes,
                            offset: c_offset,
                            source: span,
                        }))
                    } else {
                        Some(Ok(Token {
                            kind: no,
                            offset: c_offset,
                            source: c_str,
                        }))
                    }
                }
            };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_char_lexemes() {
        let source_code = "(){}=+*-!<>;,./";
        let lexer = Lexer::new(source_code);
        let expected = vec![
            Token {
                kind: TokenKind::LeftParen,
                offset: 0,
                source: &source_code[..1],
            },
            Token {
                kind: TokenKind::RightParen,
                offset: 1,
                source: &source_code[1..2],
            },
            Token {
                kind: TokenKind::LeftBracket,
                offset: 2,
                source: &source_code[2..3],
            },
            Token {
                kind: TokenKind::RightBracket,
                offset: 3,
                source: &source_code[3..4],
            },
            Token {
                kind: TokenKind::Eq,
                offset: 4,
                source: &source_code[4..5],
            },
            Token {
                kind: TokenKind::Plus,
                offset: 5,
                source: &source_code[5..6],
            },
            Token {
                kind: TokenKind::Star,
                offset: 6,
                source: &source_code[6..7],
            },
            Token {
                kind: TokenKind::Minus,
                offset: 7,
                source: &source_code[7..8],
            },
            Token {
                kind: TokenKind::Bang,
                offset: 8,
                source: &source_code[8..9],
            },
            Token {
                kind: TokenKind::Less,
                offset: 9,
                source: &source_code[9..10],
            },
            Token {
                kind: TokenKind::Greater,
                offset: 10,
                source: &source_code[10..11],
            },
            Token {
                kind: TokenKind::Semicolon,
                offset: 11,
                source: &source_code[11..12],
            },
            Token {
                kind: TokenKind::Comma,
                offset: 12,
                source: &source_code[12..13],
            },
            Token {
                kind: TokenKind::Dot,
                offset: 13,
                source: &source_code[13..14],
            },
            Token {
                kind: TokenKind::Slash,
                offset: 14,
                source: &source_code[14..15],
            },
        ];
        let mut result = vec![];
        for token in lexer {
            let token = token.unwrap();
            result.push(token);
        }

        let zip = std::iter::zip(result, expected);

        for (a, b) in zip {
            assert_eq!(a, b);
        }
    }

    #[test]
    fn number() {
        let source_code = "1024";
        let mut lexer = Lexer::new(source_code);
        let num = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: TokenKind::Number(1024.0),
            offset: 0,
            source: source_code,
        };
        assert_eq!(num, token);
    }

    #[test]
    fn number_leading_zeros() {
        let source_code = "00001024";
        let mut lexer = Lexer::new(source_code);
        let num = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: TokenKind::Number(1024.0),
            offset: 0,
            source: source_code,
        };
        assert_eq!(num, token);
    }

    #[test]
    fn number_with_one_dot() {
        let source_code = "0.1234";
        let mut lexer = Lexer::new(source_code);
        let num = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: TokenKind::Number(0.1234),
            offset: 0,
            source: source_code,
        };
        assert_eq!(num, token);
    }

    #[test]
    fn number_with_two_dots() {
        let source_code = "0.1234.1";
        let mut lexer = Lexer::new(source_code);
        let num = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: TokenKind::Number(0.1234),
            offset: 0,
            source: &source_code[..6],
        };
        assert_eq!(num, token);

        let dot = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: TokenKind::Dot,
            offset: 6,
            source: &source_code[6..7],
        };
        assert_eq!(dot, token);

        let num = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: TokenKind::Number(1.),
            offset: 7,
            source: &source_code[7..],
        };
        assert_eq!(num, token);
    }

    #[test]
    fn string() {
        let source_code = "\"This is a string!\"";
        let mut lexer = Lexer::new(&source_code);
        let str = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: TokenKind::TString,
            offset: 0,
            source: &source_code[1..18],
        };

        assert_eq!(str, token);
    }

    #[test]
    fn unterminated_string() {
        let source_code = "\"Unterminated!";
        let mut lexer = Lexer::new(&source_code);
        let str = lexer.next().unwrap();
        assert!(str.is_err());
    }

    #[test]
    fn two_strings() {
        let source_code = "\"first\" \"second\"";
        let mut lexer = Lexer::new(&source_code);
        let first = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: TokenKind::TString,
            offset: 0,
            source: &source_code[1..6],
        };
        assert_eq!(first, token);

        let second = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: TokenKind::TString,
            offset: 8,
            source: &source_code[9..15],
        };
        assert_eq!(second, token);
    }
}
