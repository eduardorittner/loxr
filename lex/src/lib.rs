#![feature(new_range_api)]

pub mod token;

use std::range::Range;
use token::TokenKind::*;
use token::*;

pub struct Lexer<'source> {
    source: &'source str,
    rest: &'source str,
    offset: usize,
    done: bool,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            source,
            rest: source,
            offset: 0,
            done: false,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        self.collect()
    }
}

#[derive(Debug)]
pub struct LexError;

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
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
                    span: Range {
                        start: c_offset,
                        end: c_offset + c_str.len(),
                    },
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
                _ => return Some(Err(LexError)),
            };
            break match started {
                Started::TString => {
                    let Some(end_of_str) = c_onwards[1..].find('"') else {
                        return Some(Err(LexError));
                    };

                    let lit = &c_onwards[1..end_of_str + 1];
                    let lit_offset = lit.len() - c.len_utf8();

                    // +2 for opening and closing '"'
                    self.offset += lit_offset + 2;
                    self.rest = &self.rest[lit_offset + 2..];

                    return Some(Ok(Token {
                        kind: TString,
                        span: Range {
                            start: c_offset + 1,
                            end: c_offset + lit.len() + 1,
                        },
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
                        span: Range {
                            start: c_offset,
                            end: c_offset + lit.len(),
                        },
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

                    return Some(Ok(Token {
                        kind: Number,
                        span: Range {
                            start: c_offset,
                            end: c_offset + lit.len(),
                        },
                    }));
                }
                Started::IfEqualElse(yes, no) => {
                    if self.rest.starts_with('=') {
                        let span = &c_onwards[..c.len_utf8() + 1];
                        self.rest = &self.rest[1..];
                        self.offset += 1;
                        Some(Ok(Token {
                            kind: yes,
                            span: Range {
                                start: c_offset,
                                end: c_offset + span.len() + 1,
                            },
                        }))
                    } else {
                        Some(Ok(Token {
                            kind: no,
                            span: Range {
                                start: c_offset,
                                end: c_offset + c_str.len(),
                            },
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
                span: Range { start: 0, end: 1 },
            },
            Token {
                kind: TokenKind::RightParen,
                span: Range { start: 1, end: 2 },
            },
            Token {
                kind: TokenKind::LeftBracket,
                span: Range { start: 2, end: 3 },
            },
            Token {
                kind: TokenKind::RightBracket,
                span: Range { start: 3, end: 4 },
            },
            Token {
                kind: TokenKind::Eq,
                span: Range { start: 4, end: 5 },
            },
            Token {
                kind: TokenKind::Plus,
                span: Range { start: 5, end: 6 },
            },
            Token {
                kind: TokenKind::Star,
                span: Range { start: 6, end: 7 },
            },
            Token {
                kind: TokenKind::Minus,
                span: Range { start: 7, end: 8 },
            },
            Token {
                kind: TokenKind::Bang,
                span: Range { start: 8, end: 9 },
            },
            Token {
                kind: TokenKind::Less,
                span: Range { start: 9, end: 10 },
            },
            Token {
                kind: TokenKind::Greater,
                span: Range { start: 10, end: 11 },
            },
            Token {
                kind: TokenKind::Semicolon,
                span: Range { start: 11, end: 12 },
            },
            Token {
                kind: TokenKind::Comma,
                span: Range { start: 12, end: 13 },
            },
            Token {
                kind: TokenKind::Dot,
                span: Range { start: 13, end: 14 },
            },
            Token {
                kind: TokenKind::Slash,
                span: Range { start: 14, end: 15 },
            },
        ];
        let mut result = vec![];
        for token in lexer {
            let token = token.unwrap();
            result.push(token);
        }

        let zip = std::iter::zip(result, expected);

        for (a, b) in zip {
            println!("{:?} {:?}", a, b);
            assert_eq!(a, b);
        }
    }

    #[test]
    fn number() {
        let source_code = "1024";
        let mut lexer = Lexer::new(source_code);
        let num = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: TokenKind::Number,
            span: Range { start: 0, end: 4 },
        };
        assert_eq!(num, token);
    }

    #[test]
    fn number_leading_zeros() {
        let source_code = "00001024";
        let mut lexer = Lexer::new(source_code);
        let num = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: TokenKind::Number,
            span: Range {
                start: 0,
                end: source_code.len(),
            },
        };
        assert_eq!(num, token);
    }

    #[test]
    fn number_with_one_dot() {
        let source_code = "0.1234";
        let mut lexer = Lexer::new(source_code);
        let num = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: TokenKind::Number,
            span: Range {
                start: 0,
                end: source_code.len(),
            },
        };
        assert_eq!(num, token);
    }

    #[test]
    fn number_with_two_dots() {
        let source_code = "0.1234.1";
        let mut lexer = Lexer::new(source_code);
        let num = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: TokenKind::Number,
            span: Range { start: 0, end: 6 },
        };
        assert_eq!(num, token);

        let dot = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: TokenKind::Dot,
            span: Range { start: 6, end: 7 },
        };
        assert_eq!(dot, token);

        let num = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: TokenKind::Number,
            span: Range { start: 7, end: 8 },
        };
        assert_eq!(num, token);
    }

    #[test]
    fn test_ident() {
        let source_code = "my_ident";
        let mut lexer = Lexer::new(source_code);
        let str = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: T![ident],
            span: Range {
                start: 0,
                end: source_code.len(),
            },
        };
        assert_eq!(str, token);
    }

    #[test]
    fn test_idents() {
        let source_code = "  my_ident  identifier";
        let mut lexer = Lexer::new(source_code);
        let str = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: T![ident],
            span: Range { start: 2, end: 10 },
        };
        assert_eq!(str, token);

        let str = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: T![ident],
            span: Range {
                start: 12,
                end: source_code.len(),
            },
        };
        assert_eq!(str, token);
    }

    #[test]
    fn string() {
        let source_code = "\"This is a string!\"";
        let mut lexer = Lexer::new(source_code);
        let str = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: TokenKind::TString,
            span: Range { start: 1, end: 18 },
        };

        assert_eq!(str, token);
    }

    #[test]
    fn unterminated_string() {
        let source_code = "\"Unterminated!";
        let mut lexer = Lexer::new(source_code);
        let str = lexer.next().unwrap();
        assert!(str.is_err());
    }

    #[test]
    fn two_strings() {
        let source_code = "\"first\" \"second\"";
        let mut lexer = Lexer::new(source_code);
        let first = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: TokenKind::TString,
            span: Range { start: 1, end: 6 },
        };
        assert_eq!(first, token);

        println!("'{}'", lexer.rest);
        let second = lexer.next().unwrap().unwrap();
        let token = Token {
            kind: TokenKind::TString,
            span: Range { start: 9, end: 15 },
        };
        assert_eq!(second, token);
    }
}
