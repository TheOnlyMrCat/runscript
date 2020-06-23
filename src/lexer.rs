use std::fmt::Debug;
use std::iter::{Peekable};
use std::mem::replace;

use crate::runfile::ScriptType;

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Clone, Debug)]
pub enum Tok {
    Newline,
    Hash,
    DoubleHash,
    HashSlash,
    HashDash,
    SingleQuote,
    DoubleQuote,
    OpenParen,
    CloseParen,
    Dollar,
    Whitespace,
    CommandPart(String),
    TargetName(String),
    MetaScript(ScriptType),
}

impl Tok {
    pub fn command_part(self) -> String {
        if let Tok::CommandPart(s) = self {
            s
        } else {
            panic!("Expected CommandPart");
        }
    }

    pub fn target_name(self) -> String {
        if let Tok::TargetName(s) = self {
            s
        } else {
            panic!("Expected TargetName");
        }
    }

    pub fn meta_script(self) -> ScriptType {
        if let Tok::MetaScript(b) = self {
            b
        } else {
            panic!("Expected MetaBuild");
        }
    }
}

#[derive(Debug)]
pub enum LexErr {

}

enum State {
    Root,
    Name,
    Meta,
    Exec,
}

pub struct Lexer<'input> {
    chars: Peekable<&'input mut dyn Iterator<Item = (usize, char)>>,
    state: State,
    buftk: Option<Spanned<Tok, usize, LexErr>>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input mut dyn Iterator<Item = (usize, char)>) -> Self {
        Lexer {
            chars: input.peekable(),
            state: State::Root,
            buftk: None,
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Tok, usize, LexErr>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(_) = &self.buftk {
            return replace(&mut self.buftk, None);
        }
        match self.state {
            State::Root => {
                loop {
                    match self.chars.next() {
                        Some((i, '#')) => {
                            match self.chars.peek() {
                                Some((_, '#')) => {
                                    self.chars.next();
                                    self.state = State::Meta;
                                    return Some(Ok((i, Tok::DoubleHash, i + 2)));
                                },
                                Some((_, '-')) => {
                                    self.chars.next();
                                    self.state = State::Meta;
                                    return Some(Ok((i, Tok::HashDash, i + 2)));
                                },
                                _ => {
                                    self.state = State::Name;
                                    return Some(Ok((i, Tok::Hash, i + 1)));
                                }
                            }
                        }
    
                        None => return None,
                        _ => continue,
                    }
                }
            },
            State::Name => {
                match self.chars.next() {
                    Some((begindex, chr)) => {
                        let mut string = String::with_capacity(5);
                        string.push(chr);
                        loop {
                            match self.chars.next() {
                                Some((i, ' ')) => {
                                    self.state = State::Meta;
                                    return Some(Ok((begindex, Tok::TargetName(string), i)));
                                },
                                Some((i, '\n')) => {
                                    self.state = State::Exec;
                                    self.buftk = Some(Ok((i, Tok::Newline, i + 1)));
                                    return Some(Ok((begindex, Tok::TargetName(string), i)));
                                }
                                Some((_, c)) => {
                                    string.push(c);
                                },
                                None => return None
                            }
                        }
                    },
                    None => return None,
                }
            },
            State::Meta => {
                loop {
                    match self.chars.next() {
                        Some((i, 'b')) => {
                            return Some(Ok(
                                match self.chars.peek() {
                                    Some((_, '!')) => {
                                        self.chars.next();
                                        (i, Tok::MetaScript(ScriptType::BuildOnly), i + 2)
                                    },
                                    Some((_, 'r')) => {
                                        self.chars.next();
                                        (i, Tok::MetaScript(ScriptType::BuildAndRun), i + 2)
                                    },
                                    _ => {
                                        (i, Tok::MetaScript(ScriptType::Build), i + 1)
                                    }
                                }
                            ));
                        },
                        Some((i, 'r')) => {
                            return Some(Ok(
                                if let Some((_, '!')) = self.chars.peek() {
                                    self.chars.next();
                                    (i, Tok::MetaScript(ScriptType::RunOnly), i + 2)
                                } else {
                                    (i, Tok::MetaScript(ScriptType::Run), i + 1)
                                }
                            ));
                        }
                        Some((i, '\n')) => {
                            self.state = State::Exec;
                            return Some(Ok((i, Tok::Newline, i + 1)))
                        },
                        None => return None,
                        _ => continue,
                    }
                }
            },
            State::Exec => {
                match self.chars.next() {
                    Some((begindex, chr)) => {
                        match chr {
                            '#' => {
                                match self.chars.next() {
                                    Some((_, '/')) => {
                                        self.state = State::Root;
                                        return Some(Ok((begindex, Tok::HashSlash, begindex + 2)))
                                    },
                                    Some(_) => {
                                        loop {
                                            match self.chars.next() {
                                                Some((_, '\n')) => return self.next(),
                                                None => return None,
                                                _ => continue,
                                            }
                                        }
                                    }
                                    None => return None,
                                }
                            },
                            '$' => return Some(Ok((begindex, Tok::Dollar, begindex + 1))),
                            '(' => return Some(Ok((begindex, Tok::OpenParen, begindex + 1))),
                            ')' => return Some(Ok((begindex, Tok::CloseParen, begindex + 1))),
                            '\'' => return Some(Ok((begindex, Tok::SingleQuote, begindex + 1))),
                            '"' => return Some(Ok((begindex, Tok::SingleQuote, begindex + 1))),
                            '\n' => return Some(Ok((begindex, Tok::Newline, begindex + 1))),
                            _ => {
                                let mut string = String::with_capacity(5);
                                string.push(chr);
                                loop {
                                    match self.chars.next() {
                                        Some((i, ' ')) => {
                                            self.buftk = Some(Ok((i, Tok::Whitespace, i + 1)));
                                            string.shrink_to_fit();
                                            return Some(Ok((begindex, Tok::CommandPart(string), i)));
                                        },
                                        Some((i, '\'')) => {
                                            self.buftk = Some(Ok((i, Tok::SingleQuote, i + 1)));
                                            string.shrink_to_fit();
                                            return Some(Ok((begindex, Tok::CommandPart(string), i)));
                                        },
                                        Some((i, '"')) => {
                                            self.buftk = Some(Ok((i, Tok::DoubleQuote, i + 1)));
                                            string.shrink_to_fit();
                                            return Some(Ok((begindex, Tok::CommandPart(string), i)));
                                        },
                                        Some((i, '\n')) => {
                                            self.buftk = Some(Ok((i, Tok::Newline, i + 1)));
                                            string.shrink_to_fit();
                                            return Some(Ok((begindex, Tok::CommandPart(string), i)));
                                        }
                                        Some((_, c)) => {
                                            string.push(c);
                                        },
                                        None => return None,
                                    }
                                }
                            }
                        }
                    },
                    None => return None,
                }
            },
        }
    }
}