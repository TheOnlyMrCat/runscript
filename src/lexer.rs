use std::fmt::Debug;
use std::iter::{Peekable};
use std::mem::replace;

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

#[derive(Clone, Debug)]
pub enum Tok {
    Newline,
    Hash,
    DoubleHash,
    HashSlash,
    CommandPart(String),
}

impl Tok {
    pub fn command_part(self) -> String {
        if let Tok::CommandPart(s) = self {
            s
        } else {
            panic!("Expected CommandPart");
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
            State::Name => todo!(),
            State::Meta => {
                loop {
                    match self.chars.next() {
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
                            '\n' => return Some(Ok((begindex, Tok::Newline, begindex + 1))),
                            _ => {
                                let mut string = String::with_capacity(5);
                                string.push(chr);
                                loop {
                                    match self.chars.next() {
                                        Some((i, ' ')) => {
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