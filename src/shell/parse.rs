//! The definition of a parser (and related methods) for the shell language.
// FIXME: consider parsing [[ exr ]] as keywords? otherwise [[ foo && bar ]] won't get parsed right
// FIXME: consider parsing out array index syntax? (e.g. ${array[some index]}
// FIXME: arithmetic substitutions don't currently support param/comand substitutions

use std::error::Error;
use std::fmt;
use std::iter::empty as empty_iter;
use std::mem;
use std::str::FromStr;
use std::sync::Arc;

use self::iter::{PeekableIterator, PositionIterator, TokenIter, TokenIterWrapper, TokenIterator};
use crate::shell::ast::{self, Arithmetic, Parameter};
use crate::shell::token::Token;
use crate::shell::token::Token::*;

mod iter;

const CASE: &str = "case";
const DO: &str = "do";
const DONE: &str = "done";
const ELIF: &str = "elif";
const ELSE: &str = "else";
const ESAC: &str = "esac";
const FI: &str = "fi";
const FOR: &str = "for";
const FUNCTION: &str = "function";
const IF: &str = "if";
const IN: &str = "in";
const THEN: &str = "then";
const UNTIL: &str = "until";
const WHILE: &str = "while";

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct SourcePos {
    pub byte: usize,
    pub line: usize,
    pub col: usize,
}

impl Default for SourcePos {
    fn default() -> Self {
        Self::new()
    }
}

impl SourcePos {
    pub fn new() -> SourcePos {
        SourcePos {
            byte: 0,
            line: 1,
            col: 1,
        }
    }

    pub fn advance(&mut self, next: &Token) {
        let newlines = match *next {
            // Most of these should not have any newlines
            // embedded within them, but permitting external
            // tokenizers means we should sanity check anyway.
            Name(ref s) | Literal(ref s) | Whitespace(ref s) => {
                s.chars().filter(|&c| c == '\n').count()
            }

            Newline => 1,
            _ => 0,
        };

        let tok_len = next.len();
        self.byte += tok_len;
        self.line += newlines;
        self.col = if newlines == 0 { self.col + tok_len } else { 1 };
    }

    /// Increments self by `num_tab` tab characters
    fn advance_tabs(&mut self, num_tab: usize) {
        self.byte += num_tab;
        self.col += num_tab;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    /// Encountered a word that could not be interpreted as a valid file descriptor.
    /// Stores the start and end position of the invalid word.
    BadFd(SourcePos, SourcePos),
    /// Encountered a `Token::Literal` where expecting a `Token::Name`.
    BadIdent(String, SourcePos),
    /// Encountered a bad token inside of `${...}`.
    BadSubst(Token, SourcePos),
    /// Encountered EOF while looking for a match for the specified token.
    /// Stores position of opening token.
    Unmatched(Token, SourcePos),
    /// Did not find a reserved keyword within a command. The first String is the
    /// command being parsed, followed by the position of where it starts. Next
    /// is the missing keyword followed by the position of where the parse
    /// expected to have encountered it.
    IncompleteCmd(&'static str, SourcePos, &'static str, SourcePos),
    /// Encountered a token not appropriate for the current context.
    Unexpected(Token, SourcePos),
    /// Encountered the end of input while expecting additional tokens.
    UnexpectedEOF,
}

impl Error for ParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            ParseError::BadFd(ref start, ref end) => write!(
                fmt,
                "file descriptor found between lines {} - {} cannot possibly be a valid",
                start, end
            ),
            ParseError::BadIdent(ref id, pos) => {
                write!(fmt, "not a valid identifier {}: {}", pos, id)
            }
            ParseError::BadSubst(ref t, pos) => {
                write!(fmt, "bad substitution {}: invalid token: {}", pos, t)
            }
            ParseError::Unmatched(ref t, pos) => {
                write!(fmt, "unmatched `{}` starting on line {}", t, pos)
            }

            ParseError::IncompleteCmd(c, start, kw, kw_pos) => write!(
                fmt,
                "did not find `{}` keyword on line {}, in `{}` command which starts on line {}",
                kw, kw_pos, c, start
            ),

            // When printing unexpected newlines, print \n instead to avoid confusingly formatted messages
            ParseError::Unexpected(Newline, pos) => {
                write!(fmt, "found unexpected token on line {}: \\n", pos)
            }
            ParseError::Unexpected(ref t, pos) => {
                write!(fmt, "found unexpected token on line {}: {}", pos, t)
            }

            ParseError::UnexpectedEOF => fmt.write_str("unexpected end of input"),
        }
    }
}

impl fmt::Display for SourcePos {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}:{}", self.line, self.col)
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum CompoundCmdKeyword {
    For,
    Case,
    If,
    While,
    Until,
    Brace,
    Subshell,
}

#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct CommandGroupDelimiters<'a, 'b, 'c> {
    /// Any token which appears after a complete command separator (e.g. `;`, `&`, or a
    /// newline) will be considered a delimeter for the command group.
    pub reserved_tokens: &'a [Token],
    /// Any `Literal` or `Name` token that matches any of these entries completely
    /// *and* appear after a complete command will be considered a delimeter.
    pub reserved_words: &'b [&'static str],
    /// Any token which matches this provided set will be considered a delimeter.
    pub exact_tokens: &'c [Token],
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SeparatorKind {
    Semi,
    Amp,
    Newline,
    Other,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum LoopKind {
    While,
    Until,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CommandGroup<C> {
    pub commands: Vec<C>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IfFragments<C> {
    pub conditionals: Vec<ast::GuardBodyPair>,
    pub else_branch: Option<CommandGroup<C>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ForFragments<W, C> {
    pub var: String,
    pub words: Option<Vec<W>>,
    pub body: CommandGroup<C>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CaseFragments<W> {
    pub word: W,
    pub arms: Vec<ast::PatternBodyPair>,
}

#[derive(Debug)]
pub struct Parser<I> {
    iter: TokenIterWrapper<I>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    /// Creates a new Parser from a Token iterator or collection.
    pub fn new<T>(iter: T) -> Parser<I>
    where
        T: IntoIterator<Item = Token, IntoIter = I>,
    {
        Parser {
            iter: TokenIterWrapper::Regular(TokenIter::new(iter.into_iter())),
        }
    }
}

/// A macro that will consume and return a token that matches a specified pattern
/// from a parser's token iterator. If no matching token is found, None will be yielded.
macro_rules! eat_maybe {
    ($parser:expr, { $($tok:pat => $blk:block),+; _ => $default:block, }) => {
        eat_maybe!($parser, { $($tok => $blk),+; _ => $default })
    };

    ($parser:expr, { $($tok:pat => $blk:block),+,}) => {
        eat_maybe!($parser, { $($tok => $blk),+ })
    };

    ($parser:expr, { $($tok:pat => $blk:block),+ }) => {
        eat_maybe!($parser, { $($tok => $blk),+; _ => {} })
    };

    ($parser:expr, { $($tok:pat => $blk:block),+; _ => $default:block }) => {
        $(if let Some(&$tok) = $parser.iter.peek() {
            $parser.iter.next();
            $blk
        } else)+ {
            $default
        }
    };
}

/// A macro that will consume a specified token from the parser's iterator
/// an run a corresponding action block to do something with the token,
/// or it will construct and return an appropriate Unexpected(EOF) error.
macro_rules! eat {
    ($parser:expr, { $($tok:pat => $blk:block),+, }) => { eat!($parser, {$($tok => $blk),+}) };
    ($parser:expr, {$($tok:pat => $blk:block),+}) => {
        eat_maybe!($parser, {$($tok => $blk),+; _ => { return Err($parser.make_unexpected_err()) } })
    };
}

/// A macro that defines a function for parsing binary operations in arithmetic
/// expressions.  It accepts a name for the function, a name for the subsequent
/// expression (which has a higher precedence) to sub parse on, and a number of
/// tokens which can be recognized as an operator for the binary operation and
/// the appropriate AST constructor for said token/operator. All operators within
/// the definition are considered to have identical precedence and are left-to-right
/// associative.
macro_rules! arith_parse {
    ($(#[$fn_attr:meta])* fn $fn_name:ident, $next_expr:ident, $($tok:pat => $constructor:path),+) => {
        $(#[$fn_attr])*
        #[inline]
        fn $fn_name(&mut self) -> ParseResult<Arithmetic> {
            let mut expr = self.$next_expr()?;
            loop {
                self.skip_whitespace();
                eat_maybe!(self, {
                    $($tok => {
                        let next = self.$next_expr()?;
                        expr = $constructor(Box::new(expr), Box::new(next));
                    }),+;
                    _ => { break },
                });
            }
            Ok(expr)
        }
    }
}

impl<I: Iterator<Item = Token>> Parser<I> {
    /// Construct an `Unexpected` error using the given token. If `None` specified, the next
    /// token in the iterator will be used (or `UnexpectedEOF` if none left).
    #[inline]
    fn make_unexpected_err(&mut self) -> ParseError {
        let pos = self.iter.pos();
        self.iter.next().map_or(ParseError::UnexpectedEOF, |t| {
            ParseError::Unexpected(t, pos)
        })
    }

    /// Returns the parser's current position in the source.
    pub fn pos(&self) -> SourcePos {
        self.iter.pos()
    }

    /// Parses a single complete command.
    ///
    /// For example, `foo && bar; baz` will yield two complete
    /// commands: `And(foo, bar)`, and `Simple(baz)`.
    pub fn complete_command(&mut self) -> ParseResult<Option<ast::AtomicTopLevelCommand>> {
        self.linebreak();

        if self.iter.peek().is_some() {
            let cmd = self.and_or_list()?;

            let (sep, cmd_comment) = eat_maybe!(self, {
                Semi => { (SeparatorKind::Semi, self.newline()) },
                Amp  => { (SeparatorKind::Amp , self.newline()) };
                _ => {
                    match self.newline() {
                        true => (SeparatorKind::Newline, true),
                        false => (SeparatorKind::Other, false),
                    }
                }
            });

            Ok(Some(if matches!(sep, SeparatorKind::Amp) {
                ast::Command::Job(cmd)
            } else {
                ast::Command::List(cmd)
            }))
        } else {
            Ok(None)
        }
    }

    /// Parses compound AND/OR commands.
    ///
    /// Commands are left associative. For example `foo || bar && baz`
    /// parses to `And(Or(foo, bar), baz)`.
    pub fn and_or_list(&mut self) -> ParseResult<ast::AndOrList> {
        let first = self.pipeline()?;
        let mut rest = Vec::new();

        loop {
            self.skip_whitespace();
            let is_and = eat_maybe!(self, {
                AndIf => { true },
                OrIf  => { false };
                _ => { break },
            });

            let post_sep_comments = self.linebreak();
            let next = self.pipeline()?;

            let next = if is_and {
                ast::AndOr::And(next)
            } else {
                ast::AndOr::Or(next)
            };

            rest.push(next);
        }

        Ok(ast::AndOrList { first, rest })
    }

    /// Parses either a single command or a pipeline of commands.
    ///
    /// For example `[!] foo | bar`.
    pub fn pipeline(&mut self) -> ParseResult<ast::ListableCommand> {
        self.skip_whitespace();
        let bang = eat_maybe!(self, {
            Bang => { true };
            _ => { false },
        });

        let mut cmds = Vec::new();
        loop {
            // We've already passed an apropriate spot for !, so it
            // is an error if it appears before the start of a command.
            if let Some(&Bang) = self.iter.peek() {
                return Err(self.make_unexpected_err());
            }

            let cmd = self.command()?;

            eat_maybe!(self, {
                Pipe => { self.linebreak(); cmds.push(cmd) };
                _ => {
                    cmds.push(cmd);
                    break;
                },
            });
        }

        if bang || cmds.len() > 1 {
            cmds.shrink_to_fit();
            Ok(ast::ListableCommand::Pipe(bang, cmds))
        } else {
            Ok(ast::ListableCommand::Single(cmds.pop().unwrap()))
        }
    }

    /// Parses any compound or individual command.
    pub fn command(&mut self) -> ParseResult<ast::PipeableCommand> {
        if let Some(kw) = self.next_compound_command_type() {
            let compound = self.compound_command_internal(Some(kw))?;
            Ok(ast::PipeableCommand::Compound(Box::new(compound)))
        } else if let Some(fn_def) = self.maybe_function_declaration()? {
            Ok(fn_def)
        } else {
            self.simple_command()
        }
    }

    /// Tries to parse a simple command, e.g. `cmd arg1 arg2 >redirect`.
    ///
    /// A valid command is expected to have at least an executable name, or a single
    /// variable assignment or redirection. Otherwise an error will be returned.
    pub fn simple_command(&mut self) -> ParseResult<ast::PipeableCommand> {
        use crate::shell::ast::{RedirectOrCmdWord, RedirectOrEnvVar};

        let mut vars = Vec::new();
        let mut cmd_args = Vec::new();

        loop {
            self.skip_whitespace();
            let is_name = {
                let mut peeked = self.iter.multipeek();
                if let Some(&Name(_)) = peeked.peek_next() {
                    Some(&Equals) == peeked.peek_next()
                } else {
                    false
                }
            };

            if is_name {
                if let Some(Name(var)) = self.iter.next() {
                    self.iter.next(); // Consume the =

                    let value = if let Some(&Whitespace(_)) = self.iter.peek() {
                        None
                    } else {
                        self.word()?
                    };
                    vars.push(RedirectOrEnvVar::EnvVar(var, value));

                    // Make sure we continue checking for assignments,
                    // otherwise it they can be interpreted as literal words.
                    continue;
                } else {
                    unreachable!();
                }
            }

            // If we find a redirect we should keep checking for
            // more redirects or assignments. Otherwise we will either
            // run into the command name or the end of the simple command.
            let exec = match self.redirect()? {
                Some(Ok(redirect)) => {
                    vars.push(RedirectOrEnvVar::Redirect(redirect));
                    continue;
                }
                Some(Err(w)) => w,
                None => break,
            };

            // Since there are no more assignments or redirects present
            // it must be the first real word, and thus the executable name.
            cmd_args.push(RedirectOrCmdWord::CmdWord(exec));
            break;
        }

        let vars = vars;

        // Now that all assignments are taken care of, any other occurances of `=` will be
        // treated as literals when we attempt to parse a word out.
        loop {
            match self.redirect()? {
                Some(Ok(redirect)) => cmd_args.push(RedirectOrCmdWord::Redirect(redirect)),
                Some(Err(w)) => cmd_args.push(RedirectOrCmdWord::CmdWord(w)),
                None => break,
            }
        }

        // "Blank" commands are only allowed if redirection occurs
        // or if there is some variable assignment
        if vars.is_empty() && cmd_args.is_empty() {
            Err(self.make_unexpected_err())
        } else {
            Ok(ast::PipeableCommand::Simple(Box::new(ast::SimpleCommand {
                redirects_or_env_vars: vars,
                redirects_or_cmd_words: cmd_args,
            })))
        }
    }

    /// Parses a continuous list of redirections and will error if any words
    /// that are not valid file descriptors are found. Essentially used for
    /// parsing redirection lists after a compound command like `while` or `if`.
    pub fn redirect_list(&mut self) -> ParseResult<Vec<ast::Redirect>> {
        let mut list = Vec::new();
        loop {
            self.skip_whitespace();
            let start_pos = self.iter.pos();
            match self.redirect()? {
                Some(Ok(io)) => list.push(io),
                Some(Err(_)) => return Err(ParseError::BadFd(start_pos, self.iter.pos())),
                None => break,
            }
        }

        Ok(list)
    }

    /// Parses a redirection token an any source file descriptor and
    /// path/destination descriptor as appropriate, e.g. `>out`, `1>& 2`, or `2>&-`.
    ///
    /// Since the source descriptor can be any arbitrarily complicated word,
    /// it makes it difficult to reliably peek forward whether a valid redirection
    /// exists without consuming anything. Thus this method may return a simple word
    /// if no redirection is found.
    ///
    /// Thus, unless a parse error is occured, the return value will be an optional
    /// redirect or word if either is found. In other words, `Ok(Some(Ok(redirect)))`
    /// will result if a redirect is found, `Ok(Some(Err(word)))` if a word is found,
    /// or `Ok(None)` if neither is found.
    pub fn redirect(&mut self) -> ParseResult<Option<Result<ast::Redirect, ast::ComplexWord>>> {
        fn could_be_numeric(word: &ast::Word) -> bool {
            let simple_could_be_numeric = |word: &ast::SimpleWord| match *word {
                ast::SimpleWord::Star
                | ast::SimpleWord::Question
                | ast::SimpleWord::SquareOpen
                | ast::SimpleWord::SquareClose
                | ast::SimpleWord::Tilde
                | ast::SimpleWord::Colon => false,

                // Literals and can be statically checked if they have non-numeric characters
                ast::SimpleWord::Escaped(ref s) | ast::SimpleWord::Literal(ref s) => {
                    s.chars().all(|c| c.is_digit(10))
                }

                // These could end up evaluating to a numeric,
                // but we'll have to see at runtime.
                ast::SimpleWord::Param(_) | ast::SimpleWord::Subst(_) => true,
            };

            match *word {
                ast::Word::Simple(ref s) => simple_could_be_numeric(s),
                ast::Word::SingleQuoted(ref s) => s.chars().all(|c| c.is_digit(10)),
                ast::Word::DoubleQuoted(ref fragments) => {
                    fragments.iter().all(simple_could_be_numeric)
                }
            }
        }

        fn as_num(word: &ast::ComplexWord) -> Option<u16> {
            match *word {
                ast::ComplexWord::Single(ast::Word::Simple(ast::SimpleWord::Literal(ref s))) => {
                    s.parse::<u16>().ok()
                }
                ast::ComplexWord::Single(_) => None,
                ast::ComplexWord::Concat(ref fragments) => {
                    let mut buf = String::new();
                    for w in fragments {
                        if let ast::Word::Simple(ast::SimpleWord::Literal(ref s)) = *w {
                            buf.push_str(s);
                        } else {
                            return None;
                        }
                    }

                    buf.parse::<u16>().ok()
                }
            }
        }

        let (src_fd, src_fd_as_word) = match self.word_preserve_trailing_whitespace()? {
            None => (None, None),
            Some(w) => match as_num(&w) {
                Some(num) => (Some(num), Some(w)),
                None => return Ok(Some(Err(w))),
            },
        };

        let redir_tok = match self.iter.peek() {
            Some(&Less) | Some(&Great) | Some(&DGreat) | Some(&Clobber) | Some(&LessAnd)
            | Some(&GreatAnd) | Some(&LessGreat) => self.iter.next().unwrap(),

            Some(&DLess) | Some(&DLessDash) => return Ok(Some(Ok(self.redirect_heredoc(src_fd)?))),

            _ => match src_fd_as_word {
                Some(w) => return Ok(Some(Err(w))),
                None => return Ok(None),
            },
        };

        self.skip_whitespace();

        macro_rules! get_path {
            ($parser:expr) => {
                match $parser.word_preserve_trailing_whitespace()? {
                    Some(p) => p,
                    None => return Err(self.make_unexpected_err()),
                }
            };
        }

        macro_rules! get_dup_path {
            ($parser:expr) => {{
                let path = if $parser.peek_reserved_token(&[Dash]).is_some() {
                    let dash = $parser.reserved_token(&[Dash])?;
                    ast::ComplexWord::Single(ast::Word::Simple(ast::SimpleWord::Literal(
                        dash.to_string(),
                    )))
                } else {
                    let path_start_pos = $parser.iter.pos();
                    let path = if let Some(p) = $parser.word_preserve_trailing_whitespace()? {
                        p
                    } else {
                        return Err($parser.make_unexpected_err());
                    };
                    let is_numeric = match path {
                        ast::ComplexWord::Single(ref p) => could_be_numeric(&p),
                        ast::ComplexWord::Concat(ref v) => v.iter().all(could_be_numeric),
                    };
                    if is_numeric {
                        path
                    } else {
                        return Err(ParseError::BadFd(path_start_pos, self.iter.pos()));
                    }
                };
                path
            }};
        }

        let redirect = match redir_tok {
            Less => ast::Redirect::Read(src_fd, get_path!(self)),
            Great => ast::Redirect::Write(src_fd, get_path!(self)),
            DGreat => ast::Redirect::Append(src_fd, get_path!(self)),
            Clobber => ast::Redirect::Clobber(src_fd, get_path!(self)),
            LessGreat => ast::Redirect::ReadWrite(src_fd, get_path!(self)),

            LessAnd => ast::Redirect::DupRead(src_fd, get_dup_path!(self)),
            GreatAnd => ast::Redirect::DupWrite(src_fd, get_dup_path!(self)),

            _ => unreachable!(),
        };

        Ok(Some(Ok(redirect)))
    }

    /// Parses a heredoc redirection and the heredoc's body.
    ///
    /// This method will look ahead after the next unquoted/unescaped newline
    /// to capture the heredoc's body, and will stop consuming tokens until
    /// the approrpiate delimeter is found on a line by itself. If the
    /// delimeter is unquoted, the heredoc's body will be expanded for
    /// parameters and other special words. Otherwise, there heredoc's body
    /// will be treated as a literal.
    pub fn redirect_heredoc(&mut self, src_fd: Option<u16>) -> ParseResult<ast::Redirect> {
        macro_rules! try_map {
            ($result:expr) => {
                $result.map_err(|e: iter::UnmatchedError| ParseError::Unmatched(e.0, e.1))?
            };
        }

        let strip_tabs = eat!(self, {
            DLess => { false },
            DLessDash => { true },
        });

        self.skip_whitespace();

        // Unfortunately we're going to have to capture the delimeter word "manually"
        // here and double some code. The problem is that we might need to unquote the
        // word--something that the parser isn't normally aware as a concept. We can
        // crawl a parsed ast::Word tree, but we would still have to convert the inner
        // trees as either a token collection or into a string, each of which is more
        // trouble than its worth (especially since ast::Word can hold a parsed and
        // and assembled Builder::Command object, which may not even be possible to
        // be represented as a string).
        //
        // Also some shells like bash and zsh seem to check for balanced tokens like
        // ', ", or ` within the heredoc delimiter (though this may just be from them
        // parsing out a word as usual), so to maintain reasonable expectations, we'll
        // do the same here.
        let mut delim_tokens = Vec::new();
        loop {
            // Normally parens are never part of words, but many
            // shells permit them to be part of a heredoc delimeter.
            if let Some(t) = self.iter.peek() {
                if t.is_word_delimiter() && t != &ParenOpen {
                    break;
                }
            } else {
                break;
            }

            for t in self.iter.balanced() {
                delim_tokens.push(try_map!(t));
            }
        }

        let mut iter = TokenIter::new(delim_tokens.into_iter());
        let mut quoted = false;
        let mut delim = String::new();
        loop {
            let start_pos = iter.pos();
            match iter.next() {
                Some(Backslash) => {
                    quoted = true;
                    if let Some(t) = iter.next() {
                        delim.push_str(t.as_str())
                    }
                }

                Some(SingleQuote) => {
                    quoted = true;
                    for t in iter.single_quoted(start_pos) {
                        delim.push_str(try_map!(t).as_str());
                    }
                }

                Some(DoubleQuote) => {
                    quoted = true;
                    let mut iter = iter.double_quoted(start_pos);
                    while let Some(next) = iter.next() {
                        match try_map!(next) {
                            Backslash => match iter.next() {
                                Some(Ok(tok @ Dollar))
                                | Some(Ok(tok @ Backtick))
                                | Some(Ok(tok @ DoubleQuote))
                                | Some(Ok(tok @ Backslash))
                                | Some(Ok(tok @ Newline)) => delim.push_str(tok.as_str()),

                                Some(t) => {
                                    let t = try_map!(t);
                                    delim.push_str(Backslash.as_str());
                                    delim.push_str(t.as_str());
                                }

                                None => delim.push_str(Backslash.as_str()),
                            },

                            t => delim.push_str(t.as_str()),
                        }
                    }
                }

                // Having backticks in a heredoc delimeter is something the major shells all
                // disagree on. Half of them (bash included) treat the precense of backticks
                // as indicating that the delimeter is quoted (and the body isn't expanded).
                // Although the POSIX standard does not indicate backticks are a form of quoting
                // its not unreasonable for them to be seen as such a way. Moreover, the presense
                // of backticks in a heredoc delimeter isn't something that is seen often, so there
                // probably won't be many problems in using this non-portable style, so we will
                // treat their presense as an indication to NOT expand the body.
                //
                // Backslashes inside the double quotes should retain their literal meaning unless
                // followed by \, $, or `, according to the POSIX standard. bash is the only major
                // shell which does not follow this rule. Since the majority of the shells seeem to
                // follow these escaping rules (one way or another), and since the standard
                // indicates this course of action, we will adopt it as well. Again, most shell
                // script maintainers probably avoid using escaping in heredoc delimeters to avoid
                // confusing, and non-portable style so picking any approach shouldn't cause too
                // many issues that cannot be fixed in a future version or with some compatability
                // flag.
                //
                // TL;DR: balanced backticks are allowed in delimeter, they cause the body to NOT
                // be expanded, and backslashes are only removed if followed by \, $, or `.
                Some(Backtick) => {
                    quoted = true;
                    delim.push_str(Backtick.as_str());
                    for t in iter.backticked_remove_backslashes(start_pos) {
                        delim.push_str(try_map!(t).as_str());
                    }
                    delim.push_str(Backtick.as_str());
                }

                Some(t) => delim.push_str(t.as_str()),
                None => break,
            }
        }

        if delim.is_empty() {
            return Err(self.make_unexpected_err());
        }

        delim.shrink_to_fit();
        let (delim, quoted) = (delim, quoted);
        let delim_len = delim.len();
        let delim_r = String::from_iter(vec![delim.as_str(), "\r"]);
        let delim_r_len = delim_r.len();

        // Here we will fast-forward to the next newline and capture the heredoc's
        // body that comes after it. Then we'll store these tokens in a safe place
        // and continue parsing them later. Although it may seem inefficient to do
        // this instead of parsing input until all pending heredocs are resolved,
        // we would have to do even more bookkeeping to store pending and resolved
        // heredocs, especially if we want to keep the builder unaware of our
        // shenanigans (since it *could* be keeping some internal state of what
        // we feed it).
        let saved_pos = self.iter.pos();
        let mut saved_tokens = Vec::new();
        while self.iter.peek().is_some() {
            // Make sure we save all tokens until the next UNQUOTED newilne
            if let Some(&Newline) = self.iter.peek() {
                saved_tokens.push(self.iter.next().unwrap());
                break;
            }

            for t in self.iter.balanced() {
                saved_tokens.push(try_map!(t));
            }
        }

        let heredoc_start_pos = self.iter.pos();
        let mut heredoc = Vec::new();
        'heredoc: loop {
            let mut line_start_pos = self.iter.pos();
            let mut line = Vec::new();
            'line: loop {
                if strip_tabs {
                    let skip_next = if let Some(&Whitespace(ref w)) = self.iter.peek() {
                        let stripped = w.trim_start_matches('\t');
                        let num_tabs = w.len() - stripped.len();
                        line_start_pos.advance_tabs(num_tabs);

                        if !stripped.is_empty() {
                            line.push(Whitespace(stripped.to_owned()));
                        }

                        true
                    } else {
                        false
                    };

                    if skip_next {
                        self.iter.next();
                    }
                }

                let next = self.iter.next();
                match next {
                    // If we haven't grabbed any input we must have hit EOF
                    // which should delimit the heredoc body
                    None if line.is_empty() => break 'heredoc,

                    // Otherwise, if we have a partial line captured, check
                    // whether it happens to be the delimeter, and append it
                    // to the body if it isn't
                    None | Some(Newline) => {
                        // Do a quick length check on the line. Odds are that heredoc lines
                        // will be much longer than the delimeter itself, and it could get
                        // slow to stringify each and every line (and alloc it in memory)
                        // when token length checks are available without converting to strings.
                        let mut line_len = 0;
                        for t in &line {
                            line_len += t.len();
                            if line_len > delim_r_len {
                                break;
                            }
                        }

                        // NB A delimeter like "\eof" becomes [Name(e), Name(of)], which
                        // won't compare to [Name(eof)], forcing us to do a string comparison
                        // NB We must also do a check using \r\n line endings. Although we could
                        // lex \r\n as a Newline token, doing so would complicate keeping track
                        // of positions in the source, as we could have one or two byte Newlines,
                        // or two different tokens to deal with.
                        if line_len == delim_len || line_len == delim_r_len {
                            let line_str = concat_tokens(&line);
                            if line_str == delim || line_str == delim_r {
                                break 'heredoc;
                            }
                        }

                        if next == Some(Newline) {
                            line.push(Newline);
                        }
                        break 'line;
                    }

                    Some(t) => line.push(t),
                }
            }

            heredoc.push((line, line_start_pos));
        }

        self.iter
            .buffer_tokens_to_yield_first(saved_tokens, saved_pos);

        let body = if quoted {
            let body = heredoc.into_iter().flat_map(|(t, _)| t).collect::<Vec<_>>();
            ast::ComplexWord::Single(ast::Word::Simple(ast::SimpleWord::Literal(concat_tokens(
                &body,
            ))))
        } else {
            let mut tok_iter = TokenIter::with_position(empty_iter(), heredoc_start_pos);
            while let Some((line, pos)) = heredoc.pop() {
                tok_iter.buffer_tokens_to_yield_first(line, pos);
            }

            let mut tok_backup = TokenIterWrapper::Buffered(tok_iter);
            mem::swap(&mut self.iter, &mut tok_backup);
            let mut body = self.word_interpolated_raw(None, heredoc_start_pos)?;
            let _ = mem::replace(&mut self.iter, tok_backup);

            if body.len() > 1 {
                ast::ComplexWord::Concat(body.into_iter().map(ast::Word::Simple).collect())
            } else {
                let body = body
                    .pop()
                    .unwrap_or_else(|| ast::SimpleWord::Literal(String::new()));
                ast::ComplexWord::Single(ast::Word::Simple(body))
            }
        };

        let word = body;
        Ok(ast::Redirect::Heredoc(src_fd, word))
    }

    /// Parses a whitespace delimited chunk of text, honoring space quoting rules,
    /// and skipping leading and trailing whitespace.
    ///
    /// Since there are a large number of possible tokens that constitute a word,
    /// (such as literals, paramters, variables, etc.) the caller may not know
    /// for sure whether to expect a word, thus an optional result is returned
    /// in the event that no word exists.
    ///
    /// Note that an error can still arise if partial tokens are present
    /// (e.g. malformed parameter).
    pub fn word(&mut self) -> ParseResult<Option<ast::ComplexWord>> {
        let ret = self.word_preserve_trailing_whitespace()?;
        self.skip_whitespace();
        Ok(ret)
    }

    fn word_preserve_trailing_whitespace(&mut self) -> ParseResult<Option<ast::ComplexWord>> {
        self.word_preserve_trailing_whitespace_with_delim(None)
    }

    fn word_preserve_trailing_whitespace_with_delim(
        &mut self,
        delim: Option<Token>,
    ) -> ParseResult<Option<ast::ComplexWord>> {
        self.skip_whitespace();

        // Make sure we don't consume comments,
        // e.g. if a # is at the start of a word.
        if let Some(&Pound) = self.iter.peek() {
            return Ok(None);
        }

        let mut words = Vec::new();
        loop {
            if delim.is_some() && self.iter.peek() == delim.as_ref() {
                break;
            }

            match self.iter.peek() {
                Some(&CurlyOpen) | Some(&CurlyClose) | Some(&SquareOpen) | Some(&SquareClose)
                | Some(&SingleQuote) | Some(&DoubleQuote) | Some(&Pound) | Some(&Star)
                | Some(&Question) | Some(&Tilde) | Some(&Bang) | Some(&Backslash)
                | Some(&Percent) | Some(&Dash) | Some(&Equals) | Some(&Plus) | Some(&Colon)
                | Some(&At) | Some(&Caret) | Some(&Slash) | Some(&Comma) | Some(&Name(_))
                | Some(&Literal(_)) => {}

                Some(&Backtick) => {
                    words.push(ast::Word::Simple(self.backticked_raw()?));
                    continue;
                }

                Some(&Dollar) | Some(&ParamPositional(_)) => {
                    words.push(ast::Word::Simple(self.parameter_raw()?));
                    continue;
                }

                Some(&Newline) | Some(&ParenOpen) | Some(&ParenClose) | Some(&Semi)
                | Some(&Amp) | Some(&Pipe) | Some(&AndIf) | Some(&OrIf) | Some(&DSemi)
                | Some(&Less) | Some(&Great) | Some(&DLess) | Some(&DGreat) | Some(&GreatAnd)
                | Some(&LessAnd) | Some(&DLessDash) | Some(&Clobber) | Some(&LessGreat)
                | Some(&Whitespace(_)) | None => break,
            }

            let start_pos = self.iter.pos();
            let w = match self.iter.next().unwrap() {
                // Unless we are explicitly parsing a brace group, `{` and `}` should
                // be treated as literals.
                //
                // Also, comments are only recognized where a Newline is valid, thus '#'
                // becomes a literal if it occurs in the middle of a word.
                tok @ Bang
                | tok @ Pound
                | tok @ Percent
                | tok @ Dash
                | tok @ Equals
                | tok @ Plus
                | tok @ At
                | tok @ Caret
                | tok @ Slash
                | tok @ Comma
                | tok @ CurlyOpen
                | tok @ CurlyClose => ast::Word::Simple(ast::SimpleWord::Literal(tok.to_string())),

                Name(s) | Literal(s) => ast::Word::Simple(ast::SimpleWord::Literal(s)),

                Star => ast::Word::Simple(ast::SimpleWord::Star),
                Question => ast::Word::Simple(ast::SimpleWord::Question),
                Tilde => ast::Word::Simple(ast::SimpleWord::Tilde),
                SquareOpen => ast::Word::Simple(ast::SimpleWord::SquareOpen),
                SquareClose => ast::Word::Simple(ast::SimpleWord::SquareClose),
                Colon => ast::Word::Simple(ast::SimpleWord::Colon),

                Backslash => match self.iter.next() {
                    // Escaped newlines become whitespace and a delimiter.
                    // Alternatively, can't escape EOF, just ignore the slash
                    Some(Newline) | None => break,
                    Some(t) => ast::Word::Simple(ast::SimpleWord::Escaped(t.to_string())),
                },

                SingleQuote => {
                    let mut buf = String::new();
                    for t in self.iter.single_quoted(start_pos) {
                        buf.push_str(t.map_err(|e| ParseError::Unmatched(e.0, e.1))?.as_str())
                    }

                    ast::Word::SingleQuoted(buf)
                }

                DoubleQuote => ast::Word::DoubleQuoted(
                    self.word_interpolated_raw(Some((DoubleQuote, DoubleQuote)), start_pos)?,
                ),

                // Parameters and backticks should have been
                // handled while peeking above.
                Backtick | Dollar | ParamPositional(_) => unreachable!(),

                // All word delimiters should have
                // broken the loop while peeking above.
                Newline | ParenOpen | ParenClose | Semi | Amp | Pipe | AndIf | OrIf | DSemi
                | Less | Great | DLess | DGreat | GreatAnd | LessAnd | DLessDash | Clobber
                | LessGreat | Whitespace(_) => unreachable!(),
            };

            words.push(w);
        }

        let ret = if words.is_empty() {
            None
        } else if words.len() == 1 {
            Some(ast::ComplexWord::Single(words.pop().unwrap()))
        } else {
            Some(ast::ComplexWord::Concat(words))
        };

        Ok(ret)
    }

    /// Parses tokens in a way similar to how double quoted strings may be interpreted.
    ///
    /// Parameters/substitutions are parsed as normal, backslashes keep their literal
    /// meaning unless they preceed $, `, ", \, \n, or the specified delimeter, while
    /// all other tokens are consumed as literals.
    ///
    /// Tokens will continue to be consumed until a specified delimeter is reached
    /// (which is also consumed). If EOF is reached before a delimeter can be found,
    /// an error will result. If a `None` is provided as a delimeter tokens will be
    /// consumed until EOF is reached and no error will result.
    ///
    /// `delim` argument structure is Option<(open token, close token)>. The close
    /// token indicates when to stop parsing the word, while the open token will be
    /// used to construct a `ParseError::Unmatched` error.
    fn word_interpolated_raw(
        &mut self,
        delim: Option<(Token, Token)>,
        start_pos: SourcePos,
    ) -> ParseResult<Vec<ast::SimpleWord>> {
        let (delim_open, delim_close) = match delim {
            Some((o, c)) => (Some(o), Some(c)),
            None => (None, None),
        };

        let mut words = Vec::new();
        let mut buf = String::new();
        loop {
            if self.iter.peek() == delim_close.as_ref() {
                self.iter.next();
                break;
            }

            macro_rules! store {
                ($word:expr) => {{
                    if !buf.is_empty() {
                        words.push(ast::SimpleWord::Literal(buf));
                        buf = String::new();
                    }
                    words.push($word);
                }};
            }

            // Make sure we don't consume any $ (or any specific parameter token)
            // we find since the `parameter` method expects to consume them.
            match self.iter.peek() {
                Some(&Dollar) | Some(&ParamPositional(_)) => {
                    store!(self.parameter_raw()?);
                    continue;
                }

                Some(&Backtick) => {
                    store!(self.backticked_raw()?);
                    continue;
                }

                _ => {}
            }

            match self.iter.next() {
                // Backslashes only escape a few tokens when double-quoted-type words
                Some(Backslash) => {
                    let special = matches!(
                        self.iter.peek(),
                        Some(&Dollar)
                            | Some(&Backtick)
                            | Some(&DoubleQuote)
                            | Some(&Backslash)
                            | Some(&Newline)
                    );

                    if special || self.iter.peek() == delim_close.as_ref() {
                        store!(ast::SimpleWord::Escaped(
                            self.iter.next().unwrap().to_string()
                        ))
                    } else {
                        buf.push_str(Backslash.as_str());
                    }
                }

                Some(Dollar) => unreachable!(),   // Sanity
                Some(Backtick) => unreachable!(), // Sanity

                Some(t) => buf.push_str(t.as_str()),
                None => match delim_open {
                    Some(delim) => return Err(ParseError::Unmatched(delim, start_pos)),
                    None => break,
                },
            }
        }

        if !buf.is_empty() {
            words.push(ast::SimpleWord::Literal(buf));
        }

        Ok(words)
    }

    /// Parses a command subsitution in the form \`cmd\`.
    ///
    /// Any backslashes that are immediately followed by \, $, or ` are removed
    /// before the contents inside the original backticks are recursively parsed
    /// as a command.
    fn backticked_raw(&mut self) -> ParseResult<ast::SimpleWord> {
        let backtick_pos = self.iter.pos();
        eat!(self, { Backtick => {} });

        // FIXME: it would be great to not have to buffer all tokens between backticks
        // and lazily consume them. Unfortunately the BacktickBackslashRemover iterator
        // returns a Result<Token, UnmatchedError>, so we can't temporarily substitute it
        // for our regular iterator (without forcing us to check the the value of each
        // `peek` or `next` operation we make).
        let tok_iter = self
            .iter
            .token_iter_from_backticked_with_removed_backslashes(backtick_pos)
            .map_err(|e| ParseError::Unmatched(e.0, e.1))?;

        let mut tok_backup = TokenIterWrapper::Buffered(tok_iter);

        mem::swap(&mut self.iter, &mut tok_backup);
        let cmd_subst = self.command_group(CommandGroupDelimiters::default());
        let _ = mem::replace(&mut self.iter, tok_backup);

        Ok(ast::SimpleWord::Subst(ast::ParameterSubstitution::Command(
            cmd_subst?.commands,
        )))
    }

    /// Parses a parameters such as `$$`, `$1`, `$foo`, etc, or
    /// parameter substitutions such as `$(cmd)`, `${param-word}`, etc.
    ///
    /// Since it is possible that a leading `$` is not followed by a valid
    /// parameter, the `$` should be treated as a literal. Thus this method
    /// returns an `Word`, which will capture both cases where a literal or
    /// parameter is parsed.
    fn parameter_raw(&mut self) -> ParseResult<ast::SimpleWord> {
        let start_pos = self.iter.pos();
        match self.iter.next() {
            Some(ParamPositional(p)) => Ok(ast::SimpleWord::Param(Parameter::Positional(p as u32))),

            Some(Dollar) => match self.iter.peek() {
                Some(&Star) | Some(&Pound) | Some(&Question) | Some(&Dollar) | Some(&Bang)
                | Some(&Dash) | Some(&At) | Some(&Name(_)) => {
                    Ok(ast::SimpleWord::Param(self.parameter_inner()?))
                }

                Some(&ParenOpen) | Some(&CurlyOpen) => self.parameter_substitution_raw(),

                _ => Ok(ast::SimpleWord::Literal(Dollar.to_string())),
            },

            Some(t) => Err(ParseError::Unexpected(t, start_pos)),
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    /// Parses the word part of a parameter substitution, up to and including
    /// the closing curly brace.
    ///
    /// All tokens that normally cannot be part of a word will be treated
    /// as literals.
    fn parameter_substitution_word_raw(
        &mut self,
        curly_open_pos: SourcePos,
    ) -> ParseResult<Option<ast::ComplexWord>> {
        let mut words = Vec::new();
        'capture_words: loop {
            'capture_literals: loop {
                let found_backslash = match self.iter.peek() {
                    None | Some(&CurlyClose) => break 'capture_words,

                    Some(&Backslash) => true,

                    // Tokens that are normally skipped or ignored either always or at the
                    // start of a word (e.g. #), we want to make sure we capture these ourselves.
                    Some(t @ &Pound)
                    | Some(t @ &ParenOpen)
                    | Some(t @ &ParenClose)
                    | Some(t @ &Semi)
                    | Some(t @ &Amp)
                    | Some(t @ &Pipe)
                    | Some(t @ &AndIf)
                    | Some(t @ &OrIf)
                    | Some(t @ &DSemi)
                    | Some(t @ &Less)
                    | Some(t @ &Great)
                    | Some(t @ &DLess)
                    | Some(t @ &DGreat)
                    | Some(t @ &GreatAnd)
                    | Some(t @ &LessAnd)
                    | Some(t @ &DLessDash)
                    | Some(t @ &Clobber)
                    | Some(t @ &LessGreat)
                    | Some(t @ &Whitespace(_))
                    | Some(t @ &Newline) => {
                        words.push(ast::Word::Simple(ast::SimpleWord::Literal(
                            t.as_str().to_owned(),
                        )));
                        false
                    }

                    // Tokens that are always part of a word,
                    // don't have to handle them differently.
                    Some(&CurlyOpen)
                    | Some(&SquareOpen)
                    | Some(&SquareClose)
                    | Some(&SingleQuote)
                    | Some(&DoubleQuote)
                    | Some(&Star)
                    | Some(&Question)
                    | Some(&Tilde)
                    | Some(&Bang)
                    | Some(&Percent)
                    | Some(&Dash)
                    | Some(&Equals)
                    | Some(&Plus)
                    | Some(&Colon)
                    | Some(&At)
                    | Some(&Caret)
                    | Some(&Slash)
                    | Some(&Comma)
                    | Some(&Name(_))
                    | Some(&Literal(_))
                    | Some(&Backtick)
                    | Some(&Dollar)
                    | Some(&ParamPositional(_)) => break 'capture_literals,
                };

                // Escaped newlines are considered whitespace and usually ignored,
                // so we have to manually capture here.
                let skip_twice = if found_backslash {
                    let mut peek = self.iter.multipeek();
                    peek.peek_next(); // Skip past the Backslash
                    if let Some(t @ &Newline) = peek.peek_next() {
                        words.push(ast::Word::Simple(ast::SimpleWord::Escaped(
                            t.as_str().to_owned(),
                        )));
                        true
                    } else {
                        // Backslash is escaping something other than newline,
                        // capture it like a regular word.
                        break 'capture_literals;
                    }
                } else {
                    false
                };

                self.iter.next(); // Skip the first token that was peeked above
                if skip_twice {
                    self.iter.next();
                }
            }

            match self.word_preserve_trailing_whitespace_with_delim(Some(CurlyClose))? {
                Some(ast::ComplexWord::Single(w)) => words.push(w),
                Some(ast::ComplexWord::Concat(ws)) => words.extend(ws),
                None => break 'capture_words,
            }
        }

        eat_maybe!(self, {
            CurlyClose => {};
            _ => { return Err(ParseError::Unmatched(CurlyOpen, curly_open_pos)); }
        });

        if words.is_empty() {
            Ok(None)
        } else if words.len() == 1 {
            Ok(Some(ast::ComplexWord::Single(words.pop().unwrap())))
        } else {
            Ok(Some(ast::ComplexWord::Concat(words)))
        }
    }

    /// Parses everything that comes after the parameter in a parameter substitution.
    ///
    /// For example, given `${<param><colon><operator><word>}`, this function will consume
    /// the colon, operator, word, and closing curly.
    ///
    /// Nothing is passed to the builder.
    fn parameter_substitution_body_raw(
        &mut self,
        param: Parameter,
        curly_open_pos: SourcePos,
    ) -> ParseResult<ast::SimpleWord> {
        let has_colon = eat_maybe!(self, {
            Colon => { true };
            _ => { false },
        });

        let op_pos = self.iter.pos();
        let op = match self.iter.next() {
            Some(tok @ Dash) | Some(tok @ Equals) | Some(tok @ Question) | Some(tok @ Plus) => tok,

            Some(CurlyClose) => return Ok(ast::SimpleWord::Param(param)),

            Some(t) => return Err(ParseError::BadSubst(t, op_pos)),
            None => return Err(ParseError::Unmatched(CurlyOpen, curly_open_pos)),
        };

        let word = self.parameter_substitution_word_raw(curly_open_pos)?;
        let maybe_len = param == Parameter::Pound && !has_colon && word.is_none();

        // We must carefully check if we get ${#-} or ${#?}, in which case
        // we have parsed a Len substitution and not something else
        let ret = if maybe_len && op == Dash {
            ast::ParameterSubstitution::Len(Parameter::Dash)
        } else if maybe_len && op == Question {
            ast::ParameterSubstitution::Len(Parameter::Question)
        } else {
            match op {
                Dash => ast::ParameterSubstitution::Default(has_colon, param, word.map(Box::new)),
                Equals => ast::ParameterSubstitution::Assign(has_colon, param, word.map(Box::new)),
                Question => ast::ParameterSubstitution::Error(has_colon, param, word.map(Box::new)),
                Plus => {
                    ast::ParameterSubstitution::Alternative(has_colon, param, word.map(Box::new))
                }
                _ => unreachable!(),
            }
        };
        Ok(ast::SimpleWord::Subst(ret))
    }

    /// Parses a parameter substitution in the form of `${...}`, `$(...)`, or `$((...))`.
    /// Nothing is passed to the builder.
    fn parameter_substitution_raw(&mut self) -> ParseResult<ast::SimpleWord> {
        let start_pos = self.iter.pos();
        match self.iter.peek() {
            Some(&ParenOpen) => {
                let is_arith = {
                    let mut peeked = self.iter.multipeek();
                    peeked.peek_next(); // Skip first ParenOpen
                    Some(&ParenOpen) == peeked.peek_next()
                };

                let subst = if is_arith {
                    eat!(self, { ParenOpen => {} });
                    eat!(self, { ParenOpen => {} });

                    // If we hit a paren right off the bat either the body is empty
                    // or there is a stray paren which will result in an error either
                    // when we look for the closing parens or sometime after.
                    self.skip_whitespace();
                    let subst = if let Some(&ParenClose) = self.iter.peek() {
                        None
                    } else {
                        Some(self.arithmetic_substitution()?)
                    };

                    // Some shells allow the closing parens to have whitespace in between
                    self.skip_whitespace();
                    eat!(self, { ParenClose => {} });
                    self.skip_whitespace();
                    eat!(self, { ParenClose => {} });

                    ast::ParameterSubstitution::Arith(subst)
                } else {
                    ast::ParameterSubstitution::Command(self.subshell_internal(true)?.commands)
                };

                Ok(ast::SimpleWord::Subst(subst))
            }

            Some(&CurlyOpen) => {
                let curly_open_pos = start_pos;
                self.iter.next();

                let param = self.parameter_inner()?;
                let subst = match self.iter.peek() {
                    Some(&Percent) => {
                        self.iter.next();
                        eat_maybe!(self, {
                            Percent => {
                                let word = self.parameter_substitution_word_raw(curly_open_pos);
                                ast::ParameterSubstitution::RemoveLargestSuffix(param, word?.map(Box::new))
                            };
                            _ => {
                                let word = self.parameter_substitution_word_raw(curly_open_pos);
                                ast::ParameterSubstitution::RemoveSmallestSuffix(param, word?.map(Box::new))
                            }
                        })
                    }

                    Some(&Pound) => {
                        self.iter.next();
                        eat_maybe!(self, {
                            Pound => {
                                let word = self.parameter_substitution_word_raw(curly_open_pos);
                                ast::ParameterSubstitution::RemoveLargestPrefix(param, word?.map(Box::new))
                            };
                            _ => {
                                match self.parameter_substitution_word_raw(curly_open_pos)? {
                                    // Handle ${##} case
                                    None if Parameter::Pound == param => ast::ParameterSubstitution::Len(Parameter::Pound),
                                    w => ast::ParameterSubstitution::RemoveSmallestPrefix(param, w.map(Box::new)),
                                }
                            }
                        })
                    }

                    // In this case the found # is the parameter itself
                    Some(&Colon) | Some(&Dash) | Some(&Equals) | Some(&Question) | Some(&Plus)
                    | Some(&CurlyClose)
                        if Parameter::Pound == param =>
                    {
                        return self.parameter_substitution_body_raw(param, curly_open_pos)
                    }

                    // Otherwise we must have ${#param}
                    _ if Parameter::Pound == param => {
                        let param = self.parameter_inner()?;
                        eat!(self, { CurlyClose => { ast::ParameterSubstitution::Len(param) } })
                    }

                    _ => return self.parameter_substitution_body_raw(param, curly_open_pos),
                };

                Ok(ast::SimpleWord::Subst(subst))
            }

            _ => Err(self.make_unexpected_err()),
        }
    }

    /// Parses a valid parameter that can appear inside a set of curly braces.
    fn parameter_inner(&mut self) -> ParseResult<Parameter> {
        let start_pos = self.iter.pos();
        let param = match self.iter.next() {
            Some(Star) => Parameter::Star,
            Some(Pound) => Parameter::Pound,
            Some(Question) => Parameter::Question,
            Some(Dollar) => Parameter::Dollar,
            Some(Bang) => Parameter::Bang,
            Some(Dash) => Parameter::Dash,
            Some(At) => Parameter::At,

            Some(Name(n)) => Parameter::Var(n),
            Some(Literal(s)) => match u32::from_str(&s) {
                Ok(n) => Parameter::Positional(n),
                Err(_) => return Err(ParseError::BadSubst(Literal(s), start_pos)),
            },

            Some(t) => return Err(ParseError::BadSubst(t, start_pos)),
            None => return Err(ParseError::UnexpectedEOF),
        };

        Ok(param)
    }

    /// Parses any number of sequential commands between the `do` and `done`
    /// reserved words. Each of the reserved words must be a literal token, and cannot be
    /// quoted or concatenated.
    pub fn do_group(&mut self) -> ParseResult<CommandGroup<ast::Command>> {
        let start_pos = self.iter.pos();
        self.reserved_word(&[DO])
            .map_err(|_| self.make_unexpected_err())?;
        let result = self.command_group(CommandGroupDelimiters {
            reserved_words: &[DONE],
            ..Default::default()
        })?;
        self.reserved_word(&[DONE])
            .map_err(|()| ParseError::IncompleteCmd(DO, start_pos, DONE, self.iter.pos()))?;
        Ok(result)
    }

    /// Parses any number of sequential commands between balanced `{` and `}`
    /// reserved words. Each of the reserved words must be a literal token, and cannot be quoted.
    pub fn brace_group(&mut self) -> ParseResult<CommandGroup<ast::Command>> {
        // CurlyClose must be encountered as a stand alone word,
        // even though it is represented as its own token
        let start_pos = self.iter.pos();
        self.reserved_token(&[CurlyOpen])?;
        let cmds = self.command_group(CommandGroupDelimiters {
            reserved_tokens: &[CurlyClose],
            ..Default::default()
        })?;
        self.reserved_token(&[CurlyClose])
            .map_err(|_| ParseError::Unmatched(CurlyOpen, start_pos))?;
        Ok(cmds)
    }

    /// Parses any number of sequential commands between balanced `(` and `)`.
    ///
    /// It is considered an error if no commands are present inside the subshell.
    pub fn subshell(&mut self) -> ParseResult<CommandGroup<ast::Command>> {
        self.subshell_internal(false)
    }

    /// Like `Parser::subshell` but allows the caller to specify
    /// if an empty body constitutes an error or not.
    fn subshell_internal(
        &mut self,
        empty_body_ok: bool,
    ) -> ParseResult<CommandGroup<ast::Command>> {
        let start_pos = self.iter.pos();
        eat!(self, { ParenOpen => {} });

        // Parens are always special tokens
        let body = self.command_group(CommandGroupDelimiters {
            exact_tokens: &[ParenClose],
            ..Default::default()
        })?;

        match self.iter.peek() {
            Some(&ParenClose) if empty_body_ok || !body.commands.is_empty() => {
                self.iter.next();
                Ok(body)
            }
            Some(_) => Err(self.make_unexpected_err()),
            None => Err(ParseError::Unmatched(ParenOpen, start_pos)),
        }
    }

    /// Peeks at the next token (after skipping whitespace) to determine
    /// if (and which) compound command may follow.
    fn next_compound_command_type(&mut self) -> Option<CompoundCmdKeyword> {
        self.skip_whitespace();
        if Some(&ParenOpen) == self.iter.peek() {
            Some(CompoundCmdKeyword::Subshell)
        } else if self.peek_reserved_token(&[CurlyOpen]).is_some() {
            Some(CompoundCmdKeyword::Brace)
        } else {
            match self.peek_reserved_word(&[FOR, CASE, IF, WHILE, UNTIL]) {
                Some(FOR) => Some(CompoundCmdKeyword::For),
                Some(CASE) => Some(CompoundCmdKeyword::Case),
                Some(IF) => Some(CompoundCmdKeyword::If),
                Some(WHILE) => Some(CompoundCmdKeyword::While),
                Some(UNTIL) => Some(CompoundCmdKeyword::Until),
                _ => None,
            }
        }
    }

    /// Parses compound commands like `for`, `case`, `if`, `while`, `until`,
    /// brace groups, or subshells, including any redirection lists to be applied to them.
    pub fn compound_command(&mut self) -> ParseResult<ast::CompoundCommand> {
        self.compound_command_internal(None)
    }

    /// Slightly optimized version of `Parse::compound_command` that will not
    /// check an upcoming reserved word if the caller already knows the answer.
    fn compound_command_internal(
        &mut self,
        kw: Option<CompoundCmdKeyword>,
    ) -> ParseResult<ast::CompoundCommand> {
        let cmd = match kw.or_else(|| self.next_compound_command_type()) {
            Some(CompoundCmdKeyword::If) => {
                let IfFragments {
                    conditionals,
                    else_branch,
                } = self.if_command()?;
                let mut io = self.redirect_list()?;

                let conditionals = conditionals
                    .into_iter()
                    .map(|gbp| {
                        let mut guard = gbp.guard;
                        let mut body = gbp.body;

                        guard.shrink_to_fit();
                        body.shrink_to_fit();

                        ast::GuardBodyPair { guard, body }
                    })
                    .collect();

                let else_branch = else_branch.map(
                    |CommandGroup {
                         commands: mut els, ..
                     }| {
                        els.shrink_to_fit();
                        els
                    },
                );

                io.shrink_to_fit();

                Ok(ast::CompoundCommand {
                    kind: ast::CompoundCommandKind::If {
                        conditionals,
                        else_branch,
                    },
                    io,
                })
            }

            Some(CompoundCmdKeyword::While) | Some(CompoundCmdKeyword::Until) => {
                let (kind, guard_body_pair) = self.loop_command()?;
                let mut io = self.redirect_list()?;

                let mut guard = guard_body_pair.guard;
                let mut body = guard_body_pair.body;

                guard.shrink_to_fit();
                body.shrink_to_fit();
                io.shrink_to_fit();

                let guard_body_pair = ast::GuardBodyPair { guard, body };

                let loop_cmd = match kind {
                    LoopKind::While => ast::CompoundCommandKind::While(guard_body_pair),
                    LoopKind::Until => ast::CompoundCommandKind::Until(guard_body_pair),
                };

                Ok(ast::CompoundCommand { kind: loop_cmd, io })
            }

            Some(CompoundCmdKeyword::For) => {
                let fragments = self.for_command()?;
                let mut io = self.redirect_list()?;

                let mut body = fragments.body.commands;
                body.shrink_to_fit();
                io.shrink_to_fit();

                Ok(ast::CompoundCommand {
                    kind: ast::CompoundCommandKind::For {
                        var: fragments.var,
                        words: fragments.words,
                        body,
                    },
                    io,
                })
            }

            Some(CompoundCmdKeyword::Case) => {
                let mut fragments = self.case_command()?;
                let mut io = self.redirect_list()?;

                for arm in &mut fragments.arms {
                    arm.patterns.shrink_to_fit();
                    arm.body.shrink_to_fit();
                }

                io.shrink_to_fit();
                Ok(ast::CompoundCommand {
                    kind: ast::CompoundCommandKind::Case {
                        word: fragments.word,
                        arms: fragments.arms,
                    },
                    io,
                })
            }

            Some(CompoundCmdKeyword::Brace) => {
                let mut cmds = self.brace_group()?.commands;
                let mut io = self.redirect_list()?;
                cmds.shrink_to_fit();
                io.shrink_to_fit();
                Ok(ast::CompoundCommand {
                    kind: ast::CompoundCommandKind::Brace(cmds),
                    io,
                })
            }

            Some(CompoundCmdKeyword::Subshell) => {
                let mut cmds = self.subshell()?.commands;
                let mut io = self.redirect_list()?;
                cmds.shrink_to_fit();
                io.shrink_to_fit();
                Ok(ast::CompoundCommand {
                    kind: ast::CompoundCommandKind::Subshell(cmds),
                    io,
                })
            }

            None => return Err(self.make_unexpected_err()),
        };

        cmd
    }

    /// Parses loop commands like `while` and `until` but does not parse any
    /// redirections that may follow.
    ///
    /// Since they are compound commands (and can have redirections applied to
    /// the entire loop) this method returns the relevant parts of the loop command,
    /// without constructing an AST node, it so that the caller can do so with redirections.
    pub fn loop_command(&mut self) -> ParseResult<(LoopKind, ast::GuardBodyPair)> {
        let start_pos = self.iter.pos();
        let kind = match self
            .reserved_word(&[WHILE, UNTIL])
            .map_err(|_| self.make_unexpected_err())?
        {
            WHILE => LoopKind::While,
            UNTIL => LoopKind::Until,
            _ => unreachable!(),
        };
        let guard = self.command_group(CommandGroupDelimiters {
            reserved_words: &[DO],
            ..Default::default()
        })?;
        match self.peek_reserved_word(&[DO]) {
            Some(_) => Ok((
                kind,
                ast::GuardBodyPair {
                    guard: guard.commands,
                    body: self.do_group()?.commands,
                },
            )),
            None => Err(ParseError::IncompleteCmd(
                WHILE,
                start_pos,
                DO,
                self.iter.pos(),
            )),
        }
    }

    /// Parses a single `if` command but does not parse any redirections that may follow.
    ///
    /// Since `if` is a compound command (and can have redirections applied to it) this
    /// method returns the relevant parts of the `if` command, without constructing an
    /// AST node, it so that the caller can do so with redirections.
    pub fn if_command(&mut self) -> ParseResult<IfFragments<ast::Command>> {
        let start_pos = self.iter.pos();
        self.reserved_word(&[IF])
            .map_err(|_| self.make_unexpected_err())?;

        macro_rules! missing_fi {
            () => {
                |_| ParseError::IncompleteCmd(IF, start_pos, FI, self.iter.pos())
            };
        }

        macro_rules! missing_then {
            () => {
                |_| ParseError::IncompleteCmd(IF, start_pos, THEN, self.iter.pos())
            };
        }

        let mut conditionals = Vec::new();
        loop {
            let guard = self.command_group(CommandGroupDelimiters {
                reserved_words: &[THEN],
                ..Default::default()
            })?;
            self.reserved_word(&[THEN]).map_err(missing_then!())?;

            let body = self.command_group(CommandGroupDelimiters {
                reserved_words: &[ELIF, ELSE, FI],
                ..Default::default()
            })?;
            conditionals.push(ast::GuardBodyPair {
                guard: guard.commands,
                body: body.commands,
            });

            let els = match self
                .reserved_word(&[ELIF, ELSE, FI])
                .map_err(missing_fi!())?
            {
                ELIF => continue,
                ELSE => {
                    let els = self.command_group(CommandGroupDelimiters {
                        reserved_words: &[FI],
                        ..Default::default()
                    })?;
                    self.reserved_word(&[FI]).map_err(missing_fi!())?;
                    Some(els)
                }
                FI => None,
                _ => unreachable!(),
            };

            return Ok(IfFragments {
                conditionals,
                else_branch: els,
            });
        }
    }

    /// Parses a single `for` command but does not parse any redirections that may follow.
    ///
    /// Since `for` is a compound command (and can have redirections applied to it) this
    /// method returns the relevant parts of the `for` command, without constructing an
    /// AST node, it so that the caller can do so with redirections.
    pub fn for_command(&mut self) -> ParseResult<ForFragments<ast::ComplexWord, ast::Command>> {
        let start_pos = self.iter.pos();
        self.reserved_word(&[FOR])
            .map_err(|_| self.make_unexpected_err())?;

        self.skip_whitespace();

        match self.iter.peek() {
            Some(&Name(_)) | Some(&Literal(_)) => {}
            _ => return Err(self.make_unexpected_err()),
        }

        let var_pos = self.iter.pos();
        let var = match self.iter.next() {
            Some(Name(v)) => v,
            Some(Literal(s)) => return Err(ParseError::BadIdent(s, var_pos)),
            _ => unreachable!(),
        };

        let var_comment = self.newline();
        let post_var_comments = self.linebreak();

        // A for command can take one of several different shapes (in pseudo regex syntax):
        // `for name [\n*] [in [word*]] [;\n* | \n+] do_group`
        // Below we'll disambiguate what situation we have as we move along.
        let words = if self.peek_reserved_word(&[IN]).is_some() {
            // Found `in` keyword, therefore we're looking at something like
            // `for name \n* in [words*] [;\n* | \n+] do_group`
            self.reserved_word(&[IN]).unwrap();

            let mut words = Vec::new();
            while let Some(w) = self.word()? {
                words.push(w);
            }

            let found_semi = eat_maybe!(self, {
                Semi => { true };
                _ => { false }
            });

            // We need either a newline or a ; to separate the words from the body
            // Thus if neither is found it is considered an error
            let words_comment = self.newline();
            if !found_semi && !words_comment {
                return Err(self.make_unexpected_err());
            }

            self.linebreak();
            Some(words)
        } else if Some(&Semi) == self.iter.peek() {
            // `for name \n*;\n* do_group`
            eat!(self, { Semi => {} });
            self.linebreak();
            None
        } else if self.peek_reserved_word(&[DO]).is_none() {
            // If we didn't find an `in` keyword, and we havent hit the body
            // (a `do` keyword), then we can reasonably say the script has
            // words without an `in` keyword.
            return Err(ParseError::IncompleteCmd(
                FOR,
                start_pos,
                IN,
                self.iter.pos(),
            ));
        } else {
            // `for name \n* do_group`
            None
        };

        if self.peek_reserved_word(&[DO]).is_none() {
            return Err(ParseError::IncompleteCmd(
                FOR,
                start_pos,
                DO,
                self.iter.pos(),
            ));
        }

        let body = self.do_group()?;
        Ok(ForFragments { var, words, body })
    }

    /// Parses a single `case` command but does not parse any redirections that may follow.
    ///
    /// Since `case` is a compound command (and can have redirections applied to it) this
    /// method returns the relevant parts of the `case` command, without constructing an
    /// AST node, it so that the caller can do so with redirections.
    pub fn case_command(&mut self) -> ParseResult<CaseFragments<ast::ComplexWord>> {
        let start_pos = self.iter.pos();

        macro_rules! missing_in {
            () => {
                |_| ParseError::IncompleteCmd(CASE, start_pos, IN, self.iter.pos())
            };
        }

        macro_rules! missing_esac {
            () => {
                |_| ParseError::IncompleteCmd(CASE, start_pos, ESAC, self.iter.pos())
            };
        }

        self.reserved_word(&[CASE])
            .map_err(|_| self.make_unexpected_err())?;

        let word = match self.word()? {
            Some(w) => w,
            None => return Err(self.make_unexpected_err()),
        };

        let post_word_comments = self.linebreak();
        self.reserved_word(&[IN]).map_err(missing_in!())?;
        let in_comment = self.newline();

        let mut pre_esac_comments = None;
        let mut arms = Vec::new();
        loop {
            let pre_pattern_comments = self.linebreak();
            if self.peek_reserved_word(&[ESAC]).is_some() {
                // Make sure we don't lose the captured comments if there are no body
                debug_assert_eq!(pre_esac_comments, None);
                pre_esac_comments = Some(pre_pattern_comments);
                break;
            }

            if let Some(&ParenOpen) = self.iter.peek() {
                self.iter.next();
            }

            // Make sure we check for missing `esac` here, otherwise if we have EOF
            // trying to parse a word will result in an `UnexpectedEOF` error
            if self.iter.peek().is_none() {
                return Err(()).map_err(missing_esac!());
            }

            let mut patterns = Vec::new();
            loop {
                match self.word()? {
                    Some(p) => patterns.push(p),
                    None => return Err(self.make_unexpected_err()),
                }

                match self.iter.peek() {
                    Some(&Pipe) => {
                        self.iter.next();
                        continue;
                    }

                    Some(&ParenClose) if !patterns.is_empty() => {
                        self.iter.next();
                        break;
                    }

                    // Make sure we check for missing `esac` here, otherwise if we have EOF
                    // trying to parse a word will result in an `UnexpectedEOF` error
                    None => return Err(()).map_err(missing_esac!()),
                    _ => return Err(self.make_unexpected_err()),
                }
            }

            let pattern_comment = self.newline();
            let body = self.command_group(CommandGroupDelimiters {
                reserved_words: &[ESAC],
                reserved_tokens: &[],
                exact_tokens: &[DSemi],
            })?;

            let (no_more_arms, arm_comment) = if Some(&DSemi) == self.iter.peek() {
                self.iter.next();
                (false, self.newline())
            } else {
                (true, false)
            };

            arms.push(ast::PatternBodyPair {
                patterns,
                body: body.commands,
            });

            if no_more_arms {
                break;
            }
        }

        self.linebreak();
        self.reserved_word(&[ESAC]).map_err(missing_esac!())?;

        Ok(CaseFragments { word, arms })
    }

    /// Parses a single function declaration if present. If no function is present,
    /// nothing is consumed from the token stream.
    pub fn maybe_function_declaration(&mut self) -> ParseResult<Option<ast::PipeableCommand>> {
        if self.peek_reserved_word(&[FUNCTION]).is_some() {
            return self.function_declaration().map(Some);
        }

        let is_fn = {
            let mut peeked = self.iter.multipeek();
            if let Some(&Name(_)) = peeked.peek_next() {
                match peeked.peek_next() {
                    Some(&Whitespace(_)) => Some(&ParenOpen) == peeked.peek_next(),
                    Some(&ParenOpen) => true,
                    _ => false,
                }
            } else {
                false
            }
        };

        if is_fn {
            self.function_declaration().map(Some)
        } else {
            Ok(None)
        }
    }

    /// Parses a single function declaration.
    ///
    /// A function declaration must either begin with the `function` reserved word, or
    /// the name of the function must be followed by `()`. Whitespace is allowed between
    /// the name and `(`, and whitespace is allowed between `()`.
    pub fn function_declaration(&mut self) -> ParseResult<ast::PipeableCommand> {
        let (name, body) = self.function_declaration_internal()?;
        Ok(ast::PipeableCommand::FunctionDef(name, Arc::new(body)))
    }

    /// Like `Parser::function_declaration`, but does not pass the result to the builder
    fn function_declaration_internal(&mut self) -> ParseResult<(String, ast::CompoundCommand)> {
        let found_fn = match self.peek_reserved_word(&[FUNCTION]) {
            Some(_) => {
                self.iter.next();
                true
            }
            None => false,
        };

        self.skip_whitespace();

        match self.iter.peek() {
            Some(&Name(_)) | Some(&Literal(_)) => {}
            _ => return Err(self.make_unexpected_err()),
        }

        let ident_pos = self.iter.pos();
        let name = match self.iter.next() {
            Some(Name(n)) => n,
            Some(Literal(s)) => return Err(ParseError::BadIdent(s, ident_pos)),
            _ => unreachable!(),
        };

        // If there is no whitespace after the function name, the only valid
        // possibility is for `()` to appear.
        let body = if Some(&ParenOpen) == self.iter.peek() {
            eat!(self, { ParenOpen => {} });
            self.skip_whitespace();
            eat!(self, { ParenClose => {} });
            None
        } else if found_fn && Some(&Newline) == self.iter.peek() {
            // Do nothing, function declaration satisfied
            None
        } else {
            // Enforce at least one whitespace between function declaration and body
            eat!(self, { Whitespace(_) => {} });
            self.skip_whitespace();

            // If we didn't find the function keyword, we MUST find `()` at this time
            if !found_fn {
                eat!(self, { ParenOpen => {} });
                self.skip_whitespace();
                eat!(self, { ParenClose => {} });
                None
            } else if Some(&ParenOpen) == self.iter.peek() {
                // Otherwise it is possible for there to be a subshell as the body
                let subshell = self.subshell_internal(true)?;
                if subshell.commands.is_empty() {
                    // Case like `function foo () ...`
                    None
                } else {
                    // Case like `function foo (subshell)`
                    Some(ast::CompoundCommand {
                        kind: ast::CompoundCommandKind::Subshell(subshell.commands),
                        io: Vec::new(),
                    })
                }
            } else {
                None
            }
        };

        let body = match body {
            Some(subshell) => subshell,
            None => {
                self.linebreak();
                self.compound_command()?
            }
        };

        Ok((name, body))
    }

    /// Skips over any encountered whitespace but preserves newlines.
    #[inline]
    pub fn skip_whitespace(&mut self) {
        loop {
            while let Some(&Whitespace(_)) = self.iter.peek() {
                self.iter.next();
            }

            let found_backslash_newline = {
                let mut peeked = self.iter.multipeek();
                Some(&Backslash) == peeked.peek_next() && Some(&Newline) == peeked.peek_next()
            };

            if found_backslash_newline {
                self.iter.next();
                self.iter.next();
            } else {
                break;
            }
        }
    }

    /// Parses zero or more `Token::Newline`s, skipping whitespace.
    #[inline]
    pub fn linebreak(&mut self) {
        while self.newline() {}
    }

    /// Tries to parse a `Token::Newline` after skipping whitespace.
    pub fn newline(&mut self) -> bool {
        self.skip_whitespace();

        match self.iter.peek() {
            Some(&Pound) => {
                self.iter.by_ref().take_while(|t| t != &Newline).count();
                true
            }

            Some(&Newline) => {
                self.iter.next();
                true
            }

            _ => false,
        }
    }

    /// Checks if the next tokens are `#/`. Nothing is consumed from the token stream.
    #[cfg(feature = "old-parser")]
    pub fn peek_end_delimiter(&mut self) -> bool {
        self.skip_whitespace();

        let mut peeked = self.iter.multipeek();
        match peeked.peek_next() {
            Some(&Pound) => matches!(peeked.peek_next(), Some(&Slash)),
            _ => false,
        }
    }

    /// Checks that one of the specified tokens appears as a reserved word.
    ///
    /// The token must be followed by a token which delimits a word when it is
    /// unquoted/unescaped.
    ///
    /// If a reserved word is found, the token which it matches will be
    /// returned in case the caller cares which specific reserved word was found.
    pub fn peek_reserved_token<'a>(&mut self, tokens: &'a [Token]) -> Option<&'a Token> {
        if tokens.is_empty() {
            return None;
        }

        let care_about_whitespace = tokens.iter().any(|tok| matches!(*tok, Whitespace(_)));

        // If the caller cares about whitespace as a reserved word we should
        // do a reserved word check without skipping any leading whitespace.
        // If we don't find anything the first time (or if the caller does
        // not care about whitespace tokens) we will skip the whitespace
        // and try again.
        let num_tries = if care_about_whitespace {
            2
        } else {
            self.skip_whitespace();
            1
        };

        for _ in 0..num_tries {
            {
                let mut peeked = self.iter.multipeek();

                let found_tok = match peeked.peek_next() {
                    Some(tok) => tokens.iter().find(|&t| t == tok),
                    None => return None,
                };

                if let ret @ Some(_) = found_tok {
                    let found_delim = match peeked.peek_next() {
                        Some(delim) => delim.is_word_delimiter(),
                        None => true, // EOF is also a valid delimeter
                    };

                    if found_delim {
                        return ret;
                    }
                }
            }

            self.skip_whitespace();
        }

        None
    }

    /// Checks that one of the specified strings appears as a reserved word.
    ///
    /// The word must appear as a single token, unquoted and unescaped, and
    /// must be followed by a token which delimits a word when it is
    /// unquoted/unescaped. The reserved word may appear as a `Token::Name`
    /// or a `Token::Literal`.
    ///
    /// If a reserved word is found, the string which it matches will be
    /// returned in case the caller cares which specific reserved word was found.
    pub fn peek_reserved_word<'a>(&mut self, words: &'a [&str]) -> Option<&'a str> {
        if words.is_empty() {
            return None;
        }

        self.skip_whitespace();
        let mut peeked = self.iter.multipeek();
        let found_tok = match peeked.peek_next() {
            Some(&Name(ref kw)) | Some(&Literal(ref kw)) => {
                words.iter().find(|&w| w == kw).copied()
            }
            _ => None,
        };

        match peeked.peek_next() {
            Some(delim) if delim.is_word_delimiter() => found_tok,
            None => found_tok, // EOF is a valid delimeter
            _ => None,
        }
    }

    /// Checks that one of the specified tokens appears as a reserved word
    /// and consumes it, returning the token it matched in case the caller
    /// cares which specific reserved word was found.
    pub fn reserved_token(&mut self, tokens: &[Token]) -> ParseResult<Token> {
        match self.peek_reserved_token(tokens) {
            Some(_) => Ok(self.iter.next().unwrap()),
            None => {
                // If the desired token is next, but we failed to find a reserved
                // token (because the token after it isn't a valid delimeter)
                // then the following token is the unexpected culprit, so we should
                // skip the upcoming one before forming the error.
                let skip_one = {
                    let peeked = self.iter.peek();
                    tokens.iter().any(|t| Some(t) == peeked)
                };

                if skip_one {
                    self.iter.next();
                }
                Err(self.make_unexpected_err())
            }
        }
    }

    /// Checks that one of the specified strings appears as a reserved word
    /// and consumes it, returning the string it matched in case the caller
    /// cares which specific reserved word was found.
    pub fn reserved_word<'a>(&mut self, words: &'a [&str]) -> Result<&'a str, ()> {
        match self.peek_reserved_word(words) {
            Some(s) => {
                self.iter.next();
                Ok(s)
            }
            None => Err(()),
        }
    }

    /// Parses commands until a configured delimeter (or EOF)
    /// is reached, without consuming the token or reserved word.
    pub fn command_group(
        &mut self,
        cfg: CommandGroupDelimiters<'_, '_, '_>,
    ) -> ParseResult<CommandGroup<ast::Command>> {
        let found_delim = |slf: &mut Parser<_>| {
            let found_exact = !cfg.exact_tokens.is_empty()
                && slf
                    .iter
                    .peek()
                    .map(|peeked| cfg.exact_tokens.iter().any(|tok| tok == peeked))
                    .unwrap_or(false);

            found_exact
                || slf.peek_reserved_word(cfg.reserved_words).is_some()
                || slf.peek_reserved_token(cfg.reserved_tokens).is_some()
        };

        let mut cmds = Vec::new();
        loop {
            if found_delim(self) {
                break;
            }

            self.linebreak();

            if found_delim(self) || self.iter.peek().is_none() {
                break;
            }

            match self.complete_command()? {
                Some(cmd) => cmds.push(cmd),
                None => break,
            }
        }

        Ok(CommandGroup { commands: cmds })
    }

    /// Parses the body of any arbitrary arithmetic expression, e.g. `x + $y << 5`.
    /// The caller is responsible for parsing the external `$(( ))` tokens.
    pub fn arithmetic_substitution(&mut self) -> ParseResult<Arithmetic> {
        let mut exprs = Vec::new();
        loop {
            self.skip_whitespace();
            exprs.push(self.arith_assig()?);

            eat_maybe!(self, {
                Comma => {};
                _ => { break },
            });
        }

        if exprs.len() == 1 {
            Ok(exprs.pop().unwrap())
        } else {
            Ok(ast::Arithmetic::Sequence(exprs))
        }
    }

    /// Parses expressions such as `var = expr` or `var op= expr`, where `op` is
    /// any of the following operators: *, /, %, +, -, <<, >>, &, |, ^.
    fn arith_assig(&mut self) -> ParseResult<Arithmetic> {
        use crate::shell::ast::Arithmetic::*;

        self.skip_whitespace();

        let assig = {
            let mut assig = false;
            let mut peeked = self.iter.multipeek();

            'assig_check: loop {
                match peeked.peek_next() {
                    Some(&Dollar) => continue, // Skip Dollar and peek next
                    Some(&Name(_)) => loop {
                        match peeked.peek_next() {
                            Some(&Whitespace(_)) => continue, // Skip whitespace and peek next
                            Some(&Star) | Some(&Slash) | Some(&Percent) | Some(&Plus)
                            | Some(&Dash) | Some(&DLess) | Some(&DGreat) | Some(&Amp)
                            | Some(&Pipe) | Some(&Caret) => {
                                assig = Some(&Equals) == peeked.peek_next()
                            }

                            // Make sure we only recognize $(( x = ...)) but NOT $(( x == ...))
                            Some(&Equals) => assig = Some(&Equals) != peeked.peek_next(),
                            _ => {}
                        }

                        break 'assig_check;
                    },
                    _ => break 'assig_check,
                }
            }

            assig
        };

        if !assig {
            return self.arith_ternary();
        }

        let var = self.arith_var()?;
        self.skip_whitespace();
        let op = match self.iter.next() {
            Some(op @ Star) | Some(op @ Slash) | Some(op @ Percent) | Some(op @ Plus)
            | Some(op @ Dash) | Some(op @ DLess) | Some(op @ DGreat) | Some(op @ Amp)
            | Some(op @ Pipe) | Some(op @ Caret) => {
                eat!(self, { Equals => {} });
                op
            }
            Some(op @ Equals) => op,
            _ => unreachable!(),
        };

        let value = Box::new(self.arith_assig()?);
        let expr = match op {
            Star => Box::new(Mult(Box::new(Var(var.clone())), value)),
            Slash => Box::new(Div(Box::new(Var(var.clone())), value)),
            Percent => Box::new(Modulo(Box::new(Var(var.clone())), value)),
            Plus => Box::new(Add(Box::new(Var(var.clone())), value)),
            Dash => Box::new(Sub(Box::new(Var(var.clone())), value)),
            DLess => Box::new(ShiftLeft(Box::new(Var(var.clone())), value)),
            DGreat => Box::new(ShiftRight(Box::new(Var(var.clone())), value)),
            Amp => Box::new(BitwiseAnd(Box::new(Var(var.clone())), value)),
            Pipe => Box::new(BitwiseOr(Box::new(Var(var.clone())), value)),
            Caret => Box::new(BitwiseXor(Box::new(Var(var.clone())), value)),
            Equals => value,
            _ => unreachable!(),
        };
        Ok(Assign(var, expr))
    }

    /// Parses expressions such as `expr ? expr : expr`.
    fn arith_ternary(&mut self) -> ParseResult<Arithmetic> {
        let guard = self.arith_logical_or()?;
        self.skip_whitespace();
        eat_maybe!(self, {
            Question => {
                let body = self.arith_ternary()?;
                self.skip_whitespace();
                eat!(self, { Colon => {} });
                let els = self.arith_ternary()?;
                Ok(ast::Arithmetic::Ternary(Box::new(guard), Box::new(body), Box::new(els)))
            };
            _ => { Ok(guard) },
        })
    }

    arith_parse!(
        /// Parses expressions such as `expr || expr`.
        fn arith_logical_or,
        arith_logical_and,
        OrIf  => ast::Arithmetic::LogicalOr
    );

    arith_parse!(
        /// Parses expressions such as `expr && expr`.
        fn arith_logical_and,
        arith_bitwise_or,
        AndIf => ast::Arithmetic::LogicalAnd
    );

    arith_parse!(
        /// Parses expressions such as `expr | expr`.
        fn arith_bitwise_or,
        arith_bitwise_xor,
        Pipe  => ast::Arithmetic::BitwiseOr
    );

    arith_parse!(
        /// Parses expressions such as `expr ^ expr`.
        fn arith_bitwise_xor,
        arith_bitwise_and,
        Caret => ast::Arithmetic::BitwiseXor
    );

    arith_parse!(
        /// Parses expressions such as `expr & expr`.
        fn arith_bitwise_and,
        arith_eq,
        Amp   => ast::Arithmetic::BitwiseAnd
    );

    /// Parses expressions such as `expr == expr` or `expr != expr`.
    #[inline]
    fn arith_eq(&mut self) -> ParseResult<Arithmetic> {
        let mut expr = self.arith_ineq()?;
        loop {
            self.skip_whitespace();
            let eq_type = eat_maybe!(self, {
                Equals => { true },
                Bang => { false };
                _ => { break }
            });

            eat!(self, { Equals => {} });
            let next = self.arith_ineq()?;
            expr = if eq_type {
                ast::Arithmetic::Eq(Box::new(expr), Box::new(next))
            } else {
                ast::Arithmetic::NotEq(Box::new(expr), Box::new(next))
            };
        }
        Ok(expr)
    }

    /// Parses expressions such as `expr < expr`,`expr <= expr`,`expr > expr`,`expr >= expr`.
    #[inline]
    fn arith_ineq(&mut self) -> ParseResult<Arithmetic> {
        let mut expr = self.arith_shift()?;
        loop {
            self.skip_whitespace();
            eat_maybe!(self, {
                Less => {
                    let eq = eat_maybe!(self, { Equals => { true }; _ => { false } });
                    let next = self.arith_shift()?;

                    expr = if eq {
                        ast::Arithmetic::LessEq(Box::new(expr), Box::new(next))
                    } else {
                        ast::Arithmetic::Less(Box::new(expr), Box::new(next))
                    };
                },
                Great => {
                    let eq = eat_maybe!(self, { Equals => { true }; _ => { false } });
                    let next = self.arith_shift()?;

                    expr = if eq {
                        ast::Arithmetic::GreatEq(Box::new(expr), Box::new(next))
                    } else {
                        ast::Arithmetic::Great(Box::new(expr), Box::new(next))
                    };
                };
                _ => { break },
            });
        }
        Ok(expr)
    }

    arith_parse!(
        /// Parses expressions such as `expr << expr` or `expr >> expr`.
        fn arith_shift,
        arith_add,
        DLess  => ast::Arithmetic::ShiftLeft,
        DGreat => ast::Arithmetic::ShiftRight
    );

    arith_parse!(
        /// Parses expressions such as `expr + expr` or `expr - expr`.
        fn arith_add,
        arith_mult,
        Plus => ast::Arithmetic::Add,
        Dash => ast::Arithmetic::Sub
    );

    arith_parse!(
        /// Parses expressions such as `expr * expr`, `expr / expr`, or `expr % expr`.
        fn arith_mult,
        arith_pow,
        Star    => ast::Arithmetic::Mult,
        Slash   => ast::Arithmetic::Div,
        Percent => ast::Arithmetic::Modulo
    );

    /// Parses expressions such as `expr ** expr`.
    fn arith_pow(&mut self) -> ParseResult<Arithmetic> {
        let expr = self.arith_unary_misc()?;
        self.skip_whitespace();

        // We must be extra careful here because ** has a higher precedence
        // than *, meaning power operations will be parsed before multiplication.
        // Thus we should be absolutely certain we should parse a ** operator
        // and avoid confusing it with a multiplication operation that is yet
        // to be parsed.
        let double_star = {
            let mut peeked = self.iter.multipeek();
            peeked.peek_next() == Some(&Star) && peeked.peek_next() == Some(&Star)
        };

        if double_star {
            eat!(self, { Star => {} });
            eat!(self, { Star => {} });
            Ok(ast::Arithmetic::Pow(
                Box::new(expr),
                Box::new(self.arith_pow()?),
            ))
        } else {
            Ok(expr)
        }
    }

    /// Parses expressions such as `!expr`, `~expr`, `+expr`, `-expr`, `++var` and `--var`.
    fn arith_unary_misc(&mut self) -> ParseResult<Arithmetic> {
        self.skip_whitespace();
        let expr = eat_maybe!(self, {
            Bang  => { ast::Arithmetic::LogicalNot(Box::new(self.arith_unary_misc()?)) },
            Tilde => { ast::Arithmetic::BitwiseNot(Box::new(self.arith_unary_misc()?)) },
            Plus  => {
                eat_maybe!(self, {
                    // Although we can optimize this out, we'll let the AST builder handle
                    // optimizations, in case it is interested in such redundant situations.
                    Dash => {
                        let next = self.arith_unary_misc()?;
                        ast::Arithmetic::UnaryPlus(Box::new(ast::Arithmetic::UnaryMinus(Box::new(next))))
                    },
                    Plus => { ast::Arithmetic::PreIncr(self.arith_var()?) };
                    _ => { ast::Arithmetic::UnaryPlus(Box::new(self.arith_unary_misc()?)) }
                })
            },

            Dash  => {
                eat_maybe!(self, {
                    // Although we can optimize this out, we'll let the AST builder handle
                    // optimizations, in case it is interested in such redundant situations.
                    Plus => {
                        let next = self.arith_unary_misc()?;
                        ast::Arithmetic::UnaryMinus(Box::new(ast::Arithmetic::UnaryPlus(Box::new(next))))
                    },
                    Dash => { ast::Arithmetic::PreDecr(self.arith_var()?) };
                    _ => { ast::Arithmetic::UnaryMinus(Box::new(self.arith_unary_misc()?)) }
                })
            };

            _ => { self.arith_post_incr()? }
        });
        Ok(expr)
    }

    /// Parses expressions such as `(expr)`, numeric literals, `var`, `var++`, or `var--`.
    /// Numeric literals must appear as a single `Literal` token. `Name` tokens will be
    /// treated as variables.
    #[inline]
    fn arith_post_incr(&mut self) -> ParseResult<Arithmetic> {
        self.skip_whitespace();
        eat_maybe!(self, {
            ParenOpen => {
                let expr = self.arithmetic_substitution()?;
                self.skip_whitespace();
                eat!(self, { ParenClose => {} });
                return Ok(expr);
            }
        });

        let num = if let Some(&Literal(ref s)) = self.iter.peek() {
            if s.starts_with("0x") || s.starts_with("0X") {
                let num = &s[2..];
                if num.is_empty() {
                    // Shells like bash and zsh treat `0x` as `0x0`
                    // so we will do the same.
                    Some(0)
                } else {
                    isize::from_str_radix(&s[2..], 16).ok()
                }
            } else if s.starts_with('0') {
                isize::from_str_radix(s, 8).ok()
            } else {
                s.parse::<isize>().ok()
            }
        } else {
            None
        };

        let expr = match num {
            Some(num) => {
                // Make sure we consume the Token::Literal which holds the number
                self.iter.next();
                ast::Arithmetic::Literal(num)
            }
            None => {
                let var = self.arith_var()?;

                // We must be extra careful here because post-increment has a higher precedence
                // than addition/subtraction meaning post-increment operations will be parsed
                // before addition. Thus we should be absolutely certain we should parse a
                // post-increment operator and avoid confusing it with an addition operation
                // that is yet to be parsed.
                let post_incr = {
                    self.skip_whitespace();
                    let mut peeked = self.iter.multipeek();
                    match peeked.peek_next() {
                        Some(&Plus) => peeked.peek_next() == Some(&Plus),
                        Some(&Dash) => peeked.peek_next() == Some(&Dash),
                        _ => false,
                    }
                };

                if post_incr {
                    eat!(self, {
                        Plus => { eat!(self, { Plus => { ast::Arithmetic::PostIncr(var) } }) },
                        Dash => { eat!(self, { Dash => { ast::Arithmetic::PostDecr(var) } }) },
                    })
                } else {
                    ast::Arithmetic::Var(var)
                }
            }
        };
        Ok(expr)
    }

    /// Parses a variable name in the form `name` or `$name`.
    #[inline]
    fn arith_var(&mut self) -> ParseResult<String> {
        self.skip_whitespace();
        eat_maybe!(self, { Dollar => {} });

        if let Some(&Name(_)) = self.iter.peek() {
            if let Some(Name(n)) = self.iter.next() {
                Ok(n)
            } else {
                unreachable!()
            }
        } else {
            Err(self.make_unexpected_err())
        }
    }
}

fn concat_tokens(tokens: &[Token]) -> String {
    let len = tokens.iter().fold(0, |len, t| len + t.len());
    let mut s = String::with_capacity(len);
    s.extend(tokens.iter().map(Token::as_str));
    s
}

#[cfg(test)]
mod tests {
    use crate::shell::ast::Command::*;
    use crate::shell::ast::CompoundCommandKind::*;
    use crate::shell::ast::*;
    use crate::shell::lexer::Lexer;
    use crate::shell::parse::*;

    fn make_parser(src: &str) -> Parser<Lexer<std::str::Chars<'_>>> {
        Parser::new(Lexer::new(src.chars()))
    }

    fn word(s: &str) -> AtomicTopLevelWord {
        ComplexWord::Single(Word::Simple(SimpleWord::Literal(String::from(s))))
    }

    fn cmd_args_simple(cmd: &str, args: &[&str]) -> Box<SimpleCommand> {
        let mut cmd_args = Vec::with_capacity(args.len() + 1);
        cmd_args.push(RedirectOrCmdWord::CmdWord(word(cmd)));
        cmd_args.extend(args.iter().map(|&a| RedirectOrCmdWord::CmdWord(word(a))));

        Box::new(SimpleCommand {
            redirects_or_env_vars: vec![],
            redirects_or_cmd_words: cmd_args,
        })
    }

    fn cmd(cmd: &str) -> AtomicTopLevelCommand {
        cmd_args(cmd, &[])
    }

    fn cmd_args(cmd: &str, args: &[&str]) -> AtomicTopLevelCommand {
        List(AndOrList {
            first: ListableCommand::Single(PipeableCommand::Simple(cmd_args_simple(cmd, args))),
            rest: vec![],
        })
    }

    #[test]
    fn test_function_declaration_comments_before_body() {
        use std::iter::repeat;

        let cases_brace = vec![
            "function foo() #comment1\n\n#comment2\n { echo body; }",
            "function foo () #comment1\n\n#comment2\n { echo body; }",
            "function foo (  ) #comment1\n\n#comment2\n { echo body; }",
            "function foo(  ) #comment1\n\n#comment2\n { echo body; }",
            "function foo #comment1\n\n#comment2\n   { echo body; }",
            "foo() #comment1\n\n#comment2\n          { echo body; }",
            "foo () #comment1\n\n#comment2\n         { echo body; }",
            "foo (  ) #comment1\n\n#comment2\n         { echo body; }",
            "foo(  ) #comment1\n\n#comment2\n         { echo body; }",
        ];

        let cases_subshell = vec![
            "function foo() #comment1\n\n#comment2\n (echo body)",
            "function foo #comment1\n\n#comment2\n   (echo body)",
            "foo() #comment1\n\n#comment2\n          (echo body)",
            "foo () #comment1\n\n#comment2\n         (echo body)",
        ];

        let name = String::from("foo");
        let body = vec![cmd_args("echo", &["body"])];
        let body_brace = CompoundCommand {
            kind: Brace(body.clone()),
            io: vec![],
        };
        let body_subshell = CompoundCommand {
            kind: Subshell(body),
            io: vec![],
        };

        let iter = cases_brace
            .into_iter()
            .zip(repeat(body_brace))
            .chain(cases_subshell.into_iter().zip(repeat(body_subshell)))
            .map(|(src, body)| (src, (name.clone(), body)));

        for (src, correct) in iter {
            assert_eq!(
                correct,
                make_parser(src).function_declaration_internal().unwrap()
            );
        }
    }

    #[test]
    fn test_word_preserve_trailing_whitespace() {
        let mut p = make_parser("test       ");
        p.word_preserve_trailing_whitespace().unwrap();
        assert!(p.iter.next().is_some());
    }

    #[test]
    fn test_parameter_substitution_command_can_contain_comments() {
        let param_subst =
            ast::SimpleWord::Subst(ast::ParameterSubstitution::Command(vec![cmd("foo")]));
        assert_eq!(
            Ok(param_subst),
            make_parser("$(foo\n#comment\n)").parameter_raw()
        );
    }

    #[test]
    fn test_backticked_command_can_contain_comments() {
        let cmd_subst =
            ast::SimpleWord::Subst(ast::ParameterSubstitution::Command(vec![cmd("foo")]));
        assert_eq!(
            Ok(cmd_subst),
            make_parser("`foo\n#comment\n`").backticked_raw()
        );
    }
}
