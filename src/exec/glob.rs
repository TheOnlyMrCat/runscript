// Copied from https://github.com/rust-lang-nursery/glob/blob/master/src/lib.rs

use std::cmp;
use std::error::Error;
use std::fmt;
use std::fs;
use std::io;
use std::path::{self, Component, Path, PathBuf};
use std::str::FromStr;

use CharSpecifier::{CharRange, SingleChar};
use MatchResult::{EntirePatternDoesntMatch, Match, SubPatternDoesntMatch};
use PatternToken::AnyExcept;
use PatternToken::{AnyChar, AnyRecursiveSequence, AnySequence, AnyWithin, Char};

#[derive(Debug)]
pub struct Paths {
    dir_patterns: Vec<Pattern>,
    require_dir: bool,
    options: MatchOptions,
    todo: Vec<Result<(PathBuf, usize), GlobError>>,
    scope: Option<PathBuf>,
}

pub fn glob(pattern: &str) -> Result<Paths, PatternError> {
    glob_with(pattern, MatchOptions::new())
}

pub fn glob_with(pattern: &str, options: MatchOptions) -> Result<Paths, PatternError> {
    #[cfg(windows)]
    fn check_windows_verbatim(p: &Path) -> bool {
        match p.components().next() {
            Some(Component::Prefix(ref p)) => p.kind().is_verbatim(),
            _ => false,
        }
    }
    #[cfg(not(windows))]
    fn check_windows_verbatim(_: &Path) -> bool {
        false
    }

    #[cfg(windows)]
    fn to_scope(p: &Path) -> PathBuf {
        // FIXME handle volume relative paths here
        p.to_path_buf()
    }
    #[cfg(not(windows))]
    fn to_scope(p: &Path) -> PathBuf {
        p.to_path_buf()
    }

    // make sure that the pattern is valid first, else early return with error
    if let Err(err) = Pattern::new(pattern) {
        return Err(err);
    }

    let mut components = Path::new(pattern).components().peekable();
    loop {
        match components.peek() {
            Some(&Component::Prefix(..)) | Some(&Component::RootDir) => {
                components.next();
            }
            _ => break,
        }
    }
    let rest = components.map(|s| s.as_os_str()).collect::<PathBuf>();
    let normalized_pattern = Path::new(pattern).iter().collect::<PathBuf>();
    let root_len = normalized_pattern.to_str().unwrap().len() - rest.to_str().unwrap().len();
    let root = if root_len > 0 {
        Some(Path::new(&pattern[..root_len]))
    } else {
        None
    };

    if root_len > 0 && check_windows_verbatim(root.unwrap()) {
        // FIXME: How do we want to handle verbatim paths? I'm inclined to
        // return nothing, since we can't very well find all UNC shares with a
        // 1-letter server name.
        return Ok(Paths {
            dir_patterns: Vec::new(),
            require_dir: false,
            options,
            todo: Vec::new(),
            scope: None,
        });
    }

    let scope = root.map_or_else(|| PathBuf::from("."), to_scope);

    let mut dir_patterns = Vec::new();
    let components =
        pattern[cmp::min(root_len, pattern.len())..].split_terminator(path::is_separator);

    for component in components {
        dir_patterns.push(Pattern::new(component)?);
    }

    if root_len == pattern.len() {
        dir_patterns.push(Pattern {
            original: "".to_string(),
            tokens: Vec::new(),
            is_recursive: false,
        });
    }

    let last_is_separator = pattern.chars().next_back().map(path::is_separator);
    let require_dir = last_is_separator == Some(true);
    let todo = Vec::new();

    Ok(Paths {
        dir_patterns,
        require_dir,
        options,
        todo,
        scope: Some(scope),
    })
}

#[derive(Debug)]
pub struct GlobError {
    path: PathBuf,
    error: io::Error,
}

impl GlobError {
    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn error(&self) -> &io::Error {
        &self.error
    }

    pub fn into_error(self) -> io::Error {
        self.error
    }
}

impl Error for GlobError {
    fn description(&self) -> &str {
        self.error.description()
    }

    #[allow(unknown_lints, bare_trait_objects)]
    fn cause(&self) -> Option<&Error> {
        Some(&self.error)
    }
}

impl fmt::Display for GlobError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "attempting to read `{}` resulted in an error: {}",
            self.path.display(),
            self.error
        )
    }
}

fn is_dir(p: &Path) -> bool {
    fs::metadata(p).map(|m| m.is_dir()).unwrap_or(false)
}

pub type GlobResult = Result<PathBuf, GlobError>;

impl Iterator for Paths {
    type Item = GlobResult;

    fn next(&mut self) -> Option<GlobResult> {
        // the todo buffer hasn't been initialized yet, so it's done at this
        // point rather than in glob() so that the errors are unified that is,
        // failing to fill the buffer is an iteration error construction of the
        // iterator (i.e. glob()) only fails if it fails to compile the Pattern
        if let Some(scope) = self.scope.take() {
            if !self.dir_patterns.is_empty() {
                // Shouldn't happen, but we're using -1 as a special index.
                assert!(self.dir_patterns.len() < !0 as usize);

                fill_todo(&mut self.todo, &self.dir_patterns, 0, &scope, self.options);
            }
        }

        loop {
            if self.dir_patterns.is_empty() || self.todo.is_empty() {
                return None;
            }

            let (path, mut idx) = match self.todo.pop().unwrap() {
                Ok(pair) => pair,
                Err(e) => return Some(Err(e)),
            };

            // idx -1: was already checked by fill_todo, maybe path was '.' or
            // '..' that we can't match here because of normalization.
            if idx == !0 as usize {
                if self.require_dir && !is_dir(&path) {
                    continue;
                }
                return Some(Ok(path));
            }

            if self.dir_patterns[idx].is_recursive {
                let mut next = idx;

                // collapse consecutive recursive patterns
                while (next + 1) < self.dir_patterns.len()
                    && self.dir_patterns[next + 1].is_recursive
                {
                    next += 1;
                }

                if is_dir(&path) {
                    // the path is a directory, so it's a match

                    // push this directory's contents
                    fill_todo(
                        &mut self.todo,
                        &self.dir_patterns,
                        next,
                        &path,
                        self.options,
                    );

                    if next == self.dir_patterns.len() - 1 {
                        // pattern ends in recursive pattern, so return this
                        // directory as a result
                        return Some(Ok(path));
                    } else {
                        // advanced to the next pattern for this path
                        idx = next + 1;
                    }
                } else if next == self.dir_patterns.len() - 1 {
                    // not a directory and it's the last pattern, meaning no
                    // match
                    continue;
                } else {
                    // advanced to the next pattern for this path
                    idx = next + 1;
                }
            }

            // not recursive, so match normally
            if self.dir_patterns[idx].matches_with(
                {
                    match path.file_name().and_then(|s| s.to_str()) {
                        // FIXME (#9639): How do we handle non-utf8 filenames?
                        // Ignore them for now; ideally we'd still match them
                        // against a *
                        None => continue,
                        Some(x) => x,
                    }
                },
                self.options,
            ) {
                if idx == self.dir_patterns.len() - 1 {
                    // it is not possible for a pattern to match a directory
                    // *AND* its children so we don't need to check the
                    // children

                    if !self.require_dir || is_dir(&path) {
                        return Some(Ok(path));
                    }
                } else {
                    fill_todo(
                        &mut self.todo,
                        &self.dir_patterns,
                        idx + 1,
                        &path,
                        self.options,
                    );
                }
            }
        }
    }
}

#[derive(Debug)]
#[allow(missing_copy_implementations)]
pub struct PatternError {
    pub pos: usize,

    pub msg: &'static str,
}

impl Error for PatternError {
    fn description(&self) -> &str {
        self.msg
    }
}

impl fmt::Display for PatternError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Pattern syntax error near position {}: {}",
            self.pos, self.msg
        )
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct Pattern {
    original: String,
    tokens: Vec<PatternToken>,
    is_recursive: bool,
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.original.fmt(f)
    }
}

impl FromStr for Pattern {
    type Err = PatternError;

    fn from_str(s: &str) -> Result<Self, PatternError> {
        Self::new(s)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
enum PatternToken {
    Char(char),
    AnyChar,
    AnySequence,
    AnyRecursiveSequence,
    AnyWithin(Vec<CharSpecifier>),
    AnyExcept(Vec<CharSpecifier>),
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
enum CharSpecifier {
    SingleChar(char),
    CharRange(char, char),
}

#[derive(Copy, Clone, PartialEq)]
enum MatchResult {
    Match,
    SubPatternDoesntMatch,
    EntirePatternDoesntMatch,
}

const ERROR_WILDCARDS: &str = "wildcards are either regular `*` or recursive `**`";
const ERROR_RECURSIVE_WILDCARDS: &str = "recursive wildcards must form a single path \
                                         component";
const ERROR_INVALID_RANGE: &str = "invalid range pattern";

impl Pattern {
    pub fn new(pattern: &str) -> Result<Self, PatternError> {
        let chars = pattern.chars().collect::<Vec<_>>();
        let mut tokens = Vec::new();
        let mut is_recursive = false;
        let mut i = 0;

        while i < chars.len() {
            match chars[i] {
                '?' => {
                    tokens.push(AnyChar);
                    i += 1;
                }
                '*' => {
                    let old = i;

                    while i < chars.len() && chars[i] == '*' {
                        i += 1;
                    }

                    let count = i - old;

                    if count > 2 {
                        return Err(PatternError {
                            pos: old + 2,
                            msg: ERROR_WILDCARDS,
                        });
                    } else if count == 2 {
                        // ** can only be an entire path component
                        // i.e. a/**/b is valid, but a**/b or a/**b is not
                        // invalid matches are treated literally
                        let is_valid = if i == 2 || path::is_separator(chars[i - count - 1]) {
                            // it ends in a '/'
                            if i < chars.len() && path::is_separator(chars[i]) {
                                i += 1;
                                true
                            // or the pattern ends here
                            // this enables the existing globbing mechanism
                            } else if i == chars.len() {
                                true
                            // `**` ends in non-separator
                            } else {
                                return Err(PatternError {
                                    pos: i,
                                    msg: ERROR_RECURSIVE_WILDCARDS,
                                });
                            }
                        // `**` begins with non-separator
                        } else {
                            return Err(PatternError {
                                pos: old - 1,
                                msg: ERROR_RECURSIVE_WILDCARDS,
                            });
                        };

                        if is_valid {
                            // collapse consecutive AnyRecursiveSequence to a
                            // single one

                            let tokens_len = tokens.len();

                            if !(tokens_len > 1 && tokens[tokens_len - 1] == AnyRecursiveSequence) {
                                is_recursive = true;
                                tokens.push(AnyRecursiveSequence);
                            }
                        }
                    } else {
                        tokens.push(AnySequence);
                    }
                }
                '[' => {
                    if i + 4 <= chars.len() && chars[i + 1] == '!' {
                        match chars[i + 3..].iter().position(|x| *x == ']') {
                            None => (),
                            Some(j) => {
                                let chars = &chars[i + 2..i + 3 + j];
                                let cs = parse_char_specifiers(chars);
                                tokens.push(AnyExcept(cs));
                                i += j + 4;
                                continue;
                            }
                        }
                    } else if i + 3 <= chars.len() && chars[i + 1] != '!' {
                        match chars[i + 2..].iter().position(|x| *x == ']') {
                            None => (),
                            Some(j) => {
                                let cs = parse_char_specifiers(&chars[i + 1..i + 2 + j]);
                                tokens.push(AnyWithin(cs));
                                i += j + 3;
                                continue;
                            }
                        }
                    }

                    // if we get here then this is not a valid range pattern
                    return Err(PatternError {
                        pos: i,
                        msg: ERROR_INVALID_RANGE,
                    });
                }
                c => {
                    tokens.push(Char(c));
                    i += 1;
                }
            }
        }

        Ok(Self {
            tokens,
            original: pattern.to_string(),
            is_recursive,
        })
    }

    pub fn escape(s: &str) -> String {
        let mut escaped = String::new();
        for c in s.chars() {
            match c {
                // note that ! does not need escaping because it is only special
                // inside brackets
                '?' | '*' | '[' | ']' => {
                    escaped.push('[');
                    escaped.push(c);
                    escaped.push(']');
                }
                c => {
                    escaped.push(c);
                }
            }
        }
        escaped
    }

    pub fn matches(&self, str: &str) -> bool {
        self.matches_with(str, MatchOptions::new())
    }

    pub fn matches_path(&self, path: &Path) -> bool {
        // FIXME (#9639): This needs to handle non-utf8 paths
        path.to_str().map_or(false, |s| self.matches(s))
    }

    pub fn matches_with(&self, str: &str, options: MatchOptions) -> bool {
        self.matches_from(true, str.chars(), 0, options) == Match
    }

    pub fn matches_path_with(&self, path: &Path, options: MatchOptions) -> bool {
        // FIXME (#9639): This needs to handle non-utf8 paths
        path.to_str()
            .map_or(false, |s| self.matches_with(s, options))
    }

    pub fn as_str(&self) -> &str {
        &self.original
    }

    fn matches_from(
        &self,
        mut follows_separator: bool,
        mut file: std::str::Chars,
        i: usize,
        options: MatchOptions,
    ) -> MatchResult {
        for (ti, token) in self.tokens[i..].iter().enumerate() {
            match *token {
                AnySequence | AnyRecursiveSequence => {
                    // ** must be at the start.
                    debug_assert!(match *token {
                        AnyRecursiveSequence => follows_separator,
                        _ => true,
                    });

                    // Empty match
                    match self.matches_from(follows_separator, file.clone(), i + ti + 1, options) {
                        SubPatternDoesntMatch => (), // keep trying
                        m => return m,
                    };

                    while let Some(c) = file.next() {
                        if follows_separator && options.require_literal_leading_dot && c == '.' {
                            return SubPatternDoesntMatch;
                        }
                        follows_separator = path::is_separator(c);
                        match *token {
                            AnyRecursiveSequence if !follows_separator => continue,
                            AnySequence
                                if options.require_literal_separator && follows_separator =>
                            {
                                return SubPatternDoesntMatch
                            }
                            _ => (),
                        }
                        match self.matches_from(
                            follows_separator,
                            file.clone(),
                            i + ti + 1,
                            options,
                        ) {
                            SubPatternDoesntMatch => (), // keep trying
                            m => return m,
                        }
                    }
                }
                _ => {
                    let c = match file.next() {
                        Some(c) => c,
                        None => return EntirePatternDoesntMatch,
                    };

                    let is_sep = path::is_separator(c);

                    if !match *token {
                        AnyChar | AnyWithin(..) | AnyExcept(..)
                            if (options.require_literal_separator && is_sep)
                                || (follows_separator
                                    && options.require_literal_leading_dot
                                    && c == '.') =>
                        {
                            false
                        }
                        AnyChar => true,
                        AnyWithin(ref specifiers) => in_char_specifiers(&specifiers, c, options),
                        AnyExcept(ref specifiers) => !in_char_specifiers(&specifiers, c, options),
                        Char(c2) => chars_eq(c, c2, options.case_sensitive),
                        AnySequence | AnyRecursiveSequence => unreachable!(),
                    } {
                        return SubPatternDoesntMatch;
                    }
                    follows_separator = is_sep;
                }
            }
        }

        // Iter is fused.
        if file.next().is_none() {
            Match
        } else {
            SubPatternDoesntMatch
        }
    }
}

// Fills `todo` with paths under `path` to be matched by `patterns[idx]`,
// special-casing patterns to match `.` and `..`, and avoiding `readdir()`
// calls when there are no metacharacters in the pattern.
fn fill_todo(
    todo: &mut Vec<Result<(PathBuf, usize), GlobError>>,
    patterns: &[Pattern],
    idx: usize,
    path: &Path,
    options: MatchOptions,
) {
    // convert a pattern that's just many Char(_) to a string
    fn pattern_as_str(pattern: &Pattern) -> Option<String> {
        let mut s = String::new();
        for token in &pattern.tokens {
            match *token {
                Char(c) => s.push(c),
                _ => return None,
            }
        }

        Some(s)
    }

    let add = |todo: &mut Vec<_>, next_path: PathBuf| {
        if idx + 1 == patterns.len() {
            // We know it's good, so don't make the iterator match this path
            // against the pattern again. In particular, it can't match
            // . or .. globs since these never show up as path components.
            todo.push(Ok((next_path, !0 as usize)));
        } else {
            fill_todo(todo, patterns, idx + 1, &next_path, options);
        }
    };

    let pattern = &patterns[idx];
    let is_dir = is_dir(path);
    let curdir = path == Path::new(".");
    match pattern_as_str(pattern) {
        Some(s) => {
            // This pattern component doesn't have any metacharacters, so we
            // don't need to read the current directory to know where to
            // continue. So instead of passing control back to the iterator,
            // we can just check for that one entry and potentially recurse
            // right away.
            let special = "." == s || ".." == s;
            let next_path = if curdir {
                PathBuf::from(s)
            } else {
                path.join(&s)
            };
            if (special && is_dir) || (!special && fs::metadata(&next_path).is_ok()) {
                add(todo, next_path);
            }
        }
        None if is_dir => {
            let dirs = fs::read_dir(path).and_then(|d| {
                d.map(|e| {
                    e.map(|e| {
                        if curdir {
                            PathBuf::from(e.path().file_name().unwrap())
                        } else {
                            e.path()
                        }
                    })
                })
                .collect::<Result<Vec<_>, _>>()
            });
            match dirs {
                Ok(mut children) => {
                    children.sort_by(|p1, p2| p2.file_name().cmp(&p1.file_name()));
                    todo.extend(children.into_iter().map(|x| Ok((x, idx))));

                    // Matching the special directory entries . and .. that
                    // refer to the current and parent directory respectively
                    // requires that the pattern has a leading dot, even if the
                    // `MatchOptions` field `require_literal_leading_dot` is not
                    // set.
                    if !pattern.tokens.is_empty() && pattern.tokens[0] == Char('.') {
                        for &special in &[".", ".."] {
                            if pattern.matches_with(special, options) {
                                add(todo, path.join(special));
                            }
                        }
                    }
                }
                Err(e) => {
                    todo.push(Err(GlobError {
                        path: path.to_path_buf(),
                        error: e,
                    }));
                }
            }
        }
        None => {
            // not a directory, nothing more to find
        }
    }
}

fn parse_char_specifiers(s: &[char]) -> Vec<CharSpecifier> {
    let mut cs = Vec::new();
    let mut i = 0;
    while i < s.len() {
        if i + 3 <= s.len() && s[i + 1] == '-' {
            cs.push(CharRange(s[i], s[i + 2]));
            i += 3;
        } else {
            cs.push(SingleChar(s[i]));
            i += 1;
        }
    }
    cs
}

fn in_char_specifiers(specifiers: &[CharSpecifier], c: char, options: MatchOptions) -> bool {
    for &specifier in specifiers.iter() {
        match specifier {
            SingleChar(sc) => {
                if chars_eq(c, sc, options.case_sensitive) {
                    return true;
                }
            }
            CharRange(start, end) => {
                // FIXME: work with non-ascii chars properly (issue #1347)
                if !options.case_sensitive && c.is_ascii() && start.is_ascii() && end.is_ascii() {
                    let start = start.to_ascii_lowercase();
                    let end = end.to_ascii_lowercase();

                    let start_up = start.to_uppercase().next().unwrap();
                    let end_up = end.to_uppercase().next().unwrap();

                    // only allow case insensitive matching when
                    // both start and end are within a-z or A-Z
                    if start != start_up && end != end_up {
                        let c = c.to_ascii_lowercase();
                        if c >= start && c <= end {
                            return true;
                        }
                    }
                }

                if c >= start && c <= end {
                    return true;
                }
            }
        }
    }

    false
}

fn chars_eq(a: char, b: char, case_sensitive: bool) -> bool {
    if cfg!(windows) && path::is_separator(a) && path::is_separator(b) {
        true
    } else if !case_sensitive && a.is_ascii() && b.is_ascii() {
        // FIXME: work with non-ascii chars properly (issue #9084)
        a.to_ascii_lowercase() == b.to_ascii_lowercase()
    } else {
        a == b
    }
}

#[allow(missing_copy_implementations)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct MatchOptions {
    pub case_sensitive: bool,

    pub require_literal_separator: bool,

    pub require_literal_leading_dot: bool,
}

impl MatchOptions {
    pub fn new() -> Self {
        Self {
            case_sensitive: true,
            require_literal_separator: false,
            require_literal_leading_dot: false,
        }
    }
}
