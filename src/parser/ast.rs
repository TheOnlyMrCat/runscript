//! Defines abstract representations of the shell source.
use std::sync::Arc;

/// Represents reading a parameter (or variable) value, e.g. `$foo`.
///
/// Generic over the representation of variable names.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Parameter {
    /// $@
    At,
    /// $*
    Star,
    /// $#
    Pound,
    /// $?
    Question,
    /// $-
    Dash,
    /// $$
    Dollar,
    /// $!
    Bang,
    /// $0, $1, ..., $9, ${100}
    Positional(usize),
    /// $foo
    Var(String),
}

/// A parameter substitution, e.g. `${param-word}`.
///
/// Generic over the representations of parameters, shell words and
/// commands, and arithmetic expansions.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParameterSubstitution {
    /// Returns the standard output of running a command, e.g. `$(cmd)`
    Command(Vec<AtomicTopLevelCommand>),
    /// Returns the length of the value of a parameter, e.g. `${#param}`
    Len(Parameter),
    /// Returns the resulting value of an arithmetic subsitution, e.g. `$(( x++ ))`
    Arith(Option<Arithmetic>),
    /// Use a provided value if the parameter is null or unset, e.g.
    /// `${param:-[word]}`.
    /// The boolean indicates the presence of a `:`, and that if the parameter has
    /// a null value, that situation should be treated as if the parameter is unset.
    Default(bool, Parameter, Option<Box<AtomicTopLevelWord>>),
    /// Assign a provided value to the parameter if it is null or unset,
    /// e.g. `${param:=[word]}`.
    /// The boolean indicates the presence of a `:`, and that if the parameter has
    /// a null value, that situation should be treated as if the parameter is unset.
    Assign(bool, Parameter, Option<Box<AtomicTopLevelWord>>),
    /// If the parameter is null or unset, an error should result with the provided
    /// message, e.g. `${param:?[word]}`.
    /// The boolean indicates the presence of a `:`, and that if the parameter has
    /// a null value, that situation should be treated as if the parameter is unset.
    Error(bool, Parameter, Option<Box<AtomicTopLevelWord>>),
    /// If the parameter is NOT null or unset, a provided word will be used,
    /// e.g. `${param:+[word]}`.
    /// The boolean indicates the presence of a `:`, and that if the parameter has
    /// a null value, that situation should be treated as if the parameter is unset.
    Alternative(bool, Parameter, Option<Box<AtomicTopLevelWord>>),
    /// Remove smallest suffix pattern from a parameter's value, e.g. `${param%pattern}`
    RemoveSmallestSuffix(Parameter, Option<Box<AtomicTopLevelWord>>),
    /// Remove largest suffix pattern from a parameter's value, e.g. `${param%%pattern}`
    RemoveLargestSuffix(Parameter, Option<Box<AtomicTopLevelWord>>),
    /// Remove smallest prefix pattern from a parameter's value, e.g. `${param#pattern}`
    RemoveSmallestPrefix(Parameter, Option<Box<AtomicTopLevelWord>>),
    /// Remove largest prefix pattern from a parameter's value, e.g. `${param##pattern}`
    RemoveLargestPrefix(Parameter, Option<Box<AtomicTopLevelWord>>),
}

/// Represents whitespace delimited text.
///
/// Generic over the representation of a whitespace delimited word.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ComplexWord {
    /// Several distinct words concatenated together.
    Concat(Vec<Word>),
    /// A regular word.
    Single(Word),
}

/// Represents whitespace delimited single, double, or non quoted text.
///
/// Generic over the representation of single-quoted literals, and non-quoted words.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Word {
    /// A regular word.
    Simple(SimpleWord),
    /// List of words concatenated within double quotes.
    DoubleQuoted(Vec<SimpleWord>),
    /// List of words concatenated within single quotes. Virtually
    /// identical as a literal, but makes a distinction between the two.
    SingleQuoted(String),
}

/// Represents the smallest fragment of any text.
///
/// Generic over the representation of a literals, parameters, and substitutions.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SimpleWord {
    /// A non-special literal word.
    Literal(String),
    /// A token which normally has a special meaning is treated as a literal
    /// because it was escaped, typically with a backslash, e.g. `\"`.
    Escaped(String),
    /// Access of a value inside a parameter, e.g. `$foo` or `$$`.
    Param(Parameter),
    /// A parameter substitution, e.g. `${param-word}`.
    Subst(ParameterSubstitution),
    /// Represents `*`, useful for handling pattern expansions.
    Star,
    /// Represents `?`, useful for handling pattern expansions.
    Question,
    /// Represents `[`, useful for handling pattern expansions.
    SquareOpen,
    /// Represents `]`, useful for handling pattern expansions.
    SquareClose,
    /// Represents `~`, useful for handling tilde expansions.
    Tilde,
    /// Represents `:`, useful for handling tilde expansions.
    Colon,
}

/// Represents redirecting a command's file descriptors.
///
/// Generic over the representation of a shell word.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Redirect {
    /// Open a file for reading, e.g. `[n]< file`.
    Read(Option<u16>, AtomicTopLevelWord),
    /// Open a file for writing after truncating, e.g. `[n]> file`.
    Write(Option<u16>, AtomicTopLevelWord),
    /// Open a file for reading and writing, e.g. `[n]<> file`.
    ReadWrite(Option<u16>, AtomicTopLevelWord),
    /// Open a file for writing, appending to the end, e.g. `[n]>> file`.
    Append(Option<u16>, AtomicTopLevelWord),
    /// Open a file for writing, failing if the `noclobber` shell option is set, e.g. `[n]>| file`.
    Clobber(Option<u16>, AtomicTopLevelWord),
    /// Lines contained in the source that should be provided by as input to a file descriptor.
    Heredoc(Option<u16>, AtomicTopLevelWord),
    /// Duplicate a file descriptor for reading, e.g. `[n]<& [n|-]`.
    DupRead(Option<u16>, AtomicTopLevelWord),
    /// Duplicate a file descriptor for writing, e.g. `[n]>& [n|-]`.
    DupWrite(Option<u16>, AtomicTopLevelWord),
}

/// A grouping of guard and body commands.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct GuardBodyPair {
    /// The guard commands, which if successful, should lead to the
    /// execution of the body commands.
    pub guard: Vec<AtomicTopLevelCommand>,
    /// The body commands to execute if the guard is successful.
    pub body: Vec<AtomicTopLevelCommand>,
}

/// A grouping of patterns and body commands.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PatternBodyPair {
    /// Pattern alternatives to match against.
    pub patterns: Vec<AtomicTopLevelWord>,
    /// The body commands to execute if the pattern matches.
    pub body: Vec<AtomicTopLevelCommand>,
}

/// Represents any valid shell command.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Command {
    /// A command that runs asynchronously, that is, the shell will not wait
    /// for it to exit before running the next command, e.g. `foo &`.
    Job(AndOrList),
    /// A list of and/or commands, e.g. `foo && bar || baz`.
    List(AndOrList),
}

/// A command which conditionally runs based on the exit status of the previous command.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AndOr<T> {
    /// A compound command which should run only if the previously run command succeeded.
    And(T),
    /// A compound command which should run only if the previously run command failed.
    Or(T),
}

/// A nonempty list of `AndOr` commands, e.g. `foo && bar || baz`.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AndOrList {
    /// The first command that always runs.
    pub first: ListableCommand,
    /// The remainder of the conditional commands which may or may not run.
    pub rest: Vec<AndOr<ListableCommand>>,
}

/// Commands that can be used within an and/or list.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ListableCommand {
    /// A chain of concurrent commands where the standard output of the
    /// previous becomes the standard input of the next, e.g.
    /// `[!] foo | bar | baz`.
    ///
    /// The bool indicates if a logical negation of the last command's status
    /// should be returned.
    Pipe(bool, Vec<PipeableCommand>),
    /// A single command not part of a pipeline.
    Single(PipeableCommand),
}

/// Commands that can be used within a pipeline.
///
/// Generic over the representations of function names, simple commands,
/// compound commands, and function bodies.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PipeableCommand {
    /// The simplest possible command: an executable with arguments,
    /// environment variable assignments, and redirections.
    Simple(Box<SimpleCommand>),
    /// A class of commands where redirection is applied to a command group.
    Compound(Box<CompoundCommand>),
    /// A function definition, associating a name with a group of commands,
    /// e.g. `function foo() { echo foo function; }`.
    FunctionDef(String, Arc<CompoundCommand>), //TODO: Probably doesn't need to be an Arc...
}

/// A class of commands where redirection is applied to a command group.
///
/// Generic over the representation of a type of compound command, and the
/// representation of a redirect.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CompoundCommand {
    /// The specific kind of compound command.
    pub kind: CompoundCommandKind,
    /// Any redirections to be applied to the entire compound command
    pub io: Vec<Redirect>,
}

/// A specific kind of a `CompoundCommand`.
///
/// Generic over the representation of shell words and commands.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CompoundCommandKind {
    /// A group of commands that should be executed in the current environment.
    Brace(Vec<AtomicTopLevelCommand>),
    /// A group of commands that should be executed in a subshell environment.
    Subshell(Vec<AtomicTopLevelCommand>),
    /// A command that executes its body as long as its guard exits successfully.
    While(GuardBodyPair),
    /// A command that executes its body as until as its guard exits unsuccessfully.
    Until(GuardBodyPair),
    /// A conditional command that runs the respective command branch when a
    /// certain of the first condition that exits successfully.
    If {
        /// A list of conditional branch-body pairs.
        conditionals: Vec<GuardBodyPair>,
        /// An else part to run if no other conditional was taken.
        else_branch: Option<Vec<AtomicTopLevelCommand>>,
    },
    /// A command that binds a variable to a number of provided words and runs
    /// its body once for each binding.
    For {
        /// The variable to bind to each of the specified words.
        var: String,
        /// The words to bind to the specified variable one by one.
        words: Option<Vec<AtomicTopLevelWord>>,
        /// The body to run with the variable binding.
        body: Vec<AtomicTopLevelCommand>,
    },
    /// A command that behaves much like a `match` statment in Rust, running
    /// a branch of commands if a specified word matches another literal or
    /// glob pattern.
    Case {
        /// The word on which to check for pattern matches.
        word: AtomicTopLevelWord,
        /// The arms to match against.
        arms: Vec<PatternBodyPair>,
    },
}

/// Represents a parsed redirect or a defined environment variable at the start
/// of a command.
///
/// Because the order in which redirects are defined may be significant for
/// execution, the parser will preserve the order in which they were parsed.
/// Thus we need a wrapper like this to disambiguate what was encountered in
/// the source program.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RedirectOrEnvVar {
    /// A parsed redirect before a command was encountered.
    Redirect(Redirect),
    /// A parsed environment variable, e.g. `foo=[bar]`.
    EnvVar(String, Option<AtomicTopLevelWord>),
}

/// Represents a parsed redirect or a defined command or command argument.
///
/// Because the order in which redirects are defined may be significant for
/// execution, the parser will preserve the order in which they were parsed.
/// Thus we need a wrapper like this to disambiguate what was encountered in
/// the source program.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RedirectOrCmdWord {
    /// A parsed redirect after a command was encountered.
    Redirect(Redirect),
    /// A parsed command name or argument.
    CmdWord(AtomicTopLevelWord),
}

/// The simplest possible command: an executable with arguments,
/// environment variable assignments, and redirections.
///
/// Generic over representations of variable names, shell words, and redirects.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SimpleCommand {
    /// Redirections or environment variables that occur before any command
    /// in the order they were parsed.
    pub redirects_or_env_vars: Vec<RedirectOrEnvVar>,
    /// Redirections or command name/argumetns in the order they were parsed.
    pub redirects_or_cmd_words: Vec<RedirectOrCmdWord>,
}

/// Represents an expression within an arithmetic subsitution.
///
/// Generic over the representation of a variable name.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Arithmetic {
    /// The value of a variable, e.g. `$var` or `var`.
    Var(String),
    /// A numeric literal such as `42` or `0xdeadbeef`.
    Literal(isize),
    /// `left ** right`.
    Pow(Box<Arithmetic>, Box<Arithmetic>),
    /// Returns the current value of a variable,
    /// and then increments its value immediately after, e.g. `var++`
    PostIncr(String),
    /// Returns the current value of a variable,
    /// and then decrements its value immediately after, e.g. `var--`
    PostDecr(String),
    /// Increments the value of a variable and returns the new value, e.g. `++var`.
    PreIncr(String),
    /// Decrements the value of a variable and returns the new value, e.g. `--var`.
    PreDecr(String),
    /// Ensures the sign of the underlying result is positive, e.g. `+(1-2)`.
    UnaryPlus(Box<Arithmetic>),
    /// Ensures the sign of the underlying result is negative, e.g. `-(1+2)`.
    UnaryMinus(Box<Arithmetic>),
    /// Returns one if the underlying result is zero, or zero otherwise, e.g. `!expr`.
    LogicalNot(Box<Arithmetic>),
    /// Flips all bits from the underlying result, e.g. `~expr`.
    BitwiseNot(Box<Arithmetic>),
    /// `left * right`
    Mult(Box<Arithmetic>, Box<Arithmetic>),
    /// `left / right`
    Div(Box<Arithmetic>, Box<Arithmetic>),
    /// `left % right`
    Modulo(Box<Arithmetic>, Box<Arithmetic>),
    /// `left + right`
    Add(Box<Arithmetic>, Box<Arithmetic>),
    /// `left - right`
    Sub(Box<Arithmetic>, Box<Arithmetic>),
    /// `left << right`
    ShiftLeft(Box<Arithmetic>, Box<Arithmetic>),
    /// `left >> right`
    ShiftRight(Box<Arithmetic>, Box<Arithmetic>),
    /// `left < right`
    Less(Box<Arithmetic>, Box<Arithmetic>),
    /// `left <= right`
    LessEq(Box<Arithmetic>, Box<Arithmetic>),
    /// `left > right`
    Great(Box<Arithmetic>, Box<Arithmetic>),
    /// `left >= right`
    GreatEq(Box<Arithmetic>, Box<Arithmetic>),
    /// `left == right`
    Eq(Box<Arithmetic>, Box<Arithmetic>),
    /// `left != right`
    NotEq(Box<Arithmetic>, Box<Arithmetic>),
    /// `left & right`
    BitwiseAnd(Box<Arithmetic>, Box<Arithmetic>),
    /// `left ^ right`
    BitwiseXor(Box<Arithmetic>, Box<Arithmetic>),
    /// `left | right`
    BitwiseOr(Box<Arithmetic>, Box<Arithmetic>),
    /// `left && right`
    LogicalAnd(Box<Arithmetic>, Box<Arithmetic>),
    /// `left || right`
    LogicalOr(Box<Arithmetic>, Box<Arithmetic>),
    /// `first ? second : third`
    Ternary(Box<Arithmetic>, Box<Arithmetic>, Box<Arithmetic>),
    /// Assigns the value of an underlying expression to a
    /// variable and returns the value, e.g. `x = 5`, or `x += 2`.
    Assign(String, Box<Arithmetic>),
    /// `expr[, expr[, ...]]`
    Sequence(Vec<Arithmetic>),
}

pub type AtomicTopLevelCommand = Command;
pub type AtomicTopLevelWord = ComplexWord;
