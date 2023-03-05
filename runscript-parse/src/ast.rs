//! Defines abstract representations of the shell source.

/// Represents reading a parameter (or variable) value, e.g. `$foo`.
///
/// Generic over the representation of variable names.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Parameter {
    /// $@
    At,
    /// $*
    Star,
    /// $#
    Hash,
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
    Var(Vec<u8>),
}

/// A parameter substitution, e.g. `${param-word}`.
///
/// Generic over the representations of parameters, shell words and
/// commands, and arithmetic expansions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParameterSubstitution {
    /// Returns the standard output of running a command, e.g. `$(cmd)`
    Command(Vec<CommandChain>),
    /// The value of a parameter
    Parameter(Parameter),
    /// Returns the length of the value of a parameter, e.g. `${#param}`
    Len(Parameter),
    /// Returns the resulting value of an arithmetic subsitution, e.g. `$(( x++ ))`
    Arith(Option<Arithmetic>),
    /// Use a provided value if the parameter is null or unset, e.g.
    /// `${param:-[word]}`.
    /// The boolean indicates the presence of a `:`, and that if the parameter has
    /// a null value, that situation should be treated as if the parameter is unset.
    Default(bool, Parameter, Option<Box<ComplexWord>>),
    /// Assign a provided value to the parameter if it is null or unset,
    /// e.g. `${param:=[word]}`.
    /// The boolean indicates the presence of a `:`, and that if the parameter has
    /// a null value, that situation should be treated as if the parameter is unset.
    Assign(bool, Parameter, Option<Box<ComplexWord>>),
    /// If the parameter is null or unset, an error should result with the provided
    /// message, e.g. `${param:?[word]}`.
    /// The boolean indicates the presence of a `:`, and that if the parameter has
    /// a null value, that situation should be treated as if the parameter is unset.
    Error(bool, Parameter, Option<Box<ComplexWord>>),
    /// If the parameter is NOT null or unset, a provided word will be used,
    /// e.g. `${param:+[word]}`.
    /// The boolean indicates the presence of a `:`, and that if the parameter has
    /// a null value, that situation should be treated as if the parameter is unset.
    Alternative(bool, Parameter, Option<Box<ComplexWord>>),
    /// Remove smallest suffix pattern from a parameter's value, e.g. `${param%pattern}`
    RemoveSmallestSuffix(Parameter, Option<Box<ComplexWord>>),
    /// Remove largest suffix pattern from a parameter's value, e.g. `${param%%pattern}`
    RemoveLargestSuffix(Parameter, Option<Box<ComplexWord>>),
    /// Remove smallest prefix pattern from a parameter's value, e.g. `${param#pattern}`
    RemoveSmallestPrefix(Parameter, Option<Box<ComplexWord>>),
    /// Remove largest prefix pattern from a parameter's value, e.g. `${param##pattern}`
    RemoveLargestPrefix(Parameter, Option<Box<ComplexWord>>),
}

/// Represents whitespace delimited text.
pub type ComplexWord = Vec<Word>;

/// Represents whitespace delimited single, double, or non quoted text.
///
/// Generic over the representation of single-quoted literals, and non-quoted words.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Word {
    /// A regular word.
    Simple(SimpleWord),
    /// List of words concatenated within double quotes.
    DoubleQuoted(Vec<SimpleWord>),
    /// List of words concatenated within single quotes. Virtually
    /// identical as a literal, but makes a distinction between the two.
    SingleQuoted(Vec<u8>),
}

/// Represents the smallest fragment of any text.
///
/// Generic over the representation of a literals, parameters, and substitutions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SimpleWord {
    /// A non-special literal word
    Literal(Vec<u8>),
    /// An escaped token
    Escaped(u8),
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
    /// Represents `{`, useful for handling pattern expansions.
    BraceOpen,
    /// Represents `}`, useful for handling pattern expansions.
    BraceClose,
    /// Represents `~`, useful for handling tilde expansions.
    Tilde,
    /// Represents `:`, useful for handling tilde expansions.
    Colon,
}

/// Represents redirecting a command's file descriptors.
///
/// Generic over the representation of a shell word.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Redirect {
    /// Open a file for reading, e.g. `[n]< file`.
    Read(Option<u16>, ComplexWord),
    /// Open a file for writing after truncating, e.g. `[n]> file`.
    Write(Option<u16>, ComplexWord),
    /// Open a file for reading and writing, e.g. `[n]<> file`.
    ReadWrite(Option<u16>, ComplexWord),
    /// Open a file for writing, appending to the end, e.g. `[n]>> file`.
    Append(Option<u16>, ComplexWord),
    /// Open a file for writing, failing if the `noclobber` shell option is set, e.g. `[n]>| file`.
    Clobber(Option<u16>, ComplexWord),
    /// Lines contained in the source that should be provided by as input to a file descriptor.
    Heredoc(Option<u16>, ComplexWord),
    /// Duplicate a file descriptor for reading, e.g. `[n]<& [n|-]`.
    DupRead(Option<u16>, ComplexWord),
    /// Duplicate a file descriptor for writing, e.g. `[n]>& [n|-]`.
    DupWrite(Option<u16>, ComplexWord),
}

/// A grouping of guard and body commands.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GuardBodyPair {
    /// The guard commands, which if successful, should lead to the
    /// execution of the body commands.
    pub guard: Vec<CommandChain>,
    /// The body commands to execute if the guard is successful.
    pub body: Vec<CommandChain>,
}

/// A grouping of patterns and body commands.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PatternBodyPair {
    /// Pattern alternatives to match against.
    pub patterns: Vec<ComplexWord>,
    /// The body commands to execute if the pattern matches.
    pub body: Vec<CommandChain>,
}

/// A command which conditionally runs based on the exit status of the previous command.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AndOr<T> {
    /// A compound command which should run only if the previously run command succeeded.
    And(T),
    /// A compound command which should run only if the previously run command failed.
    Or(T),
}

/// A nonempty list of `AndOr` commands, e.g. `foo && bar || baz`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CommandChain {
    /// The first command that always runs.
    pub first: Pipeline,
    /// The remainder of the conditional commands which may or may not run.
    pub rest: Vec<AndOr<Pipeline>>,
    /// Whether the command should run asynchronously
    pub job: bool,
}

/// Commands that can be used within an and/or list.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pipeline {
    pub command_groups: Vec<CommandGroup>,
    /// Whether to logically negate the status of the last command.
    pub negate: bool,
}

/// Commands that can be used within a pipeline.
///
/// Generic over the representations of function names, simple commands,
/// compound commands, and function bodies.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CommandGroup {
    /// The simplest possible command: an executable with arguments,
    /// environment variable assignments, and redirections.
    Simple(Box<SimpleCommand>),
    /// A group of commands that should be executed in the current environment.
    Brace {
        commands: Vec<CommandChain>,
        redirects: Vec<Redirect>,
    },
    /// A group of commands that should be executed in a subshell environment.
    Subshell {
        commands: Vec<CommandChain>,
        redirects: Vec<Redirect>,
    },
    /// A command that executes its body as long as its guard exits successfully.
    While {
        body: GuardBodyPair,
        io: Vec<Redirect>,
    },
    /// A command that executes its body as until as its guard exits unsuccessfully.
    Until {
        body: GuardBodyPair,
        io: Vec<Redirect>,
    },
    /// A conditional command that runs the respective command branch when a
    /// certain of the first condition that exits successfully.
    If {
        /// A list of conditional branch-body pairs.
        conditionals: Vec<GuardBodyPair>,
        /// An else part to run if no other conditional was taken.
        else_branch: Option<Vec<CommandChain>>,
        io: Vec<Redirect>,
    },
    /// A command that binds a variable to a number of provided words and runs
    /// its body once for each binding.
    For {
        /// The variable to bind to each of the specified words.
        var: String,
        /// The words to bind to the specified variable one by one.
        words: Option<Vec<ComplexWord>>,
        /// The body to run with the variable binding.
        body: Vec<CommandChain>,
        io: Vec<Redirect>,
    },
    /// A command that behaves much like a `match` statment in Rust, running
    /// a branch of commands if a specified word matches another literal or
    /// glob pattern.
    Case {
        /// The word on which to check for pattern matches.
        word: ComplexWord,
        /// The arms to match against.
        arms: Vec<PatternBodyPair>,
        io: Vec<Redirect>,
    },
    /// A function definition, associating a name with a group of commands,
    /// e.g. `function foo() { echo foo function; }`.
    FunctionDef(Vec<ComplexWord>, Box<CommandGroup>),
}

/// The simplest possible command: an executable with arguments,
/// environment variable assignments, and redirections.
///
/// Generic over representations of variable names, shell words, and redirects.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SimpleCommand {
    pub redirects: Vec<Redirect>,
    pub assignments: Vec<(Vec<u8>, Option<ComplexWord>)>,
    pub words: Vec<ComplexWord>,
}

/// Represents an expression within an arithmetic subsitution.
///
/// Generic over the representation of a variable name.
#[derive(Clone, Debug, PartialEq, Eq)]
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
