use chumsky::prelude::*;

mod ast;
#[doc(inline)]
pub use ast::*;

enum RedirOrAssignment {
    Redirect(Redirect),
    Assignment(Vec<u8>, Option<ComplexWord>),
}

enum RedirOrWord {
    Redirect(Redirect),
    Word(ComplexWord),
}

#[derive(Clone, Copy)]
enum RedirectType {
    Read,
    Write,
    ReadWrite,
    Append,
    Clobber,
    DupRead,
    DupWrite,
}

#[derive(Clone, Copy)]
enum SubstitutionType {
    Default(bool),
    Assign(bool),
    Error(bool),
    Alternative(bool),
    SmallSuffix,
    LargeSuffix,
    SmallPrefix,
    LargePrefix,
}

pub fn whitespace<E: chumsky::Error<u8>>() -> impl Parser<u8, Vec<u8>, Error = E> + Clone {
    choice((
        just(b'\t').repeated().at_least(1),
        just(b' ').repeated().at_least(1),
        just(b"\\\n").map(|arr| arr.to_vec()),
    ))
}

pub fn whitespace_intercommand<E: chumsky::Error<u8>>() -> impl Parser<u8, (), Error = E> + Clone {
    choice((
        just(b'\t').ignored(),
        just(b' ').ignored(),
        just(b'\n').ignored(),
        just(b'#').then(take_until(just(b'\n'))).ignored(),
    ))
}

pub fn pad<O, E: chumsky::Error<u8>>(
    parser: impl Parser<u8, O, Error = E> + Clone,
) -> impl Parser<u8, O, Error = E> + Clone {
    whitespace().repeated().ignore_then(parser)
}

pub fn pad_intercommand<O, E: chumsky::Error<u8>>(
    parser: impl Parser<u8, O, Error = E> + Clone,
) -> impl Parser<u8, O, Error = E> + Clone {
    whitespace_intercommand().repeated().ignore_then(parser)
}

pub fn command_chain() -> impl Parser<u8, CommandChain, Error = Simple<u8>> + Clone {
    recursive(|command_chain| {
        let mut parameter_complex_word = Recursive::declare();

        // This differs slightly from the spec, in that } are disallowed
        let literal = none_of(b"*?[#~%|&;<>()}$`\\\"' \t\n")
            .repeated()
            .at_least(1)
            .separated_by(just(b"\\\n"))
            .at_least(1)
            .flatten();
        let escape = just(b'\\').ignore_then(none_of(b"\n"));
        let quoted_literal = none_of(b"$`\\\"")
            .or(just(b'\\').ignore_then(one_of(b"$`\"\\")))
            .repeated()
            .at_least(1)
            .separated_by(just(b"\\\n"))
            .at_least(1)
            .flatten();
        let quoted_escape = just(b'\\').ignore_then(one_of(b"$`\"\\"));
        let name = text::ident::<u8, _>()
            .separated_by(just(b"\\\n"))
            .at_least(1)
            .flatten();

        let unbraced_parameter = choice((
            just(b'@').to(Parameter::At),
            just(b'*').to(Parameter::Star),
            just(b'#').to(Parameter::Hash),
            just(b'?').to(Parameter::Question),
            just(b'-').to(Parameter::Dash),
            just(b'$').to(Parameter::Dollar),
            just(b'!').to(Parameter::Bang),
            one_of(b"0123456789").map(|b| Parameter::Positional((b - b'0') as usize)),
            text::ident().map(Parameter::Var),
        ));
        let braced_parameter = one_of(b"0123456789")
            .repeated()
            .at_least(1)
            .map(|bytes| {
                Parameter::Positional(String::from_utf8(bytes).unwrap().parse::<usize>().unwrap())
            })
            .or(unbraced_parameter.clone());
        let subst_type = just(b':')
            .ignore_then(choice((
                just(b'-').to(SubstitutionType::Default(true)),
                just(b'=').to(SubstitutionType::Assign(true)),
                just(b'?').to(SubstitutionType::Error(true)),
                just(b'+').to(SubstitutionType::Alternative(true)),
            )))
            .or(choice((
                just(b'-').to(SubstitutionType::Default(false)),
                just(b'=').to(SubstitutionType::Assign(false)),
                just(b'?').to(SubstitutionType::Error(false)),
                just(b'+').to(SubstitutionType::Alternative(false)),
                just(b"##").to(SubstitutionType::LargePrefix),
                just(b'#').to(SubstitutionType::SmallPrefix),
                just(b"%%").to(SubstitutionType::LargeSuffix),
                just(b'%').to(SubstitutionType::SmallSuffix),
            )));
        let subst = just(b'#')
            .ignore_then(braced_parameter.clone())
            .map(ParameterSubstitution::Len)
            .or(braced_parameter
                .clone()
                .then(
                    subst_type
                        .then(pad(parameter_complex_word.clone().map(Box::new).or_not()))
                        .or_not(),
                )
                .map(|(param, subst)| match subst {
                    Some((SubstitutionType::Default(null_is_unset), word)) => {
                        ParameterSubstitution::Default(null_is_unset, param, word)
                    }
                    Some((SubstitutionType::Assign(null_is_unset), word)) => {
                        ParameterSubstitution::Assign(null_is_unset, param, word)
                    }
                    Some((SubstitutionType::Error(null_is_unset), word)) => {
                        ParameterSubstitution::Error(null_is_unset, param, word)
                    }
                    Some((SubstitutionType::Alternative(null_is_unset), word)) => {
                        ParameterSubstitution::Alternative(null_is_unset, param, word)
                    }
                    Some((SubstitutionType::SmallSuffix, word)) => {
                        ParameterSubstitution::RemoveSmallestSuffix(param, word)
                    }
                    Some((SubstitutionType::LargeSuffix, word)) => {
                        ParameterSubstitution::RemoveLargestSuffix(param, word)
                    }
                    Some((SubstitutionType::SmallPrefix, word)) => {
                        ParameterSubstitution::RemoveSmallestPrefix(param, word)
                    }
                    Some((SubstitutionType::LargePrefix, word)) => {
                        ParameterSubstitution::RemoveLargestPrefix(param, word)
                    }
                    None => ParameterSubstitution::Parameter(param),
                }));
        let backticked_command = command_chain
            .clone()
            .repeated()
            .at_least(1)
            .delimited_by(just(b'`'), just(b'`'))
            .map(ParameterSubstitution::Command);
        let parameter_or_subst = just(b'$')
            .ignore_then(
                unbraced_parameter
                    .map(ParameterSubstitution::Parameter)
                    .or(command_chain
                        .clone()
                        .repeated()
                        .at_least(1)
                        .delimited_by(just(b'('), just(b')'))
                        .map(ParameterSubstitution::Command))
                    .or(subst.delimited_by(just(b'{'), just(b'}'))),
            )
            .or(backticked_command);

        let simple_word = choice((
            literal.map(SimpleWord::Literal),
            escape.map(SimpleWord::Escaped),
            parameter_or_subst.clone().map(SimpleWord::Subst),
        ));
        let quoted_word = choice((
            quoted_literal.map(SimpleWord::Literal),
            quoted_escape.map(SimpleWord::Escaped),
            parameter_or_subst.map(SimpleWord::Subst),
        ));
        let word = choice((
            none_of(b"'")
                .repeated()
                .delimited_by(just(b'\''), just(b'\''))
                .map(Word::SingleQuoted),
            quoted_word
                .clone()
                .repeated()
                .delimited_by(just(b'"'), just(b'"'))
                .map(Word::DoubleQuoted),
            simple_word.map(Word::Simple),
        ));
        parameter_complex_word.define(
            word.clone()
                .or(whitespace().map(|bytes| Word::Simple(SimpleWord::Literal(bytes))))
                .repeated()
                .at_least(1),
        );
        let complex_word = word.repeated().at_least(1);

        let file_redirect = one_of(b"0123456789")
            .repeated()
            .then(choice((
                just(b"<>").to(RedirectType::ReadWrite),
                just(b">>").to(RedirectType::Append),
                just(b">|").to(RedirectType::Clobber),
                just(b"&<").to(RedirectType::DupRead),
                just(b"&>").to(RedirectType::DupWrite),
                just(b"<").to(RedirectType::Read),
                just(b">").to(RedirectType::Write),
            )))
            .then(pad(complex_word.clone()))
            .map(|((num, ty), word)| {
                let num = String::from_utf8(num).unwrap().parse().ok();
                match ty {
                    RedirectType::Read => Redirect::Read(num, word),
                    RedirectType::Write => Redirect::Write(num, word),
                    RedirectType::ReadWrite => Redirect::ReadWrite(num, word),
                    RedirectType::Append => Redirect::Append(num, word),
                    RedirectType::Clobber => Redirect::Clobber(num, word),
                    RedirectType::DupRead => Redirect::DupRead(num, word),
                    RedirectType::DupWrite => Redirect::DupWrite(num, word),
                }
            });
        let heredoc = one_of(b"0123456789")
            .repeated()
            .then(just(b"<<"))
            .then(just(b'-').or_not())
            .then(todo::<_, (), _>())
            .map(|_| todo!());
        let redirect = file_redirect.or(heredoc);

        let assignments = pad(redirect.clone().map(RedirOrAssignment::Redirect).or(name
            .then_ignore(just(b'='))
            .then(complex_word.clone().or_not())
            .map(|(name, word)| RedirOrAssignment::Assignment(name, word))))
        .repeated();

        let first_word = pad(complex_word.clone()).try_map(|word, span| {
            if word.len() == 1 {
                match word[0] {
                    Word::Simple(SimpleWord::Literal(ref bytes)) => match bytes.as_slice() {
                        b"if" | b"elif" | b"else" | b"fi" | b"then" | b"while" | b"for" | b"do"
                        | b"done" => {
                            return Err(Simple::custom(
                                span,
                                format!(
                                    "{} not valid in this position",
                                    std::str::from_utf8(bytes).unwrap()
                                ),
                            ))
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }
            Ok(word)
        });

        let words = pad(redirect
            .clone()
            .map(RedirOrWord::Redirect)
            .or(complex_word.map(RedirOrWord::Word)))
        .repeated();

        let simple_command =
            assignments
                .then(first_word)
                .then(words)
                .map(|((assignments, first_word), words)| {
                    let mut redirects = vec![];
                    let assignments = assignments
                        .into_iter()
                        .filter_map(|either| match either {
                            RedirOrAssignment::Redirect(redir) => {
                                redirects.push(redir);
                                None
                            }
                            RedirOrAssignment::Assignment(name, word) => Some((name, word)),
                        })
                        .collect();
                    let words = std::iter::once(first_word)
                        .chain(words.into_iter().filter_map(|either| match either {
                            RedirOrWord::Redirect(redir) => {
                                redirects.push(redir);
                                None
                            }
                            RedirOrWord::Word(word) => Some(word),
                        }))
                        .collect();
                    SimpleCommand {
                        redirects,
                        assignments,
                        words,
                    }
                });
        let command_group = choice((
            pad_intercommand(command_chain.clone())
                .repeated()
                .at_least(1)
                .delimited_by(just(b'('), pad_intercommand(just(b')')))
                .then(pad(redirect.clone()).repeated())
                .map(|(commands, redirects)| CommandGroup::Subshell {
                    commands,
                    redirects,
                }),
            pad_intercommand(command_chain.clone())
                .repeated()
                .at_least(1)
                .delimited_by(just(b'{'), pad_intercommand(just(b'}')))
                .then(pad(redirect.clone()).repeated())
                .map(|(commands, redirects)| CommandGroup::Brace {
                    commands,
                    redirects,
                }),
            just(b"if")
                .ignore_then(
                    pad_intercommand(command_chain.clone())
                        .repeated()
                        .at_least(1),
                )
                .then_ignore(pad_intercommand(just(b"then")))
                .then(
                    pad_intercommand(command_chain.clone())
                        .repeated()
                        .at_least(1),
                )
                .then(
                    pad_intercommand(just(b"elif"))
                        .ignore_then(
                            pad_intercommand(command_chain.clone())
                                .repeated()
                                .at_least(1),
                        )
                        .then_ignore(pad_intercommand(just(b"then")))
                        .then(
                            pad_intercommand(command_chain.clone())
                                .repeated()
                                .at_least(1),
                        )
                        .repeated(),
                )
                .then(
                    pad_intercommand(just(b"else"))
                        .ignore_then(
                            pad_intercommand(command_chain.clone())
                                .repeated()
                                .at_least(1),
                        )
                        .or_not(),
                )
                .then_ignore(pad_intercommand(just(b"fi")))
                .then(pad(redirect.clone()).repeated())
                .map(|(((if_, elifs), else_branch), io)| CommandGroup::If {
                    conditionals: std::iter::once(if_)
                        .chain(elifs)
                        .map(|(guard, body)| GuardBodyPair { guard, body })
                        .collect(),
                    else_branch,
                    io,
                }),
            simple_command.map(|simple_command| CommandGroup::Simple(Box::new(simple_command))),
        ));
        let pipeline = pad(just(b'!').or_not())
            .then(pad(command_group).separated_by(pad(just(b'|'))).at_least(1))
            .map(|(bang, command_groups)| Pipeline {
                command_groups,
                negate: bang.is_some(),
            });
        pipeline
            .clone()
            .then(
                pad(just(b"&&").or(just(b"||")))
                    .then(pipeline)
                    .map(|(separator, pipeline)| match separator {
                        b"&&" => AndOr::And(pipeline),
                        b"||" => AndOr::Or(pipeline),
                        _ => unreachable!(),
                    })
                    .repeated(),
            )
            .then(pad(just(b'&').or(just(b';')).or_not()))
            .map(|((first, rest), ampersand)| CommandChain {
                first,
                rest,
                job: matches!(ampersand, Some(b'&')),
            })
    })
}

#[cfg(test)]
mod tests {
    use chumsky::primitive::end;
    use chumsky::Parser;
    use pretty_assertions::assert_eq;

    use crate::ast::*;

    #[test]
    fn parser_api() {
        let command = super::command_chain();

        assert!(command.parse(b"").is_err());
    }

    #[test]
    fn command_words() {
        let command = super::command_chain();

        assert_eq!(
            (&command)
                .then_ignore(end())
                .parse(b"echo 'simple command' \"with single- and double-quoted \"args"),
            Ok(CommandChain {
                first: Pipeline {
                    command_groups: vec![CommandGroup::Simple(Box::new(SimpleCommand {
                        redirects: vec![],
                        assignments: vec![],
                        words: vec![
                            vec![Word::Simple(SimpleWord::Literal(b"echo".to_vec()))],
                            vec![Word::SingleQuoted(b"simple command".to_vec())],
                            vec![
                                Word::DoubleQuoted(vec![SimpleWord::Literal(
                                    b"with single- and double-quoted ".to_vec()
                                )]),
                                Word::Simple(SimpleWord::Literal(b"args".to_vec()))
                            ],
                        ]
                    }))],
                    negate: false,
                },
                rest: vec![],
                job: false,
            })
        );

        assert_eq!(
            command
                .then_ignore(end())
                .parse(b"echo $1$2 ${100} $@ \"$-$? $foobar$(command)\""),
            Ok(CommandChain {
                first: Pipeline {
                    command_groups: vec![CommandGroup::Simple(Box::new(SimpleCommand {
                        redirects: vec![],
                        assignments: vec![],
                        words: vec![
                            vec![Word::Simple(SimpleWord::Literal(b"echo".to_vec()))],
                            vec![
                                Word::Simple(SimpleWord::Subst(ParameterSubstitution::Parameter(
                                    Parameter::Positional(1)
                                ))),
                                Word::Simple(SimpleWord::Subst(ParameterSubstitution::Parameter(
                                    Parameter::Positional(2)
                                )))
                            ],
                            vec![Word::Simple(SimpleWord::Subst(
                                ParameterSubstitution::Parameter(Parameter::Positional(100))
                            ))],
                            vec![Word::Simple(SimpleWord::Subst(
                                ParameterSubstitution::Parameter(Parameter::At)
                            ))],
                            vec![Word::DoubleQuoted(vec![
                                SimpleWord::Subst(ParameterSubstitution::Parameter(
                                    Parameter::Dash
                                )),
                                SimpleWord::Subst(ParameterSubstitution::Parameter(
                                    Parameter::Question
                                )),
                                SimpleWord::Literal(b" ".to_vec()),
                                SimpleWord::Subst(ParameterSubstitution::Parameter(
                                    Parameter::Var(b"foobar".to_vec())
                                )),
                                SimpleWord::Subst(ParameterSubstitution::Command(vec![
                                    CommandChain {
                                        first: Pipeline {
                                            command_groups: vec![CommandGroup::Simple(Box::new(
                                                SimpleCommand {
                                                    redirects: vec![],
                                                    assignments: vec![],
                                                    words: vec![vec![Word::Simple(
                                                        SimpleWord::Literal(b"command".to_vec())
                                                    )]]
                                                }
                                            ))],
                                            negate: false
                                        },
                                        rest: vec![],
                                        job: false,
                                    }
                                ]))
                            ])],
                        ]
                    }))],
                    negate: false,
                },
                rest: vec![],
                job: false,
            })
        )
    }

    #[test]
    fn command_list() {
        let command = crate::command_chain();
        let commands = crate::pad_intercommand(command).repeated();
        assert_eq!(
            commands
                .then_ignore(end())
                .parse(b"echo some words \ncat afile.txt& tee output"),
            Ok(vec![
                CommandChain {
                    first: Pipeline {
                        command_groups: vec![CommandGroup::Simple(Box::new(SimpleCommand {
                            redirects: vec![],
                            assignments: vec![],
                            words: vec![
                                vec![Word::Simple(SimpleWord::Literal(b"echo".to_vec()))],
                                vec![Word::Simple(SimpleWord::Literal(b"some".to_vec()))],
                                vec![Word::Simple(SimpleWord::Literal(b"words".to_vec()))],
                            ]
                        }))],
                        negate: false,
                    },
                    rest: vec![],
                    job: false
                },
                CommandChain {
                    first: Pipeline {
                        command_groups: vec![CommandGroup::Simple(Box::new(SimpleCommand {
                            redirects: vec![],
                            assignments: vec![],
                            words: vec![
                                vec![Word::Simple(SimpleWord::Literal(b"cat".to_vec()))],
                                vec![Word::Simple(SimpleWord::Literal(b"afile.txt".to_vec()))],
                            ]
                        }))],
                        negate: false,
                    },
                    rest: vec![],
                    job: true,
                },
                CommandChain {
                    first: Pipeline {
                        command_groups: vec![CommandGroup::Simple(Box::new(SimpleCommand {
                            redirects: vec![],
                            assignments: vec![],
                            words: vec![
                                vec![Word::Simple(SimpleWord::Literal(b"tee".to_vec()))],
                                vec![Word::Simple(SimpleWord::Literal(b"output".to_vec()))],
                            ],
                        }))],
                        negate: false
                    },
                    rest: vec![],
                    job: false,
                }
            ])
        );
    }

    #[test]
    fn command_chaining() {
        let command = super::command_chain();

        assert_eq!(
            command
                .then_ignore(end())
                .parse(b"one && two | three || !four &&! five | six | seven &"),
            Ok(CommandChain {
                first: Pipeline {
                    command_groups: vec![CommandGroup::Simple(Box::new(SimpleCommand {
                        redirects: vec![],
                        assignments: vec![],
                        words: vec![vec![Word::Simple(SimpleWord::Literal(b"one".to_vec()))]],
                    }))],
                    negate: false,
                },
                rest: vec![
                    AndOr::And(Pipeline {
                        command_groups: vec![
                            CommandGroup::Simple(Box::new(SimpleCommand {
                                redirects: vec![],
                                assignments: vec![],
                                words: vec![vec![Word::Simple(SimpleWord::Literal(
                                    b"two".to_vec()
                                ))]]
                            })),
                            CommandGroup::Simple(Box::new(SimpleCommand {
                                redirects: vec![],
                                assignments: vec![],
                                words: vec![vec![Word::Simple(SimpleWord::Literal(
                                    b"three".to_vec()
                                ))]]
                            })),
                        ],
                        negate: false,
                    }),
                    AndOr::Or(Pipeline {
                        command_groups: vec![CommandGroup::Simple(Box::new(SimpleCommand {
                            redirects: vec![],
                            assignments: vec![],
                            words: vec![vec![Word::Simple(SimpleWord::Literal(b"four".to_vec()))]]
                        })),],
                        negate: true,
                    }),
                    AndOr::And(Pipeline {
                        command_groups: vec![
                            CommandGroup::Simple(Box::new(SimpleCommand {
                                redirects: vec![],
                                assignments: vec![],
                                words: vec![vec![Word::Simple(SimpleWord::Literal(
                                    b"five".to_vec()
                                ))]]
                            })),
                            CommandGroup::Simple(Box::new(SimpleCommand {
                                redirects: vec![],
                                assignments: vec![],
                                words: vec![vec![Word::Simple(SimpleWord::Literal(
                                    b"six".to_vec()
                                ))]]
                            })),
                            CommandGroup::Simple(Box::new(SimpleCommand {
                                redirects: vec![],
                                assignments: vec![],
                                words: vec![vec![Word::Simple(SimpleWord::Literal(
                                    b"seven".to_vec()
                                ))]]
                            })),
                        ],
                        negate: true,
                    }),
                ],
                job: true,
            })
        )
    }

    #[test]
    fn redirects_and_assignments() {
        let command = super::command_chain();

        assert_eq!(
            command
                .then_ignore(end())
                .parse(b"FOO=bar> file<file 4<>file BAZ= qux&>file quux 247>|file CORGE=grault"),
            Ok(CommandChain {
                first: Pipeline {
                    command_groups: vec![CommandGroup::Simple(Box::new(SimpleCommand {
                        redirects: vec![
                            Redirect::Write(
                                None,
                                vec![Word::Simple(SimpleWord::Literal(b"file".to_vec()))]
                            ),
                            Redirect::Read(
                                None,
                                vec![Word::Simple(SimpleWord::Literal(b"file".to_vec()))]
                            ),
                            Redirect::ReadWrite(
                                Some(4),
                                vec![Word::Simple(SimpleWord::Literal(b"file".to_vec()))]
                            ),
                            Redirect::DupWrite(
                                None,
                                vec![Word::Simple(SimpleWord::Literal(b"file".to_vec()))]
                            ),
                            Redirect::Clobber(
                                Some(247),
                                vec![Word::Simple(SimpleWord::Literal(b"file".to_vec()))]
                            ),
                        ],
                        assignments: vec![
                            (
                                b"FOO".to_vec(),
                                Some(vec![Word::Simple(SimpleWord::Literal(b"bar".to_vec()))])
                            ),
                            (b"BAZ".to_vec(), None)
                        ],
                        words: vec![
                            vec![Word::Simple(SimpleWord::Literal(b"qux".to_vec()))],
                            vec![Word::Simple(SimpleWord::Literal(b"quux".to_vec()))],
                            vec![Word::Simple(SimpleWord::Literal(b"CORGE=grault".to_vec()))]
                        ]
                    }))],
                    negate: false,
                },
                rest: vec![],
                job: false,
            })
        );
    }

    #[test]
    fn parameter_substitutions() {
        let command = super::command_chain();

        assert_eq!(
            command
                .then_ignore(end())
                .parse(b"${247} ${#} ${##} ${SHELL##z} ${SHELL%sh} ${FOO:- bar}"),
            Ok(CommandChain {
                first: Pipeline {
                    command_groups: vec![CommandGroup::Simple(Box::new(SimpleCommand {
                        redirects: vec![],
                        assignments: vec![],
                        words: vec![
                            vec![Word::Simple(SimpleWord::Subst(
                                ParameterSubstitution::Parameter(Parameter::Positional(247))
                            ))],
                            vec![Word::Simple(SimpleWord::Subst(
                                ParameterSubstitution::Parameter(Parameter::Hash)
                            ))],
                            vec![Word::Simple(SimpleWord::Subst(ParameterSubstitution::Len(
                                Parameter::Hash
                            )))],
                            vec![Word::Simple(SimpleWord::Subst(
                                ParameterSubstitution::RemoveLargestPrefix(
                                    Parameter::Var(b"SHELL".to_vec(),),
                                    Some(Box::new(vec![Word::Simple(SimpleWord::Literal(
                                        b"z".to_vec()
                                    ))]))
                                )
                            ))],
                            vec![Word::Simple(SimpleWord::Subst(
                                ParameterSubstitution::RemoveSmallestSuffix(
                                    Parameter::Var(b"SHELL".to_vec(),),
                                    Some(Box::new(vec![Word::Simple(SimpleWord::Literal(
                                        b"sh".to_vec()
                                    ))]))
                                )
                            ))],
                            vec![Word::Simple(SimpleWord::Subst(
                                ParameterSubstitution::Default(
                                    true,
                                    Parameter::Var(b"FOO".to_vec(),),
                                    Some(Box::new(vec![Word::Simple(SimpleWord::Literal(
                                        b"bar".to_vec()
                                    ))]))
                                )
                            ))],
                        ]
                    }))],
                    negate: false,
                },
                rest: vec![],
                job: false,
            })
        )
    }

    #[test]
    fn subshell_brace_groups() {
        let command = super::command_chain();

        assert_eq!(
            command
                .then_ignore(end())
                .parse(b"{ echo a; ( echo b & echo c ) >/dev/null & }"),
            Ok(CommandChain {
                first: Pipeline {
                    command_groups: vec![CommandGroup::Brace {
                        commands: vec![
                            CommandChain {
                                first: Pipeline {
                                    command_groups: vec![CommandGroup::Simple(Box::new(
                                        SimpleCommand {
                                            redirects: vec![],
                                            assignments: vec![],
                                            words: vec![
                                                vec![Word::Simple(SimpleWord::Literal(
                                                    b"echo".to_vec()
                                                ))],
                                                vec![Word::Simple(SimpleWord::Literal(
                                                    b"a".to_vec()
                                                ))],
                                            ]
                                        }
                                    ))],
                                    negate: false,
                                },
                                rest: vec![],
                                job: false,
                            },
                            CommandChain {
                                first: Pipeline {
                                    command_groups: vec![CommandGroup::Subshell {
                                        commands: vec![
                                            CommandChain {
                                                first: Pipeline {
                                                    command_groups: vec![CommandGroup::Simple(
                                                        Box::new(SimpleCommand {
                                                            redirects: vec![],
                                                            assignments: vec![],
                                                            words: vec![
                                                                vec![Word::Simple(
                                                                    SimpleWord::Literal(
                                                                        b"echo".to_vec()
                                                                    )
                                                                )],
                                                                vec![Word::Simple(
                                                                    SimpleWord::Literal(
                                                                        b"b".to_vec()
                                                                    )
                                                                )],
                                                            ]
                                                        })
                                                    )],
                                                    negate: false,
                                                },
                                                rest: vec![],
                                                job: true,
                                            },
                                            CommandChain {
                                                first: Pipeline {
                                                    command_groups: vec![CommandGroup::Simple(
                                                        Box::new(SimpleCommand {
                                                            redirects: vec![],
                                                            assignments: vec![],
                                                            words: vec![
                                                                vec![Word::Simple(
                                                                    SimpleWord::Literal(
                                                                        b"echo".to_vec()
                                                                    )
                                                                )],
                                                                vec![Word::Simple(
                                                                    SimpleWord::Literal(
                                                                        b"c".to_vec()
                                                                    )
                                                                )],
                                                            ]
                                                        })
                                                    )],
                                                    negate: false,
                                                },
                                                rest: vec![],
                                                job: false,
                                            }
                                        ],
                                        redirects: vec![Redirect::Write(
                                            None,
                                            vec![Word::Simple(SimpleWord::Literal(
                                                b"/dev/null".to_vec()
                                            ))]
                                        )]
                                    }],
                                    negate: false,
                                },
                                rest: vec![],
                                job: true,
                            }
                        ],
                        redirects: vec![],
                    }],
                    negate: false,
                },
                rest: vec![],
                job: false,
            })
        )
    }

    #[test]
    fn if_statements() {
        let command = super::command_chain();
        assert_eq!(
            command.parse(b"if true; then echo true; fi"),
            Ok(CommandChain {
                first: Pipeline {
                    command_groups: vec![CommandGroup::If {
                        conditionals: vec![GuardBodyPair {
                            guard: vec![CommandChain {
                                first: Pipeline {
                                    command_groups: vec![CommandGroup::Simple(Box::new(
                                        SimpleCommand {
                                            redirects: vec![],
                                            assignments: vec![],
                                            words: vec![vec![Word::Simple(SimpleWord::Literal(
                                                b"true".to_vec()
                                            ))],],
                                        }
                                    ))],
                                    negate: false,
                                },
                                rest: vec![],
                                job: false,
                            }],
                            body: vec![CommandChain {
                                first: Pipeline {
                                    command_groups: vec![CommandGroup::Simple(Box::new(
                                        SimpleCommand {
                                            redirects: vec![],
                                            assignments: vec![],
                                            words: vec![
                                                vec![Word::Simple(SimpleWord::Literal(
                                                    b"echo".to_vec()
                                                ))],
                                                vec![Word::Simple(SimpleWord::Literal(
                                                    b"true".to_vec()
                                                ))],
                                            ],
                                        }
                                    ))],
                                    negate: false,
                                },
                                rest: vec![],
                                job: false,
                            }],
                        },],
                        else_branch: None,
                        io: vec![],
                    }],
                    negate: false,
                },
                rest: vec![],
                job: false,
            })
        );
        assert_eq!(
            command.parse(b"if true; then echo true; elif false; then echo false; else echo huh; fi >/dev/null"),
            Ok(CommandChain {
                    first: Pipeline {
                        command_groups: vec![CommandGroup::If {
                            conditionals: vec![
                                GuardBodyPair {
                                    guard: vec![CommandChain {
                                        first: Pipeline {
                                            command_groups: vec![CommandGroup::Simple(Box::new(SimpleCommand {
                                                redirects: vec![],
                                                assignments: vec![],
                                                words: vec![
                                                    vec![Word::Simple(SimpleWord::Literal(b"true".to_vec()))],
                                                ],
                                            }))],
                                            negate: false,
                                        },
                                        rest: vec![],
                                        job: false,
                                    }],
                                    body: vec![CommandChain {
                                        first: Pipeline {
                                            command_groups: vec![CommandGroup::Simple(Box::new(SimpleCommand {
                                                redirects: vec![],
                                                assignments: vec![],
                                                words: vec![
                                                    vec![Word::Simple(SimpleWord::Literal(b"echo".to_vec()))],
                                                    vec![Word::Simple(SimpleWord::Literal(b"true".to_vec()))],
                                                ],
                                            }))],
                                            negate: false,
                                        },
                                        rest: vec![],
                                        job: false,
                                    }],
                                },
                                GuardBodyPair {
                                    guard: vec![CommandChain {
                                        first: Pipeline {
                                            command_groups: vec![CommandGroup::Simple(Box::new(SimpleCommand {
                                                redirects: vec![],
                                                assignments: vec![],
                                                words: vec![vec![Word::Simple(SimpleWord::Literal(b"false".to_vec()))]],
                                            }))],
                                            negate: false,
                                        },
                                        rest: vec![],
                                        job: false,
                                    }],
                                    body: vec![CommandChain {
                                        first: Pipeline {
                                            command_groups: vec![CommandGroup::Simple(Box::new(SimpleCommand {
                                                redirects: vec![],
                                                assignments: vec![],
                                                words: vec![
                                                    vec![Word::Simple(SimpleWord::Literal(b"echo".to_vec()))],
                                                    vec![Word::Simple(SimpleWord::Literal(b"false".to_vec()))],
                                                ],
                                            }))],
                                            negate: false,
                                        },
                                        rest: vec![],
                                        job: false,
                                    }],
                                },
                            ],
                            else_branch: Some(vec![CommandChain {
                                first: Pipeline {
                                    command_groups: vec![CommandGroup::Simple(Box::new(SimpleCommand {
                                        redirects: vec![],
                                        assignments: vec![],
                                        words: vec![
                                            vec![Word::Simple(SimpleWord::Literal(b"echo".to_vec()))],
                                            vec![Word::Simple(SimpleWord::Literal(b"huh".to_vec()))],
                                        ],
                                    }))],
                                    negate: false,
                                },
                                rest: vec![],
                                job: false,
                            }]),
                            io: vec![
                                Redirect::Write(None, vec![Word::Simple(SimpleWord::Literal(b"/dev/null".to_vec()))])
                            ],
                        }],
                        negate: false,
                    },
                    rest: vec![],
                    job: false,
                }
        ))
    }
}
