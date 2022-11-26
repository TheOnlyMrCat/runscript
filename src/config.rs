use std::collections::HashMap;

use serde::de::{self, Visitor};
use serde::{Deserialize, Deserializer};
use termcolor::{Color, ColorSpec};

#[derive(Deserialize, Default)]
#[serde(default)]
pub struct Config {
    pub file: FileConfig,
    pub output: OutputConfig,
    pub dev: DevConfig,
}

#[derive(Deserialize)]
#[serde(default)]
pub struct FileConfig {
    pub names: Vec<String>,
}

#[derive(Deserialize)]
#[serde(default)]
pub struct OutputConfig {
    pub theme: Option<String>,
    pub prompt_prefix: String,
}

#[derive(Deserialize, Default)]
#[serde(default)]
pub struct DevConfig {
    pub panic: bool,
}

impl Default for FileConfig {
    fn default() -> Self {
        Self {
            names: vec![".run".to_owned(), "run".to_owned()],
        }
    }
}

impl Default for OutputConfig {
    fn default() -> Self {
        Self {
            theme: None,
            prompt_prefix: "> ".to_owned(),
        }
    }
}

#[derive(Clone, Deserialize)]
pub struct Theme {
    #[serde(default = "default_based_on")]
    pub based_on: String,
    pub phase: HashMap<String, PhaseTheme>,
    #[serde(flatten)]
    pub command: CommandTheme,
    #[serde(default, deserialize_with = "deserialize_opt_color")]
    pub error: Option<Color>,
    #[serde(default, deserialize_with = "deserialize_opt_color")]
    pub warning: Option<Color>,
}

impl Theme {
    pub fn empty() -> Self {
        Self {
            based_on: "default".to_owned(),
            phase: HashMap::new(),
            command: CommandTheme::default(),
            error: None,
            warning: None,
        }
    }

    pub fn default() -> Self {
        Theme {
            based_on: "".to_owned(),
            phase: {
                let mut phase = HashMap::new();
                let (build, run, test) = if std::env::var_os(
                    "\x52\x55\x4E\x53\x43\x52\x49\x50\x54\x5F\x54\x52\x41\x4E\x53",
                )
                .is_some()
                {
                    (Color::Ansi256(6), Color::Ansi256(13), Color::Ansi256(7))
                } else {
                    (Color::Red, Color::Blue, Color::Green)
                };
                phase.insert("build".to_owned(), PhaseTheme { color: Some(build) });
                phase.insert("run".to_owned(), PhaseTheme { color: Some(run) });
                phase.insert("test".to_owned(), PhaseTheme { color: Some(test) });
                phase
            },
            command: {
                fn colorspec<F: FnOnce(&mut ColorSpec) -> &mut ColorSpec>(
                    f: F,
                ) -> Option<ColorSpec> {
                    let mut spec = ColorSpec::new();
                    f(&mut spec);
                    Some(spec)
                }

                CommandTheme {
                    first_argument: colorspec(|spec| spec.set_bold(true).set_intense(true)),
                    empty_argument: colorspec(|spec| spec.set_dimmed(true)),
                    single_quotes: colorspec(|spec| {
                        spec.set_fg(Some(Color::Yellow)).set_intense(true)
                    }),
                    double_quotes: colorspec(|spec| {
                        spec.set_fg(Some(Color::Yellow)).set_intense(true)
                    }),
                    substitution: colorspec(|spec| {
                        spec.set_fg(Some(Color::Cyan))
                            .set_bold(true)
                            .set_intense(true)
                    }),
                    quoted_substitution: colorspec(|spec| {
                        spec.set_fg(Some(Color::Cyan))
                            .set_bold(true)
                            .set_intense(true)
                    }),
                    glob_substitution: colorspec(|spec| spec.set_fg(Some(Color::Blue))),
                    glob_matches: colorspec(|spec| spec.set_fg(Some(Color::Blue))),
                    glob_matches_hint: colorspec(|spec| {
                        spec.set_fg(Some(Color::White)).set_dimmed(true)
                    }),
                }
            },
            error: Some(Color::Red),
            warning: Some(Color::Yellow),
        }
    }

    pub fn canonicalised(mut self) -> Self {
        self.command = self.command.canonicalised();
        self
    }

    pub fn merged_with(mut self, fallback: Theme) -> Self {
        self.based_on = fallback.based_on;
        for (key, phase) in fallback.phase.into_iter() {
            match self.phase.entry(key) {
                std::collections::hash_map::Entry::Occupied(_) => {}
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(phase);
                }
            }
        }
        self.command = self.command.merged_with(fallback.command);
        self.error = self.error.or(fallback.error);
        self.warning = self.warning.or(fallback.warning);
        self
    }
}

#[derive(Clone, Deserialize)]
pub struct PhaseTheme {
    #[serde(deserialize_with = "deserialize_opt_color")]
    pub color: Option<Color>,
}

#[derive(Clone, Deserialize, Default)]
#[serde(default)]
pub struct CommandTheme {
    #[serde(deserialize_with = "deserialize_opt_colorspec")]
    pub first_argument: Option<ColorSpec>,
    #[serde(deserialize_with = "deserialize_opt_colorspec")]
    pub empty_argument: Option<ColorSpec>,
    #[serde(deserialize_with = "deserialize_opt_colorspec")]
    pub single_quotes: Option<ColorSpec>,
    #[serde(deserialize_with = "deserialize_opt_colorspec")]
    pub double_quotes: Option<ColorSpec>,
    #[serde(deserialize_with = "deserialize_opt_colorspec")]
    pub substitution: Option<ColorSpec>,
    #[serde(deserialize_with = "deserialize_opt_colorspec")]
    pub quoted_substitution: Option<ColorSpec>,
    #[serde(deserialize_with = "deserialize_opt_colorspec")]
    pub glob_substitution: Option<ColorSpec>,
    #[serde(deserialize_with = "deserialize_opt_colorspec")]
    pub glob_matches: Option<ColorSpec>,
    #[serde(deserialize_with = "deserialize_opt_colorspec")]
    pub glob_matches_hint: Option<ColorSpec>,
}

impl CommandTheme {
    pub fn canonicalised(mut self) -> Self {
        self.quoted_substitution = self
            .quoted_substitution
            .or_else(|| self.substitution.clone());
        self.glob_substitution = self.glob_substitution.or_else(|| self.glob_matches.clone());
        self
    }

    pub fn merged_with(self, fallback: CommandTheme) -> CommandTheme {
        Self {
            first_argument: self.first_argument.or(fallback.first_argument),
            empty_argument: self.empty_argument.or(fallback.empty_argument),
            single_quotes: self.single_quotes.or(fallback.single_quotes),
            double_quotes: self.double_quotes.or(fallback.double_quotes),
            substitution: self.substitution.or(fallback.substitution),
            quoted_substitution: self.quoted_substitution.or(fallback.quoted_substitution),
            glob_substitution: self.glob_substitution.or(fallback.glob_substitution),
            glob_matches: self.glob_matches.or(fallback.glob_matches),
            glob_matches_hint: self.glob_matches_hint.or(fallback.glob_matches_hint),
        }
    }
}

fn default_based_on() -> String {
    "default".to_string()
}

fn deserialize_opt_color<'de, D>(de: D) -> Result<Option<Color>, D::Error>
where
    D: Deserializer<'de>,
{
    Some(deserialize_color(de)).transpose()
}

fn deserialize_color<'de, D>(de: D) -> Result<Color, D::Error>
where
    D: Deserializer<'de>,
{
    struct ColorVisitor;

    impl<'de> Visitor<'de> for ColorVisitor {
        type Value = Color;

        fn expecting(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(fmt, "the name of a color or a hex code")
        }

        fn visit_i8<E>(self, value: i8) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            if value < 0 {
                Err(de::Error::invalid_value(
                    de::Unexpected::Signed(value as i64),
                    &"a non-negative integer",
                ))
            } else {
                Ok(Color::Ansi256(value as u8))
            }
        }

        fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            if value < 0 {
                Err(de::Error::invalid_value(
                    de::Unexpected::Signed(value),
                    &"a non-negative integer",
                ))
            } else if value > u8::MAX as i64 {
                Err(de::Error::invalid_value(
                    de::Unexpected::Signed(value),
                    &"an integer less than 256",
                ))
            } else {
                Ok(Color::Ansi256(value as u8))
            }
        }

        fn visit_u8<E>(self, value: u8) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Color::Ansi256(value))
        }

        fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            if value > u8::MAX as u64 {
                Err(de::Error::invalid_value(
                    de::Unexpected::Unsigned(value),
                    &"an integer less than 256",
                ))
            } else {
                Ok(Color::Ansi256(value as u8))
            }
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            if value.eq_ignore_ascii_case("black") {
                Ok(Color::Black)
            } else if value.eq_ignore_ascii_case("red") {
                Ok(Color::Red)
            } else if value.eq_ignore_ascii_case("green") {
                Ok(Color::Green)
            } else if value.eq_ignore_ascii_case("yellow") {
                Ok(Color::Yellow)
            } else if value.eq_ignore_ascii_case("blue") {
                Ok(Color::Blue)
            } else if value.eq_ignore_ascii_case("magenta") {
                Ok(Color::Magenta)
            } else if value.eq_ignore_ascii_case("cyan") {
                Ok(Color::Cyan)
            } else if value.eq_ignore_ascii_case("white") {
                Ok(Color::White)
            } else if let Some(hex_code) = value.strip_prefix('#') {
                if hex_code.len() == 3 {
                    let r = u8::from_str_radix(&hex_code[0..1], 16).map_err(|_| {
                        de::Error::invalid_value(de::Unexpected::Str(hex_code), &"a hex code")
                    })?;
                    let g = u8::from_str_radix(&hex_code[1..2], 16).map_err(|_| {
                        de::Error::invalid_value(de::Unexpected::Str(hex_code), &"a hex code")
                    })?;
                    let b = u8::from_str_radix(&hex_code[2..3], 16).map_err(|_| {
                        de::Error::invalid_value(de::Unexpected::Str(hex_code), &"a hex code")
                    })?;
                    Ok(Color::Rgb(r << 4, g << 4, b << 4))
                } else if hex_code.len() == 6 {
                    let r = u8::from_str_radix(&hex_code[0..2], 16).map_err(|_| {
                        de::Error::invalid_value(de::Unexpected::Str(hex_code), &"a hex code")
                    })?;
                    let g = u8::from_str_radix(&hex_code[2..4], 16).map_err(|_| {
                        de::Error::invalid_value(de::Unexpected::Str(hex_code), &"a hex code")
                    })?;
                    let b = u8::from_str_radix(&hex_code[4..6], 16).map_err(|_| {
                        de::Error::invalid_value(de::Unexpected::Str(hex_code), &"a hex code")
                    })?;
                    Ok(Color::Rgb(r, g, b))
                } else {
                    Err(de::Error::invalid_value(
                        de::Unexpected::Str(hex_code),
                        &"a hex code",
                    ))
                }
            } else {
                Err(de::Error::invalid_value(
                    de::Unexpected::Str(value),
                    &"a colour name",
                ))
            }
        }
    }

    de.deserialize_any(ColorVisitor)
}

fn deserialize_opt_colorspec<'de, D>(de: D) -> Result<Option<ColorSpec>, D::Error>
where
    D: Deserializer<'de>,
{
    Some(deserialize_colorspec(de)).transpose()
}

fn deserialize_colorspec<'de, D>(de: D) -> Result<ColorSpec, D::Error>
where
    D: Deserializer<'de>,
{
    struct ColorSpecVisitor;

    impl<'de> Visitor<'de> for ColorSpecVisitor {
        type Value = ColorSpec;

        fn expecting(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(fmt, "a format specifier")
        }

        fn visit_i8<E>(self, value: i8) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            if value < 0 {
                Err(de::Error::invalid_value(
                    de::Unexpected::Signed(value as i64),
                    &"a non-negative integer",
                ))
            } else {
                Ok({
                    let mut spec = ColorSpec::new();
                    spec.set_fg(Some(Color::Ansi256(value as u8)));
                    spec
                })
            }
        }

        fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            if value < 0 {
                Err(de::Error::invalid_value(
                    de::Unexpected::Signed(value),
                    &"a non-negative integer",
                ))
            } else if value > u8::MAX as i64 {
                Err(de::Error::invalid_value(
                    de::Unexpected::Signed(value),
                    &"an integer less than 256",
                ))
            } else {
                Ok({
                    let mut spec = ColorSpec::new();
                    spec.set_fg(Some(Color::Ansi256(value as u8)));
                    spec
                })
            }
        }

        fn visit_u8<E>(self, value: u8) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok({
                let mut spec = ColorSpec::new();
                spec.set_fg(Some(Color::Ansi256(value)));
                spec
            })
        }

        fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            if value > u8::MAX as u64 {
                Err(de::Error::invalid_value(
                    de::Unexpected::Unsigned(value),
                    &"an integer less than 256",
                ))
            } else {
                Ok({
                    let mut spec = ColorSpec::new();
                    spec.set_fg(Some(Color::Ansi256(value as u8)));
                    spec
                })
            }
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            if value.eq_ignore_ascii_case("black") {
                Ok({
                    let mut spec = ColorSpec::new();
                    spec.set_fg(Some(Color::Black));
                    spec
                })
            } else if value.eq_ignore_ascii_case("red") {
                Ok({
                    let mut spec = ColorSpec::new();
                    spec.set_fg(Some(Color::Red));
                    spec
                })
            } else if value.eq_ignore_ascii_case("green") {
                Ok({
                    let mut spec = ColorSpec::new();
                    spec.set_fg(Some(Color::Green));
                    spec
                })
            } else if value.eq_ignore_ascii_case("yellow") {
                Ok({
                    let mut spec = ColorSpec::new();
                    spec.set_fg(Some(Color::Yellow));
                    spec
                })
            } else if value.eq_ignore_ascii_case("blue") {
                Ok({
                    let mut spec = ColorSpec::new();
                    spec.set_fg(Some(Color::Blue));
                    spec
                })
            } else if value.eq_ignore_ascii_case("magenta") {
                Ok({
                    let mut spec = ColorSpec::new();
                    spec.set_fg(Some(Color::Magenta));
                    spec
                })
            } else if value.eq_ignore_ascii_case("cyan") {
                Ok({
                    let mut spec = ColorSpec::new();
                    spec.set_fg(Some(Color::Cyan));
                    spec
                })
            } else if value.eq_ignore_ascii_case("white") {
                Ok({
                    let mut spec = ColorSpec::new();
                    spec.set_fg(Some(Color::White));
                    spec
                })
            } else if let Some(hex_code) = value.strip_prefix('#') {
                if hex_code.len() == 3 {
                    let r = u8::from_str_radix(&hex_code[0..1], 16).map_err(|_| {
                        de::Error::invalid_value(de::Unexpected::Str(hex_code), &"a hex code")
                    })?;
                    let g = u8::from_str_radix(&hex_code[1..2], 16).map_err(|_| {
                        de::Error::invalid_value(de::Unexpected::Str(hex_code), &"a hex code")
                    })?;
                    let b = u8::from_str_radix(&hex_code[2..3], 16).map_err(|_| {
                        de::Error::invalid_value(de::Unexpected::Str(hex_code), &"a hex code")
                    })?;
                    Ok({
                        let mut spec = ColorSpec::new();
                        spec.set_fg(Some(Color::Rgb(r << 4, g << 4, b << 4)));
                        spec
                    })
                } else if hex_code.len() == 6 {
                    let r = u8::from_str_radix(&hex_code[0..2], 16).map_err(|_| {
                        de::Error::invalid_value(de::Unexpected::Str(hex_code), &"a hex code")
                    })?;
                    let g = u8::from_str_radix(&hex_code[2..4], 16).map_err(|_| {
                        de::Error::invalid_value(de::Unexpected::Str(hex_code), &"a hex code")
                    })?;
                    let b = u8::from_str_radix(&hex_code[4..6], 16).map_err(|_| {
                        de::Error::invalid_value(de::Unexpected::Str(hex_code), &"a hex code")
                    })?;
                    Ok({
                        let mut spec = ColorSpec::new();
                        spec.set_fg(Some(Color::Rgb(r, g, b)));
                        spec
                    })
                } else {
                    Err(de::Error::invalid_value(
                        de::Unexpected::Str(hex_code),
                        &"a hex code",
                    ))
                }
            } else {
                Err(de::Error::invalid_value(
                    de::Unexpected::Str(value),
                    &"a colour name",
                ))
            }
        }

        fn visit_map<A>(self, map: A) -> Result<ColorSpec, A::Error>
        where
            A: serde::de::MapAccess<'de>,
        {
            #[derive(Deserialize, Default)]
            #[serde(default)]
            struct ColorSpecDe {
                #[serde(deserialize_with = "deserialize_opt_color")]
                fg: Option<Color>,
                #[serde(deserialize_with = "deserialize_opt_color")]
                bg: Option<Color>,
                bold: bool,
                dimmed: bool,
                italic: bool,
                underline: bool,
                intense: bool,
            }

            let de = serde::de::value::MapAccessDeserializer::new(map);
            ColorSpecDe::deserialize(de).map(|spec_de| {
                let mut spec = ColorSpec::new();
                spec.set_fg(spec_de.fg);
                spec.set_bg(spec_de.bg);
                spec.set_bold(spec_de.bold);
                spec.set_dimmed(spec_de.dimmed);
                spec.set_italic(spec_de.italic);
                spec.set_underline(spec_de.underline);
                spec.set_intense(spec_de.intense);
                spec
            })
        }
    }

    de.deserialize_any(ColorSpecVisitor)
}
