use std::collections::HashMap;

use serde::de::{self, Visitor};
use serde::{Deserialize, Deserializer};

#[derive(Deserialize, Default)]
#[serde(default)]
pub struct Config {
    pub file: FileConfig,
    #[serde(rename = "colors")]
    pub colours: ColourConfig,
    pub dev: DevConfig,
}

#[derive(Deserialize)]
#[serde(default)]
pub struct FileConfig {
    pub names: Vec<String>,
}

#[derive(Deserialize)]
#[serde(default)]
pub struct ColourConfig {
    pub enabled: bool,
    #[serde(deserialize_with = "deserialize_colours")]
    pub phases: HashMap<String, termcolor::Color>,
}

#[derive(Deserialize, Default)]
#[serde(default)]
pub struct DevConfig {
    pub panic: bool,
}

impl Default for FileConfig {
    fn default() -> Self {
        Self {
            names: vec!["run".to_owned(), ".run".to_owned()],
        }
    }
}

impl Default for ColourConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            phases: default_colours(),
        }
    }
}

fn default_colours() -> HashMap<String, termcolor::Color> {
    let mut map = HashMap::new();
    if std::env::var_os("\x52\x55\x4E\x53\x43\x52\x49\x50\x54\x5F\x54\x52\x41\x4E\x53").is_some() {
        map.insert("build".to_owned(), termcolor::Color::Ansi256(6));
        map.insert("run".to_owned(), termcolor::Color::Ansi256(13));
        map.insert("test".to_owned(), termcolor::Color::Ansi256(7));
    } else {
        map.insert("build".to_owned(), termcolor::Color::Red);
        map.insert("run".to_owned(), termcolor::Color::Blue);
        map.insert("test".to_owned(), termcolor::Color::Green);
    }
    map
}

fn deserialize_colours<'de, D>(de: D) -> Result<HashMap<String, termcolor::Color>, D::Error>
where
    D: Deserializer<'de>,
{
    struct ColourVisitor;

    impl<'de> Visitor<'de> for ColourVisitor {
        type Value = termcolor::Color;

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
                Ok(termcolor::Color::Ansi256(value as u8))
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
                Ok(termcolor::Color::Ansi256(value as u8))
            }
        }

        fn visit_u8<E>(self, value: u8) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(termcolor::Color::Ansi256(value))
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
                Ok(termcolor::Color::Ansi256(value as u8))
            }
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            if value.eq_ignore_ascii_case("black") {
                Ok(termcolor::Color::Black)
            } else if value.eq_ignore_ascii_case("red") {
                Ok(termcolor::Color::Red)
            } else if value.eq_ignore_ascii_case("green") {
                Ok(termcolor::Color::Green)
            } else if value.eq_ignore_ascii_case("yellow") {
                Ok(termcolor::Color::Yellow)
            } else if value.eq_ignore_ascii_case("blue") {
                Ok(termcolor::Color::Blue)
            } else if value.eq_ignore_ascii_case("magenta") {
                Ok(termcolor::Color::Magenta)
            } else if value.eq_ignore_ascii_case("cyan") {
                Ok(termcolor::Color::Cyan)
            } else if value.eq_ignore_ascii_case("white") {
                Ok(termcolor::Color::White)
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
                    Ok(termcolor::Color::Rgb(r << 4, g << 4, b << 4))
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
                    Ok(termcolor::Color::Rgb(r, g, b))
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

    struct ColourDeserialize(termcolor::Color);

    impl<'de> Deserialize<'de> for ColourDeserialize {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            deserializer
                .deserialize_any(ColourVisitor)
                .map(ColourDeserialize)
        }
    }

    struct MapVisitor;

    impl<'de> Visitor<'de> for MapVisitor {
        type Value = HashMap<String, termcolor::Color>;

        fn expecting(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(fmt, "a map of phase names to colours")
        }

        fn visit_map<V>(self, mut map: V) -> Result<Self::Value, V::Error>
        where
            V: de::MapAccess<'de>,
        {
            let mut result = HashMap::new();
            while let Some((key, value)) = map.next_entry::<String, ColourDeserialize>()? {
                result.insert(key, value.0);
            }
            Ok(result)
        }
    }

    de.deserialize_map(MapVisitor)
}
