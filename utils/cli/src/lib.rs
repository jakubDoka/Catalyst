#![feature(let_else)]

use std::collections::HashMap;

/// Cli input simply parses command line input in the most generic 
/// way and makes is convenient to access flags, values and arguments.
///
/// # Syntax
/// ```txt
/// ident = '[^ ]+'
/// value = '"' <value> ident '"' | ident
/// flag = -ident
/// valued_flag = --ident <value>
/// ```
pub struct CliInput {
    path: String,
    args: Vec<String>,
    flags: HashMap<String, Option<String>>,
}

impl CliInput {
    /// Creates a new `CliInput` from the given string. Useful for testing.
    /// 
    /// # Examples
    /// ```
    /// let input = cli::CliInput::from_str("path/to/file.rs command --field value -flag \"string arg\" --field2 \"string arg 2\"")
    ///     .unwrap();
    /// 
    /// assert_eq!(input.wd(), "path/to/file.rs");
    /// assert_eq!(input.args(), &["command", "string arg"]);
    /// assert_eq!(input.value("field"), Some("value"));
    /// assert_eq!(input.value("field2"), Some("string arg 2"));
    /// assert_eq!(input.enabled("flag"), true);
    /// ```
    pub fn from_str(str: &str) -> Result<Self, CliError> {
        Self::new_low(str.split(' ').map(|s| s.to_string()))
    }

    /// Creates a new `CliInput` taking [`std::env::args`] as input.
    pub fn new() -> Result<Self, CliError> {
        Self::new_low(std::env::args())
    }
    
    fn new_low(input: impl Iterator<Item = String>) -> Result<Self, CliError> {
        let mut input: Vec<_> = input.collect();
        input.reverse();

        let mut args = vec![];
        let mut flags = HashMap::new();
        let path = input.pop().unwrap();

        while let Some(mut arg) = input.pop() {
            let mut chars = arg.chars();
            match (chars.next(), chars.next()) {
                (Some('-'), Some('-')) => {
                    arg.drain(..2);
                    let Some(field) = input.pop() else { 
                        return Err(CliError::MissingFlagValue(arg));
                    };
                    let mut chars = field.chars();
                    match (chars.next(), chars.next()) {
                        (Some('-'), ..) => return Err(CliError::InvalidValue(arg)),
                        (Some('"'), ..) => flags.insert(arg, Some(Self::parse_string(&mut input, field)?)),
                        _ => flags.insert(arg, Some(field)),
                    };
                },
                (Some('-'), ..) => {
                    arg.drain(..1);
                    flags.insert(arg, None);
                },
                (Some('"'), ..) => args.push(Self::parse_string(&mut input, arg)?),
                _ => args.push(arg),
            }
        }

        Ok(Self { 
            path, 
            args, 
            flags 
        })
    }

    /// Return the cmd arguments
    pub fn args(&self) -> &[String] {
        &self.args
    }

    /// Returns whether flag is enabled, flags with values are not considered enabled.
    pub fn enabled(&self, flag: &str) -> bool {
        self.flags.get(flag).map_or(false, |v| v.is_none())
    }

    /// Returns the value if flag exists and has a value.
    pub fn value(&self, flag: &str) -> Option<&str> {
        self.flags
            .get(flag)
            .and_then(|v| v.as_ref())
            .and_then(|v| Some(v.as_str()))
    }

    /// Returns working directory.
    pub fn wd(&self) -> &str {
        &self.path
    } 

    fn parse_string(input: &mut Vec<String>, mut start: String) -> Result<String, CliError> {
        loop {
            if start.ends_with('"') && !start.ends_with("\\\"") {
                start.drain(..1);
                start.pop().unwrap();
                return Ok(start);
            }
            let Some(next) = input.pop() else {
                return Err(CliError::UnclosedString);
            };
            start.push(' ');
            start.push_str(&next);
        }
    }
}

/// Errors that can occur when parsing a `CliInput`.
#[derive(Debug)]
pub enum CliError {
    MissingFlagValue(String),
    InvalidValue(String),
    UnclosedString,
}