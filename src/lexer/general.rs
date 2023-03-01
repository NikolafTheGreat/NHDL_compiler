use regex::{Regex, Captures};
use std::{fmt};
use std::error::Error;

pub struct Rule <T> {
    pub pattern : Regex,
    pub result : Box< dyn Fn(&Captures) -> Option<T> >
}

#[derive(Debug)]
pub struct LexerToken <T> {
    line_number : usize,
    character_number : usize,
    token_type : T,
}

#[derive(Debug)]
pub struct LexerError {
    line_number : usize,
    character_number : usize,
    error_token : String,
}

impl Error for LexerError{}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Failed to parse the following token: \"{}\" at line: {} character: {}..{}",
            self.error_token,
            self.line_number,
            self.character_number,
            self.character_number + self.error_token.len()
        )
    }
}

pub fn lex <T> (input : &str, rules : Vec<Rule<T>>) -> Result<Vec<LexerToken<T>>, LexerError>{
    let mut output = Vec::new();
    let mut current = input;

    let mut character_number = 0;
    let mut line_number = 1;

    'outer : while !current.is_empty() {
        for rule in rules.iter() {
            if let Some(c) = rule.pattern.captures(current) {
                if let Some(token_type) = (rule.result)(&c) {
                    output.push(LexerToken {
                        line_number: line_number,
                        character_number: character_number,
                        token_type: token_type,
                    });
                }
                let length = c.get(0).unwrap().as_str().len();
                character_number += length;
                line_number += current[..length].matches('\n').count();
                current = &current[length..];
                continue 'outer;
            }
        }
        let mut length = current.find(|c : char| c.is_whitespace()).unwrap_or(current.len());
        if length == 0 {
            length = 1;
        }
        return Err (LexerError {
            line_number: line_number,
            character_number: character_number,
            error_token: current[0..length].to_string(),
        })
    }
    Ok(output)
}