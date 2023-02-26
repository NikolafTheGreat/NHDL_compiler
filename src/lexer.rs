use std::{error::Error, fmt};

#[derive(Debug)]
pub enum LexerToken {
    //Declarations
    Module,
    Enum,

    //Controllflow
    If,
    Else,
    Switch,
    Loop,

    //Scopes
    StratScope,
    EndScope,

    //Operators
    BitwiseNot,
    Multiply,
    Divide,
    Modulo,
    Plus,
    Minus,
    BitshiftRight,
    BitshiftLeft,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Concatinate,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    Assignment,

    //Parenthesies ( "(" and ")" )
    StartParenthesy,
    EndParenthesy,

    //Brackets ( "[" and "]" )
    StartBrackets,
    EndBrackets,

    //Braces ( "<" and ">" )
    StartBraces,
    EndBraces,

    //Types
    UnsignedInt(usize),
    SignedInt(usize),
    Analog(usize),
    Async,
    Reset,
    Clock,
    
    //Seperators
    Comma,
    Period,
    Colon,
    Semicolon,

    //Variables
    Flip,
    Wire,
    Reg,

    //Identifiers
    Litteral(usize),
    Identifier(String),
}

#[derive(Debug)]
pub struct LexerError {
    error_token : String,
    line_number : usize,
}

impl Error for LexerError{}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Failed to parse the following token: \"{}\" at line {}", self.error_token, self.line_number)
    }
}

pub fn lex(input : &str) -> Result<Vec<LexerToken>, LexerError> {
    let mut output = Vec::new();
    let mut current = input;
    let mut completed = false;
    let mut line_number = 1;

    loop {
        if current.is_empty() {
            completed = true;
            break;
        }

        //White spaces
        if let Some(("", rest)) = current.split_once(|c: char| c == '\n') {
            line_number += 1;
            current = rest;
            continue;
        }
        if let Some(("", rest)) = current.split_once(|c: char| c.is_whitespace()) {
            current = rest;
            continue;
        }
        
        //Declarations
        if let Some(("", rest)) = current.split_once("module") {
            if !rest.starts_with(|c: char| c.is_alphanumeric()) {
                current = rest;
                output.push(LexerToken::Module);
                continue;
            }
            
        }
        if let Some(("", rest)) = current.split_once("enum") {
            if !rest.starts_with(|c: char| c.is_alphanumeric()) {
                current = rest;
                output.push(LexerToken::Enum);
                continue;
            }
        }

        //Controllflow
        if let Some(("", rest)) = current.split_once("if") {
            if !rest.starts_with(|c: char| c.is_alphanumeric()) {
                current = rest;
                output.push(LexerToken::If);
                continue;
            }
        }
        if let Some(("", rest)) = current.split_once("else") {
            if !rest.starts_with(|c: char| c.is_alphanumeric()) {
                current = rest;
                output.push(LexerToken::Else);
                continue;
            }
        }
        if let Some(("", rest)) = current.split_once("switch") {
            if !rest.starts_with(|c: char| c.is_alphanumeric()) {
                current = rest;
                output.push(LexerToken::Switch);
                continue;
            }
        }
        if let Some(("", rest)) = current.split_once("loop") {
            if !rest.starts_with(|c: char| c.is_alphanumeric()) {
                current = rest;
                output.push(LexerToken::Loop);
                continue;
            }
        }

        //Operators
        if let Some(("", rest)) = current.split_once("!") {
            current = rest;
            output.push(LexerToken::BitwiseNot);
            continue;
        }
        if let Some(("", rest)) = current.split_once("*") {
            current = rest;
            output.push(LexerToken::Multiply);
            continue;
        }
        if let Some(("", rest)) = current.split_once("/") {
            current = rest;
            output.push(LexerToken::Divide);
            continue;
        }
        if let Some(("", rest)) = current.split_once("%") {
            current = rest;
            output.push(LexerToken::Modulo);
            continue;
        }
        if let Some(("", rest)) = current.split_once("+") {
            current = rest;
            output.push(LexerToken::Plus);
            continue;
        }
        if let Some(("", rest)) = current.split_once("-") {
            current = rest;
            output.push(LexerToken::Minus);
            continue;
        }
        if let Some(("", rest)) = current.split_once(">>") {
            current = rest;
            output.push(LexerToken::BitshiftRight);
            continue;
        }
        if let Some(("", rest)) = current.split_once("<<") {
            current = rest;
            output.push(LexerToken::BitshiftLeft);
            continue;
        }
        if let Some(("", rest)) = current.split_once("and") {
            if !rest.starts_with(|c: char| c.is_alphanumeric()) {
                current = rest;
                output.push(LexerToken::BitwiseAnd);
                continue;
            }
        }
        if let Some(("", rest)) = current.split_once("or") {
            if !rest.starts_with(|c: char| c.is_alphanumeric()) {
                current = rest;
                output.push(LexerToken::BitwiseOr);
                continue;
            }
        }
        if let Some(("", rest)) = current.split_once("xor") {
            if !rest.starts_with(|c: char| c.is_alphanumeric()) {
                current = rest;
                output.push(LexerToken::BitwiseXor);
                continue;
            }
        }
        if let Some(("", rest)) = current.split_once("|") {
            current = rest;
            output.push(LexerToken::Concatinate);
            continue;
        }
        if let Some(("", rest)) = current.split_once("<=") {
            current = rest;
            output.push(LexerToken::LessThanOrEqual);
            continue;
        }
        if let Some(("", rest)) = current.split_once(">=") {
            current = rest;
            output.push(LexerToken::GreaterThanOrEqual);
            continue;
        }
        if let Some(("", rest)) = current.split_once("==") {
            current = rest;
            output.push(LexerToken::Equal);
            continue;
        }
        if let Some(("", rest)) = current.split_once("!=") {
            current = rest;
            output.push(LexerToken::NotEqual);
            continue;
        }
        if let Some(("", rest)) = current.split_once("=") {
            current = rest;
            output.push(LexerToken::Assignment);
            continue;
        }

        //Scopes
        if let Some(("", rest)) = current.split_once("{") {
            current = rest;
            output.push(LexerToken::StratScope);
            continue;
        }
        if let Some(("", rest)) = current.split_once("}") {
            current = rest;
            output.push(LexerToken::EndScope);
            continue;
        }

        //Parenthesies
        if let Some(("", rest)) = current.split_once("(") {
            current = rest;
            output.push(LexerToken::StartParenthesy);
            continue;
        }
        if let Some(("", rest)) = current.split_once(")") {
            current = rest;
            output.push(LexerToken::EndParenthesy);
            continue;
        }

        //Brackets
        if let Some(("", rest)) = current.split_once("[") {
            current = rest;
            output.push(LexerToken::StartBrackets);
            continue;
        }
        if let Some(("", rest)) = current.split_once("]") {
            current = rest;
            output.push(LexerToken::EndBrackets);
            continue;
        }

        //Braces
        if let Some(("", rest)) = current.split_once("<") {
            current = rest;
            output.push(LexerToken::StartBraces);
            continue;
        }
        if let Some(("", rest)) = current.split_once(">") {
            current = rest;
            output.push(LexerToken::EndBraces);
            continue;
        }

        //Types
        if let Some(("", rest)) = current.split_once("u") {
            if let Some(i) = rest.find(|c: char| !c.is_numeric()) {
                if i != 0 {
                    output.push(LexerToken::UnsignedInt(rest[..i].parse().unwrap()));
                    current = &rest[i..];
                    continue;
                }
            }
        }
        if let Some(("", rest)) = current.split_once("s") {
            if let Some(i) = rest.find(|c: char| !c.is_numeric()) {
                if i != 0 {
                    output.push(LexerToken::SignedInt(rest[..i].parse().unwrap()));
                    current = &rest[i..];
                    continue;
                }
            }
        }
        if let Some(("", rest)) = current.split_once("a") {
            if let Some(i) = rest.find(|c: char| !c.is_numeric()) {
                if i != 0 {
                    output.push(LexerToken::Analog(rest[..i].parse().unwrap()));
                    current = &rest[i..];
                    continue;
                }
            }
        }
        if let Some(("", rest)) = current.split_once("async") {
            if !rest.starts_with(|c: char| c.is_alphanumeric()) {
                current = rest;
                output.push(LexerToken::Async);
                continue;
            }
        }
        if let Some(("", rest)) = current.split_once("reset") {
            if !rest.starts_with(|c: char| c.is_alphanumeric()) {
                current = rest;
                output.push(LexerToken::Reset);
                continue;
            }
        }
        if let Some(("", rest)) = current.split_once("clock") {
            if !rest.starts_with(|c: char| c.is_alphanumeric()) {
                current = rest;
                output.push(LexerToken::Clock);
                continue;
            }
        }

        //Seperators
        if let Some(("", rest)) = current.split_once(",") {
            current = rest;
            output.push(LexerToken::Comma);
            continue;
        }
        if let Some(("", rest)) = current.split_once(".") {
            current = rest;
            output.push(LexerToken::Period);
            continue;
        }
        if let Some(("", rest)) = current.split_once(":") {
            current = rest;
            output.push(LexerToken::Colon);
            continue;
        }
        if let Some(("", rest)) = current.split_once(";") {
            current = rest;
            output.push(LexerToken::Semicolon);
            continue;
        }

        //Variables
        if let Some(("", rest)) = current.split_once("flip") {
            if !rest.starts_with(|c: char| c.is_alphanumeric()) {
                current = rest;
                output.push(LexerToken::Flip);
                continue;
            }
        }
        if let Some(("", rest)) = current.split_once("wire") {
            if !rest.starts_with(|c: char| c.is_alphanumeric()) {
                current = rest;
                output.push(LexerToken::Wire);
                continue;
            }
        }
        if let Some(("", rest)) = current.split_once("reg") {
            if !rest.starts_with(|c: char| c.is_alphanumeric()) {
                current = rest;
                output.push(LexerToken::Reg);
                continue;
            }
        }

        //Identifiers
        if let Some(("", rest)) = current.split_once("0x") {
            if let Some(i) = rest.find(|c: char| !c.is_alphanumeric()) {
                if let Ok(i) = usize::from_str_radix(&rest[..i], 16) {
                    output.push(LexerToken::Litteral(i));
                    current = &rest[i..];
                    continue;
                }
            }
        }
        if let Some(("", rest)) = current.split_once("0b") {
            if let Some(i) = rest.find(|c: char| !c.is_alphanumeric()) {
                if let Ok(i) = usize::from_str_radix(&rest[..i], 2) {
                    output.push(LexerToken::Litteral(i));
                    current = &rest[i..];
                    continue;
                }
            }
        }
        if let Some(("", rest)) = current.split_once("0o") {
            if let Some(i) = rest.find(|c: char| !c.is_alphanumeric()) {
                if let Ok(i) = usize::from_str_radix(&rest[..i], 8) {
                    output.push(LexerToken::Litteral(i));
                    current = &rest[i..];
                    continue;
                }
            }
        }
        if let Some(i) = current.find(|c: char| !c.is_alphanumeric()) {
            if current.starts_with(|e: char| !e.is_numeric()) {
                output.push(LexerToken::Identifier(current[..i].into()));
                current = &current[i..];
                continue;
            }
        }

        break;
    }
    if completed {
        Ok(output)
    } else {
        if let Some(i) = current.find(|c: char| !c.is_alphanumeric()) {
            Err(LexerError {
                error_token: current[..i].to_string(),
                line_number : line_number,
            })
        } else {
            Err(LexerError {
                error_token: current.to_string(),
                line_number: line_number,
            })
        }
    }
}