use regex::Regex;
use super::general;

#[derive(Debug)]
pub enum NHDLToken {
    // Declarations
    Component,
    Enum,

    // Controllflow
    If,
    Else,
    Switch,
    Loop,
    For,

    // Operators
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
    Range,

    // Scopes ( "{" and "}" )
    StratScope,
    EndScope,

    // Parenthesies ( "(" and ")" )
    StartParenthesy,
    EndParenthesy,

    // Brackets ( "[" and "]" )
    StartBrackets,
    EndBrackets,

    // Braces ( "<" and ">" )
    StartBraces,
    EndBraces,

    // Types
    UnsignedInt(usize),
    SignedInt(usize),
    Analog(usize),
    Reset,
    Clock,
    
    // Seperators
    Comma,
    Period,
    Colon,
    Semicolon,

    // Variables
    Wire,
    Reg,

    // Modifiers
    Flip,
    Async,
    LengthOf,

    // Identifiers
    Empty,
    Litteral(usize),
    Identifier(String),
}

pub fn lex (input : &str) -> Result<Vec<general::LexerToken<NHDLToken>>, general::LexerError> {
    let rules = vec![
        
        // Ignoring whitespace and comments (both single line with // and multi line with /* and */)
        general::Rule {
            pattern : Regex::new(r"\A(\s|//.*?(\n|\z)|/\*(?s).*?\*/)").unwrap(),
            result : Box::new(|_| None),
        },

        // Declarations
        general::Rule {
            pattern : Regex::new(r"\Acomponent\b").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Component)),
        },
        general::Rule {
            pattern : Regex::new(r"\Aenum\b").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Enum)),
        },

        // Controllflow
        general::Rule {
            pattern : Regex::new(r"\Aif\b").unwrap(),
            result : Box::new(|_| Some(NHDLToken::If)),
        },
        general::Rule {
            pattern : Regex::new(r"\Aelse\b").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Else)),
        },
        general::Rule {
            pattern : Regex::new(r"\Aswitch\b").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Switch)),
        },
        general::Rule {
            pattern : Regex::new(r"\Aloop\b").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Loop)),
        },
        general::Rule {
            pattern : Regex::new(r"\Afor\b").unwrap(),
            result : Box::new(|_| Some(NHDLToken::For)),
        },

        // Operators
        general::Rule {
            pattern : Regex::new(r"\A\*").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Multiply)),
        },
        general::Rule {
            pattern : Regex::new(r"\A/").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Divide)),
        },
        general::Rule {
            pattern : Regex::new(r"\A%").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Modulo)),
        },
        general::Rule {
            pattern : Regex::new(r"\A\+").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Plus)),
        },
        general::Rule {
            pattern : Regex::new(r"\A-").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Minus)),
        },
        general::Rule {
            pattern : Regex::new(r"\A>>").unwrap(),
            result : Box::new(|_| Some(NHDLToken::BitshiftRight)),
        },
        general::Rule {
            pattern : Regex::new(r"\A<<").unwrap(),
            result : Box::new(|_| Some(NHDLToken::BitshiftLeft)),
        },
        general::Rule {
            pattern : Regex::new(r"\Aand\b").unwrap(),
            result : Box::new(|_| Some(NHDLToken::BitwiseAnd)),
        },
        general::Rule {
            pattern : Regex::new(r"\Aor\b").unwrap(),
            result : Box::new(|_| Some(NHDLToken::BitwiseOr)),
        },
        general::Rule {
            pattern : Regex::new(r"\Axor\b").unwrap(),
            result : Box::new(|_| Some(NHDLToken::BitwiseXor)),
        },
        general::Rule {
            pattern : Regex::new(r"\A\|").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Concatinate)),
        },
        general::Rule {
            pattern : Regex::new(r"\A<=").unwrap(),
            result : Box::new(|_| Some(NHDLToken::LessThanOrEqual)),
        },
        general::Rule {
            pattern : Regex::new(r"\A>=").unwrap(),
            result : Box::new(|_| Some(NHDLToken::GreaterThanOrEqual)),
        },
        general::Rule {
            pattern : Regex::new(r"\A==").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Equal)),
        },
        general::Rule {
            pattern : Regex::new(r"\A!=").unwrap(),
            result : Box::new(|_| Some(NHDLToken::NotEqual)),
        },
        general::Rule {
            pattern : Regex::new(r"\A=").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Assignment)),
        },
        general::Rule {
            pattern : Regex::new(r"\A!").unwrap(),
            result : Box::new(|_| Some(NHDLToken::BitwiseNot)),
        },
        general::Rule {
            pattern : Regex::new(r"\A::").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Range)),
        },

        // Scopes
        general::Rule {
            pattern : Regex::new(r"\A\{").unwrap(),
            result : Box::new(|_| Some(NHDLToken::StratScope)),
        },
        general::Rule {
            pattern : Regex::new(r"\A\}").unwrap(),
            result : Box::new(|_| Some(NHDLToken::EndScope)),
        },

        // Parenthesies
        general::Rule {
            pattern : Regex::new(r"\A\(").unwrap(),
            result : Box::new(|_| Some(NHDLToken::StartParenthesy)),
        },
        general::Rule {
            pattern : Regex::new(r"\A\)").unwrap(),
            result : Box::new(|_| Some(NHDLToken::EndParenthesy)),
        },

        // Brackets
        general::Rule {
            pattern : Regex::new(r"\A\[").unwrap(),
            result : Box::new(|_| Some(NHDLToken::StartBrackets)),
        },
        general::Rule {
            pattern : Regex::new(r"\A\]").unwrap(),
            result : Box::new(|_| Some(NHDLToken::EndBrackets)),
        },

        // Braces
        general::Rule {
            pattern : Regex::new(r"\A<").unwrap(),
            result : Box::new(|_| Some(NHDLToken::StartBraces)),
        },
        general::Rule {
            pattern : Regex::new(r"\A>").unwrap(),
            result : Box::new(|_| Some(NHDLToken::EndBraces)),
        },

        // Types
        general::Rule {
            pattern : Regex::new(r"\Au\((\d+)\)\b").unwrap(),
            result : Box::new(|c|
                Some(NHDLToken::UnsignedInt(
                    c.get(1).unwrap().as_str().parse().unwrap()
                )
            )),
        },
        general::Rule {
            pattern : Regex::new(r"\As\((\d+)\)\b").unwrap(),
            result : Box::new(|c|
                Some(NHDLToken::SignedInt(
                    c.get(1).unwrap().as_str().parse().unwrap()
                )
            )),
        },
        general::Rule {
            pattern : Regex::new(r"\Aa\((\d+)\)\b").unwrap(),
            result : Box::new(|c|
                Some(NHDLToken::Analog(
                    c.get(1).unwrap().as_str().parse().unwrap()
                )
            )),
        },
        general::Rule {
            pattern : Regex::new(r"\Areset\b").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Reset)),
        },
        general::Rule {
            pattern : Regex::new(r"\Aclock\b").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Clock)),
        },

        // Seperators
        general::Rule {
            pattern : Regex::new(r"\A,").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Comma)),
        },
        general::Rule {
            pattern : Regex::new(r"\A\.").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Period)),
        },
        general::Rule {
            pattern : Regex::new(r"\A:").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Colon)),
        },
        general::Rule {
            pattern : Regex::new(r"\A;").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Semicolon)),
        },

        // Variables
        general::Rule {
            pattern : Regex::new(r"\Awire\b").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Wire)),
        },
        general::Rule {
            pattern : Regex::new(r"\Areg\b").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Reg)),
        },

        // Modifiers
        general::Rule {
            pattern : Regex::new(r"\Aflip\b").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Flip)),
        },
        general::Rule {
            pattern : Regex::new(r"\Aasync\b").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Async)),
        },
        general::Rule {
            pattern : Regex::new(r"\Alength_of\b").unwrap(),
            result : Box::new(|_| Some(NHDLToken::LengthOf)),
        },

        // Identifiers
        general::Rule {
            pattern : Regex::new(r"\A_\b").unwrap(),
            result : Box::new(|_| Some(NHDLToken::Empty)),
        },
        general::Rule {
            pattern : Regex::new(r"\A((?P<dec>\d+\b)|0b(?P<bin>[01]+)\b|0o(?P<oct>[0-8]+)\b|0x(?P<hex>[0-9a-fA-F]+)\b)").unwrap(),
            result : Box::new(|c| {
                if let Some(dec) = c.name("dec") {
                    return Some(
                        NHDLToken::Litteral(dec.as_str().parse().unwrap())
                    );
                }
                if let Some(bin) = c.name("bin") {
                    return Some(
                        NHDLToken::Litteral(usize::from_str_radix(bin.as_str(), 2).unwrap())
                    );
                }
                if let Some(oct) = c.name("oct") {
                    return Some(
                        NHDLToken::Litteral(usize::from_str_radix(oct.as_str(), 8).unwrap())
                    );
                }
                if let Some(hex) = c.name("hex") {
                    return Some(
                        NHDLToken::Litteral(usize::from_str_radix(hex.as_str(), 16).unwrap())
                    );
                }
                None
            }),
        },
        general::Rule {
            pattern : Regex::new(r"\A[^0-9].*?\b").unwrap(),
            result : Box::new(|c| Some(NHDLToken::Identifier(c.get(0).unwrap().as_str().to_string()))),
        },
    // Identifier(String),
    ];
    general::lex(input, rules)
}