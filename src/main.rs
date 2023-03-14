extern crate pest;
extern crate pest_derive;

use std::fs;
use std::env;
use std::process;

mod parser;
mod ast_verification;

fn main() { 
    let path = if let Some(s) = env::args().nth(1) { s } else {
        eprintln!("Not enough arguments provided. Please provide the path to the source.");
        process::exit(1);
    };

    let input = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error when opening file: {e}");
            process::exit(1);
        }
    };
    
    let result = parser::parse_nhdl(&input);
    result.print();
}
