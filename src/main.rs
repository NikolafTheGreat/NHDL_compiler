extern crate pest;
extern crate pest_derive;
extern crate thiserror;

use std::fs;
use std::env;
use std::process;

mod parser;
mod semantic_analysis;
mod code_generation;

fn main() { 
    let in_path = if let Some(s) = env::args().nth(1) { s } else {
        eprintln!("Not enough arguments provided. Please provide the path to the source.");
        process::exit(1);
    };

    let input = match fs::read_to_string(&in_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error when opening file: {e}");
            process::exit(1);
        }
    };

    let out_path = if let Some(s) = env::args().nth(2) { s } else {
        let mut path = in_path.rsplit_once('.').unwrap().0.to_string();
        path.push_str(".firrtl");
        path
    };
    
    let ast = parser::parse_nhdl(&input);
    
    let output = match code_generation::generate_code(ast) {
        Ok(str) => str,
        Err(e) => {
            eprintln!("Error when generating code: {e}");
            process::exit(1);
        }
    };

    match fs::write(out_path, output) {
        Ok(_) => (),
        Err(e) => {
            eprintln!("Error when writing output to file: {e}");
            process::exit(1);
        }
    }
    
}
