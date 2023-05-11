extern crate pest;
extern crate pest_derive;
extern crate thiserror;

use std::fs;
use std::env;
use std::process;

mod parser;
mod simplification;
mod code_generation;

fn main() { 
    
    /*{
        use simplification::constant::Constant;
        let mut a = Constant::from_dec("-333");
        let mut b = Constant::from_dec("-3");
        println!(
            "a {} {:?} {}\nb {} {:?} {}\n GT {}, LT {}, GEQ {}, LEQ {}, EQ {}, CAT {}, ADD {}, SUB {}, MUL {}, DIV {}, REM {}",
            a.as_str(),
            a.clone(),
            a.is_negative(),
            b.as_str(),
            b.clone(),
            b.is_negative(),
            Constant::greater_than(a.clone(), b.clone()).as_str(),
            Constant::greater_than(b.clone(), a.clone()).as_str(),
            Constant::greater_than_or_equal(a.clone(), b.clone()).as_str(),
            Constant::greater_than_or_equal(b.clone(), a.clone()).as_str(),
            Constant::equal(a.clone(), b.clone()).as_str(),
            Constant::concatinate(a.clone(), b.clone()).as_str(),
            Constant::add(a.clone(), b.clone()).unwrap().as_str(),
            Constant::sub(a.clone(), b.clone()).unwrap().as_str(),
            Constant::mul(a.clone(), b.clone()).unwrap().as_str(),
            Constant::div(a.clone(), b.clone()).unwrap().as_str(),
            Constant::rem(a.clone(), b.clone()).unwrap().as_str(),
        );
    }*/
    
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
        path.push_str(".fir");
        path
    };
    
    let ast = match parser::parse_nhdl(&input) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("Error when parsing code: {e}");
            process::exit(1);
        }
    };
    //ast.print();

    let ast = match simplification::simplify::simplify(ast) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("Error when simplifying code: {e}");
            process::exit(1);
        }
    };

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
