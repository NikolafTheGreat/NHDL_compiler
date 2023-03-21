use std::collections::HashMap;

use super::parser::{AST, ASTType, ASTExpression};

pub enum TransformationErr {

}

struct Value <'a> {
    data_type : ASTType<'a>,
    content : ASTExpression<'a>
}

struct SymbolTable <'a> {
    symbols : Vec<HashMap<&'a str, Value<'a>>>
}

impl <'a> SymbolTable <'a>{
    fn new() -> Self {
        SymbolTable { 
            symbols: vec![]
        }
    }
    fn push_scope(&mut self) {
        self.symbols.push(HashMap::new());
    }
    fn pop_scope(&mut self) {
        self.symbols.pop();
    }
    fn insert(&mut self, name : &'a str, value : Value<'a>) {
        self.symbols.last_mut().unwrap().insert(name, value);
    }
    fn remove(&mut self, name : &'a str) -> Option<Value<'a>> {
        self.symbols.last_mut().unwrap().remove(name)
    }
    fn get_value(&mut self, name : &str) -> Option<&'a Value> {
        for scope in self.symbols.iter() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }
    fn get_value_mut(&mut self, name : &str) -> Option<&'a mut Value> {
        for scope in self.symbols.iter_mut() {
            if let Some(value) = scope.get_mut(name) {
                return Some(value);
            }
        }
        None
    }
    fn update_value(&mut self, name : &'a str, f : fn(Value) -> Value) {
        if let Some(v) = self.remove(name) {
            self.symbols.last_mut().unwrap().insert(name, f(v));
        }
    }
    
}

pub fn simplify(mut input: AST) -> Result<AST, TransformationErr> {

    fn monomorphise <'a> (mut input: AST<'a>, symbol_table : &mut SymbolTable) -> Result<AST<'a>, TransformationErr> {
        Ok(input)
    }
    
    fn reduce_expressions <'a> (mut input: AST<'a>, symbol_table : &mut SymbolTable) -> Result<AST<'a>, TransformationErr> {
        Ok(input)
    }

    fn unroll_loops <'a> (mut input: AST<'a>, symbol_table : &mut SymbolTable) -> Result<AST<'a>, TransformationErr> {
        Ok(input)
    }

    fn unroll_switch <'a> (mut input: AST<'a>, symbol_table : &mut SymbolTable) -> Result<AST<'a>, TransformationErr> {
        Ok(input)
    }

    fn unroll_if <'a> (mut input: AST<'a>, symbol_table : &mut SymbolTable) -> Result<AST<'a>, TransformationErr> {
        Ok(input)
    }

    let mut symbol_table = SymbolTable::new();
    input = monomorphise(input, &mut symbol_table)?;
    input = reduce_expressions(input, &mut symbol_table)?;
    input = unroll_loops(input, &mut symbol_table)?;
    input = unroll_switch(input, &mut symbol_table)?;
    input = unroll_if(input, &mut symbol_table)?;
    
    Ok(input)
}

