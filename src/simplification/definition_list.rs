/*
+------------------------------------------------------------------------------------------------------------------------------+
| This file contains the symbol table used for the syntactic analysis and the simplification of higher level language features |
+------------------------------------------------------------------------------------------------------------------------------+
*/

use thiserror::Error;

use crate::parser::{ASTAtom, ASTExpression, ASTStatement, Identifier, Number};
use super::{replace_constant, evaluate_constant_expression, constant::{self, Constant}};

#[derive(Debug, Error)]
pub enum DefinitonListError {
    #[error("Element definded multipe times in single scope")]
    DoubleDefined,
    #[error("Undefined element")]
    Undefined,
    #[error("Wrong number of generic paramiters")]
    WrongNumberOfGenerics,
    #[error("Constant has the wrong type")]
    WrongType,
    #[error("Error when evaluating constant expression : {0}")]
    ConstantEvalError(evaluate_constant_expression::ConstantEvalError),
}

pub struct DefinitonList {
    pub modules : Vec<(ASTStatement, usize, Vec<(Identifier, Vec<constant::Constant>)>)>,
    pub constants : Vec<(Identifier, ASTAtom)>,
    pub enums : Vec<ASTStatement>,
    pub nodes : Vec<Vec<(usize, usize, usize)>>
}

impl  DefinitonList {
    pub fn new() -> Self {
        DefinitonList {
            modules: Vec::new(),
            constants: Vec::new(),
            enums: Vec::new(),
            nodes: Vec::new(),
        }
    }
    pub fn insert_module(
        &mut self,
        module: ASTStatement,
        identifier_list : &Vec<String>
    ) -> Result<(), DefinitonListError> {
        for (i,_, _) in self.modules.iter() {
            if get_module_name(i).eq(get_module_name(&module), identifier_list) {
                return Err(DefinitonListError::DoubleDefined);
            }
        }
        self.modules.push((module, 0, Vec::new()));
        Ok(())
    }

    pub fn instanciate_module (
        &mut self,
        module: Identifier,
        generics: Vec<Constant>,
        identifier_list : &mut Vec<String>
    ) -> Result<(Identifier, Option<ASTStatement>), DefinitonListError> {
        for (i, n, existing) in self.modules.iter_mut() {
            if get_module_name(i).eq(&module, identifier_list) {

                let mut out = i.clone();
                if let ASTStatement::ModuleDeclaration {
                    module_name,
                    generic_names,
                    io,
                    body
                } = &mut out {

                    if generic_names.len() != generics.len() {
                        return Err(DefinitonListError::WrongNumberOfGenerics);
                    }

                    for (name, i) in existing.iter() {
                        let mut equal = true;
                        for (a, b) in generics.iter().zip(i.iter()) {
                            if !constant::Constant::equal(a.clone(), b.clone()).value[0] {
                                equal = false;
                                break;
                            }
                        }
                        if equal {
                            return Ok((name.clone(), None));
                        }
                    }

                    let mut new_name = module_name.as_str(identifier_list).to_string();
                    new_name.push_str(&format!("__INSTANCE{}__", n));
                    *n += 1;
                    while identifier_list.contains(&new_name) {
                        new_name.push('_');
                    }
                    identifier_list.push(new_name);
                    *module_name = Identifier::Evaluated(identifier_list.len() - 1);
                    
                    existing.push((module_name.clone(), generics.clone()));

                    for (mut value, (name, _type)) in generics.into_iter().zip(generic_names.iter()) {
                        let value = ASTAtom::Number {
                            signed: value.signed,
                            value: value.as_number(),
                            width: Some(Box::new(ASTExpression::Atom(Box::new(
                                ASTAtom::Number {
                                    signed: false,
                                    value: Number::DecNumber(format!("{}", value.value.len())), 
                                    width: None
                                }))
                            ))
                        };
                        replace_constant::replace_constant_in_statement(body, name, &value , identifier_list);
                        replace_constant::replace_constant_in_type(io, name, &value, identifier_list);
                    }

                    *generic_names = Vec::new();
                }
                return Ok((
                    Identifier::Evaluated(identifier_list.len() - 1), 
                    Some(out)
                ));
            }
        }
        println!("Error1");
        Err(DefinitonListError::Undefined)
    }

    pub fn insert_constant (
        &mut self,
        name: Identifier,
        value: ASTAtom,
        identifier_list : &mut Vec<String>
    ) -> Result<(), DefinitonListError> {
        for (name_, _) in self.constants.iter() {
            if name_.eq(&name, identifier_list) {
                return Err(DefinitonListError::DoubleDefined);
            }
        }
        self.constants.push((name, value));
        Ok(())
    }
    
    pub fn insert_enum (
        &mut self,
        enum_: ASTStatement,
        identifier_list : &mut Vec<String>
    ) -> Result<(), DefinitonListError> {
        for _enum in self.enums.iter() {
            if let ASTStatement::EnumDeclaration {
                enum_name,
                variant_names:_
            } = _enum {
                if enum_name.eq(if let ASTStatement::EnumDeclaration {
                    enum_name,
                    variant_names:_
                } = _enum {
                    enum_name
                } else {
                    unreachable!()
                }, identifier_list) {
                    return Err(DefinitonListError::DoubleDefined);
                }
            }
        }
        self.enums.push(enum_);
        Ok(())
    }

    pub fn get_enum_variant (
        &self,
        kind_name : &Identifier,
        variant_name : &Identifier,
        identifier_list : &mut Vec<String>
    ) -> Result<usize, DefinitonListError> {
        
        for i in self.enums.iter() {
            if let ASTStatement::EnumDeclaration {
                enum_name,
                variant_names
            } = i {
                if enum_name.eq(kind_name, identifier_list) {
                    for (i, variant_name_) in variant_names.iter().enumerate() {
                        if variant_name.eq(variant_name_, identifier_list) {
                            return Ok(i)
                        }
                    }
                }
            }
        }
        Err(DefinitonListError::Undefined)
    }

    pub fn get_enum_bit_width (
        &self,
        kind_name : &Identifier,
        identifier_list : &mut Vec<String>
    ) -> Result<usize, DefinitonListError> {
        
        for i in self.enums.iter() {
            if let ASTStatement::EnumDeclaration {
                enum_name,
                variant_names
            } = i {
                if enum_name.eq(kind_name, identifier_list) {
                    return Ok((usize::BITS - (variant_names.len()-1).leading_zeros()) as usize)
                }
            }
        }
        Err(DefinitonListError::Undefined)
    }

    pub fn push_node_evironment(&mut self) {
        self.nodes.push(Vec::new())
    }

    pub fn pop_node_evironment(&mut self) {
        self.nodes.pop();
    }

    pub fn set_newest_node(
        &mut self,
        target: usize,
        identifier_list : &mut Vec<String>
    ) -> usize {
        let mut name = identifier_list[target].clone();
        let evironment = self.nodes.last_mut().unwrap();
        for (original, last, count) in evironment.iter_mut() {
            if *original == target {
                *last = identifier_list.len();
                identifier_list.push({
                    name.push_str(&format!("{}", count));
                    *count += 1;
                    loop {
                        let mut exists = false;
                        for i in identifier_list.iter() {
                            if *i == name {
                                exists = true;
                                break;
                            }
                        }
                        if exists {
                            name.push('_');
                        } else {
                            break name;
                        }
                    }
                });
                return *last;
            }
        }
        evironment.push((target, target, 0));
        target
    }

    pub fn get_node_index (
        &self,
        target: usize,
    ) -> Option<usize> {
        let evironment = self.nodes.last().unwrap();
        for (original, last, _) in evironment.iter() {
            if *original == target {
                return Some(*last);
            }
        }
        None
    }

}

fn get_module_name<'a, 'b>(module : &'b ASTStatement) -> &'b Identifier{
    if let ASTStatement::ModuleDeclaration {
        module_name,
        generic_names:_,
        io:_,
        body:_
    } = module {
        module_name
    } else {
        unreachable!();
    }
}