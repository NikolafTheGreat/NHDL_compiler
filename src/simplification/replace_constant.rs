/*
+--------------------------------------------------------------------------------------------------------+
| This file contains the functionality needed to replcace an identifier in an expression with it's value |
+--------------------------------------------------------------------------------------------------------+
*/

use crate::parser::{ASTExpression, ASTAtom, Identifier, ASTType, ASTStatement};

pub fn replace_constant_in_expression(
    expression : &mut ASTExpression,
    constant_name : &Identifier,
    constant_value : &ASTAtom,
    identifier_list : &Vec<String>
) {
    match expression {
        ASTExpression::Atom(atom) => {
            if let Some(next) = replace_constant_in_atom(atom, constant_name, constant_value, identifier_list) {
                *atom = next;
            }
        },
        ASTExpression::BinaryOperation {
            lhs,
            operation:_,
            rhs 
        } => {
            replace_constant_in_expression(lhs, constant_name, constant_value, identifier_list);
            replace_constant_in_expression(rhs, constant_name, constant_value, identifier_list);
        },
        ASTExpression::Bundle(inner) => {
            for (_, expression) in inner.iter_mut() {
                replace_constant_in_expression(expression, constant_name, constant_value, identifier_list)
            }
        },
        ASTExpression::Function {
            function:_,
            arguments
        } => {
            for expression in arguments.iter_mut() {
                replace_constant_in_expression(expression, constant_name, constant_value, identifier_list)
            }
        },
        ASTExpression::UnaryOperation {
            operation:_,
            content
        } => {
            replace_constant_in_expression(content, constant_name, constant_value, identifier_list)
        }
    }
}

pub fn replace_constant_in_type(
    type_ : &mut ASTType,
    constant_name : &Identifier,
    constant_value : &ASTAtom,
    identifier_list : &Vec<String>
) {
    match type_ {
        ASTType::Vector {
            type_,
            size
        } => {
            replace_constant_in_type(type_, constant_name, constant_value, identifier_list);
            replace_constant_in_expression(size, constant_name, constant_value, identifier_list)
        },
        ASTType::Bundle(inner) => {
            for (_, _, expression) in inner.iter_mut() {
                replace_constant_in_type(expression, constant_name, constant_value, identifier_list)
            }
        },
        ASTType::Signed(width) => {
            replace_constant_in_expression(width, constant_name, constant_value, identifier_list)
        },
        ASTType::Unsigned(width) => {
            replace_constant_in_expression(width, constant_name, constant_value, identifier_list)
        },
        _ => ()
    }
}

pub fn replace_constant_in_statement(
    statement : &mut ASTStatement,
    constant_name : &Identifier,
    constant_value : &ASTAtom,
    identifier_list : &Vec<String>
) {
    match statement {
        ASTStatement::ConstantDeclaration {
            constant_name:_,
            type_,
            value
        } => {
            replace_constant_in_type(type_, constant_name, constant_value, identifier_list);
            replace_constant_in_expression(value, constant_name, constant_value, identifier_list);
        },
        ASTStatement::If {
            condition,
            true_body,
            false_body
        } => {
            replace_constant_in_expression(condition, constant_name, constant_value, identifier_list);
            replace_constant_in_statement(true_body, constant_name, constant_value, identifier_list);
            if let Some(false_body) = false_body {
                replace_constant_in_statement(false_body, constant_name, constant_value, identifier_list)
            }
        }
        ASTStatement::Loop {
            variable_name:_,
            from,
            to,
            body
        } => {
            replace_constant_in_expression(from, constant_name, constant_value, identifier_list);
            replace_constant_in_expression(to, constant_name, constant_value, identifier_list);
            replace_constant_in_statement(body, constant_name, constant_value, identifier_list);
        },
        ASTStatement::ModuleDeclaration {
            module_name:_,
            generic_names:_,
            io,
            body
        } => {
            replace_constant_in_type(io, constant_name, constant_value, identifier_list);
            replace_constant_in_statement(body, constant_name, constant_value, identifier_list);
        },
        ASTStatement::ModuleInstance {
            instance_name:_,
            type_:_,
            generic_values
        } => {
            for generic_value in generic_values.iter_mut() {
                replace_constant_in_expression(generic_value, constant_name, constant_value, identifier_list);
            }
        },
        ASTStatement::RegisterInstance {
            instance_name:_,
            clock,
            reset,
            initial,
            type_
        } => {
            replace_constant_in_expression(clock, constant_name, constant_value, identifier_list);
            if let Some(reset) = reset {
                replace_constant_in_expression(reset, constant_name, constant_value, identifier_list);
                if let Some(initial) = initial {
                    replace_constant_in_expression(initial, constant_name, constant_value, identifier_list);
                }
            }
            replace_constant_in_type(type_, constant_name, constant_value, identifier_list);
        },
        ASTStatement::Scope {
            internals
        } => {
            for statement in internals.iter_mut() {
                replace_constant_in_statement(statement, constant_name, constant_value, identifier_list);
            }
        },
        ASTStatement::StrongConnection {
            lhs,
            rhs
        } => {
            replace_constant_in_atom(lhs, constant_name, constant_value, identifier_list);
            replace_constant_in_expression(rhs, constant_name, constant_value, identifier_list);
        }
        ASTStatement::WeakConnection {
            lhs,
            rhs
        } => {
            replace_constant_in_atom(lhs, constant_name, constant_value, identifier_list);
            replace_constant_in_expression(rhs, constant_name, constant_value, identifier_list);
        }
        ASTStatement::WireInstance {
            instance_name:_,
            type_
        } => {
            replace_constant_in_type(type_, constant_name, constant_value, identifier_list);
        },
        ASTStatement::Node { 
            name:_, 
            value 
        } => {
            replace_constant_in_expression(value, constant_name, constant_value, identifier_list);
        },
        _ => ()
    }
}

pub fn replace_constant_in_atom(
    atom : &mut ASTAtom,
    constant_name : &Identifier,
    constant_value : &ASTAtom,
    identifier_list : &Vec<String>
) -> Option<Box<ASTAtom>>{
    match atom {
        ASTAtom::Target(name) => {
            if name.len() == 1 {
                let (name, index) = name.first().unwrap();
                if index.is_none() && name.eq(constant_name, identifier_list){
                    return Some(Box::new(constant_value.clone()));
                }
            }
            for (_, index) in name.iter_mut() {
                if let Some(index) = index {
                    replace_constant_in_expression(index, constant_name, constant_value, identifier_list);
                }
            }
        },
        ASTAtom::Number {
            signed:_,
            value:_,
            width
        } => {
            if let Some(width) = width {
                replace_constant_in_expression(width, constant_name, constant_value, identifier_list);
            }
        },
        _ => ()
    }
    None
}