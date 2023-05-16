/*
+--------------------------------------------------------------------------------------------+
| An unused bit of functionality used to rename a certain identifier in some part of the AST |
+--------------------------------------------------------------------------------------------+
*/

use crate::parser::{ASTStatement, ASTAtom, ASTType, ASTExpression, Identifier};

pub fn rename(ast : &mut ASTStatement, identifier_list : &mut Vec<String>, target : &str, mut name : String) {

    fn rename_identifier(
        identifier : &mut Identifier,
        identifier_list : &mut Vec<String>,
        target : &str,
        index : usize
    ) {
        match identifier {
            Identifier::Evaluated(i) => {
                if identifier_list.get(*i).unwrap() == target {
                    *i = index;
                }
            }
            Identifier::UnEvaluated(_) => unreachable!(),
        }
    }

    fn rename_statement(
        tree : &mut ASTStatement,
        identifier_list : &mut Vec<String>,
        target : &str,
        index : usize
    ) {
        match tree {
            ASTStatement::RegisterInstance {
                instance_name,
                clock,
                reset,
                initial,
                type_
            } => {
                rename_identifier(instance_name, identifier_list, target, index);
                rename_expression(clock, identifier_list, target, index);
                if let Some(reset) = reset {
                    rename_expression(reset, identifier_list, target, index);
                    if let Some(initial) = initial {
                        rename_expression(initial, identifier_list, target, index);
                    }
                }
                rename_type(type_, identifier_list, target, index);
            },
            ASTStatement::WireInstance {
                instance_name,
                type_
            } => {
                rename_identifier(instance_name, identifier_list, target, index);
                rename_type(type_, identifier_list, target, index);
            },
            ASTStatement::ModuleInstance {
                instance_name,
                type_,
                generic_values
            } => {
                rename_identifier(instance_name, identifier_list, target, index);
                rename_identifier(type_, identifier_list, target, index);
                for generic in generic_values.iter_mut() {
                    rename_expression(generic, identifier_list, target, index);
                }
            },
            ASTStatement::Scope {
                internals
            } => internals.iter_mut().for_each(
                |statement| rename_statement(statement, identifier_list, target, index)
            ),
            ASTStatement::ModuleDeclaration {
                module_name,
                generic_names : _,
                io ,
                body
            } => {
                rename_identifier(module_name, identifier_list, target, index);
                rename_type(io, identifier_list, target, index);
                rename_statement(body, identifier_list, target, index);
            },
            ASTStatement::EnumDeclaration {
                enum_name,
                variant_names : _
            } => {
                rename_identifier(enum_name, identifier_list, target, index);
            }
            ASTStatement::ConstantDeclaration {
                constant_name,
                type_,
                value
            } => {
                rename_identifier(constant_name, identifier_list, target, index);
                rename_type(type_, identifier_list, target, index);
                rename_expression(value, identifier_list, target, index);
            }
            ASTStatement::StrongConnection {
                lhs,
                rhs
            } => {
                rename_atom(lhs, identifier_list, target, index);
                rename_expression(rhs, identifier_list, target, index);
            },
            ASTStatement::WeakConnection {
                lhs,
                rhs
            } => {
                rename_atom(lhs, identifier_list, target, index);
                rename_expression(rhs, identifier_list, target, index);
            },
            ASTStatement::If {
                condition,
                true_body,
                false_body
            } => {
                rename_expression(condition, identifier_list, target, index);
                rename_statement(true_body, identifier_list, target, index);
                if let Some(false_body) = false_body {
                    rename_statement(false_body, identifier_list, target, index);
                }
            },
            ASTStatement::Switch {
                condition,
                paths
            } => {
                rename_expression(condition, identifier_list, target, index);
                for (_, statement) in paths.iter_mut() {
                    rename_statement(statement, identifier_list, target, index);
                }
            },
            ASTStatement::Loop {
                variable_name: _ ,
                from,
                to,
                body
            } => {
                rename_expression(from, identifier_list, target, index);
                rename_expression(to, identifier_list, target, index);
                rename_statement(body, identifier_list, target, index);
            },
            ASTStatement::Node { 
                name, 
                value 
            } => {
                rename_identifier(name, identifier_list, target, index);
                rename_expression(value, identifier_list, target, index);
            }
        }
    }

    fn rename_atom(
        tree : &mut ASTAtom,
        identifier_list : &mut Vec<String>,
        target : &str,
        index : usize
    ) {
        match tree {
            ASTAtom::Target(v) => {
                let mut lhs = v.iter_mut();
                let (identifier, vector_index) = lhs.next().unwrap();
                rename_identifier(identifier, identifier_list, target, index);
                if let Some(i) = vector_index {
                    rename_expression(i, identifier_list, target, index);
                }
                for (_, vector_index) in lhs {
                    if let Some(i) = vector_index {
                        rename_expression(i, identifier_list, target, index);
                    }
                }
            },
            _ => ()
        }
    }

    fn rename_type(
        tree : &mut ASTType,
        identifier_list : &mut Vec<String>,
        target : &str,
        index : usize
    ) {
        match tree {
            ASTType::Vector {
                type_,
                size
            } => {
                rename_type(type_, identifier_list, target, index);
                rename_expression(size, identifier_list, target, index);
            },
            ASTType::Unsigned(width) => {
                rename_expression(width, identifier_list, target, index);
            },
            ASTType::Signed(width) => {
                rename_expression(width, identifier_list, target, index);
            },
            ASTType::Bundle(content) => {
                for (_, name, type_) in content.iter_mut() {
                    rename_identifier(name, identifier_list, target, index);
                    rename_type(type_, identifier_list, target, index);
                }
            },
            ASTType::Enum (name) => {
                rename_identifier(name, identifier_list, target, index)
            },
            _ => ()
        }
    }

    fn rename_expression(
        tree : &mut ASTExpression,
        identifier_list : &mut Vec<String>,
        target : &str,
        index : usize
    ) {
        match tree {
            ASTExpression::BinaryOperation {
                lhs,
                operation:_,
                rhs
            } => {
                rename_expression(lhs, identifier_list, target, index);
                rename_expression(rhs, identifier_list, target, index);
            },
            ASTExpression::UnaryOperation {
                operation:_,
                content
            } => {
                rename_expression(content, identifier_list, target, index);
            },
            ASTExpression::Bundle (elements) => {
                for (_, exression) in elements.iter_mut() {
                    rename_expression(exression, identifier_list, target, index);
                }
            },
            ASTExpression::Function {
                function:_,
                arguments
            } => {
                for exression in arguments.iter_mut() {
                    rename_expression(exression, identifier_list, target, index);
                }
            },
            ASTExpression::Atom (atom) => {
                rename_atom(atom, identifier_list, target, index);
            },
        }
    }

    loop {
        let mut already_defined = false;
        for i in identifier_list.iter() {
            if *i == name {
                already_defined = true;
                break;
            }
        }
        if already_defined {
            name.push('_');
        } else {
            break;
        }
    }
    
    identifier_list.push(name);
    rename_statement(ast, identifier_list, target, identifier_list.len()-1);
}