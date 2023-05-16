/*
+----------------------------------------------------------------------------------------------------------------------------------+
| This file contains the functionality to simplify the complex language features into something that can be translated into FIRRTL |
+----------------------------------------------------------------------------------------------------------------------------------+
*/

use crate::parser::{AST, ASTStatement, ASTType, Identifier, ASTExpression, ASTAtom, Number, BinaryOperation};
use super::{definition_list::{DefinitonList, DefinitonListError}, replace_constant::replace_constant_in_statement, evaluate_constant_expression::{evaluate_constant_expression, ConstantEvalError}, constant::Constant};

pub fn simplify(mut ast : AST) -> Result<AST, DefinitonListError>{

    let mut definition_list = DefinitonList::new();

    for mut statement in ast.internals {
        for (name, value) in definition_list.constants.iter() {
            replace_constant_in_statement(&mut statement, name, value, &ast.identifier_list)
        }
        match statement {
            ASTStatement::ConstantDeclaration {
                constant_name,
                type_,
                value
            } => {
                let mut value = evaluate_constant_expression(*value).map_err(|e| DefinitonListError::ConstantEvalError(e))?;
                if !match *type_ {
                    ASTType::Signed(width) => {
                        let width = evaluate_constant_expression(*width)
                        .map_err(|e| DefinitonListError::ConstantEvalError(e))?.as_usize()
                        .map_err(|e|DefinitonListError::ConstantEvalError(ConstantEvalError::ConstantError(e)))?;
                        let out = value.signed && value.value.len() <= width;
                        value.extend(width);
                        out
                    },
                    ASTType::Unsigned(width) => {
                        let width = evaluate_constant_expression(*width)
                        .map_err(|e| DefinitonListError::ConstantEvalError(e))?.as_usize()
                        .map_err(|e|DefinitonListError::ConstantEvalError(ConstantEvalError::ConstantError(e)))?;
                        let out = !value.signed && value.value.len() <= width;
                        value.extend(width);
                        out
                    },
                    _ => unreachable!()
                } {
                    return Err(DefinitonListError::WrongType);
                }
                definition_list.insert_constant(constant_name, value.as_atom(), &mut ast.identifier_list)?;
            },
            ASTStatement::EnumDeclaration {
                enum_name,
                variant_names
            } => {
                definition_list.insert_enum( ASTStatement::EnumDeclaration {
                    enum_name: enum_name,
                    variant_names: variant_names
                }, &mut ast.identifier_list)?;
            },
            ASTStatement::ModuleDeclaration {
                module_name,
                generic_names,
                io,
                body
            } => {
                definition_list.insert_module(ASTStatement::ModuleDeclaration {
                    module_name: module_name,
                    generic_names: generic_names,
                    io: io,
                    body: body
                }, &mut ast.identifier_list)?;
            },
            _ => unreachable!()
        }
    }

    let mut new_internals = Vec::new();

    ast.top_module = instanciate_module(ast.top_module.clone(), Vec::new(), &mut definition_list, &mut new_internals, &mut ast.identifier_list)?;

    ast.internals = new_internals;

    Ok(ast)
}

fn instanciate_module (
    next_module_name : Identifier,
    next_module_generics : Vec<Constant>,
    definition_list : & mut DefinitonList,
    new_internals : & mut Vec<ASTStatement>,
    identifier_list : & mut Vec<String>
) -> Result<Identifier, DefinitonListError> {
    let (name, next_module) = definition_list.instanciate_module(next_module_name, next_module_generics, identifier_list)?;
    if let Some(next_module) = next_module {
        definition_list.push_node_evironment();
        let mut next_module = simplify_statement(next_module, definition_list, new_internals, identifier_list)?;
        definition_list.pop_node_evironment();
        new_internals.append(&mut next_module);
    }
    Ok(name)
}

fn simplify_statement (
    statement : ASTStatement, 
    definition_list : & mut DefinitonList,
    new_internals : & mut Vec<ASTStatement>,
    identifier_list : & mut Vec<String>
) -> Result<Vec<ASTStatement>, DefinitonListError> {
    let mut out = Vec::new();
    match statement {
        ASTStatement::ModuleDeclaration {
            module_name,
            generic_names,
            io,
            body 
        } => {
            let io = simplify_type(*io, definition_list, new_internals, identifier_list)?;
            let body = simplify_statement(*body, definition_list, new_internals, identifier_list)?;
            let body = collapse_statements(body);
            
            out.push(ASTStatement::ModuleDeclaration {
                module_name: module_name,
                generic_names: generic_names, 
                io: Box::new(io),
                body: Box::new(body)
            })
        }
        ASTStatement::RegisterInstance {
            instance_name,
            clock,
            reset,
            initial,
            type_,
        } => {
            let clock = simplify_expression(*clock, definition_list, new_internals, identifier_list)?;
            let (reset, initial) = if let Some(reset) = reset {(
                Some(Box::new(simplify_expression(*reset, definition_list, new_internals, identifier_list)?)),
                Some(Box::new(simplify_expression(*initial.unwrap(), definition_list, new_internals, identifier_list)?))
            )} else {(None, None)};
            let type_ = simplify_type(*type_, definition_list, new_internals, identifier_list)?;
            out.push(ASTStatement::RegisterInstance {
                instance_name: instance_name,
                clock: Box::new(clock),
                reset: reset,
                initial: initial,
                type_: Box::new(type_)
            });
        },
        ASTStatement::WireInstance {
            instance_name,
            type_
        } => {
            let type_ = simplify_type(*type_, definition_list, new_internals, identifier_list)?;
            out.push(ASTStatement::WireInstance {
                instance_name: instance_name,
                type_ : Box::new(type_)
            })
        },
        ASTStatement::ModuleInstance {
            instance_name,
            type_,
            generic_values,
        } => {
            let generic_values = {
                let mut out = Vec::new();
                for generic in generic_values {
                    out.push(evaluate_constant_expression(generic).map_err(|e| DefinitonListError::ConstantEvalError(e))?);
                }
                out
            };
            let type_ = instanciate_module(
                type_, 
                generic_values, 
                definition_list, 
                new_internals, 
                identifier_list
            )?;
            out.push(ASTStatement::ModuleInstance { 
                instance_name: instance_name, 
                type_: type_, 
                generic_values: Vec::new() 
            })
        },
        ASTStatement::Scope {
            internals
        } => {
            let mut temp = Vec::new();
            for statement in internals {
                temp.append(&mut simplify_statement(statement, definition_list, new_internals, identifier_list)?);
            }
            out.push(ASTStatement::Scope { internals: temp });
        },
        ASTStatement::StrongConnection {
            lhs,
            rhs
        } => {
            let rhs = simplify_expression(*rhs, definition_list, new_internals, identifier_list)?;
            out.push(ASTStatement::StrongConnection {
                lhs: lhs,
                rhs: Box::new(rhs) 
            })
        },
        ASTStatement::WeakConnection {
            lhs,
            rhs
        } => {
            let rhs = simplify_expression(*rhs, definition_list, new_internals, identifier_list)?;
            out.push(ASTStatement::WeakConnection {
                lhs: lhs,
                rhs: Box::new(rhs) 
            })
        },
        ASTStatement::If {
            condition,
            true_body,
            false_body,
        } => {
            let condition = simplify_expression(*condition, definition_list, new_internals, identifier_list)?;
            let true_body = simplify_statement(*true_body, definition_list, new_internals, identifier_list)?;
            let true_body = collapse_statements(true_body);
            let false_body = if let Some(false_body) = false_body {
                let false_body = simplify_statement(*false_body, definition_list, new_internals, identifier_list)?;
                let false_body = collapse_statements(false_body);
                Some(Box::new(false_body))
            } else {
                None
            };
            out.push(ASTStatement::If {
                condition: Box::new(condition),
                true_body: Box::new(true_body),
                false_body: false_body,
            })
        },
        ASTStatement::Switch {
            condition,
            paths
        } => {
            fn generate_if <I> (
                condition : &Box<ASTExpression>,
                path: I,
                definition_list : &mut DefinitonList,
                new_internals : & mut Vec<ASTStatement>,
                identifier_list : & mut Vec<String>
            ) -> Result<Option<Box<ASTStatement>>, DefinitonListError>
                where I : IntoIterator<Item = (Box<ASTAtom>, Box<ASTStatement>)> 
            {
                let mut path = path.into_iter();
                if let Some((variant, body)) = path.next() {
                    let variant = simplify_atom(*variant, definition_list, identifier_list)?;
                    let body = simplify_statement(*body, definition_list, new_internals, identifier_list)?;
                    let body = collapse_statements(body);
                    if let ASTAtom::Empty = variant {
                        Ok(Some(Box::new(body)))
                    } else {
                        Ok(Some(Box::new(ASTStatement::If { 
                            condition: Box::new(ASTExpression::BinaryOperation { 
                                lhs: Box::new(ASTExpression::Atom(Box::new(variant))), 
                                operation: BinaryOperation::Equal, 
                                rhs: condition.clone()
                            }), 
                            true_body: Box::new(body), 
                            false_body: generate_if(condition, path, definition_list, new_internals, identifier_list)?
                        })))
                    }
                } else {Ok(None)}
            }

            out.push(*generate_if(&condition, paths.into_iter(), definition_list, new_internals, identifier_list)?.unwrap());
        },
        ASTStatement::Loop {
            variable_name,
            from,
            to,
            body
        } => {
            let from = evaluate_constant_expression(*from)
                .map_err(|e| DefinitonListError::ConstantEvalError(e))?.as_usize()
                .map_err(|e| DefinitonListError::ConstantEvalError(ConstantEvalError::ConstantError(e)))?;
            let to = evaluate_constant_expression(*to)
                .map_err(|e| DefinitonListError::ConstantEvalError(e))?
                .as_usize().map_err(|e| DefinitonListError::ConstantEvalError(ConstantEvalError::ConstantError(e)))?;
            for i in from..to {
                let mut body = body.clone();
                replace_constant_in_statement(
                    &mut body, 
                    &variable_name, 
                    &ASTAtom::Number { 
                        signed: false, 
                        value: Number::DecNumber(format!("{}", i)), 
                        width: None
                    }, identifier_list
                );
                let body = simplify_statement(*body, definition_list, new_internals, identifier_list)?;
                let body = collapse_statements(body);
                out.push(body);
            }
        },
        ASTStatement::Node {
            name,
            value
        } => {
            let value = simplify_expression(*value, definition_list, new_internals, identifier_list)?;
            let name = Identifier::Evaluated(definition_list.set_newest_node(name.get_index(), identifier_list));
            out.push(ASTStatement::Node { name: name, value: Box::new(value) })
        },
        _ => unreachable!()
    }
    Ok(out)
}

fn simplify_expression (
    expression : ASTExpression, 
    definition_list : & mut DefinitonList,
    new_internals : & mut Vec<ASTStatement>,
    identifier_list : & mut Vec<String>
) -> Result<ASTExpression, DefinitonListError> {
    match expression {
        ASTExpression::Atom(atom) => {
            let atom = simplify_atom(*atom, definition_list, identifier_list)?;
            Ok(ASTExpression::Atom(Box::new(atom)))
        },
        ASTExpression::BinaryOperation {
            lhs, 
            operation,
            rhs
        } => {
            let lhs = simplify_expression(*lhs, definition_list, new_internals, identifier_list)?;
            let rhs = simplify_expression(*rhs, definition_list, new_internals, identifier_list)?;
            Ok(ASTExpression::BinaryOperation { lhs: Box::new(lhs), operation: operation, rhs: Box::new(rhs) })
        },
        ASTExpression::UnaryOperation {
            operation, 
            content 
        } => {
            let content = simplify_expression(*content, definition_list, new_internals, identifier_list)?;
            Ok(ASTExpression::UnaryOperation { operation: operation, content: Box::new(content) })
        },
        ASTExpression::Bundle(inner) => {
            let mut out = Vec::new();
            for (name, expression) in inner {
                out.push((
                    name, 
                    Box::new(simplify_expression(*expression, definition_list, new_internals, identifier_list)?)
                ))
            }
            Ok(ASTExpression::Bundle(out))
        }
        ASTExpression::Function {
            function, 
            arguments 
        } => {
            let mut out = Vec::new();
            for argument in arguments {
                out.push(
                    simplify_expression(argument, definition_list, new_internals, identifier_list)?
                )
            }
            Ok(ASTExpression::Function { function: function, arguments: out })
        }
    }
}

fn simplify_atom (
    atom : ASTAtom, 
    definition_list : & mut DefinitonList,
    identifier_list : & mut Vec<String>
) -> Result<ASTAtom, DefinitonListError> {
    match atom {
        ASTAtom::Empty => Ok(ASTAtom::Empty),
        ASTAtom::Enum {
            kind, 
            variant
        } => {
            let i = definition_list.get_enum_variant(&kind, &variant, identifier_list)?;
            Ok(ASTAtom::Number { signed: false, value: Number::DecNumber(format!("{}", i)), width: None })
        },
        ASTAtom::Number { 
            signed, 
            value, 
            width 
        } => {
            Ok(ASTAtom::Number { signed: signed, value: value, width: width })
        },
        ASTAtom::Target(t) => {
            let mut out =  Vec::new();
            if t.len() == 1 && t[0].1.is_none() {
                if let Some(i) = definition_list.get_node_index(t[0].0.get_index()) {
                    return Ok(ASTAtom::Target(vec![(Identifier::Evaluated(i), None)]))
                }
            }
            for (name, index) in t {
                let index = (
                    name,
                    if let Some(index) = index {
                        Some(Box::new(ASTExpression::Atom(
                            Box::new(evaluate_constant_expression(*index).map_err(|e| DefinitonListError::ConstantEvalError(e))?.as_atom()))
                        ))
                    } else {None}
                );
                out.push(
                    index
                );
            }
            Ok(ASTAtom::Target(out))
        }
    }
}

fn simplify_type (
    type_ : ASTType, 
    definition_list : & mut DefinitonList,
    new_internals : & mut Vec<ASTStatement>,
    identifier_list : & mut Vec<String>
) -> Result<ASTType, DefinitonListError> {
    match type_ {
        ASTType::AsyncReset => Ok(ASTType::AsyncReset),
        ASTType::SyncReset => Ok(ASTType::SyncReset),
        ASTType::Clock => Ok(ASTType::Clock),
        ASTType::Enum(name) => {
            let i = definition_list.get_enum_bit_width(&name, identifier_list)?;
            Ok(ASTType::Unsigned(Box::new(ASTExpression::Atom(Box::new(ASTAtom::Number { signed: false, value: Number::DecNumber(format!("{}", i)), width: None })))))
        },
        ASTType::Signed(width) => {
            let width = evaluate_constant_expression(*width).map_err(|e| DefinitonListError::ConstantEvalError(e))?.as_atom();
            Ok(ASTType::Signed(Box::new(ASTExpression::Atom(Box::new(width)))))
        },
        ASTType::Unsigned(width) => {
            let width = evaluate_constant_expression(*width).map_err(|e| DefinitonListError::ConstantEvalError(e))?.as_atom();
            Ok(ASTType::Unsigned(Box::new(ASTExpression::Atom(Box::new(width)))))
        },
        ASTType::Bundle(b) => {
            let mut out = Vec::new();
            for (flipped, name, type_) in b {
                let type_ = simplify_type(*type_, definition_list, new_internals, identifier_list)?;
                out.push((flipped, name, Box::new(type_)));
            }
            Ok(ASTType::Bundle(out))
        },
        ASTType::Vector { 
            type_, 
            size 
        } => {
            let type_ = simplify_type(*type_, definition_list, new_internals, identifier_list)?;
            let size = evaluate_constant_expression(*size).map_err(|e| DefinitonListError::ConstantEvalError(e))?.as_atom();
            Ok(ASTType::Vector { type_: Box::new(type_), size: Box::new(ASTExpression::Atom(Box::new(size))) })
        }
    }
}

fn collapse_statements (
    mut body : Vec<ASTStatement>
) -> ASTStatement {
    if body.len() == 1 {
        if matches!(&body[0], ASTStatement::Scope { internals:_ }) {
            body.remove(0)
        } else {
            ASTStatement::Scope { internals: body }
        }
    } else {
        ASTStatement::Scope { internals: body }
    }
}