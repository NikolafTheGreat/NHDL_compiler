/*
+------------------------------------------------------------------------------------------------+
| This file contains the functionality used to reduce constant expressions down to single values |
+------------------------------------------------------------------------------------------------+
*/

use crate::parser::{ASTExpression, ASTAtom, BinaryOperation, UnaryOperation, Function};
use super::constant::{Constant, ConstantError};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ConstantEvalError {
    #[error("Non constant found in expression")]
    NonConstant,
    #[error("Wrong number of arguments to function")]
    WrongNumberOfArgument,
    #[error("Invalid operation performed in constant expression")]
    InvalidOperation,
    #[error("Invalid argument provided to a function in constant expression")]
    InvalidArgument,
    #[error("Error when evaluating constant expressinon : {0}")]
    ConstantError(ConstantError),
}

pub fn evaluate_constant_expression (
    expression: ASTExpression
) -> Result<Constant, ConstantEvalError> {
    match expression {
        ASTExpression::Atom(atom) => {
            match *atom {
                ASTAtom::Number {
                    signed,
                    value,
                    width
                } => {
                    let width = {
                        if let Some(width) = width {
                            Some((evaluate_constant_expression(*width)?).as_usize().unwrap())
                        } else {
                            None
                        }
                    };
                    Ok(Constant::from_atom(signed, value, width))
                }
                _ => Err(ConstantEvalError::NonConstant)
            }
        },
        ASTExpression::BinaryOperation {
            lhs,
            operation,
            rhs
        } => {
            let lhs = evaluate_constant_expression(*lhs)?;
            let rhs = evaluate_constant_expression(*rhs)?;
            match operation {
                BinaryOperation::GreaterThan => {
                    Ok(Constant::greater_than(lhs, rhs))
                },
                BinaryOperation::LessThan => {
                    Ok(Constant::greater_than(rhs, lhs))
                },
                BinaryOperation::GreaterThanOrEqual => {
                    Ok(Constant::greater_than_or_equal(lhs, rhs))
                },
                BinaryOperation::LessThanOrEqual => {
                    Ok(Constant::greater_than_or_equal(rhs, lhs))
                },
                BinaryOperation::Equal => {
                    Ok(Constant::equal(lhs, rhs))
                },
                BinaryOperation::NotEqual => {
                    let mut out = Constant::equal(lhs, rhs);
                    out.not();
                    Ok(out)
                },

                BinaryOperation::Concatinate => {
                    Ok(Constant::concatinate(lhs, rhs))
                },
            
                BinaryOperation::And => {
                    Ok(Constant::and(lhs, rhs))
                },
                BinaryOperation::Or => {
                    Ok(Constant::or(lhs, rhs))
                },
                BinaryOperation::Xor => {
                    Ok(Constant::xor(lhs, rhs))
                },
            
                BinaryOperation::ShiftLeft => {
                    Ok(Constant::shift_left(lhs, rhs).map_err(|e| ConstantEvalError::ConstantError(e))?)
                },
                BinaryOperation::ShiftRight => {
                    Ok(Constant::shift_right(lhs, rhs).map_err(|e| ConstantEvalError::ConstantError(e))?)
                },
                BinaryOperation::DynamicShiftLeft => {
                    Ok(Constant::shift_left(lhs, rhs).map_err(|e| ConstantEvalError::ConstantError(e))?)
                },
                BinaryOperation::DynamicShiftRight => {
                    Ok(Constant::shift_right(lhs, rhs).map_err(|e| ConstantEvalError::ConstantError(e))?)
                },
            
                BinaryOperation::Plus => {
                    Ok(Constant::add(lhs, rhs).map_err(|e| ConstantEvalError::ConstantError(e))?)
                },
                BinaryOperation::Minus => {
                    Ok(Constant::sub(lhs, rhs).map_err(|e| ConstantEvalError::ConstantError(e))?)
                },
            
                BinaryOperation::Multiply => {
                    Ok(Constant::mul(lhs, rhs).map_err(|e| ConstantEvalError::ConstantError(e))?)
                },
                BinaryOperation::Divide => {
                    Ok(Constant::div(lhs, rhs).map_err(|e| ConstantEvalError::ConstantError(e))?)
                },
                BinaryOperation::Remainder => {
                    Ok(Constant::rem(lhs, rhs).map_err(|e| ConstantEvalError::ConstantError(e))?)
                },
            }
        },
        ASTExpression::UnaryOperation {
            operation,
            content
        } => {
            let mut content = evaluate_constant_expression(*content)?;
            match operation {
                UnaryOperation::Negate => content.neg(),
                UnaryOperation::Not => content.not(),
            }
            Ok(content)
        },
        ASTExpression::Function {
            function,
            arguments
        } => {
            let arguments = {
                let mut out = vec![];
                for argument in arguments {
                    out.push(evaluate_constant_expression(argument)?)
                }
                out
            };
            if arguments.len() != match function {
                Function::add => 2,
                Function::sub => 2,
            
                Function::mul => 2,
                Function::div => 2,
                Function::rem => 2,
            
                Function::lt => 2,
                Function::gt => 2,
                Function::leq => 2,
                Function::geq => 2,
                Function::eq => 2,
                Function::neq => 2,
            
                Function::pad => 2,
            
                Function::asUnsigned => 1,
                Function::asSigned => 1,
                Function::asClock => 1,
                Function::asReset => 1,
                Function::asAsyncReset => 1,
            
                Function::shl => 2,
                Function::shr => 2,
                Function::dshl => 2,
                Function::dshr => 2,
            
                Function::asSignedArithmatic => 1,
            
                Function::neg => 1,
            
                Function::not => 1,
                Function::and => 2,
                Function::or => 2,
                Function::xor => 2,
            
                Function::reduce_and => 1,
                Function::reduce_or => 1,
                Function::reduce_xor => 1,
            
                Function::cat => 2,
                Function::bits => 3,
            
                Function::head => 2,
                Function::tail => 2,
            
                Function::mux => 3,
            } {
                return Err(ConstantEvalError::WrongNumberOfArgument);
            }
            match function {
                Function::add => {
                    Ok(Constant::add(arguments[0].clone(), arguments[1].clone()).map_err(|e| ConstantEvalError::ConstantError(e))?)
                },
                Function::sub => {
                    Ok(Constant::sub(arguments[0].clone(), arguments[1].clone()).map_err(|e| ConstantEvalError::ConstantError(e))?)
                },
            
                Function::mul => {
                    Ok(Constant::mul(arguments[0].clone(), arguments[1].clone()).map_err(|e| ConstantEvalError::ConstantError(e))?)
                },
                Function::div => {
                    Ok(Constant::div(arguments[0].clone(), arguments[1].clone()).map_err(|e| ConstantEvalError::ConstantError(e))?)
                },
                Function::rem => {
                    Ok(Constant::rem(arguments[0].clone(), arguments[1].clone()).map_err(|e| ConstantEvalError::ConstantError(e))?)
                },
            
                Function::lt => {
                    Ok(Constant::greater_than(arguments[1].clone(), arguments[0].clone()))
                },
                Function::gt => {
                    Ok(Constant::greater_than(arguments[0].clone(), arguments[1].clone()))
                },
                Function::leq => {
                    Ok(Constant::greater_than_or_equal(arguments[1].clone(), arguments[0].clone()))
                },
                Function::geq => {
                    Ok(Constant::greater_than_or_equal(arguments[0].clone(), arguments[1].clone()))
                },
                Function::eq => {
                    Ok(Constant::equal(arguments[0].clone(), arguments[1].clone()))
                },
                Function::neq => {
                    let mut out = Constant::greater_than_or_equal(arguments[0].clone(), arguments[1].clone());
                    out.not();
                    Ok(out)
                },
            
                Function::pad => {
                    let mut arg = arguments[0].clone();
                    arg.extend(arguments[1].clone().as_usize().map_err(|e| ConstantEvalError::ConstantError(e))?);
                    Ok(arg)
                },
            
                Function::asUnsigned => {
                    let mut arg = arguments[0].clone();
                    arg.signed = false;
                    Ok(arg)
                },
                Function::asSigned => {
                    let mut arg = arguments[0].clone();
                    arg.signed = true;
                    Ok(arg)
                },
                Function::asClock => {
                    Err(ConstantEvalError::InvalidOperation)
                },
                Function::asReset => {
                    Err(ConstantEvalError::InvalidOperation)
                },
                Function::asAsyncReset => {
                    Err(ConstantEvalError::InvalidOperation)
                },
            
                Function::shl => {
                    Ok(Constant::shift_left(arguments[0].clone(), arguments[1].clone()).map_err(|e| ConstantEvalError::ConstantError(e))?)
                },
                Function::shr => {
                    Ok(Constant::shift_right(arguments[0].clone(), arguments[1].clone()).map_err(|e| ConstantEvalError::ConstantError(e))?)
                },
                Function::dshl => {
                    Ok(Constant::shift_left(arguments[0].clone(), arguments[1].clone()).map_err(|e| ConstantEvalError::ConstantError(e))?)
                },
                Function::dshr => {
                    Ok(Constant::shift_right(arguments[0].clone(), arguments[1].clone()).map_err(|e| ConstantEvalError::ConstantError(e))?)
                },
            
                Function::asSignedArithmatic => {
                    let mut arg = arguments[0].clone();
                    arg.neg();
                    arg.neg();
                    Ok(arg)
                },
            
                Function::neg => {
                    let mut arg = arguments[0].clone();
                    arg.neg();
                    Ok(arg)
                },
            
                Function::not => {
                    let mut arg = arguments[0].clone();
                    arg.not();
                    Ok(arg)
                },
                Function::and => {
                    Ok(Constant::and(arguments[0].clone(), arguments[1].clone()))
                },
                Function::or => {
                    Ok(Constant::or(arguments[0].clone(), arguments[1].clone()))
                },
                Function::xor => {
                    Ok(Constant::xor(arguments[0].clone(), arguments[1].clone()))
                },
            
                Function::reduce_and => {
                    let mut arg = arguments[0].clone();
                    arg.reduce_and();
                    Ok(arg)
                },
                Function::reduce_or => {
                    let mut arg = arguments[0].clone();
                    arg.reduce_or();
                    Ok(arg)
                },
                Function::reduce_xor => {
                    let mut arg = arguments[0].clone();
                    arg.reduce_xor();
                    Ok(arg)
                },
            
                Function::cat => {
                    Ok(Constant::concatinate(arguments[0].clone(), arguments[1].clone()))
                },
                Function::bits => {
                    Ok(Constant {
                        signed : false,
                        value : arguments[0].value[
                            arguments[2].as_usize().map_err(|e| ConstantEvalError::ConstantError(e))?
                            ..=
                            arguments[1].as_usize().map_err(|e| ConstantEvalError::ConstantError(e))?
                        ].to_vec()
                    })
                },
            
                Function::head => {
                    Ok(Constant {
                        signed : false,
                        value : arguments[0].value[
                            arguments[0].value.len() - arguments[1].as_usize().map_err(|e| ConstantEvalError::ConstantError(e))?
                            ..
                            ].to_vec()
                    })
                },
                Function::tail => {
                    Ok(Constant {
                        signed : false,
                        value : arguments[0].value[
                            ..
                            arguments[1].as_usize().map_err(|e| ConstantEvalError::ConstantError(e))?
                            ].to_vec()
                    })
                },
            
                Function::mux => {
                    if arguments[0].value.len() == 1 {
                        if arguments[0].value[0] {
                            Ok(arguments[1].clone())
                        } else {
                            Ok(arguments[2].clone())
                        }
                    } else {
                        Err(ConstantEvalError::InvalidArgument)
                    }
                },
            }
        }
        ASTExpression::Bundle(_) => {
            Err(ConstantEvalError::NonConstant)
        }
    }
}