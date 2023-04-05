
use super::parser::{AST, ASTStatement, ASTAtom, ASTType, ASTExpression, Number, BinaryOperation, UnaryOperation, Function};

use thiserror::Error;

#[derive(Debug, Error)]
pub enum CodeGenerationError {
    #[error("Non simple element encountered")]
    NonSimpleElement,
    #[error("Wrong number of arguments to functio¨")]
    WrongNumberOfArguments,
}

pub fn generate_code(ast : AST) -> Result<String, CodeGenerationError> {

    fn generate_new_line(indent : usize, output : &mut String,) {
        output.push_str("\n");
        for _ in 0..indent {
            output.push_str("  ");
        }
    }
    
    fn generate_statement(
        tree : &ASTStatement,
        identifier_list : &Vec<String>,
        output : &mut String,
        indent : usize
    ) -> Result<(), CodeGenerationError> {
        match tree {
            ASTStatement::RegisterInstance {
                instance_name,
                clock,
                reset,
                initial,
                type_,
            } => {
                generate_new_line(indent, output);
                output.push_str("reg ");
                output.push_str(instance_name.as_str(identifier_list));
                output.push_str(": ");
                generate_type(type_, identifier_list, output)?;
                output.push_str(", ");
                generate_expression(&clock, identifier_list, output)?;
                output.push_str(" with: (reset => (");
                generate_expression(reset, identifier_list, output)?;
                output.push_str(", ");
                generate_expression(initial, identifier_list, output)?;
                output.push_str("))");
            },
            ASTStatement::WireInstance {
                instance_name,
                type_
            } => {
                generate_new_line(indent, output);
                output.push_str("wire ");
                output.push_str(instance_name.as_str(identifier_list));
                output.push_str(": ");
                generate_type(type_, identifier_list, output)?;
            },
            ASTStatement::Scope {
                internals
            } => {
                for statement in internals.iter() {
                    generate_statement(statement, identifier_list, output, indent)?;
                }
            },
            ASTStatement::ModuleDeclaration {
                module_name,
                generic_names,
                io,
                body
            } => {
                generate_new_line(indent, output);
                output.push_str("module ");
                output.push_str(module_name.as_str(identifier_list));
                output.push_str(" :");
                if generic_names.len() != 0 {
                    return Err(CodeGenerationError::NonSimpleElement);
                }
                let mut io_input = "input in: {".to_string();
                let mut io_output = "output out: {".to_string();
                if let ASTType::Bundle(fields) = &**io {
                    for (flip, identifier, type_) in fields.iter() {
                        if *flip {
                            io_output.push_str(identifier.as_str(identifier_list));
                            io_output.push_str(": ");
                            generate_type(type_, identifier_list, &mut io_output)?;
                            io_output.push_str(", ");
                        } else {
                            io_input.push_str(identifier.as_str(identifier_list));
                            io_input.push_str(": ");
                            generate_type(type_, identifier_list, &mut io_input)?;
                            io_input.push_str(", ");
                        }
                    }
                } else {
                    unreachable!();
                }
                io_input.push_str("}");
                io_output.push_str("}");
                io_input = io_input.replace(", }", "}");
                io_output = io_output.replace(", }", "}");
                generate_new_line(indent + 1, output);
                output.push_str(&io_input);
                generate_new_line(indent + 1, output);
                output.push_str(&io_output);

                generate_statement(body, identifier_list, output, indent + 1)?;
            },
            ASTStatement::StrongConnection {
                lhs,
                rhs,
            } => {
                generate_new_line(indent, output);
                generate_atom(lhs, identifier_list, output)?;
                output.push_str(" <= ");
                generate_expression(rhs, identifier_list, output)?;
            },
            ASTStatement::WeakConnection {
                lhs,
                rhs,
            } => {
                generate_new_line(indent, output);
                generate_atom(lhs, identifier_list, output)?;
                output.push_str(" <- ");
                generate_expression(rhs, identifier_list, output)?;
            },
            ASTStatement::If {
                condition,
                true_body,
                false_body,
            } => {
                generate_new_line(indent, output);
                output.push_str("when ");
                generate_expression(&condition, identifier_list, output)?;
                output.push_str(" :");
                generate_statement(true_body, identifier_list, output, indent + 1)?;
                if let Some(false_body) = false_body {
                    generate_new_line(indent, output);
                    output.push_str("else :");
                    generate_statement(false_body, identifier_list, output, indent + 1)?;
                }
            },
            _ => return Err(CodeGenerationError::NonSimpleElement),
            
        }
        Ok(())
    }

    fn generate_atom (
        tree : &ASTAtom,
        identifier_list : &Vec<String>,
        output : &mut String,
    ) -> Result<(), CodeGenerationError> {
        match tree {
            ASTAtom::Target(inner) => {
                let mut inner = inner.iter();
                let (identifier, index) = inner.next().unwrap();
                output.push_str(identifier.as_str(identifier_list));
                if let Some(index) = index {
                    generate_expression(index, identifier_list, output)?;
                }
                for (identifier, index) in inner {
                    output.push_str(".");
                    output.push_str(identifier.as_str(identifier_list));
                    if let Some(index) = index {
                        generate_expression(index, identifier_list, output)?;
                    }
                }
            },
            ASTAtom::Number {
                signed,
                value,
                width
            } => {
                if *signed {
                    output.push_str("SInt");
                } else {
                    output.push_str("UInt");
                }
                if let Some(width) = width {
                    output.push_str("<");
                    generate_expression(width, identifier_list, output)?;
                    output.push_str(">");
                }
                output.push_str("(");
                output.push_str(match value {
                    Number::HexNumber(_) => "\"h",
                    Number::OctNumber(_) => "\"o",
                    Number::BinNumber(_) => "\"b",
                    Number::DecNumber(_) => "",
                });
                output.push_str(match value {
                    Number::HexNumber(num) => num,
                    Number::OctNumber(num) => num,
                    Number::BinNumber(num) => num,
                    Number::DecNumber(num) => num,
                });
                output.push_str(match value {
                    Number::HexNumber(_) => "\")",
                    Number::OctNumber(_) => "\")",
                    Number::BinNumber(_) => "\")",
                    Number::DecNumber(_) => ")",
                });
            },
            _ => return Err(CodeGenerationError::NonSimpleElement),
        }
        Ok(())
    }

    fn generate_type (
        tree : &ASTType,
        identifier_list : &Vec<String>,
        output : &mut String,
    ) -> Result<(), CodeGenerationError> {
        match tree {
            ASTType::Vector {
                type_,
                size,
            } => {
                generate_type(type_, identifier_list, output)?;
                output.push_str("[");
                generate_expression(size, identifier_list, output)?;
                output.push_str("]");
            },
            ASTType::Unsigned (width) => {
                output.push_str("UInt<");
                generate_expression(width, identifier_list, output)?;
                output.push_str(">");
            },
            ASTType::Signed (width) => {
                output.push_str("SInt<");
                generate_expression(width, identifier_list, output)?;
                output.push_str(">");
            },
            ASTType::Bundle (inner) => {
                output.push_str("{");
                for (flipped, name, type_) in inner.iter() {
                    if *flipped {
                        output.push_str("flip");
                    }
                    output.push_str(name.as_str(identifier_list));
                    output.push_str(" : ");
                    generate_type(type_, identifier_list, output)?;
                    output.push_str(", ");
                }
                if let Some('{') = output.pop() {
                    output.push('{')
                }
                if let Some('{') = output.pop() {
                    output.push('{')
                }
                output.push_str("}");
            },
            _ => return Err(CodeGenerationError::NonSimpleElement)
        }
        Ok(())
    }

    fn generate_expression (
        tree : &ASTExpression,
        identifier_list : &Vec<String>,
        output : &mut String,
    ) -> Result<(), CodeGenerationError> {
        match tree {
            ASTExpression::BinaryOperation {
                lhs,
                operation,
                rhs,
            } => {
                match operation {
                    BinaryOperation::GreaterThan => output.push_str("gt"),
                    BinaryOperation::LessThan => output.push_str("lt"),
                    BinaryOperation::GreaterThanOrEqual => output.push_str("geq"),
                    BinaryOperation::LessThanOrEqual => output.push_str("leq"),
                    BinaryOperation::Equal => output.push_str("eq"),
                    BinaryOperation::NotEqual => output.push_str("neq"),

                    BinaryOperation::Concatinate => output.push_str("cat"),

                    BinaryOperation::And => output.push_str("and"),
                    BinaryOperation::Or => output.push_str("or"),
                    BinaryOperation::Xor => output.push_str("xor"),

                    BinaryOperation::ShiftLeft => output.push_str("shl"),
                    BinaryOperation::ShiftRight => output.push_str("shr"),
                    BinaryOperation::DynamicShiftLeft => output.push_str("dshl"),
                    BinaryOperation::DynamicShiftRight => output.push_str("dshr"),

                    BinaryOperation::Plus => output.push_str("add"),
                    BinaryOperation::Minus => output.push_str("sub"),

                    BinaryOperation::Multiply => output.push_str("mul"),
                    BinaryOperation::Divide => output.push_str("div"),
                    BinaryOperation::Remainder => output.push_str("rem"),
                }
                output.push_str("(");
                generate_expression(lhs, identifier_list, output)?;
                output.push_str(", ");
                generate_expression(rhs, identifier_list, output)?;
                output.push_str(")");
            },
            ASTExpression::UnaryOperation {
                operation,
                content
            } => {
                match operation {
                    UnaryOperation::Negate => output.push_str("neg"),
                    UnaryOperation::Not => output.push_str("neg"),
                }
                output.push_str("(");
                generate_expression(content, identifier_list, output)?;
                output.push_str(")");
            },
            ASTExpression::Function {
                function,
                arguments,
            } => {
                match function {
                    Function::add => output.push_str("add"),
                    Function::sub => output.push_str("sub"),

                    Function::mul => output.push_str("mul"),
                    Function::div => output.push_str("div"),
                    Function::rem => output.push_str("rem"),

                    Function::lt => output.push_str("lt"),
                    Function::gt => output.push_str("gt"),
                    Function::leq => output.push_str("leq"),
                    Function::geq => output.push_str("geq"),
                    Function::eq => output.push_str("eq"),
                    Function::neq => output.push_str("neq"),

                    Function::pad => output.push_str("pad"),

                    Function::asUnsigned => output.push_str("asUInt"),
                    Function::asSigned => output.push_str("asSInt"),
                    Function::asClock => output.push_str("asClock"),
                    Function::asAsyncReset => output.push_str("asAsyncReset"),

                    Function::shl => output.push_str("shl"),
                    Function::shr => output.push_str("shr"),
                    Function::dshl => output.push_str("dshl"),
                    Function::dshr => output.push_str("dshr"),

                    Function::asSignedArithmatic => output.push_str("cvt"),

                    Function::neg => output.push_str("neg"),

                    Function::not => output.push_str("not"),
                    Function::and => output.push_str("and"),
                    Function::or => output.push_str("or"),
                    Function::xor => output.push_str("xor"),

                    Function::reduce_and => output.push_str("andr"),
                    Function::reduce_or => output.push_str("orr"),
                    Function::reduce_xor => output.push_str("xorr"),

                    Function::cat => output.push_str("cat"),
                    Function::bits => output.push_str("bits"),

                    Function::head => output.push_str("head"),
                    Function::tail => output.push_str("tail"),

                    Function::mux => output.push_str("mux"),
                    _ => return Err(CodeGenerationError::NonSimpleElement)
                }
                output.push_str("(");
                for argument in arguments.iter() {
                    generate_expression(argument, identifier_list, output)?;
                    output.push_str(", ");
                }
                if let Some('(') = output.pop() {
                    output.push('(')
                }
                if let Some('(') = output.pop() {
                    output.push('(')
                }
                output.push_str(")");
            },
            ASTExpression::Atom (atom) => {
                generate_atom(atom, identifier_list, output)?;
            },
            _ => return Err(CodeGenerationError::NonSimpleElement)
        }
        Ok(())
    }

    let mut output = String::new();

    let identifier_list = ast.identifier_list;

    output.push_str("circuit ");
    output.push_str(ast.top_module.as_str(&identifier_list));
    for statement in ast.internals {
        generate_statement(&statement, &identifier_list, &mut output, 1)?;
    }

    Ok(output)
}