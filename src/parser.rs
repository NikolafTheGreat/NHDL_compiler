use pest::{Parser, iterators::Pair};
use pest_derive::Parser;
use num::Num;
use num::BigUint;

#[derive(Parser)]
#[grammar = "parser.pest"]
struct NHDLParser;

impl <'a> AST <'a> {
    pub fn print(&self) {
        let mut input = format!("{:?}", self);
        input = input.replace(", ", ",\n");

        input = input.replace("{ ", "{\n");
        input = input.replace("[", "[\n");
        
        input = input.replace("}", "\n}");
        input = input.replace("]", "\n]");

        input = input.replace("[\n\n]", "_");

        let mut depth = 0;
        let mut output = String::new();

        for s in input.split("\n") {
            if s.contains("]") || s.contains("}") {
                depth -= 1;
            }
            for _ in 0..depth {
                output.push_str("    ");
            }
            output.push_str(s);
            output.push('\n');
            if s.contains("[") || s.contains("{") {
                depth += 1;
            }
        }

        println!("{}", output);
    }
}

#[derive(Debug,PartialEq)]
pub struct AST <'a> {
    top_module : &'a str,
    internals: Vec<ASTStatement<'a>>
}

#[derive(Debug,PartialEq)]
pub enum ASTStatement <'a> {
    ModuleInstance {
        instance_name : &'a str,
        type_name : &'a str,
        generic_values : Vec<ASTExpression<'a>>
    },
    RegisterInstance {
        instance_name : &'a str,
        type_ : Box<ASTType<'a>>
    },
    WireInstance {
        instance_name : &'a str,
        type_ : Box<ASTType<'a>>
    },
    Scope {
        internals: Vec<ASTStatement<'a>>
    },
    ModuleDeclaration {
        module_name : &'a str,
        generic_names : Vec<&'a str>,
        io : Box<ASTType<'a>>,
        body : Box<ASTStatement<'a>>
    },
    EnumDeclaration {
        enum_name : &'a str,
        variant_names : Vec<&'a str>,
    },
    ConstantDeclaration {
        constant_name : &'a str,
        value : Box<ASTExpression<'a>>
    },
    StrongConnection {
        lhs : Box<ASTAtom<'a>>,
        rhs : Box<ASTExpression<'a>>
    },
    WeakConnection {
        lhs : Box<ASTAtom<'a>>,
        rhs : Box<ASTExpression<'a>>
    },
    If {
        condition : Box<ASTExpression<'a>>,
        true_body : Box<ASTStatement<'a>>,
        false_body : Option<Box<ASTStatement<'a>>>,
    },
    Switch {
        condition : Box<ASTExpression<'a>>,
        paths : Vec<(Box<ASTAtom<'a>>, Box<ASTStatement<'a>>)>
    },
    Loop {
        variable_name : &'a str,
        from : Box<ASTExpression<'a>>,
        to : Box<ASTExpression<'a>>,
        body : Box<ASTStatement<'a>>
    },
}

#[derive(Debug,PartialEq)]
pub enum ASTAtom <'a> {
    Empty,
    Identifier(&'a str),
    Target(Vec<(Box<ASTAtom<'a>>, Option<Box<ASTExpression<'a>>>)>),
    Enum {
        kind : &'a str,
        variant : &'a str,
    },
    Number(BigUint),
}

#[derive(Debug,PartialEq)]
pub enum ASTType <'a> {
    Vector {
        type_ : Box<ASTType<'a>>,
        size : Box<ASTExpression<'a>>
    },
    Unsigned (Box<ASTExpression<'a>>),
    Signed (Box<ASTExpression<'a>>),
    Clock,
    AsyncReset,
    SyncReset,
    Bundle (Vec<(bool, &'a str, Box<ASTType<'a>>)>),
}

#[derive(Debug,PartialEq)]
pub enum ASTExpression <'a> {
    BinaryOperation {
        lhs : Box<ASTExpression<'a>>,
        operation : BinaryOperation,
        rhs : Box<ASTExpression<'a>>
    },
    UnaryOperation {
        operation : UnaryOperation,
        content : Box<ASTExpression<'a>>
    },
    ExpressionBundle (Vec<(bool, &'a str, Box<ASTExpression<'a>>)>),
    Function {
        function : Function<'a>,
        content : Vec<ASTExpression<'a>>
    },
    Atom (Box<ASTAtom<'a>>),
}

#[derive(Debug,PartialEq)]
pub enum BinaryOperation {
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    Equal,
    NotEqual,

    Concatinate,

    And,
    Or,
    Xor,

    ShiftLeft,
    ShiftRight,

    Plus,
    Minus,

    Multiply,
    Divide,
    Remainder,
}

#[derive(Debug,PartialEq)]
pub enum UnaryOperation {
    Negate,
    Not,
}

#[allow(non_camel_case_types)]
#[derive(Debug,PartialEq)]
pub enum Function <'a> {
    add,
    sub,

    mul,
    div,
    rem,

    lt,
    gt,
    leq,
    geq,
    eq,
    neq,

    pad,

    asUnsigned,
    asSigned,
    asClock,
    asReset,
    asAsyncReset,

    shl,
    shr,

    asSignedArithmatic,

    neg,

    not,
    and,
    or,
    xor,

    reduce_and,
    reduce_or,
    reduce_xor,

    cat,
    bits,

    head,
    tail,

    mux,

    Custom(&'a str)
}

pub fn parse_nhdl(input : &str) -> AST{
    let tree = match NHDLParser::parse(Rule::Top, input) {
        Ok(mut p) => p.next().unwrap(),
        Err(e) => panic!("{}", e),
    };

    fn parse(tree : Pair<Rule>) -> AST {
        match tree.as_rule() {
            Rule::OuterScope => {
                let mut inner = tree.into_inner();
                AST {
                    top_module: inner.next().unwrap().as_str(),
                    internals: inner.map(parse_statement).collect(),
                }
            },
            _ => unreachable!(),
        }
    }

    fn parse_statement(tree : Pair<Rule>) -> ASTStatement {
        match tree.as_rule() {
            Rule::ModuleInstanciation => {
                let mut inner = tree.into_inner();
                ASTStatement::ModuleInstance {
                    instance_name: inner.next().unwrap().as_str(),
                    type_name: inner.next().unwrap().as_str(), 
                    generic_values: inner.next().unwrap().into_inner().map(parse_expression).collect()
                }
            },
            Rule::RegisterInstanciation => {
                let mut inner = tree.into_inner();
                ASTStatement::RegisterInstance {
                    instance_name: inner.next().unwrap().as_str(),
                    type_: Box::new(parse_type(inner.next().unwrap()))
                }
            }
            Rule::WireInstanciation => {
                let mut inner = tree.into_inner();
                ASTStatement::WireInstance {
                    instance_name: inner.next().unwrap().as_str(),
                    type_: Box::new(parse_type(inner.next().unwrap()))
                }
            }
            Rule::Scope => ASTStatement::Scope {
                internals: tree.into_inner().map(parse_statement).collect(),
            },
            Rule::ModuleDeclaration => {
                let mut inner = tree.into_inner();
                let module_name = inner.next().unwrap().as_str();
                let mut generic_names = vec![];
                
                let mut next = inner.next().unwrap();
                if let Rule::GenericConstant = next.as_rule() {
                    generic_names.append(&mut
                        next.into_inner().map(|n| n.as_str()).collect()
                    );
                    next = inner.next().unwrap();
                }

                ASTStatement::ModuleDeclaration {
                    module_name: module_name,
                    generic_names: generic_names,
                    io: Box::new(parse_type(next)),
                    body: Box::new(parse_statement(inner.next().unwrap()))
                }
            },
            Rule::EnumDeclaration => {
                let mut inner = tree.into_inner();
                ASTStatement::EnumDeclaration {
                    enum_name: inner.next().unwrap().as_str(),
                    variant_names: inner.map(|n|n.as_str()).collect()
                }
            },
            Rule::ConstantDeclaration => {
                let mut inner = tree.into_inner();
                ASTStatement::ConstantDeclaration {
                    constant_name: inner.next().unwrap().as_str(),
                    value: Box::new(parse_expression(inner.next().unwrap()))
                }
            },
            Rule::StrongConnection => {
                let mut inner = tree.into_inner();
                ASTStatement::StrongConnection {
                    lhs: Box::new(parse_atom(inner.next().unwrap())),
                    rhs: Box::new(parse_expression(inner.next().unwrap()))
                }
            },
            Rule::WeakConnection => {
                let mut inner = tree.into_inner();
                ASTStatement::WeakConnection {
                    lhs: Box::new(parse_atom(inner.next().unwrap())),
                    rhs: Box::new(parse_expression(inner.next().unwrap()))
                }
            },
            Rule::If => {
                let mut inner = tree.into_inner();
                ASTStatement::If {
                    condition: Box::new(parse_expression(inner.next().unwrap())),
                    true_body: Box::new(parse_statement(inner.next().unwrap())),
                    false_body: inner.next().map(|n|Box::new(parse_statement(n)))
                }
            },
            Rule::Switch => {
                let mut inner = tree.into_inner();
                let condition = Box::new(parse_expression(inner.next().unwrap()));
                let mut paths = vec![];

                while let Some(n) = inner.next() {
                    paths.push((
                        Box::new(parse_atom(n)),
                        Box::new(parse_statement(inner.next().unwrap()))
                    ));
                }

                ASTStatement::Switch {
                    condition: condition,
                    paths: paths
                }
            },
            Rule::Loop => {
                let mut inner = tree.into_inner();
                ASTStatement::Loop {
                    variable_name: inner.next().unwrap().as_str(),
                    from: Box::new(parse_expression(inner.next().unwrap())),
                    to: Box::new(parse_expression(inner.next().unwrap())),
                    body: Box::new(parse_statement(inner.next().unwrap()))
                }
            },
            _ => unreachable!(),

        }
    }

    fn parse_atom(tree : Pair<Rule>) -> ASTAtom {
        match tree.as_rule() {
            Rule::Empty => ASTAtom::Empty,
            Rule::Identifier => ASTAtom::Identifier(tree.as_str()),
            Rule::Target => {
                let mut inner = tree.into_inner();

                let mut output = vec![];
                while let Some(n) = inner.next() {
                    output.push((
                        Box::new(parse_atom(n)),
                        if let Some(Rule::Expression) = inner.peek().map(|n|n.as_rule()) {
                            Some(Box::new(parse_expression(inner.next().unwrap())))
                        } else {None}
                    ))
                }

                ASTAtom::Target(output)
            }
            Rule::EnumVariant => {
                let mut inner = tree.into_inner();
                ASTAtom::Enum {
                    kind: inner.next().unwrap().as_str(),
                    variant: inner.next().unwrap().as_str()
                }
            },
            Rule::HexNumber => ASTAtom::Number(BigUint::from_str_radix(&tree.as_str()[2..], 16).unwrap()),
            Rule::OctNumber => ASTAtom::Number(BigUint::from_str_radix(&tree.as_str()[2..], 8).unwrap()),
            Rule::BinNumber => ASTAtom::Number(BigUint::from_str_radix(&tree.as_str()[2..], 2).unwrap()),
            Rule::DecNumber => {ASTAtom::Number(tree.as_str().parse().unwrap())},
            _ => unreachable!(),

        }
    }

    fn parse_type(tree : Pair<Rule>) -> ASTType {
        match tree.as_rule() {
            Rule::Vector => {
                let mut inner = tree.into_inner();
                ASTType::Vector {
                    type_: Box::new(parse_type(inner.next().unwrap())),
                    size: Box::new(parse_expression(inner.next().unwrap()))
                }
            }
            Rule::Unsigned => ASTType::Unsigned(
                Box::new(parse_expression(tree.into_inner().next().unwrap()))
            ),
            Rule::Signed => ASTType::Signed(
                Box::new(parse_expression(tree.into_inner().next().unwrap()))
            ),
            Rule::Clock => ASTType::Clock,
            Rule::SyncReset => ASTType::SyncReset,
            Rule::AsyncReset => ASTType::AsyncReset,
            Rule::TypeBundle => {
                let mut inner = tree.into_inner();
                
                let mut output = vec![];
                while let Some(mut n) = inner.next() {
                    let mut flipped = false;
                    if let Rule::Flip = n.as_rule() {
                        flipped = true;
                        n = inner.next().unwrap();
                    }
                    output.push((
                        flipped,
                        n.as_str(),
                        Box::new(parse_type(inner.next().unwrap()))
                    ))
                }

                ASTType::Bundle(output)
            },
            _ => unreachable!(),

        }
    }

    fn parse_expression(tree : Pair<Rule>) -> ASTExpression {
        match tree.as_rule() {
            Rule::Expression => parse_expression(tree.into_inner().next().unwrap()),
            Rule::HexNumber |
            Rule::OctNumber |
            Rule::DecNumber |
            Rule::Target |
            Rule::EnumVariant => {
                ASTExpression::Atom(Box::new(parse_atom(tree)))
            }
            Rule::Comparison |
            Rule::Concatination |
            Rule::Logical |
            Rule::Shift |
            Rule::Sum |
            Rule::Term => {
                let mut inner = tree.into_inner();
                let mut output = parse_expression(inner.next().unwrap());
                while let Some(n) = inner.next() {
                    output = ASTExpression::BinaryOperation {
                        lhs: Box::new(output),
                        operation: match n.as_str() {
                            "<" => BinaryOperation::GreaterThan,
                            ">" => BinaryOperation::LessThan,
                            "<=" => BinaryOperation::GreaterThanOrEqual,
                            ">=" => BinaryOperation::LessThanOrEqual,
                            "==" => BinaryOperation::Equal,
                            "!=" => BinaryOperation::NotEqual,

                            "|" => BinaryOperation::Concatinate,

                            "and" => BinaryOperation::And,
                            "or" => BinaryOperation::Or,
                            "not" => BinaryOperation::Xor,

                            "<<" => BinaryOperation::ShiftLeft,
                            ">>" => BinaryOperation::ShiftRight,

                            "+" => BinaryOperation::Plus,
                            "-" => BinaryOperation::Minus,

                            "*" => BinaryOperation::Multiply,
                            "/" => BinaryOperation::Divide,
                            "%" => BinaryOperation::Remainder,
                            _ => unreachable!()
                        },
                        rhs: Box::new(parse_expression(inner.next().unwrap()))
                    }
                }
                output
            },
            Rule::Unary => {
                let mut inner = tree.into_inner().rev();
                let mut output = parse_expression(inner.next().unwrap());
                while let Some(o) = inner.next() {
                    output = ASTExpression::UnaryOperation {
                        operation: match o.as_str() {
                            "-" => UnaryOperation::Negate,
                            "!" => UnaryOperation::Not,
                            _ => unreachable!()
                        },
                        content: Box::new(output)
                    }
                }
                output
            },
            Rule::ExpressionBundle => {
                let mut inner = tree.into_inner();
                
                let mut output = vec![];
                while let Some(mut n) = inner.next() {
                    let mut flipped = false;
                    if let Rule::Flip = n.as_rule() {
                        flipped = true;
                        n = inner.next().unwrap();
                    }
                    output.push((
                        flipped,
                        n.as_str(),
                        Box::new(parse_expression(inner.next().unwrap()))
                    ))
                }

                ASTExpression::ExpressionBundle(output)
            },
            Rule::Function => {
                let mut inner = tree.into_inner();
                let first = inner.next().unwrap();
                let function = match first.as_rule()  {
                    Rule::BuiltInFunction => {
                        match first.as_str() {
                            "add" => Function::add,
                            "sub" => Function::sub,

                            "mul" => Function::mul,
                            "div" => Function::div,
                            "rem" => Function::rem,

                            "lt" => Function::lt,
                            "gt" => Function::gt,
                            "leq" => Function::leq,
                            "geq" => Function::geq,
                            "eq" => Function::eq,
                            "neq" => Function::neq,

                            "pad" => Function::pad,

                            "asUnsigned" => Function::asUnsigned,
                            "asSigned" => Function::asSigned,
                            "asClock" => Function::asClock,
                            "asReset" => Function::asReset,
                            "asAsyncReset" => Function::asAsyncReset,

                            "shl" => Function::shl,
                            "shr" => Function::shr,

                            "asSignedArithmatic" => Function::asSignedArithmatic,

                            "neg" => Function::neg,

                            "not" => Function::not,
                            "and" => Function::and,
                            "or" => Function::or,
                            "xor" => Function::xor,

                            "reduce_and" => Function::reduce_and,
                            "reduce_or" => Function::reduce_or,
                            "reduce_xor" => Function::reduce_xor,

                            "cat" => Function::cat,
                            "bits" => Function::bits,

                            "head" => Function::head,
                            "tail" => Function::tail,

                            "mux" => Function::mux,
                            _ => unreachable!()
                        }
                    },
                    Rule::Identifier => Function::Custom(first.as_str()),
                    _ => unreachable!(),
                };
                ASTExpression::Function {
                    function: function,
                    content: inner.map(parse_expression).collect()
                }
            },
            _ => unreachable!(),

        }
    }

    parse(tree)
}