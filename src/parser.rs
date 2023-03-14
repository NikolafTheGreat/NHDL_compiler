use pest::{Parser, iterators::Pair};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "parser.pest"]
struct NHDLParser;

impl <'a> ASTNode <'a> {
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

#[derive(Debug)]
pub enum ASTNode <'a> {
    OuterScope {
        top_module : &'a str,
        internals: Vec<ASTNode<'a>>
    },
    ModuleInstance {
        instance_name : &'a str,
        type_name : &'a str,
        generic_values : Vec<ASTNode<'a>>
    },
    RegisterInstance {
        instance_name : &'a str,
        type_ : Box<ASTNode<'a>>
    },
    WireInstance {
        instance_name : &'a str,
        type_ : Box<ASTNode<'a>>
    },
    Scope {
        internals: Vec<ASTNode<'a>>
    },
    ModuleDeclaration {
        module_name : &'a str,
        generic_names : Vec<&'a str>,
        io : Box<ASTNode<'a>>,
        body : Box<ASTNode<'a>>
    },
    EnumDeclaration {
        enum_name : &'a str,
        variant_names : Vec<&'a str>,
    },
    ConstantDeclaration {
        constant_name : &'a str,
        value : Box<ASTNode<'a>>
    },
    StrongConnection {
        lhs : Box<ASTNode<'a>>,
        rhs : Box<ASTNode<'a>>
    },
    WeakConnection {
        lhs : Box<ASTNode<'a>>,
        rhs : Box<ASTNode<'a>>
    },
    If {
        condition : Box<ASTNode<'a>>,
        true_body : Box<ASTNode<'a>>,
        false_body : Option<Box<ASTNode<'a>>>,
    },
    Switch {
        condition : Box<ASTNode<'a>>,
        paths : Vec<(Box<ASTNode<'a>>, Box<ASTNode<'a>>)>
    },
    Empty,
    Loop {
        variable_name : &'a str,
        from : Box<ASTNode<'a>>,
        to : Box<ASTNode<'a>>,
        body : Box<ASTNode<'a>>
    },
    Identifier(&'a str),
    Target(Vec<(Box<ASTNode<'a>>, Option<Box<ASTNode<'a>>>)>),
    Number(usize),
    VectorType {
        type_ : Box<ASTNode<'a>>,
        size : Box<ASTNode<'a>>
    },
    UnsignedType (Box<ASTNode<'a>>),
    SignedType (Box<ASTNode<'a>>),
    ClockType,
    AsyncResetType,
    SyncResetType,
    TypeBundle (Vec<(bool, &'a str, Box<ASTNode<'a>>)>),
    BinaryOperation {
        lhs : Box<ASTNode<'a>>,
        operation : BinaryOperation,
        rhs : Box<ASTNode<'a>>
    },
    UnaryOperation {
        operation : UnaryOperation,
        content : Box<ASTNode<'a>>
    },
    ExpressionBundle (Vec<(bool, &'a str, Box<ASTNode<'a>>)>),
    Function {
        function : Function<'a>,
        content : Vec<ASTNode<'a>>
    },
    //Unfinnished(&'a str),
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum UnaryOperation {
    Negate,
    Not,
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
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

pub fn parse_nhdl(input : &str) -> ASTNode{
    let tree = match NHDLParser::parse(Rule::Top, input) {
        Ok(mut p) => p.next().unwrap(),
        Err(e) => panic!("{}", e),
    };
    fn parse(tree : Pair<Rule>) -> ASTNode {
        match tree.as_rule() {
            Rule::WHITESPACE |
            Rule::COMMENT |
            Rule::EOI |
            Rule::Top |
            Rule::Definition |
            Rule::Declaration |
            Rule::Statement |
            Rule::Connection |
            Rule::SwitchNumber |
            Rule::SwitchEnum |
            Rule::Number |
            Rule::Type |
            Rule::Reset |
            Rule::Atom |
            Rule::GenericInstaciation |
            Rule::GenericConstant |
            Rule::Flip |
            Rule::ComparisonOperation |
            Rule::ConcatinationOperation |
            Rule::LogicalOperation |
            Rule::ShiftOperation |
            Rule::SumOperation |
            Rule::TermOperation |
            Rule::UnaryOperation |
            Rule::BuiltInFunction 
            => unreachable!(),
            Rule::OuterScope => {
                let mut inner = tree.into_inner();
                ASTNode::OuterScope {
                    top_module: inner.next().unwrap().as_str(),
                    internals: inner.map(parse).collect(),
                }
            }
            Rule::ModuleInstanciation => {
                let mut inner = tree.into_inner();
                ASTNode::ModuleInstance {
                    instance_name: inner.next().unwrap().as_str(),
                    type_name: inner.next().unwrap().as_str(), 
                    generic_values: inner.next().unwrap().into_inner().map(parse).collect()
                }
            },
            Rule::RegisterInstanciation => {
                let mut inner = tree.into_inner();
                ASTNode::RegisterInstance {
                    instance_name: inner.next().unwrap().as_str(),
                    type_: Box::new(parse(inner.next().unwrap()))
                }
            }
            Rule::WireInstanciation => {
                let mut inner = tree.into_inner();
                ASTNode::WireInstance {
                    instance_name: inner.next().unwrap().as_str(),
                    type_: Box::new(parse(inner.next().unwrap()))
                }
            }
            Rule::Scope => ASTNode::Scope {
                internals: tree.into_inner().map(parse).collect(),
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

                ASTNode::ModuleDeclaration {
                    module_name: module_name,
                    generic_names: generic_names,
                    io: Box::new(parse(next)),
                    body: Box::new(parse(inner.next().unwrap()))
                }
            },
            Rule::EnumDeclaration => {
                let mut inner = tree.into_inner();
                ASTNode::EnumDeclaration {
                    enum_name: inner.next().unwrap().as_str(),
                    variant_names: inner.map(|n|n.as_str()).collect()
                }
            },
            Rule::ConstantDeclaration => {
                let mut inner = tree.into_inner();
                ASTNode::ConstantDeclaration {
                    constant_name: inner.next().unwrap().as_str(),
                    value: Box::new(parse(inner.next().unwrap()))
                }
            },
            Rule::StrongConnection => {
                let mut inner = tree.into_inner();
                ASTNode::StrongConnection {
                    lhs: Box::new(parse(inner.next().unwrap())),
                    rhs: Box::new(parse(inner.next().unwrap()))
                }
            },
            Rule::WeakConnection => {
                let mut inner = tree.into_inner();
                ASTNode::WeakConnection {
                    lhs: Box::new(parse(inner.next().unwrap())),
                    rhs: Box::new(parse(inner.next().unwrap()))
                }
            },
            Rule::If => {
                let mut inner = tree.into_inner();
                ASTNode::If {
                    condition: Box::new(parse(inner.next().unwrap())),
                    true_body: Box::new(parse(inner.next().unwrap())),
                    false_body: inner.next().map(|n|Box::new(parse(n)))
                }
            },
            Rule::Switch => {
                let mut inner = tree.into_inner();
                let condition = Box::new(parse(inner.next().unwrap()));
                let mut paths = vec![];

                while let Some(n) = inner.next() {
                    paths.push((
                        Box::new(parse(n)),
                        Box::new(parse(inner.next().unwrap()))
                    ));
                }

                ASTNode::Switch {
                    condition: condition,
                    paths: paths
                }
            },
            Rule::Empty => ASTNode::Empty,
            Rule::Loop => {
                let mut inner = tree.into_inner();
                ASTNode::Loop {
                    variable_name: inner.next().unwrap().as_str(),
                    from: Box::new(parse(inner.next().unwrap())),
                    to: Box::new(parse(inner.next().unwrap())),
                    body: Box::new(parse(inner.next().unwrap()))
                }
            },
            Rule::Identifier => ASTNode::Identifier(tree.as_str()),
            Rule::Target => {
                let mut inner = tree.into_inner();

                let mut output = vec![];
                while let Some(n) = inner.next() {
                    output.push((
                        Box::new(parse(n)),
                        if let Some(Rule::Expression) = inner.peek().map(|n|n.as_rule()) {
                            Some(Box::new(parse(inner.next().unwrap())))
                        } else {None}
                    ))
                }

                ASTNode::Target(output)
            }
            Rule::HexNumber => ASTNode::Number(usize::from_str_radix(&tree.as_str()[2..], 16).unwrap()),
            Rule::OctNumber => ASTNode::Number(usize::from_str_radix(&tree.as_str()[2..], 8).unwrap()),
            Rule::BinNumber => ASTNode::Number(usize::from_str_radix(&tree.as_str()[2..], 2).unwrap()),
            Rule::DecNumber => {ASTNode::Number(tree.as_str().parse().unwrap())},
            Rule::Vector => {
                let mut inner = tree.into_inner();
                ASTNode::VectorType {
                    type_: Box::new(parse(inner.next().unwrap())),
                    size: Box::new(parse(inner.next().unwrap()))
                }
            }
            Rule::Unsigned => ASTNode::UnsignedType(
                Box::new(parse(tree.into_inner().next().unwrap()))
            ),
            Rule::Signed => ASTNode::SignedType(
                Box::new(parse(tree.into_inner().next().unwrap()))
            ),
            Rule::Clock => ASTNode::ClockType,
            Rule::SyncReset => ASTNode::SyncResetType,
            Rule::AsyncReset => ASTNode::AsyncResetType,
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
                        Box::new(parse(inner.next().unwrap()))
                    ))
                }

                ASTNode::TypeBundle(output)
            },
            Rule::Expression => {
                parse(tree.into_inner().next().unwrap())
            }
            Rule::Comparison |
            Rule::Concatination |
            Rule::Logical |
            Rule::Shift |
            Rule::Sum |
            Rule::Term => {
                let mut inner = tree.into_inner();
                let mut output = parse(inner.next().unwrap());
                while let Some(n) = inner.next() {
                    output = ASTNode::BinaryOperation {
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
                        rhs: Box::new(parse(inner.next().unwrap()))
                    }
                }
                output
            },
            Rule::Unary => {
                let mut inner = tree.into_inner().rev();
                let mut output = parse(inner.next().unwrap());
                while let Some(o) = inner.next() {
                    output = ASTNode::UnaryOperation {
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
                        Box::new(parse(inner.next().unwrap()))
                    ))
                }

                ASTNode::ExpressionBundle(output)
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
                ASTNode::Function {
                    function: function,
                    content: inner.map(parse).collect()
                }
            },
            //_ => ASTNode::Unfinnished(tree.as_str())
        }
    }

    parse(tree)
}