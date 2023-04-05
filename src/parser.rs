use pest::{Parser, iterators::Pair};
use pest_derive::Parser;

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

#[derive(Debug,PartialEq, Clone, Copy)]
pub enum Identifier <'a> {
    UnEvaluated (&'a str),
    Evaluated (usize),
}

impl <'a> Identifier <'a> {
    pub fn as_str(&self, identifier_list : &'a Vec<String>) -> &str {
        match self {
            Identifier::UnEvaluated(str) => str,
            Identifier::Evaluated(index) => identifier_list.get(*index).unwrap(),
        }
    }
}

#[derive(Debug,PartialEq, Clone)]
pub struct AST <'a> {
    pub top_module : Identifier<'a>,
    pub internals: Vec<ASTStatement<'a>>,
    pub identifier_list : Vec<String>,
}

#[derive(Debug,PartialEq, Clone)]
pub enum ASTStatement <'a> {
    RegisterInstance {
        instance_name : Identifier<'a>,
        clock : Box<ASTExpression<'a>>,
        reset : Box<ASTExpression<'a>>,
        initial : Box<ASTExpression<'a>>,
        type_ : Box<ASTType<'a>>,
    },
    WireInstance {
        instance_name : Identifier<'a>,
        type_ : Box<ASTType<'a>>
    },
    Scope {
        internals: Vec<ASTStatement<'a>>
    },
    ModuleDeclaration {
        module_name : Identifier<'a>,
        generic_names : Vec<Identifier<'a>>,
        io : Box<ASTType<'a>>,
        body : Box<ASTStatement<'a>>
    },
    EnumDeclaration {
        enum_name : Identifier<'a>,
        variant_names : Vec<Identifier<'a>>,
    },
    ConstantDeclaration {
        constant_name : Identifier<'a>,
        type_ : Box<ASTType<'a>>,
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
        variable_name : Identifier<'a>,
        from : Box<ASTExpression<'a>>,
        to : Box<ASTExpression<'a>>,
        body : Box<ASTStatement<'a>>
    },
}

#[derive(Debug,PartialEq, Clone)]
pub enum ASTAtom <'a> {
    Empty,
    Target(Vec<(Identifier<'a>, Option<Box<ASTExpression<'a>>>)>),
    Enum {
        kind : Identifier<'a>,
        variant : Identifier<'a>,
    },
    Number {
        signed : bool,
        value : Number,
        width : Option<Box<ASTExpression<'a>>>
    },
    
}

#[derive(Debug,PartialEq, Clone)]
pub enum Number {
    HexNumber(String),
    OctNumber(String),
    BinNumber(String),
    DecNumber(String),
}

#[derive(Debug,PartialEq, Clone)]
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
    Bundle (Vec<(bool, Identifier<'a>, Box<ASTType<'a>>)>),
    Custom {
        name : Identifier<'a>,
        generic_instances : Option<Vec<ASTExpression<'a>>>
    },
}

#[derive(Debug,PartialEq, Clone)]
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
    Bundle (Vec<(bool, Identifier<'a>, Box<ASTExpression<'a>>)>),
    Function {
        function : Function,
        arguments : Vec<ASTExpression<'a>>
    },
    Atom (Box<ASTAtom<'a>>),
}

#[derive(Debug,PartialEq, Clone, Copy)]
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
    DynamicShiftLeft,
    DynamicShiftRight,

    Plus,
    Minus,

    Multiply,
    Divide,
    Remainder,
}

#[derive(Debug,PartialEq, Clone, Copy)]
pub enum UnaryOperation {
    Negate,
    Not,
}

#[allow(non_camel_case_types)]
#[derive(Debug,PartialEq, Clone, Copy)]
pub enum Function {
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
    dshl,
    dshr,

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
}

pub fn parse_nhdl(input : &str) -> AST{
    let tree = match NHDLParser::parse(Rule::Top, input) {
        Ok(mut p) => p.next().unwrap(),
        Err(e) => panic!("{}", e),
    };

    fn generate_ast(tree : Pair<Rule>) -> AST {
        match tree.as_rule() {
            Rule::OuterScope => {
                let mut inner = tree.into_inner();
                AST {
                    top_module: Identifier::UnEvaluated(inner.next().unwrap().as_str()),
                    internals: inner.map(parse_statement).collect(),
                    identifier_list : vec![],
                }
            },
            _ => unreachable!(),
        }
    }

    fn parse_statement(tree : Pair<Rule>) -> ASTStatement {
        match tree.as_rule() {
            Rule::RegisterInstanciation => {
                let mut inner = tree.into_inner();
                ASTStatement::RegisterInstance {
                    instance_name: Identifier::UnEvaluated(inner.next().unwrap().as_str()),
                    clock : Box::new(parse_expression(inner.next().unwrap())),
                    reset : Box::new(parse_expression(inner.next().unwrap())),
                    initial : Box::new(parse_expression(inner.next().unwrap())),
                    type_: Box::new(parse_type(inner.next().unwrap()))
                }
            }
            Rule::WireInstanciation => {
                let mut inner = tree.into_inner();
                ASTStatement::WireInstance {
                    instance_name: Identifier::UnEvaluated(inner.next().unwrap().as_str()),
                    type_: Box::new(parse_type(inner.next().unwrap()))
                }
            }
            Rule::Scope => ASTStatement::Scope {
                internals: tree.into_inner().map(parse_statement).collect(),
            },
            Rule::ModuleDeclaration => {
                let mut inner = tree.into_inner();
                let module_name = Identifier::UnEvaluated(inner.next().unwrap().as_str());
                let mut generic_names = vec![];
                
                let mut next = inner.next().unwrap();
                if let Rule::GenericConstant = next.as_rule() {
                    generic_names.append(&mut
                        next.into_inner().map(|n| Identifier::UnEvaluated(n.as_str())).collect()
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
                    enum_name: Identifier::UnEvaluated(inner.next().unwrap().as_str()),
                    variant_names: inner.map(|n|Identifier::UnEvaluated(n.as_str())).collect()
                }
            },
            Rule::ConstantDeclaration => {
                let mut inner = tree.into_inner();
                ASTStatement::ConstantDeclaration {
                    constant_name: Identifier::UnEvaluated(inner.next().unwrap().as_str()),
                    type_: Box::new(parse_type(inner.next().unwrap())),
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
                    variable_name: Identifier::UnEvaluated(inner.next().unwrap().as_str()),
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
            Rule::Target => {
                let mut inner = tree.into_inner();

                let mut output = vec![];
                while let Some(n) = inner.next() {
                    output.push((
                        Identifier::UnEvaluated(n.as_str()),
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
                    kind: Identifier::UnEvaluated(inner.next().unwrap().as_str()),
                    variant: Identifier::UnEvaluated(inner.next().unwrap().as_str())
                }
            },
            Rule::Number => {
                let signed = tree.as_str().starts_with('s');
                let mut inner = tree.into_inner();
                ASTAtom::Number {
                    signed: signed,
                    value: {
                        let tree = inner.next().unwrap();
                        match tree.as_rule() {
                            Rule::HexNumber => Number::HexNumber((&tree.as_str()[2..]).to_string()),
                            Rule::OctNumber => Number::OctNumber((&tree.as_str()[2..]).to_string()),
                            Rule::BinNumber => Number::BinNumber((&tree.as_str()[2..]).to_string()),
                            Rule::DecNumber => Number::DecNumber(tree.as_str().to_string()),
                            _ => unreachable!(),
                        }
                    },
                    width: inner.next().map(|e| Box::new(parse_expression(e))),
                }
            }
            _ => {
                eprintln!("{:?} : {}", tree.as_rule(), tree.as_str());
                unreachable!();
            },

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
                        Identifier::UnEvaluated(n.as_str()),
                        Box::new(parse_type(inner.next().unwrap()))
                    ))
                }

                ASTType::Bundle(output)
            },
            Rule::Custom => {
                let mut inner = tree.into_inner();

                ASTType::Custom {
                    name: Identifier::UnEvaluated(inner.next().unwrap().as_str()),
                    generic_instances: if let Some(_) = inner.peek() {
                        Some(inner.map(parse_expression).collect())
                    } else {
                        None
                    }
                }
            }
            _ => unreachable!()

        }
    }

    fn parse_expression(tree : Pair<Rule>) -> ASTExpression {
        match tree.as_rule() {
            Rule::Expression => parse_expression(tree.into_inner().next().unwrap()),
            Rule::Number |
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
                        Identifier::UnEvaluated(n.as_str()),
                        Box::new(parse_expression(inner.next().unwrap()))
                    ))
                }

                ASTExpression::Bundle(output)
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
                            "dshl" => Function::dshl,
                            "dshr" => Function::dshr,

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
                    _ => unreachable!(),
                };
                ASTExpression::Function {
                    function: function,
                    arguments: inner.map(parse_expression).collect()
                }
            },
            _ => unreachable!(),
        }
    }

    fn evaluate_identifiers(mut tree : AST) -> AST {

        fn insert(identifier : &str, identifier_list : &mut Vec<String>) -> usize{
            for (i, str) in identifier_list.iter().enumerate() {
                if str.eq(identifier) {
                    return i
                }
            }
            identifier_list.push(identifier.to_string());
            identifier_list.len() - 1
        }

        fn evaluate(identifier: &mut Identifier, identifier_list : &mut Vec<String>) {
            if let Identifier::UnEvaluated(str) = identifier {
                *identifier = Identifier::Evaluated(insert(str, identifier_list));
            }
        }

        fn evaluate_statement(tree : &mut ASTStatement, identifier_list : &mut Vec<String>) {
            match tree {
                ASTStatement::RegisterInstance {
                    instance_name,
                    clock,
                    reset,
                    initial,
                    type_,
                } => {
                    evaluate(instance_name, identifier_list);
                    evaluate_expression(clock, identifier_list);
                    evaluate_expression(reset, identifier_list);
                    evaluate_expression(initial, identifier_list);
                    evaluate_type(type_, identifier_list);
                },
                ASTStatement::WireInstance {
                    instance_name,
                    type_
                } => {
                    evaluate(instance_name, identifier_list);
                    evaluate_type(type_, identifier_list);
                },
                ASTStatement::Scope {
                    internals
                } => {
                    for statement in internals.iter_mut() {
                        evaluate_statement(statement, identifier_list);
                    }
                },
                ASTStatement::ModuleDeclaration {
                    module_name,
                    generic_names,
                    io,
                    body
                } => {
                    evaluate(module_name, identifier_list);
                    for generic in generic_names.iter_mut() {
                        evaluate(generic, identifier_list);
                    }
                    evaluate_type(io, identifier_list);
                    evaluate_statement(body, identifier_list);
                },
                ASTStatement::EnumDeclaration {
                    enum_name,
                    variant_names,
                } => {
                    evaluate(enum_name, identifier_list);
                    for variant in variant_names.iter_mut() {
                        evaluate(variant, identifier_list);
                    }
                },
                ASTStatement::ConstantDeclaration {
                    constant_name,
                    type_,
                    value,
                } => {
                    evaluate(constant_name, identifier_list);
                    evaluate_type(type_, identifier_list);
                    evaluate_expression(value, identifier_list);
                },
                ASTStatement::StrongConnection {
                    lhs,
                    rhs,
                } => {
                    evaluate_atom(lhs, identifier_list);
                    evaluate_expression(rhs, identifier_list);
                },
                ASTStatement::WeakConnection {
                    lhs,
                    rhs,
                } => {
                    evaluate_atom(lhs, identifier_list);
                    evaluate_expression(rhs, identifier_list);
                },
                ASTStatement::If {
                    condition,
                    true_body,
                    false_body,
                } => {
                    evaluate_expression(condition, identifier_list);
                    evaluate_statement(true_body, identifier_list);
                    if let Some(false_body) = false_body {
                        evaluate_statement(false_body, identifier_list);
                    }
                },
                ASTStatement::Switch {
                    condition,
                    paths,
                } => {
                    evaluate_expression(condition, identifier_list);
                    for (predicate, body) in paths.iter_mut() {
                        evaluate_atom(predicate, identifier_list);
                        evaluate_statement(body, identifier_list);
                    }
                },
                ASTStatement::Loop {
                    variable_name,
                    from,
                    to,
                    body,
                } => {
                    evaluate(variable_name, identifier_list);
                    evaluate_expression(from, identifier_list);
                    evaluate_expression(to, identifier_list);
                    evaluate_statement(body, identifier_list);
                },
            }
        }

        fn evaluate_atom(tree : &mut ASTAtom, identifier_list : &mut Vec<String>) {
            match tree {
                ASTAtom::Target(identifiers) => {
                    for (name, index) in identifiers.iter_mut() {
                        evaluate(name, identifier_list);
                        if let Some(index) = index {
                            evaluate_expression(index, identifier_list);
                        }
                    }
                },
                ASTAtom::Enum {
                    kind,
                    variant,
                } => {
                    evaluate(kind, identifier_list);
                    evaluate(variant, identifier_list);
                },
                _ => ()
            }
        }

        fn evaluate_type(tree : &mut ASTType, identifier_list : &mut Vec<String>) {
            match tree {
                ASTType::Vector {
                    type_,
                    size,
                } => {
                    evaluate_type(type_, identifier_list);
                    evaluate_expression(size, identifier_list);
                },
                ASTType::Unsigned (width) => {
                    evaluate_expression(width, identifier_list);
                },
                ASTType::Signed (width) => {
                    evaluate_expression(width, identifier_list);
                },
                ASTType::Bundle (inner) => {
                    for (_, name, type_) in inner.iter_mut() {
                        evaluate(name, identifier_list);
                        evaluate_type(type_, identifier_list);
                    }
                },
                ASTType::Custom {
                    name,
                    generic_instances,
                } => {
                    evaluate(name, identifier_list);
                    if let Some(generic_instances) = generic_instances {
                        for generic in generic_instances.iter_mut() {
                            evaluate_expression(generic, identifier_list);
                        }
                    }
                },
                _ => ()
            }
        }

        fn evaluate_expression(tree : &mut ASTExpression, identifier_list : &mut Vec<String>) {
            match tree {
                ASTExpression::BinaryOperation {
                    lhs,
                    operation:_,
                    rhs,
                } => {
                    evaluate_expression(lhs, identifier_list);
                    evaluate_expression(rhs, identifier_list);
                },
                ASTExpression::UnaryOperation {
                    operation:_,
                    content
                } => {
                    evaluate_expression(content, identifier_list);
                },
                ASTExpression::Bundle (inner) => {
                    for (_, name, expression) in inner.iter_mut() {
                        evaluate(name, identifier_list);
                        evaluate_expression(expression, identifier_list);
                    }
                },
                ASTExpression::Function {
                    function:_,
                    arguments,
                } => {
                    for argument in arguments.iter_mut() {
                        evaluate_expression(argument, identifier_list);
                    }
                },
                ASTExpression::Atom (atom) => {
                    evaluate_atom(atom, identifier_list);
                },
            }
        }

        for i in tree.internals.iter_mut() {
            evaluate_statement(i, &mut tree.identifier_list);
        }
        tree
    }

    evaluate_identifiers(generate_ast(tree))
}