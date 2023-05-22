/*
+---------------------------------------------------------------------+
| This file contains the entirety of the parsing step in compilation: |
|   * Use pest to generate the parse tree                             |
|   * Discard excess information to generate the AST                  |
|   * Gather all identifiers into a list for later use                |
+---------------------------------------------------------------------+
*/

use pest::{Parser, iterators::Pair, error};
use pest_derive::Parser;
use thiserror::Error;

#[derive(Parser)]
#[grammar = "parser.pest"]
struct NHDLParser;

#[derive(Debug, Error)]
pub enum ParserError<E> {
    #[error("Failed to parse: {0}")]
    CouldNotParse(E),
}

impl AST {
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

#[derive(Debug,PartialEq, Clone)]
pub enum Identifier  {
    UnEvaluated (String),
    Evaluated (usize),
}

impl Identifier {
    pub fn as_str<'a>(&'a self, identifier_list : &'a Vec<String>) -> &'a str {
        match self {
            Identifier::UnEvaluated(str) => str,
            Identifier::Evaluated(index) => identifier_list.get(*index).unwrap(),
        }
    }
    pub fn eq(&self, other : &Self, identifier_list : & Vec<String>) -> bool{
        match self {
            Identifier::UnEvaluated(self_str) => {
                match other {
                    Identifier::UnEvaluated(other_str) => {
                        self_str.eq(other_str)
                    },
                    Identifier::Evaluated(other_index) => {
                        self_str.eq(&identifier_list[*other_index])
                    }
                }
            },
            Identifier::Evaluated(self_index) => {
                match other {
                    Identifier::UnEvaluated(other_str) => {
                        identifier_list[*self_index].eq(other_str)
                    },
                    Identifier::Evaluated(other_index) => {
                        *self_index == *other_index
                    }
                }
            }
        }
    }
    pub fn get_index(&self) -> usize {
        match self {
            Identifier::UnEvaluated(str) => panic!("Tried to get index of unevaluated identifier: {}", str),
            &Identifier::Evaluated(i) => i
        }
    }
}

#[derive(Debug,PartialEq, Clone)]
pub struct AST  {
    pub top_module : Identifier,
    pub internals: Vec<ASTStatement>,
    pub identifier_list : Vec<String>,
}

#[derive(Debug,PartialEq, Clone)]
pub enum ASTStatement  {
    RegisterInstance {
        instance_name : Identifier,
        clock : Box<ASTExpression>,
        reset : Option<Box<ASTExpression>>,
        initial : Option< Box<ASTExpression>>,
        type_ : Box<ASTType>,
    },
    WireInstance {
        instance_name : Identifier,
        type_ : Box<ASTType>
    },
    ModuleInstance {
        instance_name : Identifier,
        type_ : Identifier,
        generic_values : Vec<ASTExpression>,
    },
    Scope {
        internals: Vec<ASTStatement>
    },
    ModuleDeclaration {
        module_name : Identifier,
        generic_names : Vec<(Identifier, Box<ASTType>)>,
        io : Box<ASTType>,
        body : Box<ASTStatement>
    },
    EnumDeclaration {
        enum_name : Identifier,
        variant_names : Vec<Identifier>,
    },
    ConstantDeclaration {
        constant_name : Identifier,
        type_ : Box<ASTType>,
        value : Box<ASTExpression>
    },
    StrongConnection {
        lhs : Box<ASTAtom>,
        rhs : Box<ASTExpression>
    },
    WeakConnection {
        lhs : Box<ASTAtom>,
        rhs : Box<ASTExpression>
    },
    If {
        condition : Box<ASTExpression>,
        true_body : Box<ASTStatement>,
        false_body : Option<Box<ASTStatement>>,
    },
    Switch {
        condition : Box<ASTExpression>,
        paths : Vec<(Box<ASTAtom>, Box<ASTStatement>)>
    },
    Loop {
        variable_name : Identifier,
        from : Box<ASTExpression>,
        to : Box<ASTExpression>,
        body : Box<ASTStatement>
    },
    Node {
        name : Identifier,
        value : Box<ASTExpression>,
    }
}

#[derive(Debug,PartialEq, Clone)]
pub enum ASTAtom  {
    Empty,
    Target(Vec<(Identifier, Option<Box<ASTExpression>>)>),
    Enum {
        kind : Identifier,
        variant : Identifier,
    },
    Number {
        signed : bool,
        value : Number,
        width : Option<Box<ASTExpression>>
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
pub enum ASTType  {
    Vector {
        type_ : Box<ASTType>,
        size : Box<ASTExpression>
    },
    Unsigned (Box<ASTExpression>),
    Signed (Box<ASTExpression>),
    Clock,
    AsyncReset,
    SyncReset,
    Bundle (Vec<(bool, Identifier, Box<ASTType>)>),
    Enum (Identifier),
}

#[derive(Debug,PartialEq, Clone)]
pub enum ASTExpression  {
    BinaryOperation {
        lhs : Box<ASTExpression>,
        operation : BinaryOperation,
        rhs : Box<ASTExpression>
    },
    UnaryOperation {
        operation : UnaryOperation,
        content : Box<ASTExpression>
    },
    Bundle (Vec<(Identifier, Box<ASTExpression>)>),
    Function {
        function : Function,
        arguments : Vec<ASTExpression>
    },
    Atom (Box<ASTAtom>),
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

pub fn parse_nhdl(input : &str) -> Result<AST, ParserError<error::Error<Rule>>>{
    let tree = match NHDLParser::parse(Rule::Top, input) {
        Ok(mut p) => p.next().unwrap(),
        Err(e) => return Err(ParserError::CouldNotParse(e)),
    };

    fn generate_ast(tree : Pair<Rule>) -> AST {
        match tree.as_rule() {
            Rule::OuterScope => {
                let mut inner = tree.into_inner();
                AST {
                    top_module: Identifier::UnEvaluated(inner.next().unwrap().as_str().to_string()),
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
                let instance_name = Identifier::UnEvaluated(inner.next().unwrap().as_str().to_string());
                let clock = Box::new(parse_expression(inner.next().unwrap()));
                let reset = if let Rule::Expression = inner.peek().unwrap().as_rule() {
                    Some(Box::new(parse_expression(inner.next().unwrap())))
                } else {None};
                let initial = if let Rule::Expression = inner.peek().unwrap().as_rule() {
                    Some(Box::new(parse_expression(inner.next().unwrap())))
                } else {None};
                let type_= Box::new(parse_type(inner.next().unwrap()));
                ASTStatement::RegisterInstance {
                    instance_name: instance_name,
                    clock : clock,
                    reset : reset,
                    initial : initial,
                    type_: type_
                }
            }
            Rule::WireInstanciation => {
                let mut inner = tree.into_inner();
                ASTStatement::WireInstance {
                    instance_name: Identifier::UnEvaluated(inner.next().unwrap().as_str().to_string()),
                    type_: Box::new(parse_type(inner.next().unwrap()))
                }
            }
            Rule::ModuleInstance => {
                let mut inner = tree.into_inner();
                
                ASTStatement::ModuleInstance {
                    instance_name: Identifier::UnEvaluated(inner.next().unwrap().as_str().to_string()),
                    type_: Identifier::UnEvaluated(inner.next().unwrap().as_str().to_string()),
                    generic_values: inner.map(parse_expression).collect()
                }
            },
            Rule::Scope => ASTStatement::Scope {
                internals: tree.into_inner().map(parse_statement).collect(),
            },
            Rule::ModuleDeclaration => {
                let mut inner = tree.into_inner();
                let module_name = Identifier::UnEvaluated(inner.next().unwrap().as_str().to_string());
                let mut generic_names = vec![];
                
                let mut next = inner.next().unwrap();
                if let Rule::GenericConstant = next.as_rule() {
                    {
                        let mut inner = next.into_inner();
                        loop {
                            generic_names.push((
                                Identifier::UnEvaluated(inner.next().unwrap().as_str().to_string()),
                                Box::new(parse_type(inner.next().unwrap()))
                            ));
                            if inner.peek().is_none() {
                                break;
                            }
                        }
                    }
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
                    enum_name: Identifier::UnEvaluated(inner.next().unwrap().as_str().to_string()),
                    variant_names: inner.map(|n|Identifier::UnEvaluated(n.as_str().to_string())).collect()
                }
            },
            Rule::ConstantDeclaration => {
                let mut inner = tree.into_inner();
                ASTStatement::ConstantDeclaration {
                    constant_name: Identifier::UnEvaluated(inner.next().unwrap().as_str().to_string()),
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
                    variable_name: Identifier::UnEvaluated(inner.next().unwrap().as_str().to_string()),
                    from: Box::new(parse_expression(inner.next().unwrap())),
                    to: Box::new(parse_expression(inner.next().unwrap())),
                    body: Box::new(parse_statement(inner.next().unwrap()))
                }
            },
            Rule::Node => {
                let mut inner = tree.into_inner();
                ASTStatement::Node {
                    name: Identifier::UnEvaluated(inner.next().unwrap().as_str().to_string()),
                    value: Box::new(parse_expression(inner.next().unwrap())), 
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
                        Identifier::UnEvaluated(n.as_str().to_string()),
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
                    kind: Identifier::UnEvaluated(inner.next().unwrap().as_str().to_string()),
                    variant: Identifier::UnEvaluated(inner.next().unwrap().as_str().to_string())
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
                            Rule::HexNumber => Number::HexNumber((&tree.as_str()[1..]).to_string()),
                            Rule::OctNumber => Number::OctNumber((&tree.as_str()[1..]).to_string()),
                            Rule::BinNumber => Number::BinNumber((&tree.as_str()[1..]).to_string()),
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
                        Identifier::UnEvaluated(n.as_str().to_string()),
                        Box::new(parse_type(inner.next().unwrap()))
                    ))
                }

                ASTType::Bundle(output)
            },
            Rule::IOTypeBundle => {
                let mut inner = tree.into_inner();
                
                let mut output = vec![];
                while let Some(mut n) = inner.next() {
                    let flipped = n.as_str().eq("out");
                    output.push((
                        flipped,
                        Identifier::UnEvaluated(inner.next().unwrap().as_str().to_string()),
                        Box::new(parse_type(inner.next().unwrap()))
                    ))
                }

                ASTType::Bundle(output)
            },
            Rule::EnumType => {
                let mut inner = tree.into_inner();
                ASTType::Enum(Identifier::UnEvaluated(inner.next().unwrap().as_str().to_string()))
            },
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
                    output.push((
                        Identifier::UnEvaluated(n.as_str().to_string()),
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
            _ => {
                println!("{}", tree.as_str());
                unreachable!()
            },
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
                    if let Some(reset) = reset {
                        evaluate_expression(reset, identifier_list);
                        if let Some(initial) = initial {
                            evaluate_expression(initial, identifier_list);
                        }
                    }
                    evaluate_type(type_, identifier_list);
                },
                ASTStatement::WireInstance {
                    instance_name,
                    type_
                } => {
                    evaluate(instance_name, identifier_list);
                    evaluate_type(type_, identifier_list);
                },
                ASTStatement::ModuleInstance {
                    instance_name,
                    type_,
                    generic_values
                } => {
                    evaluate(instance_name, identifier_list);
                    evaluate(type_, identifier_list);
                    for generic in generic_values.iter_mut() {
                        evaluate_expression(generic, identifier_list);
                    }
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
                    for (generic, type_) in generic_names.iter_mut() {
                        evaluate(generic, identifier_list);
                        evaluate_type(type_, identifier_list)
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
                ASTStatement::Node { 
                    name, 
                    value 
                } => {
                    evaluate(name, identifier_list);
                    evaluate_expression(value, identifier_list);
                }
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
                ASTAtom::Number {
                    signed:_, 
                    value:_, 
                    width 
                } => {
                    if let Some(width) = width {
                        evaluate_expression(width, identifier_list);
                    }
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
                    for (name, expression) in inner.iter_mut() {
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

        evaluate(&mut tree.top_module, &mut tree.identifier_list);
        for i in tree.internals.iter_mut() {
            evaluate_statement(i, &mut tree.identifier_list);
        }
        tree
    }

    Ok(evaluate_identifiers(generate_ast(tree)))
}