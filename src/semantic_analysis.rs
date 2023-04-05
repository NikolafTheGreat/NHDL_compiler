use std::{collections::HashMap, num::ParseIntError};

use thiserror::Error;

use super::parser::{AST, ASTType, ASTAtom, ASTExpression, ASTStatement, BinaryOperation, UnaryOperation, Function};

/*
steps:
    * Scan through scope and note down named components (modules, enums, wires, registers) and their types
    * 
*/

