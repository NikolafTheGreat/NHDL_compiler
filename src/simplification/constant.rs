/*
+-------------------------------------------------------------------------------------+
| This file contains a data type used for the simplification of constant expressions. |
| It acts as an integer with arbitrary bit width                                      |
+-------------------------------------------------------------------------------------+
*/

use num::{BigInt};

use thiserror::Error;

use crate::parser::{Number, ASTAtom};

#[derive(Debug, Clone)]
pub struct Constant {
    pub signed: bool,
    pub value: Vec<bool>,
}

#[derive(Debug, Error)]
pub enum ConstantError {
    #[error("Constant  is too long")]
    TooLong,
    #[error("Cant convert signed to unsigned")]
    Signed,
    #[error("Cant shift by signed value")]
    SignedShift,
    #[error("The types on eigther side of an expression do not match")]
    WrongType,
}

impl Constant {
    pub fn new_unsigned() -> Self {
        Self {
            signed: false,
            value: vec![],
        }
    }
    pub fn new_signed() -> Self {
        Self {
            signed: true,
            value: vec![true],
        }
    }
    pub fn as_usize(&self) -> Result<usize, ConstantError> {
        if self.signed {
            Err(ConstantError::Signed)
        } else if self.value.len() > usize::BITS as usize {
            Err(ConstantError::TooLong)
        } else {
            let mut out = 0;
            for (i, add) in self.value.iter().enumerate() {
                if *add {
                    out += 1 << i;
                }
            }
            Ok(out)
        }
    }
    /*pub fn as_isize(&self) -> Result<isize, ConstantError> {
        if self.value.len() > isize::BITS as usize {
            Err(ConstantError::TooLong)
        } else {
            let mut out = 0;
            for (i, add) in self.value.iter().enumerate() {
                if *add {
                    out += 1 << i;
                }
            }
            Ok(out)
        }
    }*/
    pub fn as_str(&mut self) -> String {
        let mut out = String::new();
        let neg = self.is_negative();
        if neg {
            out.push('-');
            self.neg();
        }
        for digit in self.value.iter().rev() {
            out.push(if *digit {
                '1'
            } else {
                '0'
            })
        }
        if neg {
            self.neg();
        }
        out
    }
    pub fn as_number(&mut self) -> Number {
        Number::BinNumber(self.as_str())
    }
    pub fn as_atom(&mut self) -> ASTAtom {
        ASTAtom::Number {
            signed: self.signed,
            value: self.as_number(),
            width: None
        }
    }
    pub fn from_bin(str: &str) -> Self {
        let mut value = vec![];
        let mut signed = false;
        let mut iter = str.chars();
        if str.starts_with('-') {
            signed = true;
            iter.next();
        }
        for i in iter.rev() {
            value.push(match i {
                '0' => false,
                '1' => true,
                _ => unreachable!()
            })
        }
        let mut out = Self {
            signed: false,
            value: value
        };
        if signed {
            out.neg()
        }
        out
    }
    pub fn from_oct(str: &str) -> Self{
        let mut value = vec![];
        let mut signed = false;
        let mut iter = str.chars();
        if str.starts_with('-') {
            signed = true;
            iter.next();
        }
        for i in iter.rev() {
            let mut digit = match i {
                '0' => vec![false,false,false],
                '1' => vec![true ,false,false],
                '2' => vec![false,true ,false],
                '3' => vec![true ,true ,false],
                '4' => vec![false,false,true ],
                '5' => vec![true ,false,true ],
                '6' => vec![false,true ,true ],
                '7' => vec![true ,true ,true ],
                _ => unreachable!()
            };
            value.append(&mut digit);
        }
        while value.ends_with(&[false]) {
            value.pop();
        }
        let mut out = Self {
            signed: false,
            value: value
        };
        if signed {
            out.neg()
        }
        out
    }

    pub fn from_hex(str: &str) -> Self{
        let mut value = vec![];
        let mut signed = false;
        let mut iter = str.chars();
        if str.starts_with('-') {
            signed = true;
            iter.next();
        }
        for i in iter.rev() {
            let mut digit = match i {
                '0' => vec![false,false,false,false],
                '1' => vec![true,false,false,false],
                '2' => vec![false,true,false,false],
                '3' => vec![true ,true,false,false],
                '4' => vec![false,false,true,false],
                '5' => vec![true ,false,true,false],
                '6' => vec![false,true ,true,false],
                '7' => vec![true ,true ,true,false],
                '8' => vec![false,false,false,true ],
                '9' => vec![true ,false,false,true ],
                'a'|'A' => vec![false,true ,false,true ],
                'b'|'B' => vec![true ,true ,false,true ],
                'c'|'C' => vec![false,false,true ,true ],
                'd'|'D' => vec![true ,false,true ,true ],
                'e'|'E' => vec![false,true ,true ,true ],
                'f'|'F' => vec![true ,true ,true ,true ],
                _ => unreachable!()
            };
            value.append(&mut digit);
        }
        while value.ends_with(&[false]) {
            value.pop();
        }
        let mut out = Self {
            signed: false,
            value: value
        };
        if signed {
            out.neg()
        }
        out
    }
    pub fn from_dec(str: &str) -> Self{
        let mut value = vec![];
        let mut signed = false;
        if str.starts_with('-') {
            signed = true;
        }
        let a : BigInt = str.parse().unwrap();
        for i in 0..a.bits() + if signed {1} else {0} {
            value.push(a.bit(i));
        }
        if value.len() == 0 {
            value.push(false);
        }
        Self {
            signed: signed,
            value: value
        }
    }

    pub fn from_atom(signed: bool, number: Number, width: Option<usize>) -> Self {
        let mut out = match number {
            Number::BinNumber(str) => Self::from_bin(&str),
            Number::OctNumber(str) => Self::from_oct(&str),
            Number::DecNumber(str) => Self::from_dec(&str),
            Number::HexNumber(str) => Self::from_hex(&str),
        };
        if let Some(width) = width {
            while out.value.len() > width {
                out.value.pop();
            }
            while out.value.len() < width {
                if out.signed {
                    out.value.push(*out.value.last().unwrap());
                } else {
                    out.value.push(false);
                }
            }
        }
        if out.signed != signed {
            panic!("Something went wrong. An the sign of a number did not match the internal atom");
        }
        out
    }
    pub fn neg(&mut self) {
        if !self.signed {
            self.signed = true;
            self.value.push(false);
        }
        self.not();
        for digit in self.value.iter_mut() {
            *digit = !*digit;
            if *digit {
                break;
            }
        }
    }
    pub fn not(&mut self) {
        for digit in self.value.iter_mut() {
            *digit = !*digit;
        }
    }
    pub fn is_negative(&self) -> bool {
        self.signed && *self.value.last().unwrap()
    }
    pub fn extend(&mut self, width : usize){
        while self.value.len() < width {
            if self.signed {
                self.value.push(*self.value.last().unwrap());
            } else {
                self.value.push(false);
            }
        }
    }
    pub fn reduce(&mut self, width : usize){
        while self.value.len() > width {
            self.value.pop();
        }
    }
    pub fn greater_than(mut a: Self, mut b: Self) -> Self {
        if a.is_negative() && !b.is_negative() {
            return Self{
                signed: false,
                value: vec![false]
            }
        }
        if !a.is_negative() && b.is_negative() {
            return Self{
                signed: false,
                value: vec![true]
            }
        }
        if a.is_negative() && b.is_negative() {
            a.neg();
            b.neg();
            return Self::greater_than(b, a);
        }
        while a.value.len() > b.value.len() {
            if a.value.pop().unwrap() {
                return Self{
                    signed: false,
                    value: vec![true]
                }
            }
        }
        while a.value.len() < b.value.len() {
            if b.value.pop().unwrap() {
                return Self{
                    signed: false,
                    value: vec![false]
                }
            }
        }
        for (a,b) in a.value.iter().rev().zip(b.value.iter().rev()) {
            if a > b {
                return Self{
                    signed: false,
                    value: vec![true]
                }
            } else if a < b {
                return Self{
                    signed: false,
                    value: vec![false]
                }
            }
        }
        Self{
            signed: false,
            value: vec![false]
        }
    }
    pub fn greater_than_or_equal(mut a: Self, mut b: Self) -> Self {
        if a.is_negative() && !b.is_negative() {
            return Self{
                signed: false,
                value: vec![false]
            }
        }
        if !a.is_negative() && b.is_negative() {
            return Self{
                signed: false,
                value: vec![true]
            }
        }
        if a.is_negative() && b.is_negative() {
            a.neg();
            b.neg();
            return Self::greater_than_or_equal(b, a);
        }
        while a.value.len() > b.value.len() {
            if a.value.pop().unwrap() {
                return Self{
                    signed: false,
                    value: vec![true]
                }
            }
        }
        while a.value.len() < b.value.len() {
            if b.value.pop().unwrap() {
                return Self{
                    signed: false,
                    value: vec![false]
                }
            }
        }
        for (a,b) in a.value.iter().rev().zip(b.value.iter().rev()) {
            if a > b {
                return Self{
                    signed: false,
                    value: vec![true]
                }
            } else if a < b {
                return Self{
                    signed: false,
                    value: vec![false]
                }
            }
        }
        Self{
            signed: false,
            value: vec![true]
        }
    }
    pub fn equal(mut a: Self, mut b: Self) -> Self {
        if a.is_negative() != b.is_negative() {
            return Self{
                signed: false,
                value: vec![false]
            }
        }
        while a.value.len() > b.value.len() {
            if a.value.pop().unwrap() {
                return Self{
                    signed: false,
                    value: vec![false]
                }
            }
        }
        while a.value.len() < b.value.len() {
            if b.value.pop().unwrap() {
                return Self{
                    signed: false,
                    value: vec![false]
                }
            }
        }
        for (a,b) in a.value.iter().rev().zip(b.value.iter().rev()) {
            if a != b {
                return Self{
                    signed: false,
                    value: vec![false]
                }
            }
        }
        Self{
            signed: false,
            value: vec![true]
        }
    }
    pub fn concatinate(mut a: Self, mut b: Self) -> Self {
        b.value.append(&mut a.value);
        b.signed = false;
        b
    }
    pub fn and(mut a: Self, mut b: Self) -> Self {
        a.extend(b.value.len());
        b.extend(a.value.len());
        Self {
            signed: false,
            value: a.value.iter().zip(b.value.iter()).map(|(a,b)| a & b).collect()
        }
    }
    pub fn or(mut a: Self, mut b: Self) -> Self {
        a.extend(b.value.len());
        b.extend(a.value.len());
        Self {
            signed: false,
            value: a.value.iter().zip(b.value.iter()).map(|(a,b)| a | b).collect()
        }
    }
    pub fn xor(mut a: Self, mut b: Self) -> Self {
        a.extend(b.value.len());
        b.extend(a.value.len());
        Self {
            signed: false,
            value: a.value.iter().zip(b.value.iter()).map(|(a,b)| a ^ b).collect()
        }
    }
    pub fn shift_left(mut a: Self, b: Self) -> Result<Self, ConstantError> {
        if b.signed {
            return Err(ConstantError::SignedShift);
        }
        let n = b.as_usize().unwrap();
        let mut value = vec![false; n];
        value.append(&mut a.value);
        Ok(Self {
            signed: a.signed,
            value: value
        })
    }
    pub fn shift_left_by_usize(mut a: Self, b: usize) -> Self {
        let mut value = vec![false; b];
        value.append(&mut a.value);
        Self {
            signed: a.signed,
            value: value
        }
    }
    pub fn shift_right(a: Self, b: Self) -> Result<Self, ConstantError> {
        if b.signed {
            return Err(ConstantError::SignedShift);
        }
        let n = b.as_usize().unwrap();
        Ok(Self {
            signed: a.signed,
            value: a.value.into_iter().rev().skip(n).rev().collect()
        })
    }

    pub fn add(mut a: Self, mut b: Self) -> Result<Self, ConstantError> {
        if a.signed != b.signed {
            return Err(ConstantError::WrongType)
        }
        a.extend(b.value.len());
        b.extend(a.value.len());
        
        let mut out = Vec::new();
        let mut carry = false;
        for (a,b) in a.value.into_iter().zip(b.value.into_iter()) {
            out.push((a ^ b) ^ carry);
            carry = (a & b) | ((a ^ b) & carry);
        }
        out.push(carry);

        Ok(Self {
            signed: a.signed,
            value: out
        })
    }

    pub fn sub(mut a: Self, mut b: Self) -> Result<Self, ConstantError> {
        if a.signed != b.signed {
            return Err(ConstantError::WrongType)
        }
        a.extend(b.value.len());
        b.extend(a.value.len());
        
        let mut out = Vec::new();
        let mut carry = false;
        for (a,b) in a.value.into_iter().zip(b.value.into_iter()) {
            out.push((a ^ b) ^ carry);
            carry = (!a & b) | (!(a ^ b) & carry);
        }
        out.push(carry);

        Ok(Self {
            signed: a.signed,
            value: out
        })
    }

    pub fn mul(mut a: Self, mut b: Self) -> Result<Self, ConstantError> {
        if a.signed != b.signed {
            return Err(ConstantError::WrongType)
        }
        
        let mut flipped = if a.is_negative() {
            a.neg();
            true
        } else {false};

        flipped ^= if b.is_negative() {
            b.neg();
            true
        } else {false};

        let mut out = if a.signed {
            Self::new_signed()
        } else {
            Self::new_unsigned()
        };

        for (i, a) in a.value.into_iter().enumerate() {
            if a {
                out = Constant::add(out, Constant::shift_left_by_usize(b.clone(), i)).unwrap();
            }
        }
        if flipped {
            out.neg();
        }
        Ok(out)
    }

    pub fn div(mut a: Self, mut b: Self) -> Result<Self, ConstantError> {
        if a.signed != b.signed {
            return Err(ConstantError::WrongType)
        }
        
        let mut flipped = if a.is_negative() {
            a.neg();
            true
        } else {false};

        flipped ^= if b.is_negative() {
            b.neg();
            true
        } else {false};

        let len = a.value.len();

        let mut out = 0;

        while Constant::greater_than_or_equal(a.clone(), b.clone()).as_usize().unwrap() == 1 {
            a = Constant::sub(a, b.clone()).unwrap();
            a.reduce(len);
            out += 1;
        }

        let mut out = Constant::from_bin(&format!("{:b}", out));
        out.extend(len);

        if flipped {
            out.neg();
        }

        Ok(out)
    }

    pub fn rem(mut a: Self, mut b: Self) -> Result<Self, ConstantError> {
        if a.signed != b.signed {
            return Err(ConstantError::WrongType)
        }
        
        let mut flipped = if a.is_negative() {
            a.neg();
            true
        } else {false};

        flipped ^= if b.is_negative() {
            b.neg();
            true
        } else {false};

        let len = a.value.len();

        while Constant::greater_than_or_equal(a.clone(), b.clone()).as_usize().unwrap() == 1 {
            a = Constant::sub(a, b.clone()).unwrap();
            a.reduce(len);
        }

        while a.value.len() > b.value.len() {
            a.value.pop();
        }

        if flipped {
            a.neg();
        }

        Ok(a)
    }

    pub fn reduce_and (&mut self) {
        let mut out = false;
        for a in self.value.iter() {
            out &= a;
        }
        self.value = vec![out];
        self.signed = false;
    }

    pub fn reduce_or (&mut self) {
        let mut out = false;
        for a in self.value.iter() {
            out |= a;
        }
        self.value = vec![out];
        self.signed = false;
    }

    pub fn reduce_xor (&mut self) {
        let mut out = false;
        for a in self.value.iter() {
            out ^= a;
        }
        self.value = vec![out];
        self.signed = false;
    }
}