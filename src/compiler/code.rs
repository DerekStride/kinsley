use std::fmt;

use self::Instruction::*;

pub type Register = u8;
pub type Constant = u16;
pub type JumpTarget = i32;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub enum Instruction {
    Halt,
    Move { dest: Register, src: Register },
    Load { dest: Register, constant: Constant },
    LoadBool { dest: Register, bool: Constant },
    Add { dest: Register, a: Register, b: Register },
    Sub { dest: Register, a: Register, b: Register },
    Mul { dest: Register, a: Register, b: Register },
    Div { dest: Register, a: Register, b: Register },
    Neg { dest: Register, src: Register },
    Lt { dest: Register, a: Register, b: Register },
    Le { dest: Register, a: Register, b: Register },
    Eq { dest: Register, a: Register, b: Register },
    NotEq { dest: Register, a: Register, b: Register },
    SetGlobal { dest: Constant, src: Register },
    GetGlobal { dest: Register, src: Constant },
    Jmp { target: JumpTarget },
}

impl Instruction {
    // Instructions encoded as u32 a unsigned 32-bit integer that can take various formats. The
    // first byte is dedicated to the opcode. While the rest are used to access object stores. We
    // denote access to Registers as R(z), Constants as K(z), Globals as G(z), and Free Variables
    // as F(z) where z is A, B, C, Ax, or Bx.
    //
    // +----------------------------------------------------------------------------------------+
    // | 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 |
    // +----------------------------------------------------------------------------------------+
    // | Opcode         |                                 Ax                                    |
    // +----------------+----------------------+------------------------------------------------+
    // | Opcode         |           A          |                      Bx                        |
    // +----------------+----------------------+-----------------------+------------------------+
    // | Opcode         |           A          |           B           |           C            |
    // +----------------+----------------------+-----------------------+------------------------+
    pub fn encode(&self) -> u32 { 0 }

    pub fn last_dest_register(&self) -> Option<Register> {
        let reg = match self {
            Move { dest, .. } => dest,
            Load { dest, .. } => dest,
            LoadBool { dest, .. } => dest,
            Add { dest, .. } => dest,
            Sub { dest, .. } => dest,
            Mul { dest, .. } => dest,
            Div { dest, .. } => dest,
            Neg { dest, .. } => dest,
            Lt { dest, .. } => dest,
            Le { dest, .. } => dest,
            Eq { dest, .. } => dest,
            NotEq { dest, .. } => dest,
            _ => return None,
        };

        Some(*reg)
    }

    pub fn try_replace_register(&self, old: Register, new: Register) -> Option<Instruction> {
        let ins = match self {
            Add { dest, a, b } => {
                let dest = try_update(*dest, old, new);
                let a = try_update(*a, old, new);
                let b = try_update(*b, old, new);
                Add { dest, a, b }
            },
            Sub { dest, a, b } => {
                let dest = try_update(*dest, old, new);
                let a = try_update(*a, old, new);
                let b = try_update(*b, old, new);
                Sub { dest, a, b }
            },
            Mul { dest, a, b } => {
                let dest = try_update(*dest, old, new);
                let a = try_update(*a, old, new);
                let b = try_update(*b, old, new);
                Mul { dest, a, b }
            },
            Div { dest, a, b } => {
                let dest = try_update(*dest, old, new);
                let a = try_update(*a, old, new);
                let b = try_update(*b, old, new);
                Div { dest, a, b }
            },
            Lt { dest, a, b } => {
                let dest = try_update(*dest, old, new);
                let a = try_update(*a, old, new);
                let b = try_update(*b, old, new);
                Lt { dest, a, b }
            },
            Le { dest, a, b } => {
                let dest = try_update(*dest, old, new);
                let a = try_update(*a, old, new);
                let b = try_update(*b, old, new);
                Le { dest, a, b }
            },
            Eq { dest, a, b } => {
                let dest = try_update(*dest, old, new);
                let a = try_update(*a, old, new);
                let b = try_update(*b, old, new);
                Eq { dest, a, b }
            },
            NotEq { dest, a, b } => {
                let dest = try_update(*dest, old, new);
                let a = try_update(*a, old, new);
                let b = try_update(*b, old, new);
                NotEq { dest, a, b }
            },
            Neg { dest, src } => {
                let dest = try_update(*dest, old, new);
                let src = try_update(*src, old, new);
                Neg { dest, src }
            },
            Move { dest, src } => {
                let dest = try_update(*dest, old, new);
                let src = try_update(*src, old, new);
                Move { dest, src }
            },
            Load { dest, constant } => {
                let dest = try_update(*dest, old, new);
                Load { dest, constant: *constant }
            },
            LoadBool { dest, bool } => {
                let dest = try_update(*dest, old, new);
                LoadBool { dest, bool: *bool }
            },
            SetGlobal { dest, src } => {
                let src = try_update(*src, old, new);
                SetGlobal { dest: *dest, src }
            },
            GetGlobal { dest, src } => {
                let dest = try_update(*dest, old, new);
                GetGlobal { dest, src: *src }
            },
            _ => return None,
        };

        Some(ins)
    }
}

#[inline]
fn try_update(reg: Register, old: Register, new: Register) -> Register {
    if reg == old { new } else { reg }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Load { dest, constant } => write!(f, "load!({}, {})", dest, constant),
            Add { dest, a, b } => write!(f, "add!({}, {}, {})", dest, a, b),
            Sub { dest, a, b } => write!(f, "sub!({}, {}, {})", dest, a, b),
            Mul { dest, a, b } => write!(f, "mul!({}, {}, {})", dest, a, b),
            Div { dest, a, b } => write!(f, "div!({}, {}, {})", dest, a, b),
            SetGlobal { dest, src } => write!(f, "set_global!({}, {})", dest, src),
            _ => write!(f, "{:?}", self),
        }
    }
}

#[macro_export]
macro_rules! load {
    ($dest:expr, $constant:expr) => (
        $crate::compiler::code::Instruction::Load { dest: $dest, constant: $constant }
    )
}

#[macro_export]
macro_rules! add {
    ($dest:expr, $a:expr, $b:expr) => (
        $crate::compiler::code::Instruction::Add { dest: $dest, a: $a, b: $b }
    )
}

#[macro_export]
macro_rules! sub {
    ($dest:expr, $a:expr, $b:expr) => (
        $crate::compiler::code::Instruction::Sub { dest: $dest, a: $a, b: $b }
    )
}

#[macro_export]
macro_rules! mul {
    ($dest:expr, $a:expr, $b:expr) => (
        $crate::compiler::code::Instruction::Mul { dest: $dest, a: $a, b: $b }
    )
}

#[macro_export]
macro_rules! div {
    ($dest:expr, $a:expr, $b:expr) => (
        $crate::compiler::code::Instruction::Div { dest: $dest, a: $a, b: $b }
    )
}

#[macro_export]
macro_rules! neg {
    ($dest:expr, $src:expr) => (
        $crate::compiler::code::Instruction::Neg { dest: $dest, src: $src }
    )
}

#[macro_export]
macro_rules! set_global {
    ($global:expr, $src:expr) => (
        $crate::compiler::code::Instruction::SetGlobal { src: $src, dest: $global }
    )
}

#[macro_export]
macro_rules! get_global {
    ($dest:expr, $global:expr) => (
        $crate::compiler::code::Instruction::GetGlobal { dest: $dest, src: $global }
    )
}

#[macro_export]
macro_rules! load_true {
    ($dest:expr) => (
        $crate::compiler::code::Instruction::LoadBool { dest: $dest, bool: 0 }
    )
}

#[macro_export]
macro_rules! load_false {
    ($dest:expr) => (
        $crate::compiler::code::Instruction::LoadBool { dest: $dest, bool: 1 }
    )
}

#[macro_export]
macro_rules! lt {
    ($dest:expr, $a:expr, $b:expr) => (
        $crate::compiler::code::Instruction::Lt { dest: $dest, a: $a, b: $b }
    )
}

#[macro_export]
macro_rules! le {
    ($dest:expr, $a:expr, $b:expr) => (
        $crate::compiler::code::Instruction::Le { dest: $dest, a: $a, b: $b }
    )
}

#[macro_export]
macro_rules! eq {
    ($dest:expr, $a:expr, $b:expr) => (
        $crate::compiler::code::Instruction::Eq { dest: $dest, a: $a, b: $b }
    )
}

#[macro_export]
macro_rules! not_eq {
    ($dest:expr, $a:expr, $b:expr) => (
        $crate::compiler::code::Instruction::NotEq { dest: $dest, a: $a, b: $b }
    )
}

#[cfg(test)]
mod tests {
    use super::Instruction::*;

    #[test]
    fn test_load() {
        assert_eq!(
            Load { dest: 0, constant: 65534 },
            load!(0, 65534),
        );
    }
}
