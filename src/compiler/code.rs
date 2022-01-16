use std::collections::HashMap;

use byteorder::{BigEndian, ByteOrder};

use crate::error::{Result, Error};

use self::Opcode::*;


// Instructions are 32-bits long that can take various formats. The first byte is dedicated to the
// opcode. While the rest are used to access object stores. We denote access to Registers as R(z),
// Constants as K(z), Globals as G(z), and Free Variables as F(z) where z is Ax, Bx, A, B, or C.
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
pub type Instruction = u32;
pub type Operands = [u8; 3];

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub enum Opcode {
    Move, // R(A) = R(B)
    Load, // R(A) = K(Bx)
    Add, // R(A) = R(B) + R(C)
    Sub, // R(A) = R(B) - R(C)
    Mul, // R(A) = R(B) * R(C)
    Div, // R(A) = R(B) / R(C)
    // Eq,
    // NotEq,
    // GreaterThan,
    // Minus,
    // Bang,
    Jmp, // IP += sBx
    // Call, // R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
}

impl From<Opcode> for u8 {
    fn from(op: Opcode) -> u8 {
        op as u8
    }
}

impl Opcode {
    pub fn load(reg: u8, constant: u16) -> Operands {
        let mut operands = [reg; 3];
        BigEndian::write_u16(&mut operands[1..], constant);

        operands
    }
}

#[derive(Clone)]
pub struct Definition {
    pub name: &'static str,
    pub operand_widths: Vec<u8>,
}

pub struct Code {
    definitions: HashMap<u8, Definition>
}

impl Code {
    pub fn new() -> Self {
        let definitions = [
            (Move, Definition { name: "Move", operand_widths: vec![1, 1] }),
            (Load, Definition { name: "Load", operand_widths: vec![1, 2] }),
            (Add, Definition { name: "Add", operand_widths: vec![1, 1, 1] }),
            (Sub, Definition { name: "Sub", operand_widths: vec![1, 1, 1] }),
            (Mul, Definition { name: "Mul", operand_widths: vec![1, 1, 1] }),
            (Div, Definition { name: "Div", operand_widths: vec![1, 1, 1] }),
            (Jmp, Definition { name: "Jmp", operand_widths: vec![0, 2] }),
        ]
            .into_iter()
            .map(|(k, v)| (k as u8, v))
            .collect();

        Self { definitions }
    }

    pub fn lookup<T: Into<u8>>(&self, op: T) -> Result<&Definition> {
        let opcode = op.into();

        match self.definitions.get(&opcode) {
            Some(x) => Ok(x),
            None => Err(Error::new(format!("opcode {:x} undefined", opcode))),
        }
    }

    pub fn make<T: Into<u8>>(&self, op: T, operands: &Operands) -> Instruction {
        let opcode = op.into();

        let def = match self.definitions.get(&opcode) {
            Some(x) => x,
            None => return 0,
        };

        let mut instruction: Instruction = (opcode as Instruction) << 24;

        let mut i = 0;
        for width in &def.operand_widths {
            match width {
                0 => {},
                1 => instruction |= (operands[i] as u32) << (8 * (2 - i)),
                2 => {
                    let o = BigEndian::read_u16(&operands[i..]) as u32;
                    instruction |= o;
                    i += 1;
                },
                _ => return 0,
            };

            i += 1;
        };

        instruction
    }

    pub fn format(&self, instructions: &[Instruction]) -> String {
        let mut buf = String::new();

        for (i, ins) in instructions.iter().enumerate() {
            let opcode = (ins >> 24) as u8;
            let def = match self.lookup(opcode) {
                Ok(x) => x,
                Err(e) => {
                    buf.push_str("ERROR: ");
                    buf.push_str(&e.to_string());
                    buf.push_str("\n");
                    continue;
                },
            };

            let operands = match Code::read_operands(def, *ins) {
                Ok(x) => x,
                Err(e) => {
                    buf.push_str("ERROR: ");
                    buf.push_str(&e.to_string());
                    buf.push_str("\n");
                    continue;
                },
            };

            buf.push_str(&format!("{:0>4} {}\n", i * 4, Code::format_ins(&def, &operands)));
        };

        buf
    }

    fn format_ins(def: &Definition, operands: &[u32]) -> String {
        let mut buf = String::new();
        let op_count = operands.len();

        match op_count {
            0 => buf.push_str(&format!("{}", def.name)),
            1 => buf.push_str(&format!("{} {}", def.name, operands[0])),
            2 => buf.push_str(&format!("{} {} {}", def.name, operands[0], operands[1])),
            3 => buf.push_str(&format!("{} {} {} {}", def.name, operands[0], operands[1], operands[2])),
            _ => buf.push_str(&format!("ERROR: unhandled operand count ({}) for {}\n", op_count, def.name)),
        }

        buf
    }

    fn read_operands(def: &Definition, ins: Instruction) -> Result<Vec<u32>> {
        let mut operands = Vec::new();

        for (i, width) in def.operand_widths.iter().enumerate() {
            let mask = match i {
                0 => 0x00FF_FFFF,
                1 => 0x0000_FFFF,
                _ => 0x0000_00FF,
            };
            let masked = ins & mask;
            let shift = 8 * (2 - i);

            match width {
                1 => operands.push(masked >> shift),
                2 => operands.push(masked),
                0 => continue,
                _  => return Err(Error::new(format!("No support for operands of width={}", width))),
            };
        }

        Ok(operands)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem;

    #[test]
    fn test_repr() {
        assert_eq!(1, mem::size_of::<Opcode>());

        assert_eq!(0, Move as u8);
        assert_eq!(1, Load as u8);
    }

    #[test]
    fn test_make() -> Result<()> {
        let code = Code::new();

        let tests: Vec<(Opcode, Operands, Instruction)> = vec![
            (Move,  [0, 1, 0],          0x0000_0100),
            (Load,  [0, 255, 254],      0x0100_FFFE),
            (Load,  [254, 1, 254],      0x01FE_01FE),
            (Add,   [0, 1, 254],        0x0200_01FE),
            (Jmp,   [0, 254, 253],      0x0600_FEFD),
        ];

        for tt in tests {
            let instruction = code.make(tt.0, &tt.1);
            assert_eq!(tt.2, instruction, "\n\nexpected:\t0x{:0>4x} {:0>4x}\ngot:\t\t0x{:0>4x} {:0>4x}\n\n", (tt.2 >> 16), tt.2 as u16, (instruction >> 16), instruction as u16);
        };

        Ok(())
    }

    #[test]
    fn test_load() {
        assert_eq!([0, 255, 254], Opcode::load(0, 65534));
        assert_eq!([253, 1, 254], Opcode::load(253, 510));
    }

    #[test]
    fn test_fmt_display() -> Result<()> {
        let code = Code::new();

        let instructions = vec![
            code.make(Move,     &[0, 1, 0]),
            code.make(Load,     &[0, 255, 254]),
            code.make(Load,     &[255, 1, 254]),
            code.make(Add,      &[0, 1, 2]),
            code.make(Jmp,      &[0, 255, 254]),
        ];

        let expected = vec![
            "0000 Move 0 1\n",
            "0004 Load 0 65534\n",
            "0008 Load 255 510\n",
            "0012 Add 0 1 2\n",
            "0016 Jmp 65534\n",
        ].join("");

        let actual = code.format(&instructions);
        assert_eq!(expected, actual, "\nexpected:\n{}\nactual:\n{}\n", expected, actual);

        Ok(())
    }
}
