use std::fmt;

use crate::{
    error::*,
    object::Primitive,
    compiler::code::Instruction,
};

const VERSION: u16 = 1;

pub struct Bytecode {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<Primitive>,
}

impl Bytecode {
    pub fn encode(&self) -> Result<Vec<u8>> {
        let mut results = self.header().encode();

        for instruction in &self.instructions {
            let ins = instruction.encode();
            let mut mask: u32 = 0xFF00_0000;
            for i in (0..4).rev() {
                let ins_byte: u8 = ((ins & mask) >> (8 * i)).try_into().unwrap();
                mask = mask >> 8;
                results.push(ins_byte);
            };
        };

        Ok(results)
    }

    fn header(&self) -> Header {
        Header::new(
            self.constants.len().try_into().unwrap(),
            self.instructions.len().try_into().unwrap(),
        )
    }
}

impl Bytecode {
    pub fn format_instructions(ins: &[Instruction]) -> String {
        ins
            .iter()
            .enumerate()
            .map(|(idx, instruction)| format!("{}: {}", idx, instruction))
            .collect::<Vec<String>>()
            .join("\n")
    }

    pub fn format_constants(con: &[Primitive]) -> String {
        con
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<String>>()
            .join("\n")
    }
}

impl fmt::Display for Bytecode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Bytecode:\n\nInstructions:\n{}\n\nConstants:\n{}",
            Bytecode::format_instructions(&self.instructions),
            Bytecode::format_constants(&self.constants),
        )
    }
}

struct Header {
    version: u16,
    const_length: u16,
    ins_length: u64,
}

impl Header {
    fn new(const_length: u16, ins_length: u64) -> Self {
        Self {
            version: VERSION,
            const_length,
            ins_length,
        }
    }

    fn encode(&self) -> Vec<u8> {
        let mut result = Vec::new();
        let v_high: u8 = (self.version >> 8).try_into().unwrap();
        let v_low: u8 = (self.version & 0x00FF).try_into().unwrap();
        result.push(v_high);
        result.push(v_low);
        let c_high: u8 = (self.const_length >> 8).try_into().unwrap();
        let c_low: u8 = (self.const_length & 0x00FF).try_into().unwrap();
        result.push(c_high);
        result.push(c_low);

        let mut mask: u64 = 0xFF << 56;
        for i in (0..8).rev() {
            let ins_byte: u8 = ((self.ins_length & mask) >> (8 * i)).try_into().unwrap();
            mask = mask >> 8;
            result.push(ins_byte);
        };

        // Pad with 4 empty bytes to align to a 128 bit header.
        result.push(0);
        result.push(0);
        result.push(0);
        result.push(0);

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_encoded(expected: Vec<u8>, actual: Bytecode) -> Result<()> {
        let encoded = actual.encode()?;
        assert_eq!(
            expected[0..16],
            encoded[0..16],
            "\n\nHeader Differs\n\n",
        );
        assert_eq!(
            expected[16..],
            encoded[16..],
            "\n\nContent Differs\n\n",
        );

        Ok(())
    }

    #[test]
    fn test_header_encoding() -> Result<()> {
        let header = Header::new(0xFFFE, 0xFDFC_FBFA_F9F8_F7F6);
        assert_eq!(
            vec![0, 1, 0xFF, 0xFE, 0xFD, 0xFC, 0xFB, 0xFA, 0xF9, 0xF8, 0xF7, 0xF6, 0, 0, 0, 0],
            header.encode(),
        );

        Ok(())
    }

    #[test]
    fn test_empty_encoding() -> Result<()> {
        let bytecode = Bytecode {
            constants: vec![],
            instructions: vec![],
        };

        assert_encoded(
            vec![0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            bytecode,
        )
    }

    #[test]
    fn test_bytecode_encoded() -> Result<()> {
        let bytecode = Bytecode {
            constants: vec![kint!(0), kint!(1)],
            instructions: vec![
                // let a = 0;
                load!(0xFF, 0xFEFD),
                set_global!(1, 0xFF),
                // let b = 1;
                load!(0xCD, 0xFCFB),
                set_global!(2, 0xCD),
            ],
        };

        let mut expected = vec![];
        let mut header = vec![0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0];
        let mut encoded = vec![
            2, 0xFF, 0xFE, 0xFD, // load!
            13, 0xFF, 0x00, 0x01, // set_global!
            2, 0xCD, 0xFC, 0xFB, // load!
            13, 0xCD, 0x00, 0x02, // set_global!
        ];

        expected.append(&mut header);
        expected.append(&mut encoded);

        assert_encoded(expected, bytecode)
    }
}
