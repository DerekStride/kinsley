use std::{
    fmt,
    collections::BTreeMap,
    ops::RangeInclusive,
};

use crate::compiler::code::{
    *,
    Instruction::*,
};

pub struct LiveRanges {
    // Key:     Register
    // Value:   (First occurance, Last occurance)
    map: BTreeMap<Register, Vec<RangeInclusive<usize>>>,
    instructions: Vec<String>,
}

impl From<&[Instruction]> for LiveRanges {
    fn from(instructions: &[Instruction]) -> Self {
        let mut live_ranges: BTreeMap<Register, Vec<RangeInclusive<usize>>> = BTreeMap::new();
        let mut strings: Vec<String> = Vec::new();

        for (idx, ins) in instructions.iter().enumerate() {
            strings.push(ins.to_string());

            if let Some(dest) = ins.last_dest_register() {
                if let Some(ranges) = live_ranges.get_mut(&dest) {
                    ranges.push(idx..=idx);
                } else {
                    live_ranges.insert(dest, vec![idx..=idx]);
                };
            };

            match ins {
                Add { a, b, .. } => {
                    if let Some(vec) = live_ranges.get_mut(a) {
                        if let Some(range) = vec.pop() { vec.push(*range.start()..=idx); };
                    };
                    if let Some(vec) = live_ranges.get_mut(b) {
                        if let Some(range) = vec.pop() { vec.push(*range.start()..=idx); };
                    };
                },
                GetGlobal { dest, .. } => {
                    if let Some(vec) = live_ranges.get_mut(dest) {
                        if let Some(range) = vec.pop() { vec.push(*range.start()..=idx); };
                    };
                },
                _ => {},
            };
        };

        Self { map: live_ranges, instructions: strings }
    }
}

impl fmt::Display for LiveRanges {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let header = self.map
            .keys()
            .map(|k| format!("r{:<3}", k))
            .collect::<Vec<String>>()
            .join("");

        writeln!(f, "  {:<25}\t{}", "", header)?;

        for (idx, ins) in self.instructions.iter().enumerate() {
            let x = self.map
                .values()
                .map(|ranges| {
                    let covered = ranges
                        .iter()
                        .any(|range| range.contains(&idx));

                    if covered { "*" } else { " " }
                })
                .collect::<Vec<&str>>()
                .join("   ");


            writeln!(f, "{:>3}: {:<25}\t{}", idx, ins, x)?;
        };

        Ok(())
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_for_live_ranges() {
        let ins = vec![
            // let a = 0;
            load!(0, 0),
            set_global!(0, 0),
            // let b = 1;
            load!(1, 1),
            set_global!(1, 1),
            // a + b;
            add!(2, 0, 1),
            // let c = 2;
            load!(0, 2), // Can reuse r0 because 'a' is no longer used.
            set_global!(2, 0),
            // let d = 3;
            load!(2, 3), // Can reuse r2 because the result of a + b is no longer used.
            set_global!(3, 2),
            // b + c;
            add!(3, 1, 0),
            // c + d;
            add!(1, 3, 4), // Can reuse r1 because b is no longer used.
        ];

        let expected = vec![
            vec![0..=4, 5..=9],
            vec![2..=9, 10..=10],
            vec![4..=4, 7..=7],
            vec![9..=10],
        ];

        let actual: Vec<Vec<RangeInclusive<usize>>> = LiveRanges::from(ins.as_slice())
            .map
            .values()
            .cloned()
            .collect();
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_display_for_live_ranges2() {
        let ins = vec![
            // let a = 0;
            load!(0, 0),
            set_global!(0, 0),
            // let b = 1;
            load!(1, 1),
            set_global!(1, 1),
            // a + b;
            add!(2, 0, 1),
            // let c = 2;
            load!(3, 2),
            set_global!(2, 3),
            // let d = 3;
            load!(4, 3),
            set_global!(3, 4),
            // b + c;
            add!(5, 1, 3),
            // c + d;
            add!(6, 3, 4),
        ];

        let expected = vec![
            vec![0..=4],
            vec![2..=9],
            vec![4..=4],
            vec![5..=10],
            vec![7..=10],
            vec![9..=9],
            vec![10..=10],
        ];

        let actual: Vec<Vec<RangeInclusive<usize>>> = LiveRanges::from(ins.as_slice())
            .map
            .values()
            .cloned()
            .collect();
        assert_eq!(expected, actual);
    }
}
