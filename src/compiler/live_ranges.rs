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

impl LiveRanges {
    pub fn is_not_used_again(&self, reg: Register, idx: usize) -> bool {
        if let Some(ranges) = self.map.get(&reg) {
            ranges
                .iter()
                .any(|range| *range.start() == idx && *range.end() == idx)
        } else {
            false
        }
    }

    pub fn reassign(&mut self, reg: Register, idx: usize) -> Option<(Register, RangeInclusive<usize>)> {
        let ranges = self.map.get(&reg)?;
        let range_idx = ranges
            .iter()
            .position(|range| range.contains(&idx))?;

        let new_register = self.first_non_overlapping_register(&ranges[range_idx], reg)?;

        let old_ranges = self.map.get_mut(&reg)?;
        let range = old_ranges.remove(range_idx);

        let new_ranges = self.map.get_mut(&new_register)?;
        new_ranges.push(range.clone());

        Some((new_register, range))
    }

    fn first_non_overlapping_register(&self, current_range: &RangeInclusive<usize>, current_reg: Register) -> Option<Register> {
        for (reg, ranges) in &self.map {
            if current_reg <= *reg { continue; };
            if !ranges.iter().any(|range| ranges_overlap(current_range, range)) {
                return Some(*reg);
            };
        };

        None
    }
}

fn ranges_overlap<T: PartialOrd>(r1: &RangeInclusive<T>, r2: &RangeInclusive<T>) -> bool {
    r1.contains(r2.start())
        || r1.contains(r2.end())
        || r2.contains(r1.start())
        || r2.contains(r1.end())
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
                SetGlobal { src, .. } => {
                    if let Some(vec) = live_ranges.get_mut(src) {
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
    fn test_reassign_non_overlapping_registers() {
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
            load!(3, 2), // Can reuse r0 becuase it's re-written at the next expression.
            set_global!(2, 3),
            // let d = 3;
            load!(0, 3),
            set_global!(3, 0),
            // c + d;
            add!(6, 1, 0), // Can re-use r2
        ];
        let mut live_ranges = LiveRanges::from(ins.as_slice());

        //                         register ----v  v---- instruction index
        let (reg, range) = live_ranges.reassign(3, 5).unwrap();
        assert_eq!(0, reg);
        assert_eq!(5..=6, range);

        let (reg, range) = live_ranges.reassign(6, 9).unwrap();
        assert_eq!(2, reg);
        assert_eq!(9..=9, range);

        assert!(live_ranges.reassign(1, 3).is_none());
        assert!(live_ranges.reassign(3, 9).is_none());
        assert!(live_ranges.reassign(1, 0).is_none());
    }

    #[test]
    fn test_multiple_ranges_for_live_ranges() {
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
            vec![4..=4, 7..=8],
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
    fn test_ranges_for_live_ranges() {
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

    #[test]
    fn test_ranges_load_and_set() {
        let ins = vec![
            // let a = 0;
            load!(0, 0),
            set_global!(0, 0),
            // let b = 1;
            load!(1, 1),
            set_global!(1, 1),
        ];

        let expected = vec![
            vec![0..=1],
            vec![2..=3],
        ];

        println!("{}", LiveRanges::from(ins.as_slice()));
        let actual: Vec<Vec<RangeInclusive<usize>>> = LiveRanges::from(ins.as_slice())
            .map
            .values()
            .cloned()
            .collect();
        assert_eq!(expected, actual);
    }
}
