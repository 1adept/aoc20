use std::collections::HashMap;

use crate::Day;
use lazy_static::lazy_static;
use regex::Regex;

pub struct Day14(Vec<Value>);

lazy_static! {
    static ref REGEX_MEM: Regex = Regex::new(r"mem\[(\d+)\] = (\d+)").unwrap();
}

/// Bitmasks are defined as 36-Bit unsigned int
/// With either an 'X' (meaning it doesnt overwrite) or a 0/1
/// Here None values represent the X-es
type Bits = [Bit; 36];
enum Value {
    Mask(Bits),
    Address((u16, Bits)),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Bit {
    Zero,
    One,
    Floating,
}

impl Day for Day14 {
    fn parse(text: &str) -> Box<Self>
    where
        Self: Sized,
    {
        Box::new(Day14(
            text.lines()
                .take_while(|line| !line.is_empty())
                .map(Value::from)
                .collect(),
        ))
    }

    fn solve1(&self) -> usize {
        self.0
            .iter()
            .fold(
                (HashMap::new(), &[Bit::Zero; 36]),
                |(mut adresses, mut mask), instr| {
                    println!("{instr}");
                    match instr {
                        Value::Mask(bits) => {
                            mask = bits;
                        }
                        Value::Address((address, bits)) => {
                            adresses.insert(address, and_v1(bits, mask));
                        }
                    }
                    (adresses, mask)
                },
            )
            .0
            .into_values()
            .map(|bits| bits_u64(&bits))
            .inspect(|val| println!("{val}"))
            .sum::<u64>() as usize
    }

    fn solve2(&self) -> usize {
        todo!()
    }
}

fn and_v1(left: &Bits, right: &Bits) -> Bits {
    let mut set = [Bit::Zero; 36];
    for (i, (left_bit, right_bit)) in (0..36).zip(left.iter().zip(right)) {
        set[i] = match (left_bit, right_bit) {
            // In v1 Floating get ignored
            (Bit::Floating, r) => r.clone(),
            (l, Bit::Floating) => l.clone(),
            (Bit::One, _) => Bit::One,
            (_, Bit::One) => Bit::One,
            _ => Bit::Zero,
        };
    }
    set
}

/// Sets a bit to a specific value
///
/// bit: which bit to set
/// bit_value: value to set the bit to
fn set_bit(value: u64, bit: u64, bit_value: u64) -> u64 {
    let mut new_value = value;
    new_value = new_value & !(1 << bit); // Unset bit at position
    new_value |= bit_value << bit; // Set with given value again
    new_value
}

fn bits_u64(bits: &Bits) -> u64 {
    let res = bits
        .iter()
        .enumerate()
        .filter(|(_, bit)| **bit == Bit::One)
        .fold(0, |acc, (i, _)| acc + (1 << i));
    res
}

fn bits_from_u64(value: u64) -> Bits {
    (0..36)
        .into_iter()
        .map(|i| {
            if i == (1 << i) & value {
                Bit::One
            } else {
                Bit::Zero
            }
        })
        .collect::<Vec<_>>()
        .try_into()
        .unwrap()
}

fn bits_from_str(value: &str) -> Bits {
    value
        .chars()
        .rev()
        .map(|c| match c {
            'X' => Bit::Floating,
            '1' => Bit::One,
            '0' => Bit::Zero,
            _ => unreachable!("No such bit"),
        })
        .collect::<Vec<_>>()
        .try_into()
        .expect("Cannot try into array")
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        if let Some(captures) = REGEX_MEM.captures(value) {
            let [address, value] = captures.extract().1;
            let address = address
                .parse()
                .expect(&format!("Cannot parse adresse '{address}'"));
            let value = bits_from_u64(value.parse().expect(&format!("Cannot parse value {value}")));
            Value::Address((address, value))
        } else {
            let len = "mask = ".len();
            let bits = bits_from_str(&value[len..]);
            Value::Mask(bits)
        }
    }
}

mod visuals {
    use std::fmt::Display;

    use super::{Bit, Bits, Value};

    impl Display for Bit {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let cha = match self {
                Bit::Zero => '0',
                Bit::One => '1',
                Bit::Floating => 'X',
            };
            write!(f, "{cha}")
        }
    }

    impl Display for Value {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let format_bits = |bits: &Bits| {
                bits.iter()
                    .rev()
                    .map(|bit| format!("{bit}"))
                    .collect::<String>()
            };

            let str = match self {
                Value::Mask(bits) => {
                    format!("mask: {}", format_bits(bits))
                }
                Value::Address((address, bits)) => format!("val{address}: {}", format_bits(bits)),
            };
            write!(f, "{str}")
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::Day;

    use super::Day14;

    const EXAMPLE: &'static str = include_str!("../../data/14_example.in");

    #[test]
    fn test_part1() {
        let day = Day14::parse(EXAMPLE);
        assert_eq!(165, day.solve1());
    }

    #[test]
    #[ignore = "todo"]
    fn test_part2() {
        let day = Day14::parse(EXAMPLE);
        // assert_eq!(?, day.solve2());
    }
}
