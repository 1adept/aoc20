use std::{collections::HashMap, str::FromStr};

use crate::Day;
use lazy_static::lazy_static;
use regex::Regex;

pub struct Day14(Vec<Value>);

lazy_static! {
    /// Regex for a mem[x] line
    static ref REGEX_MEM: Regex = Regex::new(r"mem\[(\d+)\] = (\d+)").unwrap();
}

/// Bitmasks are defined as 36-Bit unsigned int
/// With either an 'X' (meaning it doesnt overwrite) or a 0/1
/// Here None values represent the X-es
#[derive(Debug, Clone, PartialEq, Eq)]
struct BitMask([Bit; BitMask::LENGTH]);

type BitValue = u64;
enum Value {
    Mask(BitMask),
    Address((u16, BitValue)),
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
                .filter(|line| !line.is_empty())
                .map(Value::from)
                .collect(),
        ))
    }

    fn solve1(&self) -> usize {
        self.0
            .iter()
            .fold(
                (HashMap::new(), &BitMask::empty()),
                |(mut space, mut mask), instr| {
                    match instr {
                        Value::Mask(bits) => {
                            mask = bits;
                        }
                        Value::Address((address, value)) => {
                            let masked = mask.mask_ignore_floating(*value);
                            space.insert(address, masked);
                        }
                    }
                    (space, mask)
                },
            )
            .0
            .into_values()
            .sum::<u64>() as usize
    }

    fn solve2(&self) -> usize {
        self.0
            .iter()
            .fold(
                (HashMap::new(), &BitMask::empty()),
                |(mut space, mut mask), instr| {
                    match instr {
                        Value::Mask(bits) => {
                            mask = bits;
                        }
                        Value::Address((address, value)) => {
                            let addr_bits = &BitMask::from(*address as u64);
                            let masked_addr = mask.mask_carry_floating(addr_bits);

                            // All adresses being written to
                            for addr in masked_addr.unfloat() {
                                space
                                    .entry(addr.evaluate())
                                    .and_modify(|val| {
                                        *val = value;
                                    })
                                    .or_insert(value);
                            }
                        }
                    }
                    (space, mask)
                },
            )
            .0
            .into_values().copied()
            .sum::<u64>() as usize
    }
}

impl BitMask {
    const LENGTH: usize = 36;
    fn empty() -> Self {
        BitMask([Bit::Zero; BitMask::LENGTH])
    }

    fn set(&mut self, index: usize, bit: Bit) {
        self.0[index] = bit;
    }

    /// Uses self as mask to create a new BitField from bits
    fn mask_ignore_floating(&self, value: BitValue) -> BitValue {
        let mut result = value;
        self.0
            .iter()
            .enumerate()
            .filter(|(_, bit)| **bit != Bit::Floating)
            .for_each(|(i, bit)| {
                let shifted = 1 << i;
                result = match bit {
                    Bit::Zero => result & !shifted,
                    Bit::One => result | shifted,
                    Bit::Floating => unreachable!("Floating filtered"),
                };
            });
        result
    }

    /// Uses self as mask to create a new BitField from bits
    fn mask_carry_floating(&self, bits: &BitMask) -> Self {
        let mut new = BitMask::empty();
        self.0
            .iter()
            .zip(bits.0)
            .enumerate()
            .for_each(|(i, (l, r))| {
                new.set(
                    i,
                    match (&l, &r) {
                        (Bit::Floating, _) | (_, Bit::Floating) => Bit::Floating,
                        (Bit::One, _) | (_, Bit::One) => Bit::One,
                        _ => Bit::Zero,
                    },
                )
            });
        new
    }

    fn unfloat(&self) -> Vec<BitMask> {
        let mut bits = vec![BitMask::empty()];

        for i in 0..BitMask::LENGTH {
            match self.0[i] {
                Bit::Zero => continue,
                Bit::One => bits.iter_mut().for_each(|bit| bit.set(i, Bit::One)),
                Bit::Floating => {
                    let mut alt: Vec<_> = bits.to_vec();
                    alt.iter_mut().for_each(|bit| bit.set(i, Bit::One));
                    bits.extend(alt);
                }
            }
        }

        bits
    }

    fn evaluate(&self) -> u64 {
        fn eval_inner(bits: &BitMask, index: usize, result: u64) -> u64 {
            if index >= BitMask::LENGTH {
                return result;
            }

            let simple = |res| eval_inner(bits, index + 1, result + res);
            match bits.0[index] {
                Bit::One => simple(1 << index),
                Bit::Zero => simple(0),
                Bit::Floating => {
                    let zero = simple(0);
                    let one = simple(1 << index);
                    simple(zero + one)
                }
            }
        }

        eval_inner(self, 0, 0)
    }
}

impl FromStr for BitMask {
    type Err = <[Bit; BitMask::LENGTH] as TryInto<[Bit; BitMask::LENGTH]>>::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(BitMask(
            s.chars()
                .rev() // Least significant is arr[0]
                .map(|c| match c {
                    'X' => Bit::Floating,
                    '1' => Bit::One,
                    '0' => Bit::Zero,
                    _ => unreachable!("No such bit"),
                })
                .collect::<Vec<_>>()
                .try_into()
                .expect("Failed from_str"),
        ))
    }
}

impl From<u64> for BitMask {
    fn from(value: u64) -> Self {
        (0..BitMask::LENGTH)
            .map(|i| (i, 1 << i))
            .take_while(|(_, shifted)| shifted < &value)
            .fold(BitMask::empty(), |mut bits, (i, shifted)| {
                if shifted & value == shifted {
                    bits.set(i, Bit::One);
                }
                bits
            })
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        if let Some(captures) = REGEX_MEM.captures(value) {
            let [address, value] = captures.extract().1;
            let address = address
                .parse()
                .unwrap_or_else(|_| panic!("Cannot parse adresse '{address}'"));
            let value: BitValue = value
                .parse()
                .unwrap_or_else(|_| panic!("Cannot parse value '{value}'"));
            Value::Address((address, value))
        } else {
            let len = "mask = ".len();
            let bits = BitMask::from_str(&value[len..]).expect("Couldnt parse bits");
            Value::Mask(bits)
        }
    }
}

mod visuals {
    use std::fmt::Display;

    use super::{Bit, BitMask, Value};

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

    impl Display for BitMask {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let mut str = String::with_capacity(36);
            for bit in self.0.iter().rev() {
                let cha = match bit {
                    Bit::Zero => '0',
                    Bit::One => '1',
                    Bit::Floating => 'X',
                };
                str.push(cha);
            }
            write!(f, "{str}")
        }
    }

    impl Display for Value {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let str = match self {
                Value::Mask(bits) => format!("mask: {}", bits),
                Value::Address((address, bits)) => format!("val{address}: {}", bits),
            };
            write!(f, "{str}")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{BitMask, Day14};
    use crate::Day;
    use std::str::FromStr;

    const EXAMPLE1: &str = include_str!("../../data/14_example.in");
    const EXAMPLE2: &str = include_str!("../../data/14_example2.in");

    #[test]
    fn test_part1() {
        let day = Day14::parse(EXAMPLE1);
        assert_eq!(165, day.solve1());
    }

    #[test]
    fn test_bits_conversion() {
        let ass =
            |num: u64, str: &str| assert_eq!(BitMask::from(num), BitMask::from_str(str).unwrap());

        ass(42, "000000000000000000000000000000101010");
        ass(26, "000000000000000000000000000000011010");
        ass(27, "000000000000000000000000000000011011");
        ass(58, "000000000000000000000000000000111010");
        ass(59, "000000000000000000000000000000111011");
    }

    #[test]
    fn test_unfloat() {
        let adr = BitMask::from_str("000000000000000000000000000000101010").unwrap();
        let msk = BitMask::from_str("000000000000000000000000000000X1001X").unwrap();
        let res = BitMask::from_str("000000000000000000000000000000X1101X").unwrap();
        assert_eq!(res, msk.mask_carry_floating(&adr));

        let x1 = BitMask::from_str("000000000000000000000000000000011010").unwrap();
        let x2 = BitMask::from_str("000000000000000000000000000000011011").unwrap();
        let x3 = BitMask::from_str("000000000000000000000000000000111010").unwrap();
        let x4 = BitMask::from_str("000000000000000000000000000000111011").unwrap();
        let xs = vec![x1, x2, x3, x4];
        assert_eq!(xs, res.unfloat());
    }

    #[test]
    fn test_mask_v2() {
        let mem = BitMask::from(42);
        let msk = BitMask::from_str("000000000000000000000000000000X1001X").unwrap();
        let res = BitMask::from_str("000000000000000000000000000000X1101X").unwrap();
        assert_eq!(res, msk.mask_carry_floating(&mem));

        let mem = BitMask::from_str("000000000000000000000000000000011010").unwrap();
        let msk = BitMask::from_str("00000000000000000000000000000000X0XX").unwrap();
        let res = BitMask::from_str("00000000000000000000000000000001X0XX").unwrap();
        assert_eq!(res, msk.mask_carry_floating(&mem));
    }

    #[test]
    fn test_part2() {
        let day = Day14::parse(EXAMPLE2);
        assert_eq!(208, day.solve2());
    }
}
