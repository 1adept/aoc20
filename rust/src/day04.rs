use std::fmt::Display;

use crate::Day;
use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    // Regex PassPort
    static ref REGEX_PP: Regex = Regex::new(r"(?:([a-z]{3,3}):([^\s]+))(?:\s|\n)?").unwrap();
    static ref REGEX_HGT: Regex = Regex::new(r"(\d+)((?:cm)|(?:in))").unwrap();
}

#[derive(Debug, Default)]
pub struct Day04 {
    passports: Vec<Passport>,
}
#[derive(Debug, Default)]
struct Passport {
    byr: Option<u16>,    // Birth Year
    iyr: Option<u16>,    // Issue Year
    eyr: Option<u16>,    // Expiration Year
    hgt: Option<String>, // Height
    hcl: Option<String>, // Hair color
    ecl: Option<String>, // Eye color
    pid: Option<String>, // Passport ID
    cid: Option<String>, // Country ID
}

impl Day for Day04 {
    fn parse(text: &str) -> Box<Self> {
        let passports = text
            .split_terminator("\n\n")
            .filter(|str| !str.is_empty())
            .map(|pp| {
                let mut pass = Passport::default();
                REGEX_PP.captures_iter(pp).for_each(|cap| {
                    let [name, value] = cap.extract::<2>().1;
                    match name {
                        "byr" => pass.byr = Some(value.parse().unwrap()),
                        "iyr" => pass.iyr = Some(value.parse().unwrap()),
                        "eyr" => pass.eyr = Some(value.parse().unwrap()),
                        "hgt" => pass.hgt = Some(value.to_owned()),
                        "hcl" => pass.hcl = Some(value.to_owned()),
                        "ecl" => pass.ecl = Some(value.to_owned()),
                        "pid" => pass.pid = Some(value.to_owned()),
                        "cid" => pass.cid = Some(value.to_owned()),
                        _ => unreachable!("No other field named {}", cap.get(1).unwrap().as_str()),
                    };
                });
                pass
            })
            .collect();
        Box::new(Day04 { passports })
    }

    /// passports are valid if all fields (except cid) are filled
    fn solve1(&self) -> usize {
        self.passports
            .iter()
            .filter(|pass| pass.are_fields_set())
            .count()
    }

    /// passports are valid if their fields are non-empty and fulfill a condition
    /// - iyr between 2010-2020
    /// - eyr between 2020-2030
    /// - byr between 1920-2002
    /// - hgt case
    /// -- cm => between 150-193
    /// -- in => between  59- 76
    /// - hcl => hex-color
    /// - ecl => one of [amb, blu, gry, grn, hzl, oth]
    /// - pid => 9-digit number
    /// - cid => Doesnt matter
    fn solve2(&self) -> usize {
        self.passports.iter().filter(|pass| pass.is_valid()).count()
    }
}

impl Passport {
    /// cid doesnt matter
    fn are_fields_set(&self) -> bool {
        self.iyr.is_some()
            && self.eyr.is_some()
            && self.byr.is_some()
            && self.hgt.is_some()
            && self.ecl.is_some()
            && self.hcl.is_some()
            && self.pid.is_some()
    }

    /// Again CID doesnt matter
    fn is_valid(&self) -> bool {
        let is_between = |val: u16, min: u16, max: u16| val >= min && val <= max;

        self.iyr.is_some_and(|y| is_between(y, 2010, 2020))
            && self.eyr.is_some_and(|y| is_between(y, 2020, 2030))
            && self.byr.is_some_and(|y| is_between(y, 1920, 2002))
            && self.hgt.as_ref().is_some_and(|hgt| {
                if let Some(cap) = REGEX_HGT.captures(hgt) {
                    let [h, unit] = cap.extract::<2>().1;
                    let height = h.parse().unwrap();
                    match unit {
                        "cm" => is_between(height, 150, 193),
                        "in" => is_between(height, 59, 76),
                        _ => unreachable!("Cant reach with regex guard"),
                    }
                } else {
                    false
                }
            })
            && self.hcl.as_ref().is_some_and(|color| {
                color.starts_with('#') && color.chars().skip(1).all(|c| c.is_ascii_hexdigit())
            })
            && self.ecl.as_ref().is_some_and(|color| {
                ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].contains(&color.as_str())
            })
            && self.pid.as_ref().is_some_and(|id| id.len() == 9)
    }
}

impl Display for Passport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"Passport:\nIssued {:?},\nExpires {:?},\nBirth {:?},\nHeight={:?},\nColor: \n  Eyes={:?},\n  Hair={:?}\nIDs\n  Passport {:?},\n  Country={:?}",
        self.iyr, self.eyr, self.byr, self.hgt, self.ecl, self.hcl, self.pid, self.cid)
    }
}

#[cfg(test)]
mod tests {
    use crate::{day04, Day};

    const EXAMPLE: &str = include_str!("../../data/04_example.in");

    #[test]
    fn test_part1() {
        let day4 = day04::Day04::parse(EXAMPLE);
        assert_eq!(2, day4.solve1());
    }

    // Thers no test for part 2 :/
    // #[test]
    // fn test_part2() {
    //     assert_eq!()
    // }
}
