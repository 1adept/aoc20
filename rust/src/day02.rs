use crate::Day;
use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref PWD_REGEX: Regex =
        Regex::new(r"(?<min>\d+)-(?<max>\d+) (?<char>[a-z]): (?<pwd>\w+)").unwrap();
}

pub struct Day02 {
    passwords: Vec<Password>,
}
struct Password {
    min: usize,
    max: usize,
    character: char,
    password: String,
}

impl Day for Day02 {
    fn parse(text: &str) -> Self {
        let data = text
            .lines()
            .flat_map(|line| {
                if let Some(captures) = PWD_REGEX.captures(line) {
                    let min = captures
                        .name("min")
                        .map(|x| x.as_str())
                        .map(|s| s.parse())
                        .unwrap()
                        .unwrap();
                    let max = captures
                        .name("max")
                        .map(|x| x.as_str())
                        .map(|s| s.parse())
                        .unwrap()
                        .unwrap();
                    let character: char = captures
                        .name("char")
                        .unwrap()
                        .as_str()
                        .chars()
                        .next()
                        .unwrap();
                    let password = captures.name("pwd").unwrap().as_str().to_owned();

                    Some(Password {
                        min,
                        max,
                        character,
                        password,
                    })
                } else {
                    None
                }
            })
            .collect();
        Self { passwords: data }
    }

    fn solve1(&self) -> usize {
        self.passwords
            .iter()
            .filter(
                |Password {
                     min,
                     max,
                     character,
                     password,
                 }| {
                    let occurances = &password.chars().filter(|c| c == character).count();
                    occurances >= min && occurances <= max
                },
            )
            .count()
    }

    fn solve2(&self) -> usize {
        self.passwords
            .iter()
            .filter(
                |Password {
                     min,
                     max,
                     character,
                     password,
                 }| {
                    assert_ne!(0, *min);
                    assert_ne!(0, *max);
                    let mut window = password.as_str()[*min..*max].chars();
                    let first = window.next().map(|c| c == *character).unwrap_or(false);
                    first || window.last().map(|c| c == *character).unwrap_or(false)
                },
            )
            .count()
    }
}

#[cfg(test)]
mod tests {
    use crate::{day02::Day02, Day};

    pub const EXAMPLE: &'static str = include_str!("../../data/02_example.in");

    #[test]
    fn test_part11() {
        let day = Day02::parse(EXAMPLE);
        assert_eq!(2, day.solve1())
    }

    #[test]
    fn test_part2() {
        let day = Day02::parse(EXAMPLE);
        assert_eq!(1, day.solve2())
    }
}
