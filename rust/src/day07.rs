use std::collections::HashMap;

use crate::Day;
use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref REGEX_RULE: Regex = Regex::new(r"^(?P<color>[\w\s]+) bags contain (?:(?:no other bags)|(?:(\d)+ ([\w\s]+) bags?(?:, )?))+\.$").unwrap();
    static ref REGEX_BAG: Regex = Regex::new(r"(?P<amount>\d+) (?P<color>[\w\s]+) bags?").unwrap();
}

// This should have been a map from the start
pub struct Day07(Vec<Rule>);
#[derive(Debug)]
struct Rule {
    color: String,
    bags: HashMap<String, u8>,
}

impl Day for Day07 {
    fn parse(text: &str) -> Box<Self>
    where
        Self: Sized,
    {
        Box::new(Day07(
            text.lines()
                .filter(|line| !line.trim().is_empty())
                .map(Rule::from)
                .collect(),
        ))
    }

    /// This solution is kinda bad
    fn solve1(&self) -> usize {
        const GOLD: &str = "shiny gold";
        let mut contains_gold = vec![GOLD];

        let mut to_check = self
            .0
            .iter()
            .filter(|bag| !bag.bags.is_empty())
            .collect::<Vec<_>>();

        let mut found = true;
        while found {
            found = false;

            for i in (0..to_check.len()).rev() {
                let rule = to_check[i];

                if contains_gold.iter().any(|gold| rule.can_contain(gold)) {
                    found = true;
                    contains_gold.push(&rule.color);
                    to_check.remove(i);
                }
            }
        }

        contains_gold.len() - 1 // -1 for the initial gold in the bag. It cannot contain itself
    }

    fn solve2(&self) -> usize {
        const GOLD: &str = "shiny gold";

        let mapped = self
            .0
            .iter()
            .map(|rule| (&rule.color, &rule.bags))
            .collect::<HashMap<&String, &HashMap<String, u8>>>();

        let count = count(&mapped, &GOLD.to_string());

        count - 1 // -1 for the golden bag itself
    }
}

/// Counts the amount of bag contained in the {color} bag
fn count(rules: &HashMap<&String, &HashMap<String, u8>>, color: &String) -> usize {
    1 + rules // +1 for the bag itself
        .get(color)
        .unwrap()
        .iter()
        .map(|(color, amount)| *amount as usize * count(rules, color))
        .sum::<usize>()
}

impl From<&str> for Rule {
    fn from(value: &str) -> Self {
        let color: String = value
            .split_whitespace()
            .take(2)
            .collect::<Vec<_>>()
            .join(" ");
        let bags = REGEX_BAG
            .captures_iter(value)
            .fold(HashMap::new(), |mut acc, capture| {
                let [amount, color] = capture.extract().1;
                acc.insert(color.into(), amount.parse().unwrap());
                acc
            });

        Rule { color, bags }
    }
}

impl Rule {
    fn can_contain(&self, color: &str) -> bool {
        self.bags.keys().any(|key_color| key_color == color)
    }
}

#[cfg(test)]
mod tests {
    use crate::Day;

    use super::Day07;

    const EXAMPLE_1: &str = include_str!("../../data/07_example.in");
    const EXAMPLE_2: &str = include_str!("../../data/07_example2.in");

    #[test]
    fn test_part1() {
        let day = Day07::parse(EXAMPLE_1);
        assert_eq!(4, day.solve1());
    }

    #[test]
    fn test_part2_example1() {
        let day = Day07::parse(EXAMPLE_1);
        assert_eq!(32, day.solve2());
    }

    #[test]
    fn test_part2_example2() {
        let day = Day07::parse(EXAMPLE_2);
        assert_eq!(126, day.solve2());
    }
}
