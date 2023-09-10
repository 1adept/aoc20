use std::{num::ParseIntError, str::FromStr};

use crate::Day;

#[derive(Debug)]
pub struct Day19 {
    rules: Vec<Option<Rule>>,
    messages: Vec<Message>,
}

#[derive(Debug, Clone)]
enum Rule {
    Match(char),
    MatchRef(Vec<usize>, Vec<usize>),
}

#[derive(Debug)]
struct Message(String);

#[derive(Debug)]
struct RuleMatcher<'a> {
    msg: Option<&'a str>,
    todo: usize,
}

impl Day for Day19 {
    fn parse(text: &str) -> Box<Self>
    where
        Self: Sized,
    {
        let mut sections = text.split_terminator("\n\n");
        let mut rules_tmp: Vec<(usize, Rule)> = sections
            .next()
            .unwrap()
            .lines()
            .flat_map(|line| -> Result<_, ParseIntError> {
                let (index, rule_str) = line.split_once(':').unwrap();
                let index = index.parse()?;
                let rule = Rule::from_str(rule_str)?;
                Ok((index, rule))
            })
            .collect();
        rules_tmp.sort_unstable_by_key(|(i, _)| i.clone());

        let size = rules_tmp.last().unwrap().0 + 1;
        let mut rules = Vec::with_capacity(size);
        (0..size).for_each(|_| rules.push(None));
        rules_tmp.into_iter().for_each(|(i, r)| rules[i] = Some(r));

        let messages = sections
            .next()
            .expect("No Message section")
            .lines()
            .map(|line| Message(line.to_string()))
            .collect();
        Box::new(Day19 { rules, messages })
    }

    fn solve1(&self) -> usize {
        count_rule0_matches(&self.rules, &self.messages)
    }

    fn solve2(&self) -> usize {
        let mut rules = self.rules.clone();
        replace_rules_8_11(&mut rules);
        count_rule0_matches(&rules, &self.messages)
    }
}

fn count_rule0_matches(rules: &[Option<Rule>], messages: &[Message]) -> usize {
    // WTF is this ?
    let rule0 = rules.first().unwrap().as_ref().cloned().unwrap();
    messages
        .iter()
        .filter(|msg| {
            let mut matcher = RuleMatcher::new(&msg.0);
            matcher.apply(&rule0, rules);
            println!("Matcher on {msg:?} done: {matcher:?}");
            matcher.matches()
        })
        .inspect(|msg| println!("{msg:?} matches"))
        .count()
}

fn replace_rules_8_11(rules: &mut Vec<Option<Rule>>) {
    rules[8] = Some(Rule::MatchRef(vec![42], vec![42, 8]));
    rules[11] = Some(Rule::MatchRef(vec![42, 31], vec![42, 11, 31]));
}

impl FromStr for Rule {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn parse_numbers(s: &str) -> Result<Vec<usize>, ParseIntError> {
            s.split_whitespace()
                .map(|n| n.parse())
                .collect::<Result<_, _>>()
        }
        if let Some((left, right)) = s.split_once('|') {
            let left = parse_numbers(left)?;
            let right = parse_numbers(right)?;
            Ok(Rule::MatchRef(left, right))
        } else if let Ok(rules) = parse_numbers(s) {
            Ok(Rule::MatchRef(rules, Vec::with_capacity(0)))
        } else {
            // Skip '"'
            let c = s.trim_start().chars().nth(1).expect("No char");
            Ok(Rule::Match(c))
        }
    }
}

impl<'a> RuleMatcher<'a> {
    fn new(msg: &'a str) -> Self {
        Self {
            msg: Some(msg),
            todo: 0,
        }
    }

    fn matches(&self) -> bool {
        self.msg.is_some_and(|str| str.is_empty() && self.todo == 0)
    }

    fn can_match(&self) -> bool {
        self.msg.is_some()
    }

    fn fail(&mut self) {
        self.msg = None;
    }

    fn apply(&mut self, rule: &Rule, rules: &[Option<Rule>]) {
        if self.matches() {
            // DONE!
        } else if self.todo > 50 || !self.can_match() {
            self.fail()
        } else {
            match rule {
                Rule::Match(c) => {
                    let temp = self.msg;
                    self.msg = self
                        .msg
                        .filter(|msg| msg.starts_with(*c))
                        .map(|msg| &msg[1..]);
                    println!("{c} // {temp:?} => {:?}", self.msg);
                    self.todo -= 1;
                }
                Rule::MatchRef(left, right) => {
                    let match_side = |side: &[usize]| {
                        side.iter()
                            .flat_map(|rule_index| rules[*rule_index].as_ref())
                            .fold(
                                RuleMatcher {
                                    todo: self.todo + side.len() - 1,
                                    ..*self
                                },
                                |mut temp, rule| {
                                    temp.apply(rule, rules);
                                    temp
                                },
                            )
                    };

                    if let Some(next) = [left, right]
                        .iter()
                        .filter(|list| !list.is_empty())
                        .map(|list| match_side(&list))
                        .find(|list| list.can_match())
                    {
                        self.msg = next.msg;
                    } else {
                        self.fail();
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Day;

    const EXAMPLE: &str = include_str!("../../data/19_example.in");
    const EXAMPLE2: &str = include_str!("../../data/19_example2.in");

    #[test]
    fn test_part1() {
        let day = Day19::parse(EXAMPLE);
        assert_eq!(2, day.solve1());
    }

    #[test]
    // #[ignore = "todo"]
    fn test_part2_no_replace() {
        let day = Day19::parse(EXAMPLE2);
        let no_replace = count_rule0_matches(&day.rules, &day.messages);
        assert_eq!(3, no_replace);
    }

    #[test]
    // #[ignore = "todo"]
    fn test_part2_replace() {
        let mut day = Day19::parse(EXAMPLE2);
        // assert_eq!(12, day.solve2());
        replace_rules_8_11(&mut day.rules);
        let rule0 = day.rules.first().unwrap().as_ref().unwrap();
        [
            // (false, "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa"),
            // (true, "bbabbbbaabaabba"),
            // (true, "babbbbaabbbbbabbbbbbaabaaabaaa"),
            // (true, "aaabbbbbbaaaabaababaabababbabaaabbababababaaa"),
            // (true, "bbbbbbbaaaabbbbaaabbabaaa"),
            // (true, "bbbababbbbaaaaaaaabbababaaababaabab"),
            // (true, "ababaaaaaabaaab"),
            // (true, "ababaaaaabbbaba"),
            // (true, "baabbaaaabbaaaababbaababb"),
            // (true, "abbbbabbbbaaaababbbbbbaaaababb"),
            // (true, "aaaaabbaabaaaaababaa"),
            (false, "aaaabbaaaabbaaa"),
            // (true, "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa"),
            // (false, "babaaabbbaaabaababbaabababaaab"),
            // (true, "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"),
        ]
        .into_iter()
        .for_each(|(expected, str)| {
            let mut matcher = RuleMatcher::new(str);
            matcher.apply(rule0, &day.rules);
            let result = matcher.matches();
            if expected != result {
                println!("Wrong for {str}");
            }
            assert!(expected == result)
        })
    }
}
