use std::{ops::RangeInclusive, rc::Rc};

use itertools::Itertools;

use crate::Day;

pub struct Day16 {
    rules: Rc<[Rule]>,
    ticket: Ticket,
    nearby: Rc<[Ticket]>,
}

/// Specifies valid ranges for a field
struct Rule(RangeInclusive<usize>);

struct Ticket(Rc<[usize]>);

impl Day for Day16 {
    fn parse(text: &str) -> Box<Self>
    where
        Self: Sized,
    {
        let mut sections = text.split("\n\n");
        let rule_section = sections.next().expect("No rule section");
        let my_ticket = sections.next().expect("No my section");
        let nearby_tickets = sections.next().expect("No nearby tickets");

        let rules: Rc<[Rule]> = rule_section
            .lines()
            .filter(|line| !line.is_empty())
            .flat_map(|line| {
                let (_class, ranges) = line.split_once(':').expect("No rules");
                ranges.split(" or ").map(|range| {
                    let (min, max) = range.trim().split_once('-').expect("No range");
                    let min = min.parse().expect(&format!("Couldnt parse '{min}'"));
                    let max = max.parse().expect(&format!("Couldnt parse '{max}'"));
                    Rule(min..=max)
                })
            })
            .collect();

        let parse_ticket = |line: &str| {
            Ticket(
                line.split(',')
                    .flat_map(|num| num.trim_end().parse())
                    .collect::<Rc<[usize]>>(),
            )
        };

        let my_ticket = my_ticket
            .lines()
            .skip(1) // Skip "your ticket:"
            .take(1) // Take single line
            .map(parse_ticket)
            .last()
            .expect("Failed parsing ticket");

        let nearby_tickets = nearby_tickets
            .lines()
            .skip(1) // Skip "nearby tickets:"
            .map(parse_ticket)
            .collect();

        Box::new(Day16 {
            rules,
            ticket: my_ticket,
            nearby: nearby_tickets,
        })
    }

    fn solve1(&self) -> usize {
        self.nearby
            .iter()
            // Take unique numbers
            .flat_map(|ticket| ticket.0.iter())
            .unique()
            // filter to numbers where no rule matches the number
            .filter(|number| {
                self.rules
                    .iter()
                    .all(|rule| !rule.is_invalid_number(*number))
            })
            .sum()
        // 19870 TOO LOW (parse error?)
    }

    fn solve2(&self) -> usize {
        todo!()
    }
}

impl Rule {
    fn is_invalid_number(&self, number: &usize) -> bool {
        self.0.contains(&number)
    }

    fn is_invalid(&self, ticket: &Ticket) -> bool {
        ticket.0.iter().any(|num| !self.0.contains(num))
    }

    fn invalid_numbers<'a>(&self, ticket: &'a Ticket) -> Vec<&'a usize> {
        ticket
            .0
            .iter()
            .filter(|num| !self.0.contains(num))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::Day;

    use super::Day16;

    const EXAMPLE: &'static str = include_str!("../../data/16_example.in");

    #[test]
    fn test_part1() {
        let day = Day16::parse(EXAMPLE);
        assert_eq!(71, day.solve1());
    }

    #[test]
    #[ignore = "todo"]
    fn test_part2() {
        let day = Day16::parse(EXAMPLE);
        assert_eq!(99999999, day.solve2());
    }
}
