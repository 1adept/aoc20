//! Trying Rc<[T]> instead of Vec<T> as suggested by https://youtu.be/A4cKi7PTJSs?si=jT-fZbZp8QOg_dGV

//! Result: Its supposed to be more efficient but there were some troubles with them. Its not much hassle when used as a field in a struct and its only accessed but not modified. Otherwise a vec is more convenient

use std::{collections::BTreeMap, ops::RangeInclusive, rc::Rc};

use crate::Day;

#[derive(Debug)]
pub struct Day16 {
    rules: Rc<[RuleClass]>,
    ticket: Ticket,
    nearby: Rc<[Ticket]>,
}

#[derive(Debug, Clone)]
struct RuleClass {
    /// Aliases: [field]
    name: String,
    rules: Rc<[Rule]>,
}

impl PartialEq for RuleClass {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

/// Specifies valid ranges for a field
type Rule = RangeInclusive<usize>;
type Ticket = Rc<[usize]>;

impl Day for Day16 {
    fn parse(text: &str) -> Box<Self>
    where
        Self: Sized,
    {
        let mut sections = text.split("\n\n");
        let rule_section = sections.next().expect("No rule section");
        let my_ticket = sections.next().expect("No my section");
        let nearby_tickets = sections.next().expect("No nearby tickets");

        let rules: Rc<[RuleClass]> = rule_section
            .lines()
            .filter(|line| !line.is_empty())
            .map(|line| {
                let (class, ranges) = line.split_once(':').expect("No rules");
                let rules = ranges
                    .split(" or ")
                    .map(|range| {
                        let (min, max) = range.trim().split_once('-').expect("No range");
                        let min = min.parse().expect(&format!("Couldnt parse '{min}'"));
                        let max = max.parse().expect(&format!("Couldnt parse '{max}'"));
                        min..=max
                    })
                    .collect();

                RuleClass {
                    name: class.to_owned(),
                    rules,
                }
            })
            .collect();

        let parse_ticket = |line: &str| {
            line.split(',')
                .flat_map(|num| num.trim_end().parse())
                .collect::<Rc<[usize]>>()
        };

        let ticket = my_ticket
            .lines()
            .skip(1) // Skip "your ticket:" Header
            .take(1) // Take single line
            .map(parse_ticket)
            .last()
            .expect("Failed parsing ticket");

        let nearby = nearby_tickets
            .lines()
            .skip(1) // Skip "nearby tickets:" Header
            .map(parse_ticket)
            .collect();

        Box::new(Day16 {
            rules,
            ticket,
            nearby,
        })
    }

    fn solve1(&self) -> usize {
        filter_tickets_by_status(&self.rules, &self.nearby, false)
            .iter()
            // Take numbers
            .flat_map(|ticket| ticket.iter())
            // And only those which match no rule
            .filter(|number| {
                self.rules
                    .iter()
                    .all(|rule_class| !rule_class.is_valid_number(&number))
            })
            .sum()
    }

    fn solve2(&self) -> usize {
        let valid_tickets = filter_tickets_by_status(&self.rules, &self.nearby, true);
        let rules = assign_rules(&self.rules, &valid_tickets);

        rules
            .iter()
            .zip(self.ticket.iter())
            .filter_map(|(rc, number)| {
                if rc.name.contains("departure") {
                    Some(number)
                } else {
                    None
                }
            })
            .product()
        // 157 too low
    }
}

/// Assign a single Rule to each Ticket-Number
/// The resulting Vector is sorted to its corresponding number by index
/// Meaning Rule 1 is responsible for number 1 on each ticket
fn assign_rules<'a>(rules: &'a [RuleClass], tickets: &[&Ticket]) -> Vec<&'a RuleClass> {
    let ticket_len = tickets
        .first()
        .map(|t| t.len())
        .expect("Cant read ticket length");
    let numbers: BTreeMap<_, _> = (0..ticket_len)
        .map(|index| {
            (
                index,
                tickets
                    .iter()
                    .map(|ticket| &ticket[index])
                    .collect::<Vec<_>>(),
            )
        })
        .collect();

    // Maps each index to all valid rules
    let mut unique: Vec<_> = Vec::new();
    let mut choice: Vec<_> = numbers
        .iter()
        // Assign all valid rules to all numbers
        .map(|(i, nums)| {
            (
                *i,
                rules
                    .iter()
                    .filter(|rc| nums.iter().all(|num| rc.is_valid_number(num)))
                    .collect::<Vec<_>>(),
            )
        })
        .collect();

    loop {
        let (new_unique, new_choice): (Vec<_>, Vec<_>) = choice
            .into_iter()
            .map(|(i, multi)| {
                (
                    i,
                    multi
                        .into_iter()
                        // Filter out 'choice' rules that are unique since last loop
                        .filter(|rc| !unique.iter().any(|(_, uq)| uq == rc))
                        .collect::<Vec<_>>(),
                )
            })
            // Partition by unique
            .partition(|(_, multi)| multi.len() == 1);
        // Assign new choices
        choice = new_choice;
        // If no new unique assignments, no further loops neccessary
        if new_unique.is_empty() {
            break;
        }
        // Map unique vector to single rule
        unique.extend(new_unique.into_iter().map(|(i, uqs)| (i, uqs[0])));
    }

    assert!(choice.is_empty()); // Choice must now be empty
                                // Sort unique so we can ditch the index tuple
    unique.sort_unstable_by_key(|(i, _)| *i);

    // Ditch index tuple and return result
    unique.into_iter().map(|(_, rc)| rc).collect()
}

fn filter_tickets_by_status<'a>(
    rules: &[RuleClass],
    tickets: &'a [Ticket],
    valid: bool,
) -> Vec<&'a Ticket> {
    tickets
        .iter()
        .filter(|ticket| {
            rules
                .iter()
                .any(|rule_class| valid == rule_class.is_ticket_valid(ticket))
        })
        .collect()
}

impl RuleClass {
    fn is_valid_number(&self, number: &usize) -> bool {
        self.rules.iter().any(|rule| rule.contains(&number))
    }

    fn is_ticket_valid(&self, ticket: &Ticket) -> bool {
        ticket
            .iter()
            .all(|number| self.rules.iter().any(|rule| rule.contains(number)))
    }
}

#[cfg(test)]
mod tests {
    use crate::Day;

    use super::Day16;

    const EXAMPLE: &'static str = include_str!("../../data/16_example.in");
    // const EXAMPLE2: &'static str = include_str!("../../data/16_example2.in");

    #[test]
    fn test_part1() {
        let day = Day16::parse(EXAMPLE);
        assert_eq!(71, day.solve1());
    }

    #[test]
    #[ignore = "no example given"]
    fn test_part2() {}
}
