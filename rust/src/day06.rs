use std::collections::HashSet;

use crate::Day;

pub struct Day06(Vec<GroupAnswers>);
struct GroupAnswers(Vec<HashSet<char>>);

impl Day for Day06 {
    fn parse(text: &str) -> Box<Self>
    where
        Self: Sized,
    {
        let groups = text
            .split_terminator("\n\n")
            .map(|group| {
                group
                    .lines()
                    .map(|line| line.chars().collect())
                    .collect::<Vec<_>>()
            })
            .map(GroupAnswers)
            .collect::<Vec<_>>();
        Box::new(Day06(groups))
    }

    fn solve1(&self) -> usize {
        self.0
            .iter()
            .map(|group| group.unique_answers().len())
            .sum()
    }

    fn solve2(&self) -> usize {
        self.0
            .iter()
            .map(|group| {
                group
                    .unique_answers()
                    .iter()
                    // Unique answers where everyone aswered 'yes'
                    .filter(|unique| group.0.iter().all(|answer| answer.contains(unique)))
                    .count()
            })
            .sum()
    }
}

impl GroupAnswers {
    fn unique_answers(&self) -> HashSet<char> {
        self.0.iter().fold(HashSet::new(), |acc, next| {
            acc.union(next).cloned().collect::<HashSet<_>>()
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::Day;

    use super::Day06;

    const EXAMPLE: &str = include_str!("../../data/06_example.in");

    #[test]
    fn test_part1() {
        let day = Day06::parse(EXAMPLE);
        assert_eq!(11, day.solve1());
    }

    #[test]
    fn test_part2() {
        let day = Day06::parse(EXAMPLE);
        assert_eq!(6, day.solve2());
    }
}
