use std::collections::BTreeMap;

use crate::Day;

pub struct Day15(Vec<Number>);

type Number = usize;

impl Day for Day15 {
    fn parse(text: &str) -> Box<Self>
    where
        Self: Sized,
    {
        Box::new(Day15(
            text.split_terminator(',')
                .map(|str| {
                    str.trim_end()
                        .parse()
                        .unwrap_or_else(|_| panic!("Cant parse '{str}'"))
                })
                .collect(),
        ))
    }

    fn solve1(&self) -> usize {
        solve_number_game(&self.0, 2020)
    }

    fn solve2(&self) -> usize {
        solve_number_game(&self.0, 30000000)
    }
}

fn solve_number_game(numbers: &[Number], rounds: usize) -> usize {
    // Mapping from Number to turns, when it was last mentioned
    let mut mentions: BTreeMap<Number, usize> =
        BTreeMap::from_iter(numbers.iter().enumerate().map(|(i, num)| (*num, i + 1)));

    let mut last = *numbers.last().unwrap();
    for i in (numbers.len() + 1)..=rounds {
        let say = if let Some(mention) = mentions.get(&last) {
            (i - 1).abs_diff(*mention)
        } else {
            0
        };

        mentions.insert(last, i - 1);
        last = say;
    }
    last
}

#[cfg(test)]
mod tests {
    use crate::Day;

    use super::Day15;

    const EXAMPLE: &str = include_str!("../../data/15_example.in");

    #[test]
    fn test_part1() {
        let day = Day15::parse(EXAMPLE);
        assert_eq!(436, day.solve1());
    }

    #[test]
    fn test_part1_1() {
        let day = Day15::parse("1,3,2");
        assert_eq!(1, day.solve1());
    }

    #[test]
    fn test_part1_2() {
        let day = Day15::parse("2,1,3");
        assert_eq!(10, day.solve1());
    }

    #[test]
    fn test_part1_3() {
        let day = Day15::parse("1,2,3");
        assert_eq!(27, day.solve1());
    }

    #[test]
    fn test_part1_4() {
        let day = Day15::parse("2,3,1");
        assert_eq!(78, day.solve1());
    }

    #[test]
    fn test_part1_5() {
        let day = Day15::parse("3,2,1");
        assert_eq!(438, day.solve1());
    }

    #[test]
    fn test_part1_6() {
        let day = Day15::parse("3,1,2");
        assert_eq!(1836, day.solve1());
    }

    #[test]
    fn test_part2() {
        let day = Day15::parse(EXAMPLE);
        assert_eq!(175594, day.solve2());
    }

    #[test]
    fn test_part2_1() {
        let day = Day15::parse("1,3,2");
        assert_eq!(2578, day.solve2());
    }

    #[test]
    fn test_part2_2() {
        let day = Day15::parse("2,1,3");
        assert_eq!(3544142, day.solve2());
    }

    #[test]
    fn test_part2_3() {
        let day = Day15::parse("1,2,3");
        assert_eq!(261214, day.solve2());
    }

    #[test]
    fn test_part2_4() {
        let day = Day15::parse("2,3,1");
        assert_eq!(6895259, day.solve2());
    }

    #[test]
    fn test_part2_5() {
        let day = Day15::parse("3,2,1");
        assert_eq!(18, day.solve2());
    }

    #[test]
    fn test_part2_6() {
        let day = Day15::parse("3,1,2");
        assert_eq!(362, day.solve2());
    }
}
