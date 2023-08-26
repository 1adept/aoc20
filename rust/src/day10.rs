use std::collections::HashMap;

use crate::Day;

pub struct Day10(Vec<usize>);

impl Day for Day10 {
    fn parse(text: &str) -> Box<Self>
    where
        Self: Sized,
    {
        let mut numbers: Vec<usize> = text.lines().flat_map(|line| line.parse()).collect();
        numbers.sort();
        Box::new(Day10(numbers))
    }

    fn solve1(&self) -> usize {
        let (ones, threes) = outlets_with_jolt_ones_threes(&self.0);
        ones * threes
    }

    fn solve2(&self) -> usize {
        num_arrangements(&self.0)
    }
}

/// A 'jolt' is the difference between two outlets,
/// 0->1 and 10->11 have a jolt of 1, 1->4 has a 'jolt' of 3
fn outlets_with_jolt_ones_threes(adapters: &[usize]) -> (usize, usize) {
    let (ones, threes) = adapters
        .iter()
        .fold((0, (0, 0)), |(num, mut acc), next| {
            if next - num == 1 {
                acc.0 += 1;
            } else if next - num == 3 {
                acc.1 += 1;
            } else {
                unreachable!("There are no 1-jolt or 3-jolt!?");
            }
            (*next, acc)
        })
        .1;
    (ones, threes + 1)
}

fn num_arrangements(adapters: &[usize]) -> usize {
    let last = adapters.last().unwrap() + 3;

    let get_route = |routes: &HashMap<_, _>, i: &usize, offset: usize| -> usize {
        *i.checked_sub(offset)
            .map(|n| routes.get(&n).unwrap_or(&0))
            .unwrap_or(&0)
    };

    let mut combis = HashMap::new();
    combis.insert(0, 1); // At start (always 0) theres only 1 possible combination

    adapters.iter().chain(&[last]).for_each(|i| {
        let r1 = get_route(&combis, i, 1);
        let r2 = get_route(&combis, i, 2);
        let r3 = get_route(&combis, i, 3);

        combis.insert(*i, r1 + r2 + r3);
    });

    *combis.get(&last).unwrap()
}

#[cfg(test)]
mod tests {
    use crate::{day10::Day10, Day};

    const EXAMPLE_1: &'static str = include_str!("../../data/10_example1.in");
    const EXAMPLE_2: &'static str = include_str!("../../data/10_example2.in");

    #[test]
    fn test_part1() {
        let day1 = Day10::parse(EXAMPLE_1);
        assert_eq!(7 * 5, day1.solve1());

        let day2 = Day10::parse(EXAMPLE_2);
        assert_eq!(22 * 10, day2.solve1());
    }

    #[test]
    fn test_part2() {
        let day1 = Day10::parse(EXAMPLE_1);
        assert_eq!(8, day1.solve2());

        let day2 = Day10::parse(EXAMPLE_2);
        assert_eq!(19208, day2.solve2());
    }
}
