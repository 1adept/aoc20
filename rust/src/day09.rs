use crate::Day;

pub struct Day09(Vec<u64>);

impl Day for Day09 {
    fn parse(text: &str) -> Box<Self>
    where
        Self: Sized,
    {
        Box::new(Day09(text.lines().flat_map(|line| line.parse()).collect()))
    }

    fn solve1(&self) -> usize {
        last_preamble(&self.0, 25) as usize
    }

    fn solve2(&self) -> usize {
        todo!()
    }
}

fn last_preamble(numbers: &[u64], preamble: usize) -> u64 {
    *numbers
        .windows(preamble + 1)
        .find(|window| !window_sums_to(&window[..preamble], window.last().unwrap()))
        .map(|window| window.last().unwrap())
        .unwrap()
}

fn window_sums_to(numbers: &[u64], sum_to: &u64) -> bool {
    let size = numbers.len();
    for i in 0..size {
        let first = numbers[i];
        for j in (i + 1)..size {
            let second = numbers[j];
            if (first + second) == *sum_to {
                return true;
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use crate::{day09::last_preamble, Day};

    use super::Day09;

    const EXAMPLE: &'static str = include_str!("../../data/09_example.in");

    #[test]
    fn test_part1() {
        let day = Day09::parse(EXAMPLE);
        assert_eq!(127, last_preamble(&day.as_ref().0, 5));
    }

    #[test]
    fn test_part2() {
        todo!()
    }
}
