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
        let key = self.solve1() as u64;
        let numbers = continuous_numbers_to_sum(&self.0, &key);
        let min = numbers.iter().min().unwrap();
        let max = numbers.iter().max().unwrap();
        (min + max) as usize
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
        for second in numbers.iter().skip(i + 1).take(size) {
            if (first + second) == *sum_to {
                return true;
            }
        }
    }
    false
}

fn continuous_numbers_to_sum<'a>(numbers: &'a [u64], sum_to: &u64) -> &'a [u64] {
    let mut start: usize = 0;
    let mut end: usize = 1;
    let mut sum = numbers[0] + numbers[1]; // Min 2 numbers

    while sum != *sum_to {
        if sum < *sum_to {
            end += 1;
            sum += numbers[end];
        } else {
            // sum > *sum_to
            sum -= numbers[start];
            start += 1;
        }
    }
    &numbers[start..=end]
}

#[cfg(test)]
mod tests {
    use crate::{day09::last_preamble, Day};

    use super::{continuous_numbers_to_sum, Day09};

    const EXAMPLE: &str = include_str!("../../data/09_example.in");

    #[test]
    fn test_part1() {
        let day = Day09::parse(EXAMPLE);
        assert_eq!(127, last_preamble(&day.as_ref().0, 5));
    }

    #[test]
    fn test_part2() {
        let day = Day09::parse(EXAMPLE);
        let key = last_preamble(&day.as_ref().0, 5);
        let nums = continuous_numbers_to_sum(&day.as_ref().0, &key);
        let min = nums.iter().min().unwrap();
        let max = nums.iter().max().unwrap();
        let res = (min + max) as usize;
        assert_eq!(62, res);
    }
}
