use crate::Day;

pub struct Day15;

impl Day for Day15 {
    fn parse(text: &str) -> Box<Self>
    where
        Self: Sized,
    {
        todo!()
    }

    fn solve1(&self) -> usize {
        todo!()
    }

    fn solve2(&self) -> usize {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::Day;

    use super::Day15;

    const EXAMPLE: &'static str = include_str!("../../data/15_example.in");

    #[test]
    fn test_part1() {
        let day = Day15::parse(EXAMPLE);
        assert_eq!(436, day.solve1());
    }

    #[test]
    #[ignore = "todo"]
    fn test_part2() {
        // let day = Day15::parse(EXAMPLE);
        // assert_eq!(?, day.solve1());
    }
}
