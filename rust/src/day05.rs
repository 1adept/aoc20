use std::collections::HashSet;

use crate::Day;

pub struct Day05(Vec<BoardingPass>);
struct BoardingPass(u16);

impl Day for Day05 {
    fn parse(text: &str) -> Box<Self>
    where
        Self: Sized,
    {
        Box::new(Day05(text.lines().map(BoardingPass::from).collect()))
    }

    fn solve1(&self) -> usize {
        self.0.iter().map(|pass| pass.0 as usize).max().unwrap_or(0)
    }

    fn solve2(&self) -> usize {
        let ids: HashSet<u16> = self.0.iter().map(|pass| pass.0).collect();

        for id in &ids {
            if !ids.contains(&(id + 1)) && ids.contains(&(id + 2)) {
                return (id + 1) as usize;
            }
        }
        unreachable!("You have to have a seat ...");
    }
}

impl From<&str> for BoardingPass {
    fn from(value: &str) -> Self {
        let (row, col) = BoardingPass::calc_seats(value);
        let id: u16 = (row as u16) * 8 + (col as u16);
        BoardingPass(id)
    }
}

impl BoardingPass {
    fn calc_seats(pass_str: &str) -> (u8, u8) {
        const MAX_ROW: u8 = 127; // Last row
        const MAX_COL: u8 = 7; // Last Column

        let mut row: (u8, u8) = (0, MAX_ROW);
        let mut col: (u8, u8) = (0, MAX_COL);

        let get_half_range = |range: (u8, u8)| (range.1 + 1 - range.0).checked_div(2).unwrap();

        pass_str.chars().for_each(|c| {
            match c {
                'F' => row.1 -= get_half_range(row),
                'B' => row.0 += get_half_range(row),
                'L' => col.1 -= get_half_range(col),
                'R' => col.0 += get_half_range(col),
                _ => unreachable!(),
            };
        });

        assert_eq!(row.0, row.1);
        assert_eq!(col.0, col.1);

        (row.0, col.0)
    }
}

#[cfg(test)]
mod tests {
    use crate::Day;

    use super::{BoardingPass, Day05};

    const EXAMPLE: &str = include_str!("../../data/05_example.in");

    #[test]
    fn test_boarding_pass_creation() {
        let pass1 = BoardingPass::from("FBFBBFFRLR");
        let pass2 = BoardingPass::from("BFFFBBFRRR");
        let pass3 = BoardingPass::from("FFFBBBFRRR");
        let pass4 = BoardingPass::from("BBFFBBFRLL");

        assert_eq!(357, pass1.0);
        assert_eq!(567, pass2.0);
        assert_eq!(119, pass3.0);
        assert_eq!(820, pass4.0);
    }

    #[test]
    fn test_part1() {
        assert_eq!(820, Day05::parse(EXAMPLE).solve1());
    }

    // No test for part2 given
}
