use std::{collections::BTreeMap, fmt::Display, str::FromStr};

use itertools::Itertools;

use crate::Day;

pub struct Day17(Cube);

#[derive(Debug, PartialEq, Eq, Clone)]
enum State {
    Active,
    Inactive,
}

#[derive(Debug, Clone)]
struct Cube {
    cells: BTreeMap<Position, State>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
struct Position {
    x: isize,
    y: isize,
    z: isize,
    w: isize,
}

impl Day for Day17 {
    fn parse(text: &str) -> Box<Self>
    where
        Self: Sized,
    {
        Box::new(Day17(Cube::from_str(text).expect("Parsing cube failed")))
    }

    fn solve1(&self) -> usize {
        let mut cube = self.0.clone();
        for _ in 0..6 {
            cycle(&mut cube, Position::surrounding3);
        }

        cube.count_state(&State::Active)
    }

    fn solve2(&self) -> usize {
        let mut cube = self.0.clone();
        for _ in 0..6 {
            cycle(&mut cube, Position::surrounding4);
        }

        cube.count_state(&State::Active)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct ParseCubeError;
impl FromStr for Cube {
    type Err = ParseCubeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parse_cell = |c| match c {
            '.' => Ok(State::Inactive),
            '#' => Ok(State::Active),
            _ => Err(ParseCubeError),
        };

        let cells = s
            .lines()
            .filter(|line| !line.is_empty())
            .enumerate()
            .flat_map(|(x, line)| {
                line.chars().enumerate().map(move |(y, c)| {
                    parse_cell(c).map(|st| (Position::from((x as isize, y as isize, 0isize)), st))
                })
            })
            .collect::<Result<BTreeMap<_, _>, ParseCubeError>>()?;

        Ok(Cube { cells })
    }
}

impl From<(isize, isize, isize)> for Position {
    fn from((x, y, z): (isize, isize, isize)) -> Self {
        Position { x, y, z, w: 0 }
    }
}

impl From<(isize, isize, isize, isize)> for Position {
    fn from((x, y, z, w): (isize, isize, isize, isize)) -> Self {
        Position { x, y, z, w }
    }
}

impl Cube {
    fn get(&self, pos: &Position) -> &State {
        self.cells.get(pos).unwrap_or(&State::Inactive)
    }

    fn count_state(&self, state: &State) -> usize {
        self.cells.values().filter(|st| *st == state).count()
    }
}

impl Position {
    fn surrounding3(&self) -> Box<dyn Iterator<Item = Position>> {
        let x = self.x;
        let y = self.y;
        let z = self.z;
        Box::new(
            (x - 1..=x + 1)
                .flat_map(move |x| {
                    (y - 1..=y + 1)
                        .flat_map(move |y| (z - 1..=z + 1).map(move |z| Position { x, y, z, w: 0 }))
                })
                .filter(move |p| !(p.x == x && p.y == y && p.z == z)),
        )
    }

    fn surrounding4(&self) -> Box<dyn Iterator<Item = Position>> {
        let x = self.x;
        let y = self.y;
        let z = self.z;
        let w = self.w;
        Box::new(
            (x - 1..=x + 1)
                .flat_map(move |x| {
                    (y - 1..=y + 1).flat_map(move |y| {
                        (z - 1..=z + 1).flat_map(move |z| {
                            (w - 1..=w + 1).map(move |w| Position { x, y, z, w })
                        })
                    })
                })
                .filter(move |p| !(p.x == x && p.y == y && p.z == z && p.w == w)),
        )
    }
}

fn cycle<S>(cube: &mut Cube, surroundings: S)
where
    S: Fn(&Position) -> Box<dyn Iterator<Item = Position>>,
{
    let mut changes = Vec::new();

    let watch_positions: Vec<_> = cube
        .cells
        .iter()
        // Only take active cells
        .filter_map(|(p, st)| if *st == State::Active { Some(p) } else { None })
        // Take surrounding cells, because they might change too
        // Cells that are not next to active cells have no way to activate
        // Re-Add the current cell, because it gets filtered in #surroundings
        .flat_map(|p| surroundings(p).chain(vec![p.clone()].into_iter()))
        // Dont double-check
        // There are probaby doubles because we are taking the surrounding cells too
        .unique()
        .collect();

    for position in watch_positions {
        let active_neighbors = surroundings(&position)
            .map(|p| cube.get(&p))
            .filter(|st| **st == State::Active)
            .count();

        let state = cube.get(&position);
        let new_state = match (state, active_neighbors) {
            (State::Active, 2..=3) => State::Active,
            (State::Inactive, 3) => State::Active,
            _ => State::Inactive,
        };

        if *state != new_state {
            changes.push((position.clone(), new_state));
        }
    }

    changes.into_iter().for_each(|(p, st)| {
        match st {
            State::Active => {
                cube.cells.insert(p, st);
            }
            State::Inactive => {
                cube.cells.remove(&p);
            }
        };
    });
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Position { x, y, z, w } = self;
        let str = format!("({x}, {y}, {z}, {w})");
        write!(f, "{str}")
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        day17::{cycle, Position, State},
        Day,
    };

    use super::Day17;

    const EXAMPLE: &str = include_str!("../../data/17_example.in");

    #[test]
    fn test_cube_indexing() {
        let day = Day17::parse(EXAMPLE);

        assert_eq!(&State::Inactive, day.0.get(&Position::from((0, 0, 0))));
        assert_eq!(&State::Active, day.0.get(&Position::from((0, 1, 0))));
        assert_eq!(&State::Active, day.0.get(&Position::from((1, 2, 0))));
    }

    #[test]
    fn test_part1() {
        let day = Day17::parse(EXAMPLE);
        assert_eq!(112, day.solve1());
    }

    #[test]
    fn test_part2() {
        let day = Day17::parse(EXAMPLE);
        assert_eq!(848, day.solve2());
    }

    fn assert_state_at(day: &Day17, x: isize, y: isize, z: isize, state: State) {
        assert_eq!(&state, day.0.get(&Position { x, y, z, w: 0 }));
    }

    #[test]
    fn test_neighbors() {
        let day = Day17::parse(EXAMPLE);

        let first: Position = (0, 0, 0).into();
        let actives = first
            .surrounding3()
            .filter(|p| day.0.get(p) == &State::Active)
            .count();
        assert_eq!(1, actives);

        let b: Position = (2, 1, 0).into();
        let actives = b
            .surrounding3()
            .filter(|p| day.0.get(p) == &State::Active)
            .count();
        assert_eq!(3, actives);
    }

    #[test]
    fn test_init() {
        use State::Active as A;
        use State::Inactive as I;

        let day = Day17::parse(EXAMPLE);

        let ass = |x, y, z, st| assert_state_at(&day, x, y, z, st);

        ass(0, 0, 0, I);
        ass(0, 1, 0, A);
        ass(0, 2, 0, I);
        ass(1, 0, 0, I);
        ass(1, 1, 0, I);
        ass(1, 2, 0, A);
        ass(2, 0, 0, A);
        ass(2, 1, 0, A);
        ass(2, 2, 0, A);
    }

    #[test]
    fn test_surround_count() {
        let pos: Position = (0, 0, 0).into();

        assert_eq!(26, pos.surrounding3().count());
        assert_eq!(80, pos.surrounding4().count());
    }

    #[test]
    fn test_cycles_count() {
        let mut day = Day17::parse(EXAMPLE);
        let get_count = |day: &Day17| day.0.count_state(&State::Active);

        assert_eq!(5, get_count(&day));
        cycle(&mut day.0, Position::surrounding3);
        assert_eq!(11, get_count(&day));
    }

    #[test]
    fn test_cycle() {
        /*
        0x0
        00x
        xxx

        000 <- former 1. row
        x0x
        0xx
        0x0
        */
        use State::Active as A;
        use State::Inactive as I;

        let day = Day17::parse(EXAMPLE);
        let mut cube = day.0;
        cycle(&mut cube, Position::surrounding3);

        assert_eq!(A, *cube.get(&(1, 0, 0).into()));
        assert_eq!(I, *cube.get(&(1, 1, 0).into()));
        assert_eq!(A, *cube.get(&(1, 2, 0).into()));
        assert_eq!(I, *cube.get(&(2, 0, 0).into()));
        assert_eq!(A, *cube.get(&(2, 1, 0).into()));
        assert_eq!(A, *cube.get(&(2, 2, 0).into()));
        assert_eq!(I, *cube.get(&(3, 0, 0).into()));
        assert_eq!(A, *cube.get(&(3, 1, 0).into()));
        assert_eq!(I, *cube.get(&(3, 2, 0).into()));
    }
}
