use core::panic;

use crate::Day;

pub struct Day12(Vec<Instruction>);

#[derive(Debug)]
struct Navigable {
    facing: Dir,
    east: isize,
    north: isize,
}

#[derive(Debug)]
enum Instruction {
    Move(Dir, isize),
    Turn(Turn, isize),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum Dir {
    North,
    East,
    South,
    West,
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
enum Turn {
    Left,
    Forward,
    Right,
}

impl Day for Day12 {
    fn parse(text: &str) -> Box<Self>
    where
        Self: Sized,
    {
        Box::new(Day12(
            text.lines()
                .take_while(|line| !line.is_empty())
                .map(Instruction::from)
                .collect(),
        ))
    }

    fn solve1(&self) -> usize {
        let mut ship = Navigable::ship();

        for instr in &self.0 {
            match *instr {
                Instruction::Move(dir, amount) => ship.move_dir(dir, amount),
                Instruction::Turn(turn, degree) => match turn {
                    Turn::Forward => ship.move_dir(ship.facing, degree),
                    _ => ship.turn(turn, degree),
                },
            }
        }

        manhattan_distance((0, 0), (ship.north, ship.east))
    }

    fn solve2(&self) -> usize {
        let mut ship = Navigable::ship();
        let mut waypoint = Navigable::waypoint();

        for instr in &self.0 {
            match *instr {
                Instruction::Move(dir, amount) => waypoint.move_dir(dir, amount),
                Instruction::Turn(turn, degrees) => match turn {
                    Turn::Forward => {
                        ship.north += waypoint.north * degrees;
                        ship.east += waypoint.east * degrees;
                    }
                    _ => {
                        waypoint.turn(turn, degrees);

                        match waypoint.facing {
                            Dir::North => waypoint.north = waypoint.north.abs(),
                            Dir::East => waypoint.east = waypoint.east.abs(),
                            Dir::South => waypoint.north = -waypoint.north.abs(),
                            Dir::West => waypoint.east = -waypoint.east.abs(),
                        }
                    }
                },
            }
        }

        // 33234 too low
        manhattan_distance((0, 0), (ship.north, ship.east))
    }
}

impl From<u8> for Dir {
    fn from(value: u8) -> Self {
        match value {
            0 => Dir::North,
            1 => Dir::East,
            2 => Dir::South,
            3 => Dir::West,
            _ => panic!("Cant convert to Dir"),
        }
    }
}

impl From<&str> for Instruction {
    fn from(value: &str) -> Self {
        let amount = value[1..]
            .trim_end()
            .parse()
            .expect("Not a parsable number");
        match &value[..1] {
            "N" => Instruction::Move(Dir::North, amount),
            "S" => Instruction::Move(Dir::South, amount),
            "E" => Instruction::Move(Dir::East, amount),
            "W" => Instruction::Move(Dir::West, amount),
            "F" => Instruction::Turn(Turn::Forward, amount),
            "R" => Instruction::Turn(Turn::Right, amount),
            "L" => Instruction::Turn(Turn::Left, amount),
            _ => unreachable!("Doesnt exist"),
        }
    }
}

/// Ignores first argument because it must always be zero
/// It just exists as a reminder
fn manhattan_distance(_: (isize, isize), to: (isize, isize)) -> usize {
    // from is always 0 in this case because ship starts at (0, 0)
    to.0.unsigned_abs() + to.1.unsigned_abs()
}

impl Navigable {
    fn ship() -> Self {
        Navigable {
            facing: Dir::East,
            east: 0,
            north: 0,
        }
    }

    fn waypoint() -> Self {
        Navigable {
            facing: Dir::East,
            east: 10,
            north: 1,
        }
    }

    fn turn(&mut self, turn: Turn, degree: isize) {
        assert_ne!(Turn::Forward, turn);

        let facing_num = self.facing as i8;
        // Direction number (-1/0/1) to shift to
        let turn_num_dir = (turn as i8) - 1;

        // For every 90° do 1 step
        let steps = (degree / 90) as i8;

        let new_enum_num = facing_num + (steps * turn_num_dir);
        let new_facing_num = new_enum_num.rem_euclid(4) as u8;

        self.facing = new_facing_num.into();
    }

    fn move_dir(&mut self, dir: Dir, amount: isize) {
        match dir {
            Dir::North => self.north += amount,
            Dir::East => self.east += amount,
            Dir::South => self.north -= amount,
            Dir::West => self.east -= amount,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Day12, Dir, Navigable, Turn};
    use crate::Day;

    const EXAMPLE: &str = include_str!("../../data/12_example.in");

    #[test]
    fn test_ship_turn() {
        let ship_turn = |turn: Turn, degree: isize| {
            // Facing East by default for ship
            let mut ship = Navigable::ship();
            ship.turn(turn, degree);
            ship.facing
        };

        assert_eq!(Dir::North, ship_turn(Turn::Left, 90));
        assert_eq!(Dir::West, ship_turn(Turn::Left, 180));
        assert_eq!(Dir::South, ship_turn(Turn::Left, 270));
        assert_eq!(Dir::East, ship_turn(Turn::Left, 360));

        assert_eq!(Dir::South, ship_turn(Turn::Right, 90));
        assert_eq!(Dir::West, ship_turn(Turn::Right, 180));
        assert_eq!(Dir::North, ship_turn(Turn::Right, 270));
        assert_eq!(Dir::East, ship_turn(Turn::Right, 360));
    }

    #[test]
    fn test_part1() {
        let day = Day12::parse(EXAMPLE);
        assert_eq!(25, day.solve1());
    }

    #[test]
    fn test_part2() {
        let day = Day12::parse(EXAMPLE);
        assert_eq!(286, day.solve2());
    }
}
