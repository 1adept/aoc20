use crate::Day;

pub struct Day11(Map);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Cell {
    Floor,
    Occupied,
    Empty,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Map {
    width: usize,
    height: usize,
    cells: Vec<Cell>,
}

impl Day for Day11 {
    fn parse(text: &str) -> Box<Self>
    where
        Self: Sized,
    {
        let width = text.lines().take(1).flat_map(|line| line.chars()).count();
        let height = width;
        Box::new(Day11(Map {
            width,
            height,
            cells: text
                .lines()
                .flat_map(|line| line.chars().map(Cell::from))
                .collect(),
        }))
    }

    fn solve1(&self) -> usize {
        let rotate1 = |map: &Map| {
            map.rotate(
                |row, col| map.get_adjacent_cells(row, col),
                |cell, consider| match (cell, consider) {
                    (Cell::Floor, _) => Cell::Floor,
                    (Cell::Empty, neigh) if neigh.iter().all(|n| **n != Cell::Occupied) => {
                        Cell::Occupied
                    }
                    (Cell::Occupied, neigh)
                        if 4 <= neigh.iter().filter(|n| ***n == Cell::Occupied).count() =>
                    {
                        Cell::Empty
                    }
                    (c, _) => *c,
                },
            )
        };

        let mut map = self.0.occupy();

        while let Some(next) = rotate1(&map) {
            map = next;
        }

        map.cells().filter(|c| Cell::Occupied == **c).count()
    }

    fn solve2(&self) -> usize {
        let rotate2 = |map: &Map| {
            map.rotate(
                |row, col| map.get_seen_cells(row, col),
                |cell, seen| match (cell, seen) {
                    (Cell::Floor, _) => Cell::Floor,
                    (Cell::Occupied, nbs)
                        if 5 <= nbs.iter().filter(|n| ***n == Cell::Occupied).count() =>
                    {
                        Cell::Empty
                    }
                    (Cell::Empty, nbs) if nbs.iter().all(|n| **n != Cell::Occupied) => {
                        Cell::Occupied
                    }
                    (c, _) => *c,
                },
            )
        };

        let mut map = self.0.occupy();
        while let Some(next) = rotate2(&map) {
            map = next;
        }

        map.cells().filter(|c| **c == Cell::Occupied).count()
    }
}

impl From<char> for Cell {
    fn from(value: char) -> Self {
        match value {
            '.' => Cell::Floor,
            'L' => Cell::Empty,
            '#' => Cell::Occupied,
            _ => unreachable!("No such cell {value}"),
        }
    }
}

impl Map {
    /// Occupy all seats
    fn occupy(&self) -> Map {
        let cells = self
            .cells
            .iter()
            .map(|c| match c {
                Cell::Floor => Cell::Floor,
                _ => Cell::Occupied,
            })
            .collect();
        Map { cells, ..*self }
    }

    fn index(&self, row: usize, col: usize) -> usize {
        row * self.width + col
    }

    fn pos(&self, index: usize) -> (usize, usize) {
        let row = index / self.width;
        let col = index % self.width;
        (row, col)
    }

    fn get(&self, row: usize, col: usize) -> Option<&Cell> {
        if row < self.height && col < self.width {
            Some(&self.cells[self.index(row, col)])
        } else {
            None
        }
    }

    fn traverse(
        &self,
        (row, col): (usize, usize),
        (move_col, move_row): (isize, isize),
    ) -> Vec<&Cell> {
        let mut cells = Vec::new();

        let w = self.width as isize;
        let h = self.height as isize;

        let move_pos = move |row: isize, col: isize| {
            let next_row = row + move_row;
            let next_col = col + move_col;
            if next_row >= 0 && next_row < h && next_col >= 0 && next_col < w {
                Some((next_row, next_col))
            } else {
                None
            }
        };

        let mut r = row as isize;
        let mut c = col as isize;
        while let Some((row, col)) = move_pos(r, c) {
            r = row;
            c = col;
            let cell = self.get(row as usize, col as usize).expect("Checked");
            cells.push(cell);
            if cell != &Cell::Floor {
                break;
            }
        }
        cells
    }

    fn get_seen_cells(&self, at_row: usize, at_col: usize) -> Vec<&Cell> {
        if let Some(cell) = self.get(at_row, at_col) {
            // Floors cant see
            if cell == &Cell::Floor {
                return Vec::with_capacity(0);
            };
        }

        let pos = (at_row, at_col);

        let left = self.traverse(pos, (-1, 0));
        let right = self.traverse(pos, (1, 0));
        let up = self.traverse(pos, (0, -1));
        let down = self.traverse(pos, (0, 1));

        let up_left = self.traverse(pos, (-1, -1));
        let up_right = self.traverse(pos, (-1, 1));
        let down_right = self.traverse(pos, (1, 1));
        let down_left = self.traverse(pos, (1, -1));

        let mut cells = Vec::new();
        cells.extend(left);
        cells.extend(right);
        cells.extend(up);
        cells.extend(down);
        cells.extend(up_left);
        cells.extend(up_right);
        cells.extend(down_left);
        cells.extend(down_right);

        cells
    }

    fn get_adjacent_cells(&self, at_row: usize, at_col: usize) -> Vec<&Cell> {
        let mut cells = Vec::with_capacity(8);

        let row_min = at_row.saturating_sub(1);
        let row_max = self.height.min(at_row + 2); // +2 because its exclusive
        let col_min = at_col.saturating_sub(1);
        let col_max = self.width.min(at_col + 2); // +2 because its exclusive

        for row in row_min..row_max {
            for col in col_min..col_max {
                if row == at_row && col == at_col {
                    continue;
                }
                cells.push(self.get(row, col).expect("Checked range"));
            }
        }
        cells
    }

    fn rotate<'a, F, T>(&self, get_cells: F, transform: T) -> Option<Map>
    where
        F: Fn(usize, usize) -> Vec<&'a Cell>,
        T: Fn(&Cell, &[&Cell]) -> Cell,
    {
        let mut changed = false;
        let mut cells = Vec::with_capacity(self.width * self.height);

        for (row, col) in self.iter() {
            let curr_cell = self.get(row, col).expect("Checked range");
            let considered_cells = get_cells(row, col);

            let new_cell = transform(curr_cell, &considered_cells);
            cells.push(new_cell);

            changed |= curr_cell != &new_cell;
        }

        if changed {
            Some(Map {
                width: self.width,
                height: self.height,
                cells,
            })
        } else {
            None
        }
    }

    fn iter(&self) -> MapIterator {
        MapIterator {
            map: self,
            index: 0,
        }
    }

    fn cells(&self) -> MapCellIterator {
        MapCellIterator {
            map: self,
            index: 0,
        }
    }
}

struct MapIterator<'a> {
    map: &'a Map,
    index: usize,
}

struct MapCellIterator<'a> {
    map: &'a Map,
    index: usize,
}

impl<'a> Iterator for MapCellIterator<'a> {
    type Item = &'a Cell;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= (self.map.width * self.map.height) {
            return None;
        }

        let (row, col) = self.map.pos(self.index);
        let cell = self.map.get(row, col).expect("Checked range");

        self.index += 1;
        Some(cell)
    }
}

impl Iterator for MapIterator<'_> {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= (self.map.width * self.map.height) {
            return None;
        }

        let (row, col) = self.map.pos(self.index);
        self.index += 1;
        Some((row, col))
    }
}

mod visuals {
    use std::fmt::Display;

    use super::{Cell, Map};

    impl Display for Cell {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}",
                match self {
                    Cell::Floor => '.',
                    Cell::Empty => 'L',
                    Cell::Occupied => '#',
                }
            )
        }
    }

    impl Display for Map {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let mut str = String::new();

            for row in 0..self.height {
                for col in 0..self.width {
                    let char = match self.get(row, col).expect("Checked range") {
                        Cell::Floor => '.',
                        Cell::Occupied => '#',
                        Cell::Empty => 'L',
                    };
                    str.push(char);
                }
                str.push('\n');
            }

            write!(f, "{str}")
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::Day;

    use super::Day11;

    const EXAMPLE: &str = include_str!("../../data/11_example.in");

    // #[test]
    // fn test_index() {
    //     let day = Day11::parse(EXAMPLE);
    //     let index = 0;
    //     let w = day.0.width as isize;
    //     let m = (day.0.width * day.0.height) as isize;

    // let move_index = |index: &isize, col: isize, row: isize| {
    //     let next = index + (w * row) + col;
    //     if next >= 0 && next <
    // };

    //     assert_e
    // }

    #[test]
    fn test_part1() {
        let day = Day11::parse(EXAMPLE);
        assert_eq!(37, day.solve1());
    }

    #[test]
    fn test_part2() {
        let day = Day11::parse(EXAMPLE);
        assert_eq!(26, day.solve2());
    }
}
