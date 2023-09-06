use crate::Day;

pub struct Day13 {
    earliest_departure: Timestamp,
    buses: Vec<Bus>,
}

/// Minutes since some fixed reference point
/// At Time 0 every bus simultaniously departs
type Timestamp = usize;
/// Identification and indicates how often a bus leaves
/// i.e. ID=7 means the bus takes 7 Minutes from departing to arriving at the same location again
type Bus = Option<usize>;

impl Day for Day13 {
    fn parse(text: &str) -> Box<Self>
    where
        Self: Sized,
    {
        let (timestamp, buses) = text.split_once('\n').unwrap();

        let earliest_departure = timestamp.parse().expect("Invalid Timestamp");
        let buses = buses
            .split(',')
            // .filter(|str| str != &"x")
            .map(|bus| match bus {
                "x" => None,
                _ => Some(bus.trim_end().parse().expect("Invalid bus id {bus}")),
            })
            .collect();

        Box::new(Day13 {
            earliest_departure,
            buses,
        })
    }

    fn solve1(&self) -> usize {
        let (bus_id, trips) = self
            .buses
            .iter()
            .flatten()
            .map(|bus| (bus, 1 + (self.earliest_departure / bus)))
            .min_by_key(|(bus, trips)| *bus * trips)
            .expect("Didnt find any bus");
        let time = bus_id * trips;
        let wait = time - self.earliest_departure;
        bus_id * wait
    }

    fn solve2(&self) -> usize {
        self.buses
            .iter()
            .enumerate()
            .flat_map(|(i, bus)| bus.map(|id| (i, id)))
            // Start at timestamp=0 and distance=1
            // distance saves the distance between all checked busses arraving at the given offset
            .fold((0, 1), |(mut timestamp, distance), (offset, bus)| {
                // advance timestamp by last common distance until the current bus also matches.
                while (timestamp + offset) % bus != 0 {
                    timestamp += distance;
                }
                // Now we can multiply our distance with the current bus to get the next common distance
                (timestamp, distance * bus)
            })
            .0 // Get timestamp
    }
}

#[cfg(test)]
mod tests {
    use super::Day13;
    use crate::Day;

    const EXAMPLE: &str = include_str!("../../data/13_example.in");

    #[test]
    fn test_part1() {
        let day = Day13::parse(EXAMPLE);
        assert_eq!(295, day.solve1());
    }

    #[test]
    fn test_part2() {
        let day = Day13::parse(EXAMPLE);
        assert_eq!(1068781, day.solve2());
    }

    #[test]
    fn test_part2_1() {
        let day = Day13::parse("0\n17,x,13,19");
        assert_eq!(3417, day.solve2());
    }

    #[test]
    fn test_part2_2() {
        let day = Day13::parse("0\n67,7,59,61");
        assert_eq!(754018, day.solve2());
    }

    #[test]
    fn test_part2_3() {
        let day = Day13::parse("0\n67,x,7,59,61");
        assert_eq!(779210, day.solve2());
    }

    #[test]
    fn test_part2_4() {
        let day = Day13::parse("0\n67,7,x,59,61");
        assert_eq!(1261476, day.solve2());
    }

    #[test]
    fn test_part2_5() {
        let day = Day13::parse("0\n1789,37,47,1889");
        assert_eq!(1202161486, day.solve2());
    }
}
