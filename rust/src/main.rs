mod day02;

use std::{env, process};

trait Day {
    fn parse(text: &str) -> Self;
    fn solve1(&self) -> usize;
    fn solve2(&self) -> usize;
}

fn main() {
    let mut args = env::args();
    args.next().unwrap();

    if let (Some(day_number), Some(path)) = (args.next(), args.next()) {
        println!("Got day {day_number} and file {path}");

        let day_number = day_number
            .parse()
            .expect("Please provide a valid day-number");
        if let Ok(text) = std::fs::read_to_string(path) {
            let day = match day_number {
                2 => day02::Day02::parse(&text),
                _ if day_number > 25 => unreachable!("Too high"),
                _ if day_number < 0 => unreachable!("Too low"),
                _ => todo!("Day not done yet!"),
            };

            println!("Day {day_number:0>2} Part1 => {}", day.solve1());
            println!("Day {day_number:0>2} Part2 => {}", day.solve2());
        }
    } else {
        eprintln!("Please provide day and input data");
        process::exit(1);
    }
}
