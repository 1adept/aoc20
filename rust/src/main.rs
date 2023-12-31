mod day02;
mod day04;
mod day05;
mod day06;
mod day07;
mod day09;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
mod day17;
mod day18;
mod day19;

use std::{env, process};

trait Day {
    fn parse(text: &str) -> Box<Self>
    where
        Self: Sized;
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
            let day: Box<dyn Day> = match day_number {
                2 => day02::Day02::parse(&text),
                4 => day04::Day04::parse(&text),
                5 => day05::Day05::parse(&text),
                6 => day06::Day06::parse(&text),
                7 => day07::Day07::parse(&text),
                9 => day09::Day09::parse(&text),
                10 => day10::Day10::parse(&text),
                11 => day11::Day11::parse(&text),
                12 => day12::Day12::parse(&text),
                13 => day13::Day13::parse(&text),
                14 => day14::Day14::parse(&text),
                15 => day15::Day15::parse(&text),
                16 => day16::Day16::parse(&text),
                17 => day17::Day17::parse(&text),
                18 => day18::Day18::parse(&text),
                19 => day19::Day19::parse(&text),
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
