// use crate::Day;
// use regex::Regex;

// const EXAMPLE: &'static str = include_str!("../../data/02_example.in");

// struct Day02 {}
// impl Day for Day02 {
//     fn solve1(input: String) -> u32 {
//         input
//             .lines()
//             .flat_map(|line| {
//                 if let Some(captures) = Day02::PWD_REGEX.captures(line) {
//                     let min = captures.name("min");
//                     let max = captures.name("max");
//                     let chars = captures.name("char");
//                     let pwd = captures.name("pwd");

//                     let occurances = pwd.iter().filter(|c| c == chars).count();
//                     if occurances >= min && occurances <= max {
//                         None
//                     } else {
//                         Some(pwd)
//                     }
//                 }
//             })
//             .count()
//     }

//     fn solve2(input: String) {
//         todo!()
//     }
// }
// impl Day02 {
//     const PWD_REGEX: Regex =
//         Regex::new(r"(?<min>\d+)-(?<max>\d+) (?<char>[a-z]): (?<pwd>\w+)").unwrap();

//     fn parse(input: &str) -> &[&str] {}
// }
