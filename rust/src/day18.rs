use std::{fmt::Display, rc::Rc, str::FromStr};

use crate::Day;

pub struct Day18(Rc<[Scope]>);

#[derive(Debug, PartialEq, Clone)]
enum Operator {
    Add,
    Mul,
}

#[derive(Debug, Clone)]
enum Expr {
    Number(usize),
    Op(Operator),
    Scope(Scope),
}

#[derive(Debug, Clone)]
struct Scope {
    exprs: Vec<Expr>,
}

impl Day for Day18 {
    fn parse(text: &str) -> Box<Self>
    where
        Self: Sized,
    {
        Box::new(Day18(
            text.lines()
                .filter(|line| !line.is_empty())
                .map(|line| Scope::from_str(line).expect("Failed scope parse"))
                .collect::<Rc<_>>(),
        ))
    }

    fn solve1(&self) -> usize {
        self.0.iter().map(|scope| scope.solve()).sum()
    }

    fn solve2(&self) -> usize {
        self.0.iter().map(|scope| scope.solve_precedence()).sum()
    }
}

#[derive(Debug)]
struct ParseOperatorError;
impl FromStr for Operator {
    type Err = ParseOperatorError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.trim() {
            "+" => Ok(Operator::Add),
            "*" => Ok(Operator::Mul),
            _ => Err(ParseOperatorError),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct ParseScopeError;
impl FromStr for Scope {
    type Err = ParseScopeError;

    fn from_str(str: &str) -> Result<Self, Self::Err> {
        fn parse_single_token(str: &str) -> Expr {
            if let Ok(op) = Operator::from_str(str) {
                Expr::Op(op)
            } else if let Ok(number) = str.parse() {
                Expr::Number(number)
            } else {
                panic!("No a token: {str}")
            }
        }

        fn get_last_scope(tokens: &mut [Expr]) -> Option<&mut Scope> {
            if let Some(Expr::Scope(s)) = tokens.last_mut() {
                Some(s)
            } else {
                None
            }
        }

        let tokens = str
            .split_whitespace()
            .fold((Vec::new(), 0usize), |(mut tokens, mut level), next| {
                let level_delta: isize = {
                    let up = next.chars().take_while(|c| *c == '(').count();
                    let down = next.chars().rev().take_while(|c| *c == ')').count();
                    up as isize - down as isize
                };
                let prev_level = level;
                level = level
                    .checked_add_signed(level_delta)
                    .expect("Level cannot go negative");

                let token = parse_single_token(match level_delta {
                    0 => next,
                    lvl if lvl > 0 => next.trim_start_matches('('),
                    _ => next.trim_end_matches(')'),
                });

                if level == 0 && level_delta == 0 {
                    // Base level
                    tokens.push(token);
                } else {
                    // Scope
                    assert!(level_delta.abs() <= 2);

                    // Goto last scope
                    let mut last_scope = get_last_scope(&mut tokens);
                    for _ in 1..prev_level {
                        last_scope = get_last_scope(&mut last_scope.unwrap().exprs);
                    }

                    if level_delta > 0 {
                        for _ in 0..level_delta {
                            if let Some(scope) = last_scope {
                                scope.add_token(Expr::Scope(Scope::new()));
                                last_scope = get_last_scope(&mut scope.exprs);
                            } else {
                                tokens.push(Expr::Scope(Scope::new()));
                                last_scope = get_last_scope(&mut tokens);
                            }
                        }
                    }

                    last_scope
                        .unwrap_or_else(|| {
                            panic!(
                                "\n{} « No scope at {token:?} lvl:{level}, delta:{level_delta}",
                                token
                            )
                        })
                        .add_token(token);
                }

                (tokens, level)
            })
            .0;
        Ok(Scope { exprs: tokens })
    }
}

impl Operator {
    fn apply(&self, left: usize, right: usize) -> usize {
        match self {
            Operator::Add => left + right,
            Operator::Mul => left * right,
        }
    }
}

impl Scope {
    fn new() -> Self {
        Self { exprs: Vec::new() }
    }

    fn add_token(&mut self, token: Expr) {
        self.exprs.push(token);
    }

    fn solve(&self) -> usize {
        self.exprs
            .iter()
            .fold(
                (None::<usize>, None::<&Operator>),
                |(mut value, mut operator), next| {
                    match next {
                        Expr::Number(n) => match (value, operator) {
                            (None, None) => value = Some(*n),
                            (Some(n2), Some(o)) => value = Some(o.apply(n2, *n)),
                            (Some(_), None) => unreachable!("Bad Expression: Two numbers!"),
                            (None, Some(_)) => unreachable!("Bad Expression!"),
                        },
                        Expr::Op(op) => operator = Some(op),
                        Expr::Scope(s) => match (value, operator) {
                            (None, None) => value = Some(s.solve()),
                            (Some(v), Some(o)) => value = Some(o.apply(v, s.solve())),
                            (Some(_), None) => unreachable!("Bad Expression: Two numbers!"),
                            _ => unreachable!("Bad Expression!"),
                        },
                    }

                    (value, operator)
                },
            )
            .0
            .unwrap()
    }

    fn solve_precedence(&self) -> usize {
        let first = if let Expr::Scope(s) = &self.exprs[0] {
            Expr::Number(s.solve_precedence())
        } else {
            self.exprs[0].clone()
        };
        let step =
            self.exprs
                .windows(2)
                .skip(1)
                .step_by(2)
                .fold(vec![first], |mut exprs, window| {
                    let [op, right] = window else { unreachable!() };
                    match (op, right) {
                        (Expr::Op(Operator::Add), Expr::Number(n)) => {
                            let last = exprs.pop();
                            if let Some(Expr::Number(number)) = last {
                                exprs.push(Expr::Number(number + n));
                            } else {
                                panic!("Bad Expression!")
                            }
                        }
                        (Expr::Op(Operator::Add), Expr::Scope(s)) => {
                            let last = exprs.pop();
                            if let Some(Expr::Number(number)) = last {
                                exprs.push(Expr::Number(number + s.solve_precedence()));
                            } else {
                                panic!("Bad Expression!")
                            }
                        }
                        (Expr::Op(Operator::Mul), Expr::Scope(s)) => {
                            exprs.push(Expr::Op(Operator::Mul));
                            exprs.push(Expr::Number(s.solve_precedence()));
                        }
                        _ => {
                            exprs.push(Expr::Op(Operator::Mul));
                            exprs.push(right.clone());
                        }
                    }
                    exprs
                });
        Scope { exprs: step }.solve()
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c = match self {
            Operator::Add => '+',
            Operator::Mul => '*',
        };
        write!(f, "{c}")
    }
}

impl Display for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({})",
            self.exprs
                .iter()
                .map(|e| format!(" {e}"))
                .collect::<String>()
                .trim_start()
        )
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Expr::Number(n) => format!("{n}"),
            Expr::Op(o) => format!("{o}"),
            Expr::Scope(s) => format!("{s}"),
        };
        write!(f, "{str}")
    }
}

#[cfg(test)]
mod tests {
    use crate::Day;
    use std::str::FromStr;

    use super::{Day18, Scope};

    const EXAMPLES: &str = include_str!("../../data/18_example.in");

    #[test]
    fn test_parse_expr() {
        EXAMPLES
            .lines()
            .map(|str| (str, Scope::from_str(str).expect("Failed scope parsing")))
            .for_each(|(str, expr)| assert_eq!(format!("({str})"), format!("{expr}")));
    }

    #[test]
    fn test_part1() {
        let results = [71, 51, 26, 437, 12240, 13632];
        results
            .iter()
            .zip(
                EXAMPLES
                    .lines()
                    .map(|line| Scope::from_str(line).expect("Failed scope parsing")),
            )
            .for_each(|(expected, expr)| assert_eq!(*expected, expr.solve()));

        assert_eq!(
            results.iter().sum::<usize>(),
            Day18::parse(EXAMPLES).solve1()
        );
    }

    #[test]
    fn test_part2() {
        let results = [231, 51, 46, 1445, 669060, 23340];
        results
            .iter()
            .zip(
                EXAMPLES
                    .lines()
                    .map(|line| Scope::from_str(line).expect("Failed scope parsing")),
            )
            .for_each(|(expected, expr)| assert_eq!(*expected, expr.solve_precedence()));

        assert_eq!(
            results.iter().sum::<usize>(),
            Day18::parse(EXAMPLES).solve2()
        );
    }
}
