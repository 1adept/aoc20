#!/usr/bin/env nu

use library.nu mkPath

def parse [test:bool] {
  mkPath 2 $test
    | open
    | lines
    | str replace '-' ' '
    | split column ' ' min max char password
    | update char {|c| str replace ':' ''}
    | update min {|m| ($m.min | into int) }
    | update max {|m| ($m.max | into int) }
}

# Run Part1 of Day 2
export def part1 [
  --test(-t) # Run example file
] {
  parse (if ($test) { true } else { false })
    | filter {|row| 
      let count = ($row.password 
        | split chars
        | filter {|c| $c == $row.char }
        | str join
        | str length)
        $count >= $row.min and $count <= $row.max
    }
    | length
}

# Run part2 of day 2
export def part2 [
  --test(-t) # Run example file
] {
  parse (if ($test) { true } else { false })
    | filter {|row|
      let chars = ($row.password | str substring ($row.min - 1)..$row.max | split chars)
      let first = ($chars | first)
      let last = ($chars | last)
      $first == $row.char xor $last == $row.char
    } 
    | length
}
