// cargo-deps: regex

use std::io;
use std::io::prelude::*;
use std::vec::Vec;
use std::iter::Iterator;
use regex::Regex;

fn main() {
    let rule_regex = Regex::new(
        r"(?P<from_1>\d+)-(?P<to_1>\d+) or (?P<from_2>\d+)-(?P<to_2>\d+)")
        .unwrap();
    let mut rules = Vec::new();
    let mut error_rate = 0;
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines().map(|x| x.unwrap());
    for line in lines.by_ref()
        .take_while(|x| !x.is_empty()) {
        let captures = rule_regex.captures(&line).unwrap();
        rules.push(((captures["from_1"].parse::<u32>().unwrap(),
                     captures["to_1"]  .parse::<u32>().unwrap()),
                    (captures["from_2"].parse::<u32>().unwrap(),
                     captures["to_2"]  .parse::<u32>().unwrap())));
    }

    lines.next(); lines.next(); lines.next(); lines.next();

    for line in lines {
        error_rate += &line.split(',')
            .map(|x| x.parse::<u32>().unwrap())
            .filter(|&x| !rules.iter().any(
                    |&((from_1, to_1), (from_2, to_2))|
                    (from_1 <= x && x <= to_1) || (from_2 <= x && x <= to_2)))
            .sum::<u32>()
    }

    println!("{:?}", error_rate);
}

