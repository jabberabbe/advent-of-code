// cargo-deps: regex
use std::io;
use std::io::prelude::*;
use std::str::FromStr;
use std::iter::Iterator;
use regex::Regex;

fn main() {
    let mut i: u32 = 0;
    let re = Regex::new(r"^(?P<min>\d+)-(?P<max>\d+) (?P<char>.): (?P<pwd>.+)$").unwrap();
    for line in io::stdin().lock().lines().map(|x| x.unwrap()) {
        // data.push(u32::from_str(line.unwrap().trim()).unwrap());
        let cap = re.captures(&line).unwrap();
        let pwd = &cap["pwd"];
        let test_char: char = cap["char"].chars().next().unwrap();
        let min = usize::from_str(&cap["min"]).unwrap();
        let max = usize::from_str(&cap["max"]).unwrap();
        let count = pwd.chars().filter(|&x| x == test_char).count();
        if min <= count && count <= max {
            i += 1;
        }
    }
    println!("{}", i);
}
