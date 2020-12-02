// cargo-deps: regex
use std::io;
use std::io::prelude::*;
use std::str::FromStr;
use std::iter::Iterator;
use regex::Regex;

fn main() {
    let mut i: u32 = 0;
    let re = Regex::new(r"^(?P<pos1>\d+)-(?P<pos2>\d+) (?P<char>.): (?P<pwd>.+)$").unwrap();
    for line in io::stdin().lock().lines().map(|x| x.unwrap()) {
        // data.push(u32::from_str(line.unwrap().trim()).unwrap());
        let cap = re.captures(&line).unwrap();
        let pwd = &cap["pwd"];
        let test_char: char = cap["char"].chars().next().unwrap();
        let pos1 = usize::from_str(&cap["pos1"]).unwrap();
        let pos2 = usize::from_str(&cap["pos2"]).unwrap();
        if  (pwd.chars().nth(pos1-1).unwrap() == test_char && pwd.chars().nth(pos2-1).unwrap() != test_char) ||
            (pwd.chars().nth(pos1-1).unwrap() != test_char && pwd.chars().nth(pos2-1).unwrap() == test_char) {
            i += 1;
        }
    }
    println!("{}", i);
}
