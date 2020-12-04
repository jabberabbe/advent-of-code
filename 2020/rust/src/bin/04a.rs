// cargo-deps: regex

// This program requires preprocessing of the input.
// Stdin should contain one passport per line, and no
// empty lines between passports. This format can be
// obtained from the input.txt downloaded from the
// Advent of Code website with vim for example:
//     vim -c ':set textwidth=300' -c ':normal VGgq' -c ':v/./d' -c ':wq' input.txt

use std::io;
use std::io::prelude::*;
use std::iter::Iterator;
use regex::RegexSet;

fn main() {
    let fields = RegexSet::new(&[
        "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid",
    ]).unwrap();
    let mut count: u32 = 0;
    for line in io::stdin().lock().lines().map(|x| x.unwrap()) {
        let matches: Vec<_> = fields.matches(&line).into_iter().collect();
        if (0..7).all(|x| matches.contains(&x)) {
            count += 1
        }
    }
    println!("{}", count);
}

