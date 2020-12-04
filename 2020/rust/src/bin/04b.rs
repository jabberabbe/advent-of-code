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
        r"byr:(19[2-9][0-9]|200[0-2])(\s+|$)",
        r"iyr:(201[0-9]|2020)(\s+|$)",
        r"eyr:(202[0-9]|2030)(\s+|$)",
        r"hgt:((1[5-8][0-9]|19[0-3])cm|(59|6[0-9]|7[0-6])in)(\s+|$)",
        r"hcl:#[0-9a-f]{6}(\s+|$)",
        r"ecl:(amb|blu|brn|gry|grn|hzl|oth)(\s+|$)",
        r"pid:\d{9}(\s+|$)",
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

