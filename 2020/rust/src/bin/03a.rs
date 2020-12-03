use std::io;
use std::io::prelude::*;
use std::iter::Iterator;

fn main() {
    let mut count: u32 = 0;
    for (i, line) in io::stdin().lock().lines().map(|x| x.unwrap()).enumerate() {
        if line.chars().cycle().nth(i*3).unwrap() == '#' {
            count += 1;
        }
    }
    println!("{}", count);
}
