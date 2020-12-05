use std::io;
use std::io::prelude::*;
use std::iter::Iterator;

fn main() {
    println!("{}", io::stdin().lock().lines().map(|x| seat_id(x.unwrap().as_str())).max().unwrap());
}

fn seat_id(input_str: &str) -> u16 {
    let mut accum: u16 = 0;
    for x in input_str.chars() {
        accum *= 2;
        if x == 'B' || x == 'R' {
            accum += 1;
        }
    }
    return accum;
}

