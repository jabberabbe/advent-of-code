use std::io;
use std::io::prelude::*;
use std::iter::Iterator;
use std::collections::BTreeSet;

fn main() {
    let seats: BTreeSet<_> = io::stdin().lock().lines()
        .map(|x| seat_id(x.unwrap().as_str())).collect();
    let mut iter = seats.iter();
    for i in iter.next().unwrap().clone()..iter.next_back().unwrap().clone() {
        if !seats.contains(&i) {
            println!("{}", i);
        }
    }
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

