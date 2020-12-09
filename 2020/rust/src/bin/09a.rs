use std::io;
use std::io::prelude::*;
use std::vec::Vec;
use std::iter::Iterator;

fn main() {
    let input: Vec<_> = io::stdin().lock().lines()
             .map(|x| x.unwrap().parse::<u64>().unwrap()).collect();
    println!("{}", input.windows(26).filter_map(|win| {
        let mut iter = win.iter();
        let &y = iter.next_back().unwrap();
        for (i, x1) in iter.enumerate() {
            for x2 in win[i+1..].iter() {
                if x1 + x2 == y {
                    return None
                }
            }
        }
        Some(y)
    }).next().unwrap());
}

