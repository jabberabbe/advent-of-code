use std::io;
use std::io::prelude::*;
use std::iter::Iterator;

fn main() {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();
    let _ = lines.next().unwrap().unwrap().parse::<u32>().unwrap();
    let available_ids: Vec<_> = lines.next().unwrap().unwrap().split(',')
        .enumerate().filter_map(|(i, x)|
            if let Some(parsed) = x.parse::<u64>().ok() {
                Some(((i as u64), parsed))
            } else {
                None
            }).collect();


    let mut jump = 1;
    let mut synchronized_ts = 0;
    for (rem, modulus) in available_ids {
        while (synchronized_ts + rem) % modulus != 0 {
            synchronized_ts += jump
        }
        jump *= modulus
    }

    println!("{}", synchronized_ts);
}

