use std::io;
use std::io::prelude::*;
use std::iter::Iterator;

fn main() {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();
    let start_ts = lines.next().unwrap().unwrap().parse::<u32>().unwrap();
    let available_ids: Vec<_> = lines.next().unwrap().unwrap().split(',')
        .filter_map(|x| x.parse::<u32>().ok()).collect();

    let (first_departure, bus_id) = (start_ts..).into_iter().filter_map(|ts| {
        available_ids.iter().filter_map(|id| {
            if ts % id == 0 {
                Some((ts, id))
            } else {
                None
            }
        }).next()
    }).next().unwrap();

    println!("{}", (first_departure - start_ts) * bus_id);
}

