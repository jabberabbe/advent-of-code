use std::io;
use std::io::prelude::*;
use std::collections::HashMap;
use std::iter::Iterator;

struct Number {
    // Index of the last position.
    last_seen: usize,
    // Distance from last position to the previous one;
    // None if this number occurred only once.
    previous_distance: Option<usize>
}

fn main() {
    let mut numbers = HashMap::new();
    let mut last_number = 0;
    let mut init_size = 0;
    for (i, n) in io::stdin().lock().lines().next().unwrap().unwrap()
        .split(',').map(|x| x.parse::<usize>().unwrap()).enumerate() {
        numbers.insert(n, Number { last_seen: i, previous_distance: None });
        last_number = n;
        init_size += 1;
    }

    for i in (0..).into_iter().map(|x| x + init_size)
        .take_while(|&x| x < 30000000) {
        last_number = yield_new(&mut numbers, last_number, i);
    }
    println!("{}", last_number);
}

fn yield_new(map: &mut HashMap<usize, Number>, last: usize, i: usize) -> usize {
    let n = map.get(&last).unwrap();
    let new = match n.previous_distance {
        None      => 0,
        Some(idx) => idx
    };

    if let Some(new_n) = map.get_mut(&new) {
        new_n.previous_distance = Some(i - new_n.last_seen);
        new_n.last_seen = i;
    } else {
        map.insert(new, Number { last_seen: i, previous_distance: None });
    }

    new
}

