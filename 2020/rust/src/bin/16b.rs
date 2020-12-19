// cargo-deps: regex

use std::io;
use std::io::prelude::*;
use std::vec::Vec;
use std::collections::BTreeSet;
use std::iter::Iterator;
use regex::Regex;

fn main() {
    let rule_regex = Regex::new(
        r"(?P<from_1>\d+)-(?P<to_1>\d+) or (?P<from_2>\d+)-(?P<to_2>\d+)")
        .unwrap();
    let mut rules = Vec::new();
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines().map(|x| x.unwrap());
    for line in lines.by_ref()
        .take_while(|x| !x.is_empty()) {
        let captures = rule_regex.captures(&line).unwrap();
        rules.push(((captures["from_1"].parse::<u64>().unwrap(),
                     captures["to_1"]  .parse::<u64>().unwrap()),
                    (captures["from_2"].parse::<u64>().unwrap(),
                     captures["to_2"]  .parse::<u64>().unwrap())));
    }

    let mut field_positions = Vec::new();
    for _ in rules.iter() {
        field_positions.push((0..rules.len()).collect::<BTreeSet<_>>());
    }

    lines.next();
    let my_ticket = lines.next().unwrap().as_str().split(',')
        .map(|y| y.parse::<u64>().unwrap()).collect::<Vec<_>>();
    lines.next(); lines.next();

    for ticket in lines.map(|x| x.as_str().split(',')
                            .map(|y| y.parse::<u64>().unwrap()).collect::<Vec<_>>())
                       .filter(|line|
                               line.iter().all(|&x| rules.iter().any(
                                               |&((from_1, to_1), (from_2, to_2))|
                                               (from_1 <= x && x <= to_1) ||
                                               (from_2 <= x && x <= to_2)))) {
        for (field_pos, &x) in ticket.iter().enumerate() {
            for (invalid_field_id, _) in rules.iter().enumerate().filter(
                |&(_, &((from_1, to_1), (from_2, to_2)))|
                !((from_1 <= x && x <= to_1) || (from_2 <= x && x <= to_2))) {
                field_positions[invalid_field_id].remove(&field_pos);
            }
        }
    }


    let mut changed = true;
    while changed {
        changed = false;
        for field_id in 0..field_positions.len() {
            if field_positions[field_id].len() == 1 {
                let pos_idx = field_positions[field_id].iter().next().unwrap().clone();
                for i in 0..field_positions.len() {
                    if field_id != i && field_positions[i].remove(&pos_idx) {
                        changed = true;
                    }
                }
            }
        }
    }

    println!("{}", field_positions.into_iter().take(6)
                   .map(|v| my_ticket[v.into_iter().next().unwrap()])
                   .product::<u64>());
}

