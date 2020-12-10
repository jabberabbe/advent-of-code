use std::io;
use std::io::prelude::*;
use std::vec::Vec;
use std::iter::Iterator;

fn main() {
    let mut input: Vec<_> = io::stdin().lock().lines()
             .map(|x| x.unwrap().parse::<u32>().unwrap()).collect();
    input.sort();
    input.push(input[input.len()-1] + 3);
    let (mut jolts1, mut jolts3) = input.windows(2).fold((0, 0), |(jolts1, jolts3), x| {
        if x[1] - x[0] == 1 {
            (jolts1 + 1, jolts3)
        } else if x[1] - x[0] == 3 {
            (jolts1, jolts3 + 1)
        } else {
            (jolts1, jolts3)
        }
    });
    if input[0] == 1 {
        jolts1 += 1;
    } else if input[0] == 3 {
        jolts3 += 1;
    }
    println!("{}", jolts1 * jolts3);
}

