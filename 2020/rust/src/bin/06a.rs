// This program requires preprocessing of the input.  Stdin should contain one
// group per line, and no empty lines between groups. People in the same group
// are separated by spaces. This format can be obtained from the input.txt
// downloaded from the Advent of Code website with vim:
//     vim -c ':set textwidth=300' -c ':normal VGgq' -c ':v/./d' -c ':wq' input.txt

use std::io;
use std::io::prelude::*;
use std::iter::Iterator;

fn main() {
    println!("{}", io::stdin().lock().lines()
             .map(|x| count_answers(x.unwrap().as_str())).sum::<u32>());
}

fn count_answers(input_str: &str) -> u32 {
    let mut answered = [false; 26];
    let mut accum: u32 = 0;
    // character 'a' is 97 in ASCII code
    for x in input_str.bytes().filter(|&x| x != b' ').map(|x| usize::from(x-97)) {
        if !answered[x] {
            accum += 1;
            answered[x] = true;
        }
    }
    return accum;
}

