use std::io;
use std::io::prelude::*;
use std::str::FromStr;
use std::vec::Vec;

fn main() {
    let mut data: Vec<u32> = Vec::new();
    for line in io::stdin().lock().lines() {
        data.push(u32::from_str(line.unwrap().trim()).unwrap());
    }
    for (pos, &x1) in data.iter().enumerate() {
        if let Some(&x2) = &data[(pos+1)..].iter().find(|&x2| x1 + x2 == 2020) {
            println!("{}", x1*x2);
            break;
        }
    }
}
