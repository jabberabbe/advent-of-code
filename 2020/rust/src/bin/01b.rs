use std::io;
use std::io::prelude::*;
use std::str::FromStr;
use std::vec::Vec;

fn main() {
    let mut data: Vec<u32> = Vec::new();
    for line in io::stdin().lock().lines() {
        data.push(u32::from_str(line.unwrap().trim()).unwrap());
    }
    for (pos1, &x1) in data.iter().enumerate() {
        for (pos2, &x2) in (&data[(pos1+1)..]).iter().enumerate() {
            if let Some(&x3) = (&data[(pos1+pos2+1)..]).iter().find(|&x3| x1 + x2 + x3 == 2020) {
               println!("{}", x1*x2*x3);
               break;
            }
        }
    }
}
