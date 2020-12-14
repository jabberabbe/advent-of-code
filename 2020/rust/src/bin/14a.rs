#[macro_use]
extern crate lazy_static;

use std::io;
use std::io::prelude::*;
use std::collections::HashMap;
use std::iter::Iterator;
use regex::Regex;

struct Status {
    bit_mask_0: u64,
    bit_mask_1: u64,
    memory: HashMap<u64, u64>
}

enum Instr {
    SetMask(u64, u64),
    SetMem(u64, u64)
}

fn main() {
    let sum = io::stdin().lock().lines().map(|x| get_instruction(&x.unwrap()))
        .fold(Status { bit_mask_1: 0,
            bit_mask_0: 0,
            memory: HashMap::new() }, exec_instr)
        .memory
        .values()
        .sum::<u64>();
    println!("{}", sum);
}

fn get_instruction(s: &str) -> Instr {
    lazy_static! {
        static ref MEM_RE: Regex = Regex::new(r"^mem\[(?P<addr>\d+)\] = (?P<val>\d+)$").unwrap();
    }

    if let Some(captures) = MEM_RE.captures(s) {
        Instr::SetMem(
            captures["addr"].parse::<u64>().unwrap(),
            captures["val"].parse::<u64>().unwrap())
    } else {
        let mut bit_mask_0 = 0;
        let mut bit_mask_1 = 0;
        for c in s[7..].chars() {
            bit_mask_0 *= 2;
            bit_mask_1 *= 2;
            match c {
                'X' => bit_mask_0 += 1,
                '1' => bit_mask_1 += 1,
                '0' => (),
                _   => unreachable!()
            }
        }
        Instr::SetMask(bit_mask_0, bit_mask_1)
    }
}

fn exec_instr(mut s: Status, i: Instr) -> Status {
    match i {
        Instr::SetMask(bit_mask_0, bit_mask_1) => {
            s.bit_mask_0 = bit_mask_0;
            s.bit_mask_1 = bit_mask_1;
        },
        Instr::SetMem(addr, val) => {
            s.memory.insert(addr, val & s.bit_mask_0 | s.bit_mask_1);
        }
    }
    s
}

