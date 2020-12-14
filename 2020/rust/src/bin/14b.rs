#[macro_use]
extern crate lazy_static;

use std::io;
use std::io::prelude::*;
use std::vec::Vec;
use std::collections::HashMap;
use std::iter::Iterator;
use regex::Regex;

struct Status {
    floating_bits: Vec<u8>,
    bit_mask_1: u64,
    memory: HashMap<u64, u64>
}

enum Instr {
    SetMask(Vec<u8>, u64),
    SetMem(u64, u64)
}

fn main() {
    let sum = io::stdin().lock().lines().map(|x| get_instruction(&x.unwrap()))
        .fold(Status { bit_mask_1: 0,
            floating_bits: Vec::new(),
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
        let mut bit_mask_1 = 0;
        let mut floating_bits = Vec::new();
        for (i, c) in s[7..].chars().enumerate() {
            bit_mask_1 *= 2;
            match c {
                'X' => floating_bits.push(35 - i as u8),
                '1' => bit_mask_1 += 1,
                '0' => (),
                _   => unreachable!()
            }
        }
        Instr::SetMask(floating_bits, bit_mask_1)
    }
}

fn exec_instr(mut s: Status, i: Instr) -> Status {
    match i {
        Instr::SetMask(floating_bits, bit_mask_1) => {
            s.floating_bits = floating_bits;
            s.bit_mask_1 = bit_mask_1;
        },
        Instr::SetMem(addr, val) => {
            for floating_choose in 0..(2u16.pow(s.floating_bits.len() as u32)) {
                let mut float_0_mask = 0;
                let mut float_1_mask = 0;
                for (i, &bit_index) in s.floating_bits.iter().enumerate() {
                    if floating_choose & (1 << i) > 0 {
                        float_1_mask |= 1 << bit_index;
                    } else {
                        float_0_mask |= 1 << bit_index;
                    }
                }
                s.memory.insert((addr | s.bit_mask_1 | float_1_mask) & (!float_0_mask), val);
            }
        }
    }

    s
}

