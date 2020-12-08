use std::io;
use std::io::prelude::*;
use std::vec::Vec;
use std::iter::Iterator;

#[derive(Copy, Clone)]
enum InstrType {
    Acc, Jmp, Nop
}

struct Instr {
    typ: InstrType,
    arg: i32
}

fn main() {
    let mut instructions = get_instructions();
    let acc = run_until_loop(&mut instructions, 0, 0);
    println!("{}", acc);
}

// Vec<(visited/not visited, Instruction)>
fn get_instructions() -> Vec<(bool, Instr)> {
    let mut vec = Vec::new();
    for line in io::stdin().lock().lines()
        .map(|x| x.unwrap()).filter(|x| !x.is_empty()) {
            let instr_type = match &line[0..3] {
                "nop" => InstrType::Nop,
                "acc" => InstrType::Acc,
                "jmp" => InstrType::Jmp,
                _ => unreachable!()
            };
            let arg = line[4..].parse().unwrap();
            vec.push((false, Instr { typ: instr_type, arg: arg }));
        }
    vec
}

fn run_until_loop(instructions: &mut Vec<(bool, Instr)>,
                  acc: i32, pos: usize) -> i32 {
    if !instructions[pos].0 {
        instructions[pos].0 = true;
        match instructions[pos].1.typ {
            InstrType::Acc => run_until_loop(instructions,
                                             acc + instructions[pos].1.arg, pos + 1),
            InstrType::Jmp => run_until_loop(instructions, acc,
                                             ((pos as i32) + instructions[pos].1.arg) as usize),
            InstrType::Nop => run_until_loop(instructions, acc, pos + 1)
        }
    } else {
        acc
    }
}

