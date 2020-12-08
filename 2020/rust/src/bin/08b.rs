use std::io;
use std::io::prelude::*;
use std::vec::Vec;
use std::iter::Iterator;

#[derive(Copy, Clone)]
enum InstrType {
    Acc, Jmp, Nop, End
}

struct Instr {
    typ: InstrType,
    arg: i32
}

fn main() {
    let mut instructions = get_instructions();
    println!("{}", (0..instructions.len()).filter_map(|i| {
        let old_typ = instructions[i].typ;
        instructions[i].typ = match old_typ {
            InstrType::Jmp => InstrType::Nop,
            InstrType::Nop => InstrType::Jmp,
            _ => return None
        };
        let acc = run_to_end(&instructions, 0, 0, &mut vec![false; instructions.len()]);
        instructions[i].typ = old_typ;
        acc
    }).next().unwrap());
}

// Vec<(visited/not visited, Instruction)>
fn get_instructions() -> Vec<Instr> {
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
            vec.push(Instr { typ: instr_type, arg: arg });
        }
    vec.push(Instr { typ: InstrType::End, arg: 0 });
    vec
}

fn run_to_end(instructions: &Vec<Instr>,
              acc: i32, pos: usize, visited: &mut Vec<bool>) -> Option<i32> {
    if !visited[pos] {
        visited[pos] = true;
        match instructions[pos].typ {
            InstrType::Acc => run_to_end(instructions,
                                         acc + instructions[pos].arg, pos + 1, visited),
            InstrType::Jmp => run_to_end(instructions, acc,
                                         ((pos as i32) + instructions[pos].arg) as usize,
                                         visited),
            InstrType::Nop => run_to_end(instructions, acc, pos + 1, visited),
            InstrType::End => Some(acc)
        }
    } else {
        None
    }
}

