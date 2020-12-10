use std::io;
use std::io::prelude::*;
use std::vec::Vec;
use std::iter::Iterator;
use std::cell::Cell;
use typed_arena::Arena;

struct Adapter<'a> {
    base_joltage: u16,
    accepted_adapters: Vec<&'a Adapter<'a>>,
    cached_paths: Cell<Option<u64>>, // The number of paths from joltage 0 to this adapter
}

fn main() {
    let mut input: Vec<_> = io::stdin().lock().lines()
             .map(|x| x.unwrap().parse::<u16>().unwrap()).collect();
    input.sort();
    input.push(input[input.len()-1] + 3);
    let arena = Arena::new();
    let device_joltage = build_graph(&input, &arena);
    calculate_paths(device_joltage);
    println!("{}", device_joltage.cached_paths.get().unwrap());
}

fn build_graph<'a>(input: &Vec<u16>, arena: &'a Arena<Adapter<'a>>) -> &'a Adapter<'a> {
    let zero_joltage = arena.alloc(Adapter {
        base_joltage: 0,
        accepted_adapters: Vec::new(),
        cached_paths: Cell::new(Some(1)) });

    // Initialize first nodes
    let adapter_1 = arena.alloc(insert_adapter(&[zero_joltage], input[0]));
    let adapter_2 = arena.alloc(insert_adapter(&[zero_joltage, adapter_1], input[1]));
    let adapter_3 = arena.alloc(insert_adapter(&[zero_joltage, adapter_1, adapter_2], input[2]));

    // This is the fold accumulator
    let mut last_adapters: [&Adapter<'a>; 3] = [adapter_1, adapter_2, adapter_3];
    for &adapter in &input[3..] {
        let new_adapter = arena.alloc(insert_adapter(&last_adapters[..], adapter));
        last_adapters[0] = last_adapters[1];
        last_adapters[1] = last_adapters[2];
        last_adapters[2] = new_adapter;
    }

    last_adapters[2]
}

fn insert_adapter<'a>(existing_adapters: &[&'a Adapter<'a>],
                      new_joltage: u16) -> Adapter<'a> {
    Adapter {
        base_joltage: new_joltage,
        accepted_adapters: existing_adapters.iter().filter_map(|&a| {
            if new_joltage - a.base_joltage <= 3 {
                Some(a)
            } else {
                None
            }
        }).collect(),
        cached_paths: Cell::new(None)
    }
}

fn calculate_paths(leaf: &Adapter) -> u64 {
    match leaf.cached_paths.get() {
        Some(x) => x,
        None => {
            let x = leaf.accepted_adapters.iter().map(|&x| calculate_paths(x)).sum();
            leaf.cached_paths.set(Some(x));
            x
        }
    }
}

