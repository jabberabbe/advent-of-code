// cargo-deps: typed-arena

// This program requires preprocessing of the input.
// Stdin should contain rules separated by blank lines,
// and each rule should have the following format:
//
//     muted tomato
//     1 bright brown
//     1 dotted gold
//     2 faded gray
//     1 posh yellow
//     [blank line]
//     [new rule]
//     ...
//
// This format can be obtained from the input.txt
// downloaded from the Advent of Code website with
// vim for example:
//
//     vim -e input.txt <<EOF
//     :%s/bags//g
//     :%s/bag//g
//     :%s/\./\r/g
//     :%s/, /\r/g
//     :%s/contain/\r/g
//     :%s/^\s\+//g
//     :%s/\s\+$//g
//     :wq
//     EOF
//

use std::io;
use std::io::prelude::*;
use std::iter::Iterator;
use std::collections::HashMap;
use std::collections::BTreeSet;
use std::vec::Vec;
use std::cell::RefCell;
use typed_arena::Arena;

struct Bag<'a> {
    // The pointer to this vector (held by the RefCell) also serves
    // as an index of the node
    contained_in: RefCell<Vec<&'a Bag<'a>>>,
}

fn main() {
    let bag_arena = Arena::new();
    let mut bag_colors = HashMap::new();
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).ok();
    for bag_rule in buffer.split("\n\n").filter(|s| !s.is_empty()) {
        let mut iter = bag_rule.lines();
        let parent_bag = get_or_create_bag(iter.next().unwrap(), &mut bag_colors, &bag_arena);
        for color in iter {
            if color != "no other" {
                let contained_bag = get_or_create_bag(&color[2..], &mut bag_colors, &bag_arena);
                contained_bag.contained_in.borrow_mut().push(parent_bag);
            }
        }
    }

    let bag = bag_colors.get("shiny gold").unwrap();
    let mut visited = BTreeSet::new();
    println!("{}", count_ancestors(bag, &mut visited));
}

fn get_or_create_bag<'a>(color: &'a str, map: &mut HashMap<&'a str, &'a Bag<'a>>,
                     arena: &'a Arena<Bag<'a>>) -> &'a Bag<'a> {
    match map.get(color) {
        Some(&bag_ref) => bag_ref,
        None => {
            let bag_ref = arena.alloc(Bag { contained_in: RefCell::new(Vec::new()) });
            map.insert(color, bag_ref);
            return bag_ref;
        }
    }
}

fn count_ancestors<'a>(node: &Bag<'a>, visited: &mut BTreeSet<*mut Vec<&'a Bag<'a>>>) -> usize {
    for parent in node.contained_in.borrow().iter() {
        if !visited.contains(&(parent.contained_in.as_ptr())) {
            visited.insert(parent.contained_in.as_ptr());
            count_ancestors(parent, visited);
        }
    }
    return visited.len();
}

