use std::io;
use std::io::prelude::*;
use std::iter::Iterator;

#[derive(Copy, Clone, Debug)]
struct FerryLocation {
    x: i32,
    y: i32,
    d: Direction
}

#[derive(Copy, Clone, Debug)]
struct Direction(bool, bool); // axe, versus

#[derive(Debug)]
enum Instruction {
    Move(Option<Direction>, i32), // None if forward
    Turn(bool, bool) // counterclockwise
}

fn main() {
    let last_location = io::stdin().lock().lines()
        .map(|x| parse_instruction(x.unwrap().as_str()))
        .fold(FerryLocation { x: 0, y: 0,
            d: Direction(false, false) }, follow_instruction);
    println!("{}", last_location.x.abs() + last_location.y.abs())
}

fn follow_instruction(loc: FerryLocation, i: Instruction) -> FerryLocation {
    match i {
        Instruction::Move(d, n) => match d.unwrap_or(loc.d).0 {
            false => FerryLocation {
                x: loc.x + n * (if d.unwrap_or(loc.d).1 { -1 } else { 1 }),
                y: loc.y, d: loc.d },
            true  => FerryLocation {
                y: loc.y + n * (if d.unwrap_or(loc.d).1 { -1 } else { 1 }),
                x: loc.x, d: loc.d }
        }
        Instruction::Turn(r1, r2) => FerryLocation {
            x: loc.x, y: loc.y,
            d: Direction(r2 ^ loc.d.0, r1 ^ (r2 & loc.d.0) ^ loc.d.1) }
    }
}

fn parse_instruction(s: &str) -> Instruction {
    let n = s[1..].parse::<i32>().unwrap();
    match s.chars().nth(0).unwrap() {
        'N' => Instruction::Move(Some(Direction(true,  false)), n),
        'S' => Instruction::Move(Some(Direction(true,  true)),  n),
        'E' => Instruction::Move(Some(Direction(false, false)), n),
        'W' => Instruction::Move(Some(Direction(false, true)),  n),
        'F' => Instruction::Move(None, n),

        'L' => Instruction::Turn(      (n/90)/2 != 0,       (n/90)%2 != 0),
        'R' => Instruction::Turn(((360-n)/90)/2 != 0, ((360-n)/90)%2 != 0),

        _ => unreachable!()
    }
}

