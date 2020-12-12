use std::io;
use std::io::prelude::*;
use std::iter::Iterator;

#[derive(Copy, Clone, Debug)]
struct FerryLocation {
    x: i32,
    y: i32,
    w_x: i32, // relative to the ship's position
    w_y: i32,
}

#[derive(Copy, Clone, Debug)]
struct Direction(bool, bool); // axe, versus

#[derive(Debug)]
enum Instruction {
    Forward(i32),
    MoveWaypoint(Direction, i32),
    Rotate(bool, bool) // counterclockwise
}

fn main() {
    let last_location = io::stdin().lock().lines()
        .map(|x| parse_instruction(x.unwrap().as_str()))
        .fold(FerryLocation { x: 0, y: 0, w_x: 10, w_y: 1 }, follow_instruction);
    println!("{}", last_location.x.abs() + last_location.y.abs())
}

fn follow_instruction(loc: FerryLocation, i: Instruction) -> FerryLocation {
    match i {
        Instruction::MoveWaypoint(d, n) => match d.0 {
            false => FerryLocation {
                w_x: loc.w_x + n * (if d.1 { -1 } else { 1 }), w_y: loc.w_y,
                x: loc.x, y: loc.y },
            true  => FerryLocation {
                w_x: loc.w_x, w_y: loc.w_y + n * (if d.1 { -1 } else { 1 }),
                x: loc.x, y: loc.y }
        }
        Instruction::Forward(n)     => FerryLocation {
            x: loc.x + n * loc.w_x, y: loc.y + n * loc.w_y,
            w_x: loc.w_x, w_y: loc.w_y },
        Instruction::Rotate(r1, r2) => FerryLocation {
            w_x: (if r2 { loc.w_y } else { loc.w_x }) * (if r1 ^ r2 { -1 } else { 1 }),
            w_y: (if r2 { loc.w_x } else { loc.w_y }) * (if r1      { -1 } else { 1 }),
            x: loc.x, y: loc.y }
    }
}

fn parse_instruction(s: &str) -> Instruction {
    let n = s[1..].parse::<i32>().unwrap();
    match s.chars().nth(0).unwrap() {
        'N' => Instruction::MoveWaypoint(Direction(true,  false), n),
        'S' => Instruction::MoveWaypoint(Direction(true,  true),  n),
        'E' => Instruction::MoveWaypoint(Direction(false, false), n),
        'W' => Instruction::MoveWaypoint(Direction(false, true),  n),
        'F' => Instruction::Forward(n),

        'L' => Instruction::Rotate(      (n/90)/2 != 0,       (n/90)%2 != 0),
        'R' => Instruction::Rotate(((360-n)/90)/2 != 0, ((360-n)/90)%2 != 0),

        _ => unreachable!()
    }
}

