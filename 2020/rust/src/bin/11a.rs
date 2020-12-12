use std::io;
use std::io::prelude::*;
use std::iter::Iterator;

fn main() {
    let mut has_seat   = [0; 93*97];
    let mut occupied_1 = [0; 93*97];
    let mut occupied_2 = [0; 93*97];

    build_seat_map(&mut has_seat);

    let mut changed = true;
    loop {
        changed &= tick(&has_seat, &occupied_1, &mut occupied_2);
        changed &= tick(&has_seat, &occupied_2, &mut occupied_1);
        if !changed { break }
    }

    println!("{}", occupied_1.iter().map(|&x| x as u32).sum::<u32>());
}

fn build_seat_map(seat_map: &mut [u8]) {
    for (row, l) in io::stdin().lock().lines().map(|x| x.unwrap()).enumerate() {
        for (col, &c) in l.as_bytes().into_iter().enumerate() {
            match c {
                b'L' => seat_map[97*row + col] = 0xff,
                b'.' => seat_map[97*row + col] = 0x00,
                _    => unreachable!()
            }
        }
    }
}

// Returns true if something has changed, otherwise false
fn tick(seat_map: &[u8], occupied: &[u8], out: &mut [u8]) -> bool {
    let mut changed = false;

    for row in 0..93 {
        for col in 0..97 {
            if seat_map[row*97 + col] > 0 {
                let mut adjacent_occupied = 0;
                for i in (row as i32)-1..=(row as i32)+1 {
                    if i >= 0 && i < 93 {
                        for j in (col as i32)-1..=(col as i32)+1 {
                            if j >= 0 && j < 97 && (i != (row as i32) || j != (col as i32)) {
                                adjacent_occupied += occupied[(i*97 + j) as usize]
                            }
                        }
                    }
                }

                if occupied[row*97 + col] > 0 && adjacent_occupied >= 4 {
                    out[row*97 + col] = 0; changed = true
                } else if occupied[row*97 + col] == 0 && adjacent_occupied == 0 {
                    out[row*97 + col] = 1; changed = true
                } else {
                    out[row*97 + col] = occupied[row*97 + col]
                }
            }
        }
    }

    changed
}

