#![feature(slice_rotate)]

use std::io;


fn summarise(digits: Vec<u32>, paired: Vec<u32>) -> u32 {

    digits.iter().zip(paired).fold(0, |sum, (&x, y)| if x == y { sum + x } else { sum })
}

fn main() {

    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();  // should be no biggie :)

    let digits : Vec<u32> = input.chars().map(|c| c.to_digit(10).unwrap()).collect();
    let split = digits.len() / 2;

    let mut paired : Vec<u32> = digits.clone();
    paired.rotate(split);

    let s = summarise(digits, paired);

    println!("sum is {}", s);
}
