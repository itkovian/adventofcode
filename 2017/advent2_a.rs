use std::io;
use std::io::Read;




fn main() {


    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let ls : Vec<Vec<u32>> = input.lines()
                                  .map(|l| l.split(" ")
                                           .map(|s| s.parse::<u32>().unwrap())
                                           .collect())
                                  .collect();

    let s = ls.iter().fold(0, |sum, ref xs| {
        sum + xs.iter().max().unwrap() - xs.iter().min().unwrap()
    });

    println!("checksum is {}", s);


}
