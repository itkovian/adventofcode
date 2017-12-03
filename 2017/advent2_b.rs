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

    let s = ls.iter().fold(0, |sum, xs| {
        let mut ys : Vec<u32> = xs.to_vec();
        ys.sort();

        let mut zs : Vec<u32> = ys.to_vec();
        zs.reverse();

        let mut inc = 0;
        for y in ys.iter() {

            for z in zs.iter() {

                if y == z {
                    continue
                }

                if y % z == 0 {
                    inc = y / z;
                    println!("Found {} and {} yielding {}", y, z, inc);
                    break;
                }
            }
        }

        sum + inc
    });

    println!("checksum is {}", s);


}
