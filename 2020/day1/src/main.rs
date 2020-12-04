use std::io::{stdin, Read};

fn main() {
    let mut buffer = String::new();
    stdin().read_to_string(& mut buffer);

    let mut numbers = buffer.lines().map(
        |s| s.parse::<u32>().unwrap()
    ).collect::<Vec<u32>>();

    numbers.sort();

    for a in numbers.iter() {
        for b in numbers.clone().iter() {
            for c in numbers.clone().iter() {

                let s = a + b + c;
                if s > 2020 {
                    break;
                }
                if s == 2020 {
                    println!("{} x {} x {} = {}", a, b, c, a*b*c);
                    break;
                }
            }
        }
    }
}
