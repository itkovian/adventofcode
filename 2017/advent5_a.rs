use std::io;
use std::io::Read;


fn walk(digits: &mut Vec<i32>) -> u32 {

    let mut c : u32 = 0;
    let mut index = 0;
    let upper = digits.len() - 1;

    while let Some(inc) = digits.get_mut(index) {

        if *inc >= 0 {
            index = index + *inc as usize;
        }
        else {
            index = index - inc.abs() as usize;
        }

        *inc += 1;
        c += 1;
    }
    c
}


fn main() {

    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let mut digits : Vec<i32> = input.lines().map(|s| s.parse::<i32>().unwrap()).collect();

    let c = walk(&mut digits);

    println!("moves: {}", c);

}
