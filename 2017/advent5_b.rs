use std::io;
use std::io::Read;


fn walk(digits: &mut Vec<i32>) -> u32 {

    let mut c : u32 = 0;
    let mut index = 0;

    while let Some(i) = digits.get_mut(index) {
        if *i>= 0 {
            index = index + *i as usize;
        }
        else {
            index = index - i.abs() as usize;
        }

        if *i >= 3 {
            *i -= 1;
        } else {
            *i += 1;
        }
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
