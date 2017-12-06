use std::collections::HashMap;
use std::io;


fn find_max(banks: &Vec<u32>)  -> (usize, u32) {

    let mut i = 0;
    let mut m = banks[i];

    for (i_, &v) in banks.iter().enumerate() {

        if v > m {
            i = i_;
            m = v;
        }
    }

    (i, m)
}


fn redistribute(banks: &mut Vec<u32>) -> (u32, u32) {

    let mut c = 0;
    let mut indexes: HashMap<Vec<u32>, u32> = HashMap::new();
    let l = banks.len() as u32;
    let mut cycle_length = 0;

    indexes.insert(banks.clone(), c);

    loop {

        let (mut i, mut m) = find_max(&banks);
        let (split, rem) = if m > l { (m / l, m % l) } else { (1, 0) };

        banks[i] = 0;
        while m > rem {
            i += 1;
            if i as u32 >= l { i = 0; }
            banks[i] += split;
            m -= split
        }
        i += 1;
        if i as u32 >= l { i = 0; }
        banks[i] += rem;
        c += 1;

        match indexes.get(&banks.clone()) {
            Some(cycle_start) => {
                cycle_length = c - cycle_start;
                break;
            },
            None => ()
        };
        indexes.insert(banks.clone(), c);
    }

    (c, cycle_length)
}


fn main() {

    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();

    let mut banks : Vec<u32> = input.trim_right().split(" ").map(|d| d.parse::<u32>().unwrap()).collect();

    let (l, c) = redistribute(&mut banks);

    println!("{} {}", l, c);
}
