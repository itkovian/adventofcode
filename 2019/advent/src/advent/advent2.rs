/*
Copyright 2019 Andy Georges <itkovian+adventofcode@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;


fn compute(instr: &mut Vec<usize>, pos: usize) -> Vec<usize> {

    if instr[pos] == 99 {
        return instr.to_vec();
    }
    match instr[pos .. pos+4] {
        [opcode, op1_pos, op2_pos, res_pos] => {
            let op1 = instr[op1_pos];
            let op2 = instr[op2_pos];
            let res = match opcode {
                1 => op1 + op2,
                2 => op1 * op2,
                _ => panic!("Unexpected opcode {}", opcode)
            };
            instr[res_pos] = res;
            return compute(instr, pos + 4)
        }
        _ => panic!("Computer says no!")
    };

    //instr.to_vec()
}

pub fn advent2a(p: &mut BufReader<File>) -> () {

    let mut program = String::new();
    p.read_line(& mut program).expect("Cannot read input line");
    let mut instr = program
        .split(',')
        .map(|s| s.parse::<usize>().unwrap())
        .collect::<Vec<usize>>();

    instr[1] = 12;
    instr[2] = 2;

    let instr_ = compute(& mut instr, 0);

    println!("Position 0 after halt: {}", instr_[0]);
}


pub fn  advent2b(p: &mut BufReader<File>) {

    let mut program = String::new();
    p.read_line(&mut program).expect("Cannot read input line");
    let instr = program
        .split(',')
        .map(|s| s.parse::<usize>().unwrap())
        .collect::<Vec<usize>>();

    for noun in 0..100 {
        for verb  in 0..100 {
            let mut instr_ = instr.clone();
            instr_[1] = noun;
            instr_[2] = verb;
            if compute(& mut instr_, 0)[0] == 19690720 {
                println!("The input should be {}", 100 * noun + verb);
                return;
            }
        }
    }

}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_computer() {
        assert_eq!(compute(& mut vec![1,0,0,0,99], 0), vec![2,0,0,0,99]);
        assert_eq!(compute(& mut vec![2,3,0,3,99], 0), vec![2,3,0,6,99]);
        assert_eq!(compute(& mut vec![2,4,4,5,99,0], 0), vec![2,4,4,5,99,9801]);
        assert_eq!(compute(& mut vec![1,1,1,4,99,5,6,0,99], 0), vec![30,1,1,4,2,5,6,0,99]);
    }

}