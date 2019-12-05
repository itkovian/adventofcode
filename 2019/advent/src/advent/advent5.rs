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

use log::debug;

use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use std::sync::mpsc;
use std::sync::mpsc::{Receiver, Sender};

fn digits(v: i32) -> Vec<i32> {
    let mut v_ = v;
    let mut ds = Vec::new();
    for _ in 0..6 {
        let d = v_ % 10;
        ds.push(d);
        v_ = v_ / 10;
    }
    ds
}

fn fetch_argument(instr: &Vec<i32>, arg: i32, mode: i32) -> i32 {
    match mode {
        // address mode
        0 => instr[arg as usize],
        // immediate mode
        1 => arg,
        _ => panic!("Invalid parameter mode"),
    }
}

fn compute(instr: &mut Vec<i32>, pos: usize, p: &mut BufReader<&[u8]>, output: &Sender<i32>) -> () {
    if instr[pos] == 99 {
        return;
    }

    debug!("Instructions: {:?}", instr);

    // opcode has argument modes in front of the actual opcode meaning
    // last two digits are the instruction
    let mut modes = digits((instr[pos] / 100) as i32); // already in the perfect order :p
    modes.push(0);
    modes.push(0);
    modes.push(0);

    let mut newpos = 0;
    debug!("At pos {}: {}", pos, instr[pos]);
    match instr[pos] % 100 {
        // addition
        1 => {
            // 3 arguments
            debug!(
                "Instruction at {}: {} {} {} {}",
                pos,
                instr[pos],
                instr[pos + 1],
                instr[pos + 2],
                instr[pos + 3]
            );
            let op1 = fetch_argument(instr, instr[pos + 1], modes[0]);
            let op2 = fetch_argument(instr, instr[pos + 2], modes[1]);
            assert_eq!(modes[2], 0);
            let res_address = instr[pos + 3];
            instr[res_address as usize] = op1 + op2;
            newpos = pos + 4;
        }
        // multiplication
        2 => {
            // 3 arguments
            debug!(
                "Instruction at {}: {} {} {} {}",
                pos,
                instr[pos],
                instr[pos + 1],
                instr[pos + 2],
                instr[pos + 3]
            );
            let op1 = fetch_argument(instr, instr[pos + 1], modes[0]);
            let op2 = fetch_argument(instr, instr[pos + 2], modes[1]);
            assert_eq!(modes[2], 0);
            let res_address = instr[pos + 3];
            instr[res_address as usize] = op1 * op2;
            newpos = pos + 4;
        }
        // input
        3 => {
            // 1 argument
            let mut input = String::new();
            p.read_line(&mut input).expect("Cannot read input");
            let res_address = instr[pos + 1] as usize;
            instr[res_address] = input.trim().parse::<i32>().unwrap();
            debug!("Stored {} at pos {}", input.trim(), res_address);
            newpos = pos + 2;
        }
        // output
        4 => {
            // 1 argument
            let op = fetch_argument(instr, instr[pos + 1], modes[0]);
            debug!("Output of instruction at address {}: {}", pos, op);
            output.send(op).expect("Cannot send output");
            newpos = pos + 2;
        }
        // jnz
        5 => {
            let op1 = fetch_argument(instr, instr[pos + 1], modes[0]);
            newpos = match op1 {
                0 => pos + 3,
                _ => fetch_argument(instr, instr[pos + 2], modes[1]) as usize,
            };
            debug!("JNZ op1 {} to pos {}", op1, newpos);
        }
        // jz
        6 => {
            newpos = match fetch_argument(instr, instr[pos + 1], modes[0]) {
                0 => fetch_argument(instr, instr[pos + 2], modes[1]) as usize,
                _ => pos + 3,
            };
            debug!("JZ to pos {}", newpos);
        }
        // lt
        7 => {
            debug!(
                "Instruction at {}: {} {} {} {}",
                pos,
                instr[pos],
                instr[pos + 1],
                instr[pos + 2],
                instr[pos + 3]
            );
            let op1 = fetch_argument(instr, instr[pos + 1], modes[0]);
            let op2 = fetch_argument(instr, instr[pos + 2], modes[1]);
            debug!("Arguments: op1: {}, op2: {}", op1, op2);
            assert_eq!(modes[2], 0);
            let res_address = instr[pos + 3] as usize;
            instr[res_address] = if op1 < op2 { 1 } else { 0 };
            debug!("Stored {} at {}", (op1 < op2), res_address);
            newpos = pos + 4;
        }
        // eq
        8 => {
            debug!(
                "Instruction at {}: {} {} {} {}",
                pos,
                instr[pos],
                instr[pos + 1],
                instr[pos + 2],
                instr[pos + 3]
            );
            let op1 = fetch_argument(instr, instr[pos + 1], modes[0]);
            let op2 = fetch_argument(instr, instr[pos + 2], modes[1]);
            debug!("Arguments: op1: {}, op2: {}", op1, op2);
            assert_eq!(modes[2], 0);
            let res_address = instr[pos + 3] as usize;
            instr[res_address] = if op1 == op2 { 1 } else { 0 };
            debug!("Stored {} at {}", (op1 == op2), res_address);
            newpos = pos + 4;
        }
        _ => panic!("Unknown instruction"),
    };
    compute(instr, newpos, p, output);
}

fn read(p: &mut BufReader<File>) -> Vec<i32> {
    let mut program = String::new();
    p.read_line(&mut program).expect("Cannot read input line");
    program
        .split(',')
        .map(|s| s.parse::<i32>().unwrap())
        .collect::<Vec<i32>>()
}

pub fn advent5a(p: &mut BufReader<File>) -> () {
    let (tx, rx): (Sender<i32>, Receiver<i32>) = mpsc::channel();
    let mut instr = read(p);
    let data = String::from("1\n");
    let mut input = BufReader::new(data.as_bytes());
    compute(&mut instr, 0, &mut input, &tx);
    loop {
        match rx.recv() {
            Ok(v) => println!("Output: {}", v),
            _ => break,
        }
    }
}

pub fn advent5b(p: &mut BufReader<File>) {
    let (tx, rx): (Sender<i32>, Receiver<i32>) = mpsc::channel();
    let mut instr = read(p);
    let data = String::from("5\n");
    let mut input = BufReader::new(data.as_bytes());
    compute(&mut instr, 0, &mut input, &tx);
    println!("Got output: {}", rx.recv().unwrap());
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_digits() {
        assert_eq!(digits(123456), vec![6, 5, 4, 3, 2, 1]);
    }

    #[test]
    fn test_fetch_argument() {
        let instr: Vec<i32> = vec![5, 4, 3, 2, 1, 0];
        assert_eq!(fetch_argument(&instr, 4, 1), 4);
        assert_eq!(fetch_argument(&instr, 4, 0), 1);
    }

    #[test]
    fn test_computer() {
        let (tx, _rx): (Sender<i32>, Receiver<i32>) = mpsc::channel();
        let data = String::from("5\n");
        let mut input = BufReader::new(data.as_bytes());
        let mut instr = vec![1, 0, 0, 0, 99];
        compute(&mut instr, 0, &mut input, &tx);
        assert_eq!(instr, vec![2, 0, 0, 0, 99]);
        let mut instr = vec![2, 3, 0, 3, 99];
        compute(&mut instr, 0, &mut input, &tx);
        assert_eq!(instr, vec![2, 3, 0, 6, 99]);
        let mut instr = vec![2, 4, 4, 5, 99, 0];
        compute(&mut instr, 0, &mut input, &tx);
        assert_eq!(instr, vec![2, 4, 4, 5, 99, 9801]);
        let mut instr = vec![1, 1, 1, 4, 99, 5, 6, 0, 99];
        compute(&mut instr, 0, &mut input, &tx);
        assert_eq!(instr, vec![30, 1, 1, 4, 2, 5, 6, 0, 99]);
    }

    #[test]
    fn test_addressing_mode() {
        let (tx, _rx): (Sender<i32>, Receiver<i32>) = mpsc::channel();
        let data = String::from("5\n");
        let mut input = BufReader::new(data.as_bytes());
        let mut instr = vec![1002, 4, 3, 4, 33];
        compute(&mut instr, 0, &mut input, &tx);
        assert_eq!(instr, vec![1002, 4, 3, 4, 99]);
    }

    #[test]
    fn test_equal_position_mode() {
        let (tx, rx): (Sender<i32>, Receiver<i32>) = mpsc::channel();
        let mut instr = vec![3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8];
        let data = String::from("5\n");
        let mut input = BufReader::new(data.as_bytes());
        compute(&mut instr, 0, &mut input, &tx);
        let res = rx.recv().unwrap();
        assert_eq!(res, 0);

        let mut instr = vec![3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8];
        let data = String::from("8\n");
        let mut input = BufReader::new(data.as_bytes());
        compute(&mut instr, 0, &mut input, &tx);
        let res = rx.recv().unwrap();
        assert_eq!(res, 1);
    }

    #[test]
    fn test_equal_immediate_mode() {
        let (tx, rx): (Sender<i32>, Receiver<i32>) = mpsc::channel();
        let mut instr = vec![3, 3, 1108, -1, 8, 3, 4, 3, 99];
        let data = String::from("7\n");
        let mut input = BufReader::new(data.as_bytes());
        compute(&mut instr, 0, &mut input, &tx);
        let res = rx.recv().unwrap();
        assert_eq!(res, 0);

        let mut instr = vec![3, 3, 1108, -1, 8, 3, 4, 3, 99];
        let data = String::from("8\n");
        let mut input = BufReader::new(data.as_bytes());
        compute(&mut instr, 0, &mut input, &tx);
        let res = rx.recv().unwrap();
        assert_eq!(res, 1);
    }

    #[test]
    fn test_lessthan_position_mode() {
        let (tx, rx): (Sender<i32>, Receiver<i32>) = mpsc::channel();
        let mut instr = vec![3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8];
        let data = String::from("7\n");
        let mut input = BufReader::new(data.as_bytes());
        compute(&mut instr, 0, &mut input, &tx);
        let res = rx.recv().unwrap();
        assert_eq!(res, 1);

        let mut instr = vec![3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8];
        let data = String::from("8\n");
        let mut input = BufReader::new(data.as_bytes());
        compute(&mut instr, 0, &mut input, &tx);
        let res = rx.recv().unwrap();
        assert_eq!(res, 0);
    }

    #[test]
    fn test_lessthan_immediate_mode() {
        let (tx, rx): (Sender<i32>, Receiver<i32>) = mpsc::channel();
        let mut instr = vec![3, 3, 1107, -1, 8, 3, 4, 3, 99];
        let data = String::from("7\n");
        let mut input = BufReader::new(data.as_bytes());
        compute(&mut instr, 0, &mut input, &tx);
        let res = rx.recv().unwrap();
        assert_eq!(res, 1);

        let mut instr = vec![3, 3, 1107, -1, 8, 3, 4, 3, 99];
        let data = String::from("8\n");
        let mut input = BufReader::new(data.as_bytes());
        compute(&mut instr, 0, &mut input, &tx);
        let res = rx.recv().unwrap();
        assert_eq!(res, 0);
    }

    #[test]
    fn test_jz_position_mode() {
        let (tx, rx): (Sender<i32>, Receiver<i32>) = mpsc::channel();
        let mut instr = vec![3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9];
        let data = String::from("3\n");
        let mut input = BufReader::new(data.as_bytes());
        compute(&mut instr, 0, &mut input, &tx);
        let res = rx.recv().unwrap();
        assert_eq!(res, 1);

        let mut instr = vec![3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9];
        let data = String::from("0\n");
        let mut input = BufReader::new(data.as_bytes());
        compute(&mut instr, 0, &mut input, &tx);
        let res = rx.recv().unwrap();
        assert_eq!(res, 0);
    }

    #[test]
    fn test_jnz_immediate_mode() {
        let (tx, rx): (Sender<i32>, Receiver<i32>) = mpsc::channel();
        let mut instr = vec![3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1];
        let data = String::from("3\n");
        let mut input = BufReader::new(data.as_bytes());
        compute(&mut instr, 0, &mut input, &tx);
        let res = rx.recv().unwrap();
        assert_eq!(res, 1);

        let mut instr = vec![3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1];
        let data = String::from("0\n");
        let mut input = BufReader::new(data.as_bytes());
        compute(&mut instr, 0, &mut input, &tx);
        let res = rx.recv().unwrap();
        assert_eq!(res, 0);
    }

    #[test]
    fn test_jz_immediate_mode() {
        let (tx, rx): (Sender<i32>, Receiver<i32>) = mpsc::channel();
        let mut instr = vec![3, 3, 1106, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1];
        let data = String::from("3\n");
        let mut input = BufReader::new(data.as_bytes());
        compute(&mut instr, 0, &mut input, &tx);
        let res = rx.recv().unwrap();
        assert_eq!(res, 0);

        let mut instr = vec![3, 3, 1106, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1];
        let data = String::from("0\n");
        let mut input = BufReader::new(data.as_bytes());
        compute(&mut instr, 0, &mut input, &tx);
        let res = rx.recv().unwrap();
        assert_eq!(res, 1);
    }

    #[test]
    fn test_larger_if_example() {
        let (tx, rx): (Sender<i32>, Receiver<i32>) = mpsc::channel();
        let mut instr = vec![
            3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0,
            0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4,
            20, 1105, 1, 46, 98, 99,
        ];
        let data = String::from("7\n");
        let mut input = BufReader::new(data.as_bytes());
        compute(&mut instr, 0, &mut input, &tx);
    }
}
