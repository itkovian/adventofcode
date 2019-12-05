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
use itertools::Itertools;
use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

fn range(p: &mut BufReader<File>) -> (i32, i32) {
    let mut range_line = String::new();
    p.read_line(&mut range_line).unwrap();
    let rs: Vec<i32> = range_line
        .trim()
        .split("-")
        .map(|v| v.parse::<i32>().unwrap())
        .collect();
    (rs[0], rs[1])
}

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

fn check_value_a(v: i32) -> (bool, Vec<i32>) {
    let ds = digits(v);
    let mut equal = false;
    for i in 0..5 {
        if ds[i] == ds[i + 1] {
            equal = true;
        }
        if ds[i] < ds[i + 1] {
            return (false, ds);
        }
    }
    (equal, ds)
}

fn check_value_b(v: i32) -> bool {
    let (a, ds) = check_value_a(v);
    a && {
        let group_sizes: HashSet<usize> = ds
            .iter()
            .group_by(|e| **e)
            .into_iter()
            .map(|(_, group)| group.cloned().collect::<Vec<i32>>().len())
            .collect::<HashSet<usize>>();

        match group_sizes.get(&2) {
            Some(_) => true,
            None => false,
        }
    }
}

pub fn advent4a(p: &mut BufReader<File>) -> () {
    let (lower, upper) = range(p);

    println!("Range: {} to  {}", lower, upper);

    let mut count = 0;
    for v in lower..(upper + 1) {
        if check_value_a(v).0 {
            count += 1;
        }
    }

    println!("We found {} potential values", count);
}

pub fn advent4b(p: &mut BufReader<File>) -> () {
    let (lower, upper) = range(p);

    println!("Range: {} to  {}", lower, upper);

    let mut count = 0;
    for v in lower..(upper + 1) {
        if check_value_b(v) {
            count += 1;
        }
    }

    println!("We found {} potential values", count);
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_digits() {
        assert_eq!(digits(123456), vec![6, 5, 4, 3, 2, 1]);
    }

    #[test]
    fn test_check_value_a() {
        assert_eq!(check_value_a(111111).0, true);
        assert_eq!(check_value_a(123456).0, false);
        assert_eq!(check_value_a(122345).0, true);
    }

    #[test]
    fn test_check_value_b() {
        assert_eq!(check_value_b(112233), true);
        assert_eq!(check_value_b(123444), false);
        assert_eq!(check_value_b(111122), true);
    }
}
