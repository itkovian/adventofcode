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

fn fuel(mass: i32) -> i32 {
    f64::floor(mass as f64 / 3.0)  as i32 - 2
}

fn fuel_for_fuel(mass: i32) -> i32 {
    let mut total_fuel = 0;
    let mut m_ = mass;
    loop {
        let f = fuel(m_);
        if f < 0 {
            break;
        }
        total_fuel += f;
        m_ = f;
    }
    total_fuel
}

struct Fuel {
    mass: i32
}

impl Iterator for Fuel {
    type Item = i32;

    fn next(&mut self) -> Option<i32> {

        let f = fuel(self.mass);
        if f > 0 {
            self.mass = f;
            Some(f)
        } else  {
            None
        }
    }

}

fn fuel_for_fuel2(mass: i32) -> i32 {
    let f = Fuel { mass: mass };
    f.fold(0, |acc, f| acc + f)
}

pub fn advent1a(p: &mut BufReader<File>) -> () {
    let sum = p
        .lines()
        .fold(0, |acc, l| acc + fuel(l.unwrap().trim().parse::<i32>().unwrap()));
    println!("Sum of fuel usage: {:?}", sum);
}

pub fn advent1b(p: &mut BufReader<File>) -> () {
    let sum = p
        .lines()
        .fold(0, |acc, l| acc + fuel_for_fuel(l.unwrap().trim().parse::<i32>().unwrap()));
    println!("Sum of fuel usage: {:?}", sum);
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_fuel() {
        assert_eq!(fuel(12), 2);
        assert_eq!(fuel(14), 2);
        assert_eq!(fuel(1969), 654);
        assert_eq!(fuel(100756), 33583);
    }

    #[test]
    fn test_fuel_for_fuel() {
        assert_eq!(fuel_for_fuel(2), 0);
        assert_eq!(fuel_for_fuel(14), 2);
        assert_eq!(fuel_for_fuel(1969), 966);
        assert_eq!(fuel_for_fuel(100756), 50346);
    }

    #[test]
    fn test_fuel_for_fuel2() {
        assert_eq!(fuel_for_fuel2(2), 0);
        assert_eq!(fuel_for_fuel2(14), 2);
        assert_eq!(fuel_for_fuel2(1969), 966);
        assert_eq!(fuel_for_fuel2(100756), 50346);
    }

}