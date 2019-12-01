use std::io::prelude::*;
use std::io::stdin;

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

pub fn advent1a() -> () {
    let sum = stdin()
        .lock()
        .lines()
        .fold(0, |acc, l| acc + fuel(l.unwrap().trim().parse::<i32>().unwrap()));
    println!("Sum of fuel usage: {:?}", sum);
}

pub fn advent1b() -> () {
    let sum = stdin()
        .lock()
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