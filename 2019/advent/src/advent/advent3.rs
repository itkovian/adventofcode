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
use std::cmp::min;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use std::collections::{HashMap, HashSet};

type Pos = (i32, i32);
type Count = i32;
type Info = (Pos, Count);

#[derive(Debug, PartialEq)]
enum Direction {
    L,
    U,
    R,
    D,
}

impl Direction {
    fn make_move(&self) -> (i32, i32) {
        match self {
            Direction::L => (-1, 0),
            Direction::R => (1, 0),
            Direction::U => (0, 1),
            Direction::D => (0, -1),
        }
    }
}

fn path(route: &Vec<(Direction, i32)>) -> Vec<Info> {
    let mut p = vec![(0, 0)];

    route.iter().fold(p[0], |(x, y), (d, steps)| {
        let (mx, my) = d.make_move();
        for i in 1..(*steps + 1) {
            p.push((x + i * mx, y + i * my));
        }
        (x + steps * mx, y + steps * my)
    });

    p.iter().cloned().zip((0 as i32)..).collect()
}

fn conversion(p: &str) -> (Direction, i32) {
    let d = match &p[0..1] {
        "L" => Direction::L,
        "U" => Direction::U,
        "R" => Direction::R,
        "D" => Direction::D,
        _ => panic!("Unknown direction char {}", &p[0..1]),
    };

    (d, (p[1..]).parse::<i32>().unwrap())
}

fn intersections(route1: &Vec<Info>, route2: &Vec<Info>) -> HashSet<Pos> {
    let rs1: HashSet<Pos> = route1.iter().cloned().map(|i| i.0).collect();
    let rs2: HashSet<Pos> = route2.iter().cloned().map(|i| i.0).collect();
    rs1.intersection(&rs2).cloned().collect()
}

fn distance(route1: &Vec<Info>, route2: &Vec<Info>) -> i32 {
    let is = intersections(route1, route2);
    let mut distances: Vec<i32> = is.iter().map(|(x, y)| x.abs() + y.abs()).collect();

    distances.sort();
    distances[1]
}

fn wire_length(route1: &Vec<Info>, route2: &Vec<Info>) -> i32 {
    let is = intersections(route1, route2);

    let hm1: HashMap<Pos, i32> = route1.iter().cloned().collect();
    let hm2: HashMap<Pos, i32> = route2.iter().cloned().collect();

    let mut m = route1.last().unwrap().1 + route2.last().unwrap().1;
    for pos in is.iter() {
        let d1 = hm1.get(pos).unwrap();
        let d2 = hm2.get(pos).unwrap();
        if d1 + d2 > 0 {
            m = min(m, d1 + d2);
        }
    }

    m
}

fn read(p: &mut BufReader<File>) -> Vec<Vec<Info>> {
    p.lines()
        .map(|l| {
            l.unwrap()
                .trim()
                .split(",")
                .map(|r| conversion(&r))
                .collect::<Vec<(Direction, i32)>>()
        })
        .map(|r| path(&r))
        .collect::<Vec<Vec<Info>>>()
}

pub fn advent3a(p: &mut BufReader<File>) -> () {
    let routes = read(p);
    let distance = distance(&routes[0], &routes[1]);
    println!("Closest intersection is {:?}", distance);
}

pub fn advent3b(p: &mut BufReader<File>) -> () {
    let routes = read(p);
    let length = wire_length(&routes[0], &routes[1]);
    println!("Minimal wire length: {}", length);
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_conversion() {
        assert_eq!(conversion("L1"), (Direction::L, 1));
        assert_eq!(conversion("U4000"), (Direction::U, 4000));
        assert_eq!(conversion("R24"), (Direction::R, 24));
        assert_eq!(conversion("D14"), (Direction::D, 14));
    }

    #[test]
    fn test_path() {
        let route1 = "R2,U2,L2,D2"
            .split(",")
            .map(|r| conversion(&r))
            .collect::<Vec<(Direction, i32)>>();

        assert_eq!(
            path(&route1),
            vec![
                ((0, 0), 0),
                ((1, 0), 1),
                ((2, 0), 2),
                ((2, 1), 3),
                ((2, 2), 4),
                ((1, 2), 5),
                ((0, 2), 6),
                ((0, 1), 7),
                ((0, 0), 8)
            ]
        );
    }

    #[test]
    fn test_distance() {
        let r1 = path(
            &"R75,D30,R83,U83,L12,D49,R71,U7,L72"
                .split(",")
                .map(|r| conversion(&r))
                .collect::<Vec<(Direction, i32)>>(),
        );
        let r2 = path(
            &"U62,R66,U55,R34,D71,R55,D58,R83"
                .split(",")
                .map(|r| conversion(&r))
                .collect::<Vec<(Direction, i32)>>(),
        );

        assert_eq!(distance(&r1, &r2), 159);
    }
}
