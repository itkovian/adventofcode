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

mod advent;

use advent::*;
use clap::{App, Arg};
use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

const VERSION: &str = env!("CARGO_PKG_VERSION");
type Callback = fn(&mut BufReader<File>) -> ();

fn not_started(_bf: &mut BufReader<File>) -> () {
    println!("Please solve something.");
}

fn main() {

    let matches = App::new("advent")
        .version(VERSION)
        .author("Andy Georges")
        .about("Advent of code solutions for 2019")
        .arg(
            Arg::with_name("day")
                .long("day")
                .takes_value(true)
                .default_value("0")
                .help("Exercise to execute")
        )
        .arg(
            Arg::with_name("inputs_dir")
                .long("inputs")
                .takes_value(true)
                .default_value("input")
                .help("Location of the input files")
        )
        .get_matches();

    let mut advent_map : HashMap<&str, Callback> = HashMap::new();
    let advent_ex = matches.value_of("day").unwrap();
    let input_path = Path::join(
        Path::new(matches.value_of("inputs_dir").unwrap()),
        format!("{}.input", advent_ex)
    );

    let input_file = File::open(&input_path).expect(&format!("Cannot open input file: {:?}", input_path));
    let mut reader = BufReader::new(input_file);

    advent_map.insert("0", not_started);
    advent_map.insert("1a", advent1::advent1a);
    advent_map.insert("1b", advent1::advent1b);
    advent_map.insert("2a", advent2::advent2a);
    advent_map.insert("2b", advent2::advent2b);

    println!("Hello, advent of code!");
    match advent_map.get(advent_ex) {
        Some(f) => f(&mut reader),
        None => {
            println!("No solution yet for {}", advent_ex);
        }
    }

}
