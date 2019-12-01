
mod advent;

use advent::*;
use clap::{App, Arg};
use std::collections::HashMap;

type Callback = fn() -> ();

fn not_started() -> () {
    println!("Please solve something.");
}

fn main() {

    let matches = App::new("advent")
        .version("2019.0.1")
        .author("Andy Georges")
        .about("Advent of code solutions for 2019")
        .arg(
            Arg::with_name("day")
                .long("day")
                .takes_value(true)
                .default_value("0")
                .help("Exercise to execute")
        )
        .get_matches();

    let mut adventMap : HashMap<&str, Callback> = HashMap::new();

    adventMap.insert("0", not_started);
    adventMap.insert("1a", advent1::advent1a);
    adventMap.insert("1b", advent1::advent1b);

    println!("Hello, advent of code!");
    match adventMap.get(matches.value_of("day").unwrap()) {
        Some(f) => f(),
        None => {
            println!("No solution yet for {}", matches.value_of("day").unwrap())
        }
    }

}
