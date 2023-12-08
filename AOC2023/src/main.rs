use std::fs::{read_to_string, write};
use std::path::Path;

use aoc2023::argparse::{parse, AppArguments};
use aoc2023::day::{DayNum, Runner};
use aoc2023::solutions::{get_runners, RunnerMap};

fn run_day(runner: &Runner, part: u8, _time: bool) {
    let solution = match part {
        1 => (runner.part1)(),
        2 => (runner.part2)(),
        x => {
            eprintln!("Invalid part: {}", x);
            std::process::exit(1);
        }
    };

    println!("Day: {}\tPart: {}\tSolution: {}", runner.day(), part, solution);
}

fn get_runner(mut runners: RunnerMap, day_num: DayNum) -> Runner {
    runners.remove(&day_num).unwrap_or_else(|| {
        eprintln!("Day {} missing", day_num);
        std::process::exit(1);
    })
}

fn run_all(time: bool) {
    let runners = get_runners();
    for (_, r) in runners.iter() {
        run_day(r, 1, time);
        run_day(r, 2, time);
    }
}


fn main() {
    match parse() {
        Err(err) => {
            eprintln!("Error: {err}");
            std::process::exit(1);
        }
        Ok(args) => match args {
            AppArguments::Prepare { day, overwrite } => {
                let new_path = format!("./src/day{:02}.rs", day);
                if !Path::new(&new_path).exists() || overwrite {
                    let template = read_to_string("./src/day_template.tmp").unwrap();
                    let new_file = template.replace("X", format!("{:02}", day).as_str());
                    write(new_path, new_file).unwrap();
                }

                let input_path = format!("./input/day{:02}.in", day);
                if !Path::new(&input_path).exists() || overwrite {
                    write(input_path, "").unwrap();
                }
            }
            AppArguments::Download { day: _ } => {
                eprintln!("Download not Implemented");
                std::process::exit(1);
            }
            AppArguments::Solve { day, part, time } => {
                let runner = get_runner(get_runners(), day);
                match part {
                    None => {
                        run_day(&runner, 1, time);
                        run_day(&runner, 2, time);
                    }
                    Some(p) => run_day(&runner, p, time)
                }
            }
            AppArguments::All { time } => run_all(time),
        },
    };
}