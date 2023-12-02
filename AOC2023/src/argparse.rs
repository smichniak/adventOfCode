use std::process;
use chrono::{Datelike, Local};
use crate::day::DayNum;

pub enum AppArguments {
    Prepare {
        day: DayNum,
    },
    Download {
        day: DayNum,
    },
    Solve {
        day: DayNum,
        part: Option<u8>,
        time: bool,
    },
    All {
        time: bool,
    },
}

pub fn parse() -> Result<AppArguments, Box<dyn std::error::Error>> {
    let mut args = pico_args::Arguments::from_env();
    let today = Local::now().day().try_into().unwrap();

    let app_args = match args.subcommand()?.as_deref() {
        Some("prepare") => AppArguments::Prepare {
            day: args.opt_value_from_str("--day")?.unwrap_or(today),
        },
        Some("download") => AppArguments::Download {
            day: args.opt_value_from_str("--day")?.unwrap_or(today),
        },
        Some("solve") => AppArguments::Solve {
            day: args.opt_value_from_str("--day")?.unwrap_or(today),
            part: args.opt_value_from_str("--part")?,
            time: args.contains("--time"),
        },
        Some("all") => AppArguments::All {
            time: args.contains("--time"),
        },
        Some(x) => {
            eprintln!("Unknown command: {x}");
            process::exit(1);
        }
        None => {
            eprintln!("No command specified.");
            process::exit(1);
        }
    };

    let remaining = args.finish();
    if !remaining.is_empty() {
        eprintln!("Warning: unknown argument(s): {remaining:?}.");
    }

    Ok(app_args)
}