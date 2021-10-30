use clap::{Parser};

/// The strange knowledge manager.
#[derive(Parser)]
#[clap(version = "0.1", author = "Thibault Polge <thibault@thb.lt>")]
struct Opts {
    /// Set path to store.
    #[clap(short, long)]
    store: Option<String>,
    /// A level of verbosity, and can be used multiple times
    #[clap(short, long, parse(from_occurrences))]
    verbose: i32,
    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Parser)]
enum SubCommand {
    #[clap()]
    Query(Query),
    Init(Init),
}

/// Query things in the Tkacz store.
#[derive(Parser)]
struct Query {
    /// Print debug info
    #[clap(short)]
    query: Option<String>
}

/// Initialize a Tkacz store.
#[derive(Parser)]
struct Init {
    /// Print debug info
    #[clap(long)]
    force: bool
}

fn main() {
    let opts: Opts = Opts::parse();

    println!("I'm happy");

    // Vary the output based on how many times the user used the "verbose" flag
    // (i.e. 'myprog -v -v -v' or 'myprog -vvv' vs 'myprog -v'
    match opts.verbose {
        0 => println!("No verbose info"),
        1 => println!("Some verbose info"),
        2 => println!("Tons of verbose info"),
        _ => println!("Don't be ridiculous"),
    }

    // You can handle information about subcommands by requesting their matches by name
    // (as below), requesting just the name used, or both at the same time
    match opts.subcmd {
        SubCommand::Query(t) => {
            println!("Querying...");
        }
        SubCommand::Init(t) => {
            println!("Initializing…");
        }
    }

    // more program logic goes here...
}
