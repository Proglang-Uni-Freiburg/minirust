use std::env;

use ast::err::Error;
use eval::eval;
use ir::transform;
use parse::parse;
use std::time::Instant;
use typing::type_check;

const ELAPSED: bool = false;
const PRINT_PARSED: bool = false;
const PRINT_IR: bool = false;

fn main() {
    let args: Vec<String> = env::args().collect();
    let now = Instant::now();

    if args.len() <= 1 {
        eprintln!("{}", Error::new("expected path to main file as first argument"));
        std::process::exit(1)
    } else if let Err(e) = run(args[1].clone()) {
        eprintln!("{}", e);
        std::process::exit(1)
    }

    if ELAPSED {
        println!("took {:.2?}", now.elapsed());
    }
}

fn run<T: AsRef<std::path::Path>>(path: T) -> ast::err::Result<()> {
    let parsed = parse(path.as_ref())?;
    if PRINT_PARSED {
        println!("PARSED\n{:#?}\n\n", parsed);
    }
    let debruijn = transform(&parsed)?;
    if PRINT_IR {
        println!("TRANSFORMED\n{:#?}\n\n", debruijn);
    }
    let ffi = type_check(&debruijn)?;
    eval(&debruijn, &ffi)?;
    Ok(())
}
