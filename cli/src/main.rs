use std::env;

use ast::{err::Error, Value};
use eval::eval;
use ir::transform;
use parse::parse;
use typing::type_check;
use std::time::Instant;

const DEBUG: bool = false;
fn main() {
    let args: Vec<String> = env::args().collect();

    let now = Instant::now();

    if args.len() < 1 {
        eprintln!("{}", Error::new("expected file path"));
        std::process::exit(1)
    } else {
        match run(args[1].clone()) {
            Ok(_) => {}
            Err(e) => {
                eprintln!("{}", e);
                std::process::exit(1)
            } 
        }
    }
    if DEBUG {
        let elapsed = now.elapsed();
        println!("took {:.2?}", elapsed);
    }
}

fn run<T: AsRef<std::path::Path>>(path: T) -> ast::err::Result<()> {
    let parsed = parse(path.as_ref())?;
    if DEBUG {
        println!("PARSED\n{:#?}\n\n", parsed);
    }
    let debruijn = transform(&parsed)?;
    if DEBUG {
        println!("TRANSFORMED\n{:#?}\n\n", debruijn);
    }
    let mut ffi = type_check(&debruijn)?;
    if DEBUG {
        println!("TYPE CHECKING\nsuccessful");
    };
    let value = eval(&debruijn, &mut ffi)?;
    if DEBUG {
        println!("RESULT\n{:#?}", value);
    };
    match value.it() {
        Value::Unit => {}
        _ => println!("{}", value),
    }
    Ok(())
}
