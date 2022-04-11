use std::env;

use ast::err::Error;
use eval::eval;
use ir::transform;
use parse::parse;
use typing::type_check;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() <= 1 {
        eprintln!("{}", Error::new("expected file path as argument"));
    } else {
        match run(
            args[1].clone(),
            args.iter()
                .enumerate()
                .find(|(i, x)| i > &1usize && x == &"--debug")
                .is_some(),
        ) {
            Ok(_) => {}
            Err(e) => eprintln!("{}", e),
        }
    }
}

fn run<T: AsRef<std::path::Path>>(path: T, debug: bool) -> ast::err::Result<()> {
    let parsed = parse(path.as_ref())?;
    if debug {
        println!("PARSED\n{:#?}\n\n", parsed);
    }
    let debruijn = transform(parsed)?;
    if debug {
        println!("TRANSFORMED\n{:#?}\n\n", debruijn);
    }
    type_check(&debruijn)?;
    if debug {
        println!("TYPE CHECKING\nsuccessful");
    };
    let value = eval(&debruijn)?;
    if debug {
        println!("RESULT\n{:#?}", value);
    };
    println!("{}", value);
    Ok(())
}
