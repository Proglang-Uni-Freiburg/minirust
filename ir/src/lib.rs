mod resolve;
mod debruijn;
use ast::ctx::Ctx;
use ast::err::Result;
use ast::tag::Tag;
use ast::Top;

use resolve::resolve;
use debruijn::debruijn_program;


ast::def_from_to_ast_types! {
    from => Named,
    to => Debruijn,
    prefix => ast
}

pub fn transform(program: FromProgram) -> Result<ToProgram> {
    let tag = program.tag.clone();
    let mut _program = vec![];
    _program.extend(program.it().clone());
    let mut used = vec![];
    resolve(&mut _program, program.it(), &mut used)?;
    let mut ctx = Ctx::default();
    let mut env = Ctx::default();
    debruijn_program(
        &Tag::new(
            tag,
            _program
                .into_iter()
                .filter(|x| !matches!(x.it(), Top::Use(_)))
                .collect(),
        ),
        &mut ctx,
        &mut env,
    )
}
