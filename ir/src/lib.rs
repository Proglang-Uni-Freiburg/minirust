mod debruijn;
mod resolve;
use ast::ctx::Ctx;
use ast::err::Result;
use ast::tag::Tag;
use ast::Top;

use debruijn::debruijn_program;
use resolve::resolve;

ast::def_from_to_ast_types! {
    from => Named,
    to => Debruijn,
    prefix => ast
}

pub fn transform(program: &FromProgram) -> Result<ToProgram> {
    // resolving imports
    let tag = program.tag.clone();
    let mut _program = vec![];
    _program.extend(program.clone().into());
    let mut used = vec![];
    resolve(&mut _program, program.as_ref(), &mut used)?;

    // use debruijn indices
    let mut ctx = Ctx::default();
    let mut env = Ctx::default();
    debruijn_program(
        &Tag::new(
            tag,
            _program
                .into_iter()
                .filter(|x| !matches!(x.as_ref(), &Top::Use(_)))
                .collect(),
        ),
        &mut ctx,
        &mut env,
    )
}
