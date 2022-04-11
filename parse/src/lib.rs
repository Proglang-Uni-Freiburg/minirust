mod grammar;

use std::fs::read_to_string;

use ast::{self, Named};
use ast::{_Program, err::Error, err::Result, tag::Tag};

pub fn parse<T: AsRef<std::path::Path>>(path: T) -> Result<_Program<Named>> {
    let ast_path: ast::Path = path
        .as_ref()
        .with_extension("")
        .iter()
        .map(|x| x.to_str().unwrap().to_string())
        .collect();

    let src = read_to_string(path).map_err(|_| {
        Error::new(format!(
            "could not find {}.foo from cwd",
            ast_path.join("/")
        ))
    })?;
    parse_string(src, ast_path)
}

pub fn parse_string(src: String, path: ast::Path) -> Result<_Program<Named>> {
    let src = src
        .trim_end_matches(&['\n', ' ', '\t', '\r'] as &[_])
        .trim_start_matches(&['\n', ' ', '\t', '\r'] as &[_]);
    grammar::lang::program(src, &path).map_err(|e| {
        Error::new("syntax error").label(
            &Tag::new((path, (e.location.offset, e.location.offset + 1)), ()),
            format!("expected one of {}", e.expected),
        )
    })
}

const KEYWORDS: [&str; 9] = [
    "let", "fn", "match", "struct", "enum", "alias", "false", "true", "_",
];
