use crate::{
    tag::{Item, Tag},
    Path,
};
use annotate_snippets::display_list::{DisplayList, FormatOptions};
use annotate_snippets::snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation};

pub type Result<T> = std::result::Result<T, Error>;

pub type CodeRef = (Path, (usize, usize));

#[derive(Clone, Debug)]
pub struct Error {
    label: String,
    items: Vec<(CodeRef, String)>,
    footer: Vec<String>,
}

impl Error {
    pub fn new<T: Into<String>>(label: T) -> Self {
        Error {
            label: label.into(),
            items: vec![],
            footer: vec![],
        }
    }
    pub fn label<V: GetCodeRef, T: Into<String>>(mut self, code_ref: &V, label: T) -> Self {
        self.items.push((code_ref.code_ref().clone(), label.into()));
        self
    }

    pub fn help<T: Into<String>>(mut self, label: T) -> Self {
        self.footer.push(label.into());
        self
    }

    pub fn build(&self, src: Option<String>, color: bool) -> String {
        let mut snip = Snippet {
            title: Some(Annotation {
                label: Some(self.label.clone()),
                id: None,
                annotation_type: AnnotationType::Error,
            }),
            slices: vec![],
            footer: vec![],
            opt: FormatOptions {
                color,
                ..Default::default()
            },
        };
        for ((path, (start, end)), label) in self.items.iter() {
            let path = format!("{}.foo", &path.join("/"));
            let (source, line, (start, end)) = {
                match src {
                    Some(ref s) => parse_err(s.clone(), (*start, *end)),
                    None => {
                        let src = std::fs::read_to_string(std::path::Path::new(&path)).unwrap();
                        parse_err(src, (*start, *end))
                    }
                }
            };
            snip.slices.push(Slice {
                source,
                line_start: line,
                origin: Some(path),
                fold: true,
                annotations: vec![SourceAnnotation {
                    label: label.clone(),
                    annotation_type: AnnotationType::Error,
                    range: (start, end),
                }],
            })
        }
        for label in self.footer.iter() {
            snip.footer.push(Annotation {
                label: Some(label.clone()),
                id: None,
                annotation_type: AnnotationType::Help,
            });
        }
        let dl = DisplayList::from(snip);
        format!("{}", dl)
    }

    pub fn display(&self) -> String {
        self.build(None, true)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.display())
    }
}

pub trait GetCodeRef {
    fn code_ref(&self) -> &CodeRef;
    fn start(&self) -> usize;
    fn end(&self) -> usize;
}

impl<I: Item> GetCodeRef for Tag<CodeRef, I> {
    fn code_ref(&self) -> &CodeRef {
        &self.tag
    }

    fn start(&self) -> usize {
        self.tag.1 .0
    }

    fn end(&self) -> usize {
        self.tag.1 .1
    }
}

fn parse_err(src: String, (start, end): (usize, usize)) -> (String, usize, (usize, usize)) {
    // 1 indexed
    let lines = src.lines().into_iter().collect::<Vec<&str>>();

    let end = std::cmp::min(end, src.len());

    fn inc_or_zero(u: usize) -> usize {
        if u >= 1 {
            u - 1
        } else {
            0
        }
    }

    // 1 indexed
    let (line_start, line_end) = (src[..start + 1].lines().count(), src[..end].lines().count());

    // 0 indexed
    let (low, high) = (
        inc_or_zero(line_start),
        std::cmp::min(lines.len(), line_end),
    );

    let src_slice = lines[low..high].join("\n");

    let until_slice = inc_or_zero(lines[..low].join("").chars().count() + low);

    let mut new_start = if line_start > 1 {
        start - until_slice - 1
    } else {
        start - until_slice
    };
    let new_end = std::cmp::min(
        if line_start > 1 {
            end - until_slice - 1
        } else {
            end - until_slice
        },
        src_slice.len(),
    );
    if new_start == new_end {
        new_start -= 1;
    }
    (
        src_slice,
        low + 1,
        (
            new_start,
            if line_start != line_end {
                new_end - 1
            } else {
                new_end
            },
        ),
    )
}
