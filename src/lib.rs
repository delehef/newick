use pest::Parser;
use sorbus::*;
use std::collections::HashMap;
use thiserror::Error;

use pest_derive::Parser;
#[derive(Parser)]
#[grammar = "nhx.pest"]
pub struct NhxParser;

#[derive(Error, Debug)]
pub enum NewickError {
    #[error("failed to open file")]
    FileError(#[from] std::io::Error),

    #[error("no tree at root")]
    NoTreeAtRoot(),

    #[error("expected one tree, found {0}")]
    TooManyTrees(usize),

    #[error("not a float: {0}")]
    NotAFloat(String),

    #[error("parse error: {0}")]
    ParseError(#[from] pest::error::Error<Rule>),
}

#[derive(Debug)]
pub struct Data {
    pub name: Option<String>,
    pub attrs: HashMap<String, String>,
}

pub type NewickNode = Node<Data>;
pub type NewickTree = Tree<Data>;

pub trait Newick {
    fn is_duplication(&self, n: usize) -> bool;
    fn leaf_names(&self) -> Box<dyn Iterator<Item = &str> + '_>;
    fn to_newick(&self) -> String;
}

impl Newick for NewickTree {
    fn is_duplication(&self, n: usize) -> bool {
        self[n].data.attrs.get("D").map_or(false, |d| d == "Y")
    }

    fn leaf_names(&self) -> Box<dyn Iterator<Item = &str> + '_> {
        Box::new(
            self.nodes().filter(|n| n.children().is_empty()).filter_map(|n| n.data.name.as_deref()),
        )
    }
    fn to_newick(&self) -> String {
        fn fmt_node(t: &NewickTree, n: usize, r: &mut String) {
            if t[n].is_leaf() {
                if let Some(n) = t[n].data.name.as_ref() {
                    r.push_str(n)
                }
                if let Some(l) = t[n].branch_length {
                    r.push_str(&format!(":{}", l))
                }
                if !t[n].data.attrs.is_empty() {
                    r.push_str("[&&NHX");
                    for (k, v) in t[n].data.attrs.iter() {
                        r.push_str(&format!(":{}={}", k, v));
                    }
                    r.push(']');
                }
            } else {
                r.push('(');

                let mut children = t[n].children().iter().peekable();
                while let Some(c) = children.next() {
                    fmt_node(t, *c, r);
                    if children.peek().is_some() {
                        r.push(',');
                    }
                }
                r.push(')');
                if let Some(n) = t[n].data.name.as_ref() {
                    r.push_str(n)
                }
                if let Some(l) = t[n].branch_length {
                    r.push_str(&format!(":{}", l))
                }
                if !t[n].data.attrs.is_empty() {
                    r.push_str("[&&NHX");
                    for (k, v) in t[n].data.attrs.iter() {
                        r.push_str(&format!(":{}={}", k, v));
                    }
                    r.push(']');
                }
            }
        }
        let mut r = String::new();
        if !self.is_empty() {
            fmt_node(self, self.root(), &mut r);
            r.push(';');
        }
        r
    }
}

pub fn from_string<S: AsRef<str>>(content: S) -> Result<Vec<NewickTree>, NewickError> {
    use pest::iterators::Pair;

    fn parse(pair: Pair<Rule>, trees: &mut Vec<NewickTree>) -> Result<(), NewickError> {
        match pair.as_rule() {
            Rule::Trees => {
                for proto_tree in pair.into_inner() {
                    if let Some(root) = proto_tree.into_inner().next() {
                        trees.push(NewickTree::new());
                        parse_inner(root, None, trees.last_mut().unwrap())?;
                    }
                }
            }
            _ => {
                unimplemented!()
            }
        }

        Ok(())
    }

    fn parse_inner(
        pair: Pair<Rule>,
        parent: Option<usize>,
        tree: &mut NewickTree,
    ) -> Result<(), NewickError> {
        let my_id = tree.add_node(parent, Data { name: None, attrs: HashMap::new() });

        for inner in pair.into_inner() {
            match inner.as_rule() {
                Rule::Clade => {
                    parse_inner(inner, Some(my_id), tree)?;
                }
                Rule::Leaf => {
                    parse_inner(inner, Some(my_id), tree)?;
                }
                Rule::name => {
                    tree[my_id].data.name = Some(inner.as_str().to_owned());
                }
                Rule::Attributes => {
                    for attr in inner.into_inner() {
                        parse_attrs(attr, &mut tree[my_id])?;
                    }
                }
                _ => unimplemented!(),
            };
        }

        Ok(())
    }

    fn parse_attrs(pair: Pair<Rule>, me: &mut NewickNode) -> Result<(), NewickError> {
        match pair.as_rule() {
            Rule::float => pair
                .as_str()
                .parse::<f32>()
                .map_err(|_| NewickError::NotAFloat(pair.as_str().to_owned()))
                .map(|x| me.branch_length = Some(x)),
            Rule::NhxEntry => {
                let mut kv = pair.into_inner();
                let k = kv.next().unwrap().as_str().to_owned();
                let v = kv.next().map(|x| x.as_str().to_owned()).unwrap_or_default();
                me.data.attrs.insert(k, v);
                Ok(())
            }
            _ => {
                unimplemented!();
            }
        }
    }

    let root = NhxParser::parse(Rule::Trees, content.as_ref())?.next().unwrap();

    let mut r = Vec::new();
    let _ = parse(root, &mut r);
    Ok(r)
}

pub fn one_from_string<S: AsRef<str>>(content: S) -> Result<NewickTree, NewickError> {
    let tree = from_string(content.as_ref())?;
    if tree.len() != 1 {
        Err(NewickError::TooManyTrees(tree.len()))
    } else {
        Ok(tree.into_iter().next().unwrap())
    }
}

pub fn from_filename<S: AsRef<str>>(filename: S) -> Result<Vec<NewickTree>, NewickError> {
    let content = std::fs::read_to_string(filename.as_ref()).map_err(NewickError::FileError)?;
    from_string(&content)
}

pub fn one_from_filename<S: AsRef<str>>(filename: S) -> Result<NewickTree, NewickError> {
    let tree = from_filename(filename.as_ref())?;
    if tree.len() != 1 {
        Err(NewickError::TooManyTrees(tree.len()))
    } else {
        Ok(tree.into_iter().next().unwrap())
    }
}
