use pest::Parser;
use sorbus::*;
use std::collections::HashMap;

use pest_derive::Parser;
#[derive(Parser)]
#[grammar = "nhx.pest"]
pub struct NhxParser;

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
            self.nodes()
                .iter()
                .filter(|n| n.children().is_empty())
                .filter_map(|n| n.data.name.as_ref().map(|n| n.as_str())),
        )
    }
    fn to_newick(&self) -> String {
        fn fmt_node(t: &NewickTree, n: usize, r: &mut String) {
            if t[n].is_leaf() {
                t[n].data.name.as_ref().map(|n| r.push_str(n));
                t[n].branch_length.map(|l| r.push_str(&format!(":{}", l)));
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
                        r.push_str(",\n");
                    }
                }
                r.push(')');
                t[n].data.name.as_ref().map(|n| r.push_str(n));
                t[n].branch_length.map(|l| r.push_str(&format!(":{}", l)));
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
        fmt_node(self, 0, &mut r);
        r.push(';');
        r
    }
}

pub fn from_string(content: &str) -> Result<NewickTree, pest::error::Error<Rule>> {
    use pest::iterators::Pair;

    fn parse_attrs(pair: Pair<Rule>, me: &mut NewickNode) {
        match pair.as_rule() {
            Rule::float => me.branch_length = Some(pair.as_str().parse::<f32>().unwrap()),
            Rule::NhxEntry => {
                let mut kv = pair.into_inner();
                let k = kv.next().unwrap().as_str().to_owned();
                let v = kv
                    .next()
                    .map(|x| x.as_str().to_owned())
                    .unwrap_or(String::new());
                me.data.attrs.insert(k, v);
            }
            _ => {
                unimplemented!();
            }
        }
    }

    fn parse_node(pair: Pair<Rule>, parent: usize, tree: &mut NewickTree) {
        let my_id = tree.add_node(
            parent,
            Data {
                name: None,
                attrs: HashMap::new(),
            },
        );

        pair.into_inner().for_each(|inner| match inner.as_rule() {
            Rule::Clade => {
                parse_node(inner, my_id, tree);
            }
            Rule::Leaf => {
                parse_node(inner, my_id, tree);
            }
            Rule::name => {
                tree[my_id].data.name = Some(inner.as_str().to_owned());
            }
            Rule::Attributes => {
                for attr in inner.into_inner() {
                    parse_attrs(attr, &mut tree[my_id])
                }
            }
            _ => unimplemented!(),
        });
    }

    let root = NhxParser::parse(Rule::Tree, &content)?.next().unwrap();

    let mut r = NewickTree::new();
    let _ = parse_node(root, 0, &mut r);
    Ok(r)
}

pub fn from_filename(filename: &str) -> Result<NewickTree, pest::error::Error<Rule>> {
    let content = std::fs::read_to_string(filename).expect("cannot read file");
    from_string(&content)
}
