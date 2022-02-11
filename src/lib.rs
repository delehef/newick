use pest::Parser;
use std::{collections::HashMap, usize};

use pest_derive::Parser;
#[derive(Parser)]
#[grammar = "nhx.pest"]
pub struct NhxParser;

#[derive(Debug)]
pub struct Node {
    pub parent: usize,
    pub id: usize,
    pub name: Option<String>,
    pub children: Option<Vec<usize>>,
    pub length: Option<f32>,
    pub data: HashMap<String, String>,
}
impl Node {
    pub fn new_clade(parent: usize, id: usize) -> Self {
        Node {
            id,
            name: None,
            parent,
            length: None,
            data: HashMap::new(),
            children: Some(Vec::new()),
        }
    }

    pub fn new_leaf(parent: usize, id: usize) -> Self {
        Node {
            id,
            name: None,
            parent,
            length: None,
            data: HashMap::new(),
            children: None,
        }
    }

    pub fn is_duplication(&self) -> bool {
        self.data.get("D").map_or(false, |d| d == "Y")
    }
    pub fn is_leaf(&self) -> bool {
        self.children.is_none()
    }
}

pub struct Tree {
    nodes: Vec<Node>,
}
impl Tree {
    pub fn print(&self) {
        fn print_node(nodes: &[Node], n: usize, o: usize) {
            println!(
                "{}{}:{:?} - {:?}",
                str::repeat(" ", o),
                &nodes[n].name.as_ref().unwrap_or(&String::new()),
                &nodes[n].length.unwrap_or(-1.),
                &nodes[n].data
            );
            nodes[n]
                .children
                .as_ref()
                .map(|children| children.iter().for_each(|c| print_node(nodes, *c, o + 2)));
        }
        print_node(&self.nodes, 0, 0);
    }

    pub fn parent(&self, n: usize) -> Option<usize> {
        if self.nodes[n].parent == 0 {
            None
        } else {
            Some(self.nodes[n].parent)
        }
    }

    pub fn find_leaf<P>(&self, f: P) -> Option<usize>
    where
        P: Fn(&Node) -> bool,
    {
        match self
            .nodes
            .iter()
            .enumerate()
            .filter(|(_, n)| n.is_leaf())
            .find(|(_i, n)| f(n))
        {
            Some((i, _)) => Some(i),
            None => None,
        }
    }

    pub fn mrca(&self, nodes: &[usize]) -> Option<usize> {
        if nodes.is_empty() {
            None
        } else {
            let ancestries = nodes
                .iter()
                .map(|&n| self.ascendance(n))
                .collect::<Vec<_>>();

            for p in ancestries[0].iter() {
                if ancestries.iter().all(|a| a.contains(p)) {
                    return Some(*p);
                }
            }
            None
        }
    }

    pub fn ascendance(&self, n: usize) -> Vec<usize> {
        let mut r = Vec::new(); // TODO: use log-n pre-allocation
        while let Some(x) = self.parent(n) {
            r.push(x)
        }
        r
    }

    pub fn descendants(&self, n: usize) -> Vec<usize> {
        fn find_descendants(t: &Tree, n: usize, ax: &mut Vec<usize>) {
            ax.push(t[n].id);
            if let Some(children) = t[n].children.as_ref() {
                for &c in children.iter() {
                    find_descendants(t, c, ax);
                }
            }
        }

        let mut r = vec![];
        if let Some(children) = self[n].children.as_ref() {
            for &c in children.iter() {
                find_descendants(self, c, &mut r);
            }
        }
        r
    }

    pub fn leaves_of(&self, n: usize) -> Vec<usize> {
        fn find_descendants_leaves(t: &Tree, n: usize, ax: &mut Vec<usize>) {
            if let Some(children) = t[n].children.as_ref() {
                for &c in children.iter() {
                    find_descendants_leaves(t, c, ax);
                }
            } else {
                ax.push(t[n].id);
            }
        }

        let mut r = vec![];
        find_descendants_leaves(self, n, &mut r);
        r
    }

    pub fn siblings(&self, n: usize) -> Vec<usize> {
        self.descendants(self[n].parent)
            .into_iter()
            .filter(|&nn| nn != n)
            .filter(|n| self[*n].is_leaf())
            .collect()
    }

    pub fn from_string(content: &str) -> Result<Self, pest::error::Error<Rule>> {
        use pest::iterators::Pair;
        enum Kind {
            Clade,
            Leaf,
        }

        fn parse_attrs(pair: Pair<Rule>, me: &mut Node) {
            match pair.as_rule() {
                Rule::float => me.length = Some(pair.as_str().parse::<f32>().unwrap()),
                Rule::NhxEntry => {
                    let mut kv = pair.into_inner();
                    let k = kv.next().unwrap().as_str().to_owned();
                    let v = kv
                        .next()
                        .map(|x| x.as_str().to_owned())
                        .unwrap_or(String::new());
                    me.data.insert(k, v);
                }
                _ => {
                    unimplemented!();
                }
            }
        }

        fn parse_node(
            pair: Pair<Rule>,
            parent: usize,
            storage: &mut Vec<Node>,
            my_kind: Kind,
        ) -> usize {
            let my_id = storage.len();
            storage.push(match my_kind {
                Kind::Clade => Node::new_clade(parent, my_id),
                Kind::Leaf => Node::new_leaf(parent, my_id),
            });

            pair.into_inner().for_each(|inner| match inner.as_rule() {
                Rule::Clade => {
                    let child = parse_node(inner, my_id, storage, Kind::Clade);
                    match &mut storage[my_id].children {
                        Some(children) => children.push(child),
                        _ => unimplemented!(),
                    }
                }
                Rule::Leaf => {
                    let child = parse_node(inner, my_id, storage, Kind::Leaf);
                    match &mut storage[my_id].children {
                        Some(children) => children.push(child),
                        _ => unimplemented!(),
                    }
                }
                Rule::name => {
                    storage[my_id].name = Some(inner.as_str().to_owned());
                }
                Rule::Attributes => {
                    for attr in inner.into_inner() {
                        parse_attrs(attr, &mut storage[my_id])
                    }
                }
                _ => unimplemented!(),
            });

            my_id
        }

        let root = NhxParser::parse(Rule::Tree, &content)?.next().unwrap();

        let mut r = Vec::new();
        let _ = parse_node(root, 0, &mut r, Kind::Clade);
        Ok(Tree { nodes: r })
    }

    pub fn from_filename(filename: &str) -> Result<Self, pest::error::Error<Rule>> {
        let content = std::fs::read_to_string(filename).expect("cannot read file");
        Self::from_string(&content)
    }

    pub fn depth(&self) -> f32 {
        todo!()
    }

    pub fn node_depth(&self, n: usize) -> f32 {
        let mut depth = self.nodes[n].length.unwrap();
        let mut parent = self.nodes[n].parent;
        while parent != 0 {
            depth += self.nodes[parent].length.unwrap();
            parent = self.nodes[parent].parent;
        }
        depth
    }

    pub fn node_topological_depth(&self, n: usize) -> f32 {
        let mut depth = 0.;
        let mut parent = self.nodes[n].parent;
        while parent != 0 {
            depth += 1.;
            parent = self.nodes[parent].parent;
        }
        depth
    }

    pub fn topological_depth(&self) -> (usize, f32) {
        self.leaves()
            .map(|n| (n, self.node_topological_depth(n)))
            .max_by(|x, y| x.1.partial_cmp(&y.1).unwrap())
            .unwrap()
    }

    pub fn leaves<'a>(&'a self) -> impl Iterator<Item = usize> + 'a {
        (0..self.nodes.len()).filter(move |n| self.nodes[*n].is_leaf())
    }
    pub fn inners<'a>(&'a self) -> impl Iterator<Item = usize> + 'a {
        (0..self.nodes.len()).filter(move |n| !self.nodes[*n].is_leaf())
    }

    pub fn leaf_names(&self) -> Vec<(usize, Option<&String>)> {
        self.leaves()
            .map(|n| (n, self.nodes[n].name.as_ref()))
            .collect()
    }
}
impl std::ops::Index<usize> for Tree {
    type Output = Node;
    fn index(&self, i: usize) -> &Self::Output {
        &self.nodes[i]
    }
}
impl std::ops::IndexMut<usize> for Tree {
    fn index_mut(&mut self, i: usize) -> &mut Self::Output {
        &mut self.nodes[i]
    }
}
impl std::fmt::Display for Tree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn fmt_node(t: &Tree, n: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match &t[n].children {
                Some(children) => {
                    write!(f, "(")?;

                    let mut children = children.iter().peekable();
                    while let Some(c) = children.next() {
                        fmt_node(t, *c, f)?;
                        if children.peek().is_some() {
                            write!(f, ",\n")?;
                        }
                    }
                    write!(f, ")")?;
                    if !t[n].is_leaf() {
                        t[n].name.as_ref().map(|n| write!(f, "{}", n));
                    };
                    t[n].length.map(|l| write!(f, ":{}", l));
                    if !t[n].data.is_empty() {
                        write!(f, "[&&NHX")?;
                        for (k, v) in t[n].data.iter() {
                            write!(f, ":{}={}", k, v)?
                        }
                        write!(f, "]")?;
                    }
                }
                None => {
                    t[n].name.as_ref().map(|n| write!(f, "{}", n));
                    t[n].length.map(|l| write!(f, ":{}", l));
                    if !t[n].data.is_empty() {
                        write!(f, "[&&NHX")?;
                        for (k, v) in t[n].data.iter() {
                            write!(f, ":{}={}", k, v)?
                        }
                        write!(f, "]")?;
                    }
                }
            }
            Ok(())
        }
        fmt_node(self, 0, f)?;
        write!(f, ";")
    }
}
