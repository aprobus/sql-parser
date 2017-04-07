use std::iter::Iterator;
use std::iter::Peekable;

use lexer::{Token, SqlType};
use concrete_tree;
use concrete_tree::{ParseTree, NodeType};

pub struct Offset {}

pub struct Limit {}

pub struct Having {}

pub struct Grouping {}

pub struct Filter {}

#[derive(PartialEq, Debug, Clone)]
pub enum FieldType {
    Literal(String),
    Primitive
}

#[derive(PartialEq, Debug, Clone)]
pub struct Field {
    name: String,
    field_type: FieldType
}

#[derive(PartialEq, Debug, Clone)]
pub enum JoinType {
    Inner,
    Left,
    Right,
    Full
}

#[derive(PartialEq, Debug, Clone)]
pub enum Source {
    Table(String),
    Join { left: Box<Source>, right: Box<Source>, join_type: JoinType },
    Query
}

pub struct Query {
    fields: Vec<Field>,
    sources: Vec<Source>
}

fn read_token(tree: &ParseTree) -> Token {
    match tree.node_type {
        NodeType::Concrete(ref token) => {
            token.clone()
        },
        _ => {
            panic!("Whatup");
        }
    }
}

fn parse_field_type(tree: &ParseTree) -> FieldType {
    match tree.node_type {
        NodeType::FieldValueLiteral => {
            let field_name = read_token(&tree.children[0]).text.clone();
            FieldType::Literal(field_name)
        },
        _ => {
            panic!("Unknown type");
        }
    }
}

fn parse_named_field_def(tree: &ParseTree) -> Field {
    match tree.node_type {
        NodeType::NamedFieldDef => {
            let field_type = parse_field_type(&tree.children[0]);
            let column_name = read_token(&tree.children[2]).text.clone();

            Field { name: column_name, field_type: field_type }
        },
        NodeType::FieldDef => {
            let field_type = parse_field_type(&tree.children[0]);
            let column_name = match field_type {
                FieldType::Literal(ref field_name) => {
                    field_name.clone()
                },
                _ => {
                    "anon".to_string()
                }
            };

            Field { name: column_name, field_type: field_type }
        },
        _ => {
            panic!("Unknown type");
        }
    }
}

fn parse_selection(tree: &ParseTree) -> Vec<Field> {
    //TODO: Strict validation?
    tree.children.iter().filter_map(|child| {
        match child.node_type {
            NodeType::FieldDef | NodeType::NamedFieldDef => {
                Some(parse_named_field_def(&child))
            },
            _ => {
                None
            }
        }
    }).collect()
}

fn parse_source(tree: &ParseTree) -> Source {
    if tree.node_type != NodeType::SourceTable {
        panic!("Unknown type");
    }

    match tree.children[0].node_type {
        NodeType::Concrete(ref token) => {
            Source::Table(token.text.clone())
        },
        _ => {
            panic!("Waddup");
        }
    }
}

fn parse_sources(tree: &ParseTree) -> Vec<Source> {
    if tree.node_type != NodeType::Source {
        panic!("Unknown type {:?}", tree.node_type);
    }

    let mut child_iter = tree.children.iter();
    match child_iter.next().unwrap().node_type {
        NodeType::Concrete(ref token) => {
            match token.sql_type {
                SqlType::From => { },
                _ => {
                    panic!("Wattup");
                }
            }
        },
        _ => {
            panic!("Wattup");
        }
    }

    child_iter.enumerate().filter_map(|(i, child)| {
        if i & 1 == 0 {
            Some(parse_source(&child))
        } else {
            match child.node_type {
                NodeType::Concrete(ref token) => {
                    match token.sql_type {
                        SqlType::Separator => {},
                        _ => {
                            panic!("Not separator");
                        }
                    }
                },
                _ => {
                    panic!("Not concrete node");
                }
            }
            None
        }
    }).collect()
}

fn is_node_type(tree: Option<&ParseTree>, expected_type: NodeType) -> bool {
    tree.map(|ref child| child.node_type == NodeType::Source).unwrap_or(false)
}

pub fn parse(tree: &ParseTree) -> Query {
    let mut child_iter = tree.children.iter();
    let mut current_child = child_iter.next();

    let fields = parse_selection(current_child.as_ref().unwrap());
    current_child = child_iter.next();

    let sources = if is_node_type(current_child, NodeType::Source) {
        let sources = parse_sources(current_child.unwrap());
        current_child = child_iter.next();
        sources
    } else {
        vec![]
    };

    Query {
        fields: fields,
        sources: sources
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::{Token, SqlTokenizer};
    use concrete_tree;

    #[test]
    fn test_query() {
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select age, name as person_name from people")).unwrap();

        let query = parse(&parse_tree);
        assert_eq!(&query.fields, &vec![
                   Field{ name: "age".to_string(), field_type: FieldType::Literal("age".to_string()) },
                   Field{ name: "person_name".to_string(), field_type: FieldType::Literal("name".to_string()) }
        ]);

        assert_eq!(&query.sources, &vec![ Source::Table("people".to_string()) ]);
    }
}
