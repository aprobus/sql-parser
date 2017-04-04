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

pub struct Selection {
    fields: Vec<Field>
}

pub struct Source {}

pub struct Query {
    selection: Selection
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

fn parse_selection(tree: &ParseTree) -> Selection {
    let fields = tree.children.iter().filter_map(|child| {
        match child.node_type {
            NodeType::FieldDef | NodeType::NamedFieldDef => {
                Some(parse_named_field_def(&child))
            },
            _ => {
                None
            }
        }
    }).collect();

    Selection { fields: fields }
}

pub fn parse(tree: &ParseTree) -> Query {
    let selection = parse_selection(&tree.children[0]);
    Query { selection: selection }
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
        assert_eq!(&query.selection.fields, &vec![
                   Field{ name: "age".to_string(), field_type: FieldType::Literal("age".to_string()) },
                   Field{ name: "person_name".to_string(), field_type: FieldType::Literal("name".to_string()) }
        ]);
    }
}
