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
    Literal(String)
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

fn parse_selection(tree: &ParseTree) -> Result<Selection, String> {
    let mut fields = vec![];

    for child in tree.children.iter() {
        match child.node_type {
            NodeType::NamedFieldDef => {

            },
            NodeType::FieldDef => {
                let field_val = &child.children[0];

                let field_name = match field_val.children[0].node_type {
                    NodeType::Concrete(ref token) => {
                        token.text.clone()
                    },
                    _ => {
                        panic!("Whatup");
                    }
                };

                fields.push(Field { name: field_name.clone(), field_type: FieldType::Literal(field_name.clone()) });
            },
            _ => {}
        }
    }

    Ok(Selection { fields: fields })
}

pub fn parse(tree: &ParseTree) -> Result<Query, String> {
    let selection = parse_selection(&tree.children[0]).unwrap();
    Ok(Query { selection: selection })
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::{Token, SqlTokenizer};
    use concrete_tree;

    #[test]
    fn test_query() {
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select age from people")).unwrap();

        let query = parse(&parse_tree).unwrap();
        assert_eq!(&query.selection.fields, &vec![Field{ name: "age".to_string(), field_type: FieldType::Literal("age".to_string()) }]);
    }
}
