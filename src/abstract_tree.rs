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
pub enum BoolOp {
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual
}

#[derive(PartialEq, Debug, Clone)]
pub enum BoolLogic {
    And,
    Or
}

#[derive(PartialEq, Debug, Clone)]
pub enum BoolExpr {
    LogicExpr { left: Box<BoolExpr>, bool_logic: BoolLogic, right: Box<BoolExpr> },
    Cond { left: FieldType, bool_op: BoolOp, right: FieldType }
}

#[derive(PartialEq, Debug, Clone)]
pub enum FieldType {
    Literal(String),
    Primitive(String),
    Star
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
    sources: Vec<Source>,
    filter: Option<BoolExpr>
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
        NodeType::FieldValuePrimitive => {
            let field_val = read_token(&tree.children[0]).text.clone();
            FieldType::Primitive(field_val)
        },
        NodeType::FieldValueStar => {
            FieldType::Star
        },
        _ => {
            panic!("Unknown type: {:?}", tree.node_type);
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
    assert_node_type(tree, NodeType::Selection);

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
    assert_node_type(tree, NodeType::SourceTable);

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
    assert_node_type(tree, NodeType::Source);

    let mut child_iter = tree.children.iter();
    assert_opt_tree_sql_type(child_iter.next(), SqlType::From);

    child_iter.enumerate().filter_map(|(i, child)| {
        if i & 1 == 0 {
            Some(parse_source(&child))
        } else {
            assert_tree_sql_type(child, SqlType::Separator);
            None
        }
    }).collect()
}

fn parse_bool_expr(tree: &ParseTree) -> (NodeType, BoolExpr) {
    match tree.node_type {
        NodeType::ExprBoolLogic => {
            let top_left = Box::new(parse_bool_expr(&tree.children[0]).1);

            let top_bool_logic = match node_sql_type(&tree.children[1]) {
                SqlType::And => BoolLogic::And,
                SqlType::Or => BoolLogic::Or,
                _ => {
                    panic!("Waddup");
                }
            };

            let (right_node_type, right_bool_expr) = parse_bool_expr(&tree.children[2]);

            match right_node_type {
                NodeType::ExprBoolLogic => {
                    match right_bool_expr {
                        BoolExpr::LogicExpr{ ref left, ref bool_logic, ref right } => {
                            if top_bool_logic == BoolLogic::And && bool_logic == &BoolLogic::Or {
                                return (NodeType::ExprBoolLogic, BoolExpr::LogicExpr {
                                    bool_logic: BoolLogic::Or,
                                    left: Box::new(BoolExpr::LogicExpr {
                                        bool_logic: BoolLogic::And,
                                        left: top_left,
                                        right: left.clone()
                                    }),
                                    right: right.clone()
                                });
                            }
                        },
                        _ => { }
                    }
                },
                _ => { }
            }

            let top_right = Box::new(right_bool_expr);
            (NodeType::ExprBoolLogic, BoolExpr::LogicExpr { bool_logic: top_bool_logic, left: top_left, right: top_right })
        },
        NodeType::ExprBoolCond => {
            let left_value = parse_field_type(&tree.children[0]);
            let bool_op = match node_sql_type(&tree.children[1]) {
                SqlType::Equal => BoolOp::Equal,
                SqlType::NotEqual => BoolOp::NotEqual,
                SqlType::GreaterThan => BoolOp::GreaterThan,
                SqlType::GreaterThanEqual => BoolOp::GreaterThanEqual,
                SqlType::LessThan => BoolOp::LessThan,
                SqlType::LessThanEqual => BoolOp::LessThanEqual,
                _ => {
                    panic!("Waddup");
                }
            };
            let right_value = parse_field_type(&tree.children[2]);

            (NodeType::ExprBoolCond, BoolExpr::Cond { left: left_value, bool_op: bool_op, right: right_value })
        },
        NodeType::ExprParenGroup => {
            assert_tree_sql_type(&tree.children[0], SqlType::OpenParen);
            let sub_expr = parse_bool_expr(&tree.children[1]).1;
            assert_tree_sql_type(&tree.children[2], SqlType::CloseParen);
            (NodeType::ExprParenGroup, sub_expr)
        },
        _ => {
            panic!("Waddup");
        }
    }
}

fn parse_filter(tree: &ParseTree) -> BoolExpr {
    assert_node_type(tree, NodeType::Filter);
    assert_tree_sql_type(&tree.children[0], SqlType::Where);

    parse_bool_expr(&tree.children[1]).1
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

    let filter = if is_node_type(current_child, NodeType::Filter) {
        let filter = parse_filter(current_child.unwrap());
        current_child = child_iter.next();
        Some(filter)
    } else {
        None
    };

    Query {
        fields: fields,
        sources: sources,
        filter: filter
    }
}

fn assert_node_type(tree: &ParseTree, expected_type: NodeType) {
    if tree.node_type != expected_type {
        panic!("Expected type {:?}, got {:?}", expected_type, tree.node_type);
    }
}

fn assert_tree_sql_type(tree: &ParseTree, sql_type: SqlType) {
    match tree.node_type {
        NodeType::Concrete(ref token) => {
            if token.sql_type != sql_type {
                panic!("Expected {:?}, got {:?}", sql_type, token.sql_type);
            }
        },
        _ => {
            panic!("Invalid node type: {:?}", tree.node_type);
        }
    }
}

fn assert_opt_tree_sql_type(tree_opt: Option<&ParseTree>, sql_type: SqlType) {
    if let Some(tree) = tree_opt {
        assert_tree_sql_type(tree, sql_type);
    } else {
        panic!("Tree missing!");
    }
}

fn node_sql_type(tree: &ParseTree) -> SqlType {
    match tree.node_type {
        NodeType::Concrete(ref token) => {
            token.sql_type.clone()
        },
        _ => {
            panic!("Waddup");
        }
    }
}

fn is_node_type(tree: Option<&ParseTree>, expected_type: NodeType) -> bool {
    tree.map(|ref child| child.node_type == expected_type).unwrap_or(false)
}

fn is_sql_type(tree: &ParseTree, expected_type: SqlType) -> bool {
    match tree.node_type {
        NodeType::Concrete(ref token) => {
            token.sql_type == expected_type
        },
        _ => false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::{Token, SqlTokenizer};
    use concrete_tree;

    #[test]
    fn test_query() {
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select age, name as person_name from people where age < 5")).unwrap();

        let query = parse(&parse_tree);
        assert_eq!(&query.fields, &vec![
                   Field{ name: "age".to_string(), field_type: FieldType::Literal("age".to_string()) },
                   Field{ name: "person_name".to_string(), field_type: FieldType::Literal("name".to_string()) }
        ]);

        assert_eq!(&query.sources, &vec![ Source::Table("people".to_string()) ]);
        assert_eq!(query.filter, Some(BoolExpr::Cond{ left: FieldType::Literal("age".to_string()), bool_op: BoolOp::LessThan, right: FieldType::Primitive("5".to_string()) }));
    }

    #[test]
    fn test_query_bool_precedence() {
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select * from people where age > 1 and age > 2 or age > 3 and (age > 4 or age > 5)")).unwrap();

        let query = parse(&parse_tree);

        let cond_1 = BoolExpr::Cond {
            left: FieldType::Literal("age".to_string()),
            bool_op: BoolOp::GreaterThan,
            right: FieldType::Primitive("1".to_string())
        };

        let cond_2 = BoolExpr::Cond {
            left: FieldType::Literal("age".to_string()),
            bool_op: BoolOp::GreaterThan,
            right: FieldType::Primitive("2".to_string())
        };

        let cond_3 = BoolExpr::Cond {
            left: FieldType::Literal("age".to_string()),
            bool_op: BoolOp::GreaterThan,
            right: FieldType::Primitive("3".to_string())
        };

        let cond_4 = BoolExpr::Cond {
            left: FieldType::Literal("age".to_string()),
            bool_op: BoolOp::GreaterThan,
            right: FieldType::Primitive("4".to_string())
        };

        let cond_5 = BoolExpr::Cond {
            left: FieldType::Literal("age".to_string()),
            bool_op: BoolOp::GreaterThan,
            right: FieldType::Primitive("5".to_string())
        };

        let where_clause = BoolExpr::LogicExpr {
            left: Box::new(BoolExpr::LogicExpr {
                left: Box::new(cond_1),
                bool_logic: BoolLogic::And,
                right: Box::new(cond_2)
            }),
            bool_logic: BoolLogic::Or,
            right: Box::new(BoolExpr::LogicExpr {
                left: Box::new(cond_3),
                bool_logic: BoolLogic::And,
                right: Box::new(BoolExpr::LogicExpr {
                    left: Box::new(cond_4),
                    bool_logic: BoolLogic::Or,
                    right: Box::new(cond_5)
                })
            })
        };

        assert_eq!(query.filter, Some(where_clause));
    }
}
