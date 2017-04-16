use std::iter::Iterator;
use std::iter::Peekable;

use lexer::{Token, SqlType};
use concrete_tree;
use concrete_tree::{ParseTree, NodeType};

#[derive(PartialEq, Debug, Clone)]
pub enum Direction {
    Asc,
    Desc
}

#[derive(PartialEq, Debug, Clone)]
pub struct Order (String, Direction);

#[derive(PartialEq, Debug, Clone)]
pub struct Grouping (String);

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
    Cond { left: ValueExpr, bool_op: BoolOp, right: ValueExpr }
}

#[derive(PartialEq, Debug, Clone)]
pub enum MathOp {
    Add,
    Subtract,
    Divide,
    Multiply
}

#[derive(PartialEq, Debug, Clone)]
pub enum ValueExpr {
    Literal(String),
    ScopedLiteral(String, String),
    Primitive(String),
    ScopedStar(String),
    Star,
    Function { name: String, args: Vec<ValueExpr>},
    Math(MathOp, Box<ValueExpr>, Box<ValueExpr>)
}

#[derive(PartialEq, Debug, Clone)]
pub struct Field {
    alias: Option<String>,
    field_type: ValueExpr
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

#[derive(PartialEq, Debug, Clone)]
pub enum Limit {
    All,
    Amount(i64)
}

pub struct Query {
    fields: Vec<Field>,
    sources: Vec<Source>,
    filter: Option<BoolExpr>,
    groups: Vec<Grouping>,
    having: Option<BoolExpr>,
    orders: Vec<Order>,
    limit: Limit,
    offset: i64,
}

fn parse_math_op(tree: &ParseTree) -> MathOp {
    match node_sql_type(&tree) {
        SqlType::Star => MathOp::Multiply,
        SqlType::Plus => MathOp::Add,
        SqlType::Minus => MathOp::Subtract,
        SqlType::Divide => MathOp::Divide,
        _ => { panic!("Unknown math op") }
    }
}

fn op_rank(math_op: &MathOp) -> u8 {
    match *math_op {
        MathOp::Multiply => 0,
        MathOp::Divide => 1,
        MathOp::Add => 2,
        MathOp::Subtract => 3
    }
}

fn parse_field_type(tree: &ParseTree) -> ValueExpr {
    match tree.node_type {
        NodeType::FieldValueLiteral => {
            let field_name = node_token(&tree.children[0]).text.clone();
            ValueExpr::Literal(field_name)
        },
        NodeType::FieldValuePrimitive => {
            let field_val = node_token(&tree.children[0]).text.clone();
            ValueExpr::Primitive(field_val)
        },
        NodeType::FieldValueStar => {
            ValueExpr::Star
        },
        NodeType::FieldValueScoped => {
            assert_tree_sql_type(&tree.children[0], SqlType::Literal);
            assert_tree_sql_type(&tree.children[1], SqlType::Dot);

            let table_name = typed_node_token(&tree.children[0], SqlType::Literal).text.clone();

            let column_token = node_token(&tree.children[2]);
            match column_token.sql_type {
                SqlType::Literal => ValueExpr::ScopedLiteral(table_name, column_token.text.clone()),
                SqlType::Star => ValueExpr::ScopedStar(table_name),
                _ => { panic!("Unknown scoped value: {:?}", column_token.sql_type) }
            }
        },
        NodeType::FieldValueMath => {
            let mut value_exprs = vec![parse_field_type(&tree.children[0])];
            let mut math_ops = vec![parse_math_op(&tree.children[1])];

            let mut right_child = &tree.children[2];
            while right_child.node_type == NodeType::FieldValueMath {
                value_exprs.push(parse_field_type(&right_child.children[0]));
                math_ops.push(parse_math_op(&right_child.children[1]));
                right_child = &right_child.children[2];
            }

            value_exprs.push(parse_field_type(right_child));

            //TODO: Inefficient!
            // 1. Finding min op every iteration
            // 2. Multiple shifts per combination
            while value_exprs.len() > 1 {
                let (i, _) = math_ops.iter().map(|op| op_rank(op)).enumerate().min_by_key(|&(i, op)| op).unwrap();

                let op = math_ops.remove(i);
                let left = value_exprs.remove(i);
                let right = value_exprs.remove(i);

                value_exprs.insert(i, ValueExpr::Math(op, Box::new(left), Box::new(right)));
            }

            value_exprs.remove(0)
        },
        NodeType::FieldValueParenGroup => {
            assert_tree_sql_type(&tree.children[0], SqlType::OpenParen);
            let child_expr = parse_field_type(&tree.children[1]);
            assert_tree_sql_type(&tree.children[2], SqlType::CloseParen);

            child_expr
        },
        NodeType::FieldValueFunction => {
            assert_tree_sql_type(&tree.children[0], SqlType::Literal);
            let function_name = node_token(&tree.children[0]).text.clone();
            assert_tree_sql_type(&tree.children[1], SqlType::OpenParen);
            assert_tree_sql_type(&tree.children.last().unwrap(), SqlType::CloseParen);

            let args = tree.children.iter().skip(2).take(tree.children.len() - 3).enumerate().flat_map(|(i, child)| {
                if i & 1 == 0 {
                    Some(parse_field_type(child))
                } else {
                    assert_tree_sql_type(child, SqlType::Separator);
                    None
                }
            }).collect::<Vec<ValueExpr>>();

            ValueExpr::Function { name: function_name, args: args }
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
            let column_name = node_token(&tree.children[2]).text.clone();

            Field { alias: Some(column_name), field_type: field_type }
        },
        NodeType::FieldDef => {
            let field_type = parse_field_type(&tree.children[0]);

            Field { alias: None, field_type: field_type }
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

fn parse_groups(tree: &ParseTree) -> Vec<Grouping> {
    assert_node_type(tree, NodeType::Grouping);

    assert_tree_sql_type(&tree.children[0], SqlType::Group);
    assert_tree_sql_type(&tree.children[1], SqlType::By);

    let mut child_iter = tree.children.iter().skip(2);
    let mut groups = vec![];

    loop {
        match child_iter.next().unwrap().node_type {
            NodeType::Concrete(ref token) => {
                groups.push(Grouping(token.text.clone()));
            },
            _ => {
                panic!("Waddup");
            }
        }

        if let Some(child) = child_iter.next() {
            assert_tree_sql_type(child, SqlType::Separator);
        } else {
            break;
        }
    }

    groups
}

fn parse_having(tree: &ParseTree) -> BoolExpr {
    assert_node_type(tree, NodeType::Having);

    assert_tree_sql_type(&tree.children[0], SqlType::Having);
    //TODO: Verify having types
    parse_bool_expr(&tree.children[1]).1
}

fn parse_order(tree: &ParseTree) -> Vec<Order> {
    assert_node_type(tree, NodeType::Sort);

    assert_tree_sql_type(&tree.children[0], SqlType::Order);
    assert_tree_sql_type(&tree.children[1], SqlType::By);

    tree.children.iter().skip(2).enumerate().filter_map(|(i, child)| {
        if i & 1 == 0 {
            assert_node_type(child, NodeType::SortField);

            let field_name = typed_node_token(&child.children[0], SqlType::Literal).text.clone();

            let dir = if let Some(dir_tree) = child.children.get(1) {
                let dir_type = node_sql_type(dir_tree);
                match dir_type {
                    SqlType::Asc => Direction::Asc,
                    SqlType::Desc => Direction::Desc,
                    _ => { panic!("Unexpected sql type: {:?}", dir_type) }
                }
            } else {
                Direction::Asc
            };

            Some(Order(field_name, dir))
        } else {
            assert_tree_sql_type(&child, SqlType::Separator);
            None
        }
    }).collect::<Vec<Order>>()
}

fn parse_limit(tree: &ParseTree) -> Limit {
    assert_node_type(tree, NodeType::Limit);

    assert_tree_sql_type(&tree.children[0], SqlType::Limit);

    let token = node_token(&tree.children[1]);
    match token.sql_type {
        SqlType::All => {
            Limit::All
        },
        SqlType::Int => {
            let limit = token.text.parse::<i64>().unwrap();
            if limit < 0 {
                panic!("Limit cannot be less than 0");
            }
            Limit::Amount(limit)
        },
        _ => {
            panic!("Unknown limit type");
        }
    }
}

fn parse_offset(tree: &ParseTree) -> i64 {
    assert_node_type(tree, NodeType::Offset);

    assert_tree_sql_type(&tree.children[0], SqlType::Offset);

    let token = node_token(&tree.children[1]);
    match token.sql_type {
        SqlType::Int => {
            let offset = token.text.parse::<i64>().unwrap();
            if offset < 0 {
                panic!("Offset cannot be less than 0");
            }
            offset
        },
        _ => {
            panic!("Unknown offset type");
        }
    }
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

    let mut groups = vec![];
    if is_node_type(current_child, NodeType::Grouping) {
        groups = parse_groups(current_child.unwrap());
        current_child = child_iter.next();
    }

    let having = if is_node_type(current_child, NodeType::Having) {
        let having = parse_having(current_child.unwrap());
        current_child = child_iter.next();
        Some(having)
    } else {
        None
    };

    let orders = if is_node_type(current_child, NodeType::Sort) {
        let orders = parse_order(current_child.unwrap());
        current_child = child_iter.next();
        orders
    } else {
        vec![]
    };

    let limit = if is_node_type(current_child, NodeType::Limit) {
        let limit = parse_limit(current_child.unwrap());
        current_child = child_iter.next();
        limit
    } else {
        Limit::All
    };

    let offset = if is_node_type(current_child, NodeType::Offset) {
        let offset = parse_offset(current_child.unwrap());
        current_child = child_iter.next();
        offset
    } else {
        0i64
    };

    Query {
        fields: fields,
        sources: sources,
        filter: filter,
        groups: groups,
        having: having,
        orders: orders,
        limit: limit,
        offset: offset
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

fn node_token(tree: &ParseTree) -> &Token {
    match tree.node_type {
        NodeType::Concrete(ref token) => {
            token
        },
        _ => {
            panic!("Expected Concrete, found {:?}", tree.node_type);
        }
    }
}

fn typed_node_token(tree: &ParseTree, sql_type: SqlType) -> &Token {
    let token = node_token(tree);
    if token.sql_type == sql_type {
        token
    } else {
        panic!("Expected {:?}, received {:?}", sql_type, token.sql_type);
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
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select age, name as person_name from people where age < 5 limit 2 offset 1")).unwrap();

        let query = parse(&parse_tree);
        assert_eq!(&query.fields, &vec![
                   Field{ alias: None, field_type: ValueExpr::Literal("age".to_string()) },
                   Field{ alias: Some("person_name".to_string()), field_type: ValueExpr::Literal("name".to_string()) }
        ]);

        assert_eq!(&query.sources, &vec![ Source::Table("people".to_string()) ]);
        assert_eq!(query.filter, Some(BoolExpr::Cond{ left: ValueExpr::Literal("age".to_string()), bool_op: BoolOp::LessThan, right: ValueExpr::Primitive("5".to_string()) }));
        assert_eq!(query.limit, Limit::Amount(2));
        assert_eq!(query.offset, 1);
    }

    #[test]
    fn test_group() {
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select age from people group by age having count(*) > 2")).unwrap();

        let query = parse(&parse_tree);
        assert_eq!(query.groups, vec![Grouping("age".to_string())]);

        let function_call = ValueExpr::Function { name: "count".to_string(), args: vec![ValueExpr::Star] };
        assert_eq!(query.having, Some(BoolExpr::Cond { left: function_call, bool_op: BoolOp::GreaterThan, right: ValueExpr::Primitive("2".to_string()) }));
    }

    #[test]
    fn test_orders() {
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select * from people order by age desc, name")).unwrap();

        let query = parse(&parse_tree);
        assert_eq!(query.orders, vec![Order("age".to_string(), Direction::Desc), Order("name".to_string(), Direction::Asc)]);
    }

    #[test]
    fn test_scoped_field_select() {
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select people.name as lolz from people")).unwrap();

        let query = parse(&parse_tree);
        assert_eq!(&query.fields, &vec![ Field{ alias: Some("lolz".to_string()), field_type: ValueExpr::ScopedLiteral("people".to_string(), "name".to_string()) }, ]);
    }

    #[test]
    fn test_scoped_star_select() {
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select people.* from people")).unwrap();

        let query = parse(&parse_tree);
        assert_eq!(&query.fields, &vec![ Field{ alias: None, field_type: ValueExpr::ScopedStar("people".to_string()) }, ]);
    }

    #[test]
    fn test_query_bool_precedence() {
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select * from people where age > 1 and age > 2 or age > 3 and (age > 4 or age > 5)")).unwrap();

        let query = parse(&parse_tree);

        let cond_1 = BoolExpr::Cond {
            left: ValueExpr::Literal("age".to_string()),
            bool_op: BoolOp::GreaterThan,
            right: ValueExpr::Primitive("1".to_string())
        };

        let cond_2 = BoolExpr::Cond {
            left: ValueExpr::Literal("age".to_string()),
            bool_op: BoolOp::GreaterThan,
            right: ValueExpr::Primitive("2".to_string())
        };

        let cond_3 = BoolExpr::Cond {
            left: ValueExpr::Literal("age".to_string()),
            bool_op: BoolOp::GreaterThan,
            right: ValueExpr::Primitive("3".to_string())
        };

        let cond_4 = BoolExpr::Cond {
            left: ValueExpr::Literal("age".to_string()),
            bool_op: BoolOp::GreaterThan,
            right: ValueExpr::Primitive("4".to_string())
        };

        let cond_5 = BoolExpr::Cond {
            left: ValueExpr::Literal("age".to_string()),
            bool_op: BoolOp::GreaterThan,
            right: ValueExpr::Primitive("5".to_string())
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

    #[test]
    fn test_query_math_precedence() {
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select 1 * 2 - 3 / 4 * (5 + 6)")).unwrap();

        let query = parse(&parse_tree);

        let mult_1_2 = ValueExpr::Math(MathOp::Multiply, Box::new(ValueExpr::Primitive("1".to_string())), Box::new(ValueExpr::Primitive("2".to_string())));
        let add_5_6 = ValueExpr::Math(MathOp::Add, Box::new(ValueExpr::Primitive("5".to_string())), Box::new(ValueExpr::Primitive("6".to_string())));
        let mult_4 = ValueExpr::Math(MathOp::Multiply, Box::new(ValueExpr::Primitive("4".to_string())), Box::new(add_5_6));
        let div_3 = ValueExpr::Math(MathOp::Divide, Box::new(ValueExpr::Primitive("3".to_string())), Box::new(mult_4));
        let sub = ValueExpr::Math(MathOp::Subtract, Box::new(mult_1_2), Box::new(div_3));

        assert_eq!(query.fields, vec![Field { alias: None, field_type: sub }]);
    }

    #[test]
    fn test_query_math_func() {
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select 2 * count(*) + 1")).unwrap();

        let query = parse(&parse_tree);

        let func = ValueExpr::Function{ name: "count".to_string(), args: vec![ValueExpr::Star] };
        let mult = ValueExpr::Math(MathOp::Multiply, Box::new(ValueExpr::Primitive("2".to_string())), Box::new(func));
        let add = ValueExpr::Math(MathOp::Add, Box::new(mult), Box::new(ValueExpr::Primitive("1".to_string())));

        assert_eq!(query.fields, vec![Field { alias: None, field_type: add }]);
    }
}
