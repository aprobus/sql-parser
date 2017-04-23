use std::iter::Iterator;

use lexer::{Token, SqlType};
use concrete_tree::{ParseTree, ParseErr, NodeType};

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
    Join { join_type: JoinType, left: Box<Source>, right: Box<Source>, cond: BoolExpr },
    Query(Query)
}

#[derive(PartialEq, Debug, Clone)]
pub enum Limit {
    All,
    Amount(i64)
}

#[derive(PartialEq, Debug, Clone)]
pub struct Query {
    pub fields: Vec<Field>,
    pub sources: Vec<Source>,
    pub filter: Option<BoolExpr>,
    pub groups: Vec<Grouping>,
    pub having: Option<BoolExpr>,
    pub orders: Vec<Order>,
    pub limit: Limit,
    pub offset: i64,
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

fn parse_field_type(tree: &ParseTree, allow_star: bool) -> Result<ValueExpr, ParseErr> {
    match tree.node_type {
        NodeType::FieldValueLiteral => {
            let field_name = node_token(&tree.children[0]).text.clone();
            Ok(ValueExpr::Literal(field_name))
        },
        NodeType::FieldValuePrimitive => {
            let field_val = node_token(&tree.children[0]).text.clone();
            Ok(ValueExpr::Primitive(field_val))
        },
        NodeType::FieldValueStar => {
            if allow_star {
                Ok(ValueExpr::Star)
            } else {
                Err(ParseErr { token: Some(node_token(&tree.children[0]).clone()) })
            }
        },
        NodeType::FieldValueScoped => {
            assert_tree_sql_type(&tree.children[0], SqlType::Literal);
            assert_tree_sql_type(&tree.children[1], SqlType::Dot);

            let table_name = typed_node_token(&tree.children[0], SqlType::Literal).text.clone();

            let column_token = node_token(&tree.children[2]);
            match column_token.sql_type {
                SqlType::Literal => Ok(ValueExpr::ScopedLiteral(table_name, column_token.text.clone())),
                SqlType::Star => Ok(ValueExpr::ScopedStar(table_name)),
                _ => { Err(ParseErr{ token: Some(column_token.clone()) }) }
            }
        },
        NodeType::FieldValueMath => {
            let mut value_exprs = vec![try!(parse_field_type(&tree.children[0], true))];
            let mut math_ops = vec![parse_math_op(&tree.children[1])];

            let mut right_child = &tree.children[2];
            while right_child.node_type == NodeType::FieldValueMath {
                value_exprs.push(try!(parse_field_type(&right_child.children[0], true)));
                math_ops.push(parse_math_op(&right_child.children[1]));
                right_child = &right_child.children[2];
            }

            value_exprs.push(try!(parse_field_type(right_child, true)));

            //TODO: Inefficient!
            // 1. Finding min op every iteration
            // 2. Multiple shifts per combination
            while value_exprs.len() > 1 {
                let (i, _) = math_ops.iter().map(|op| op_rank(op)).enumerate().min_by_key(|&(_, op)| op).unwrap();

                let op = math_ops.remove(i);
                let left = value_exprs.remove(i);
                let right = value_exprs.remove(i);

                value_exprs.insert(i, ValueExpr::Math(op, Box::new(left), Box::new(right)));
            }

            Ok(value_exprs.remove(0))
        },
        NodeType::FieldValueParenGroup => {
            assert_tree_sql_type(&tree.children[0], SqlType::OpenParen);
            let child_expr = try!(parse_field_type(&tree.children[1], true));
            assert_tree_sql_type(&tree.children[2], SqlType::CloseParen);

            Ok(child_expr)
        },
        NodeType::FieldValueFunction => {
            assert_tree_sql_type(&tree.children[0], SqlType::Literal);
            let function_name = node_token(&tree.children[0]).text.clone();
            assert_tree_sql_type(&tree.children[1], SqlType::OpenParen);
            assert_tree_sql_type(&tree.children.last().unwrap(), SqlType::CloseParen);

            let args = parse_separated_list(&mut tree.children.iter().skip(2).take(tree.children.len() - 3), |i, child| parse_field_type(child, i == 0));

            args.map(|args| ValueExpr::Function { name: function_name, args: args })
        },
        _ => {
            Err(ParseErr{ token: find_first_token(tree).map(|token| token.clone()) })
        }
    }
}

fn parse_named_field_def(tree: &ParseTree) -> Result<Field, ParseErr> {
    match tree.node_type {
        NodeType::NamedFieldDef => {
            let field_type = try!(parse_field_type(&tree.children[0], true));
            let column_name = node_token(&tree.children[2]).text.clone();

            Ok(Field { alias: Some(column_name), field_type: field_type })
        },
        NodeType::FieldDef => {
            let field_type = try!(parse_field_type(&tree.children[0], true));

            Ok(Field { alias: None, field_type: field_type })
        },
        _ => {
            Err(ParseErr { token: find_first_token(tree).map(|token| token.clone()) })
        }
    }
}

fn parse_selection(tree: &ParseTree) -> Result<Vec<Field>, ParseErr> {
    assert_node_type(tree, NodeType::Selection);

    let mut child_iter = tree.children.iter();
    assert_opt_tree_sql_type(child_iter.next(), SqlType::Select);

    parse_separated_list(&mut child_iter, |_, child| parse_named_field_def(&child) )
}

fn parse_join_type(tree: &ParseTree) -> Result<JoinType, ParseErr> {
    assert_node_type(tree, NodeType::JoinType);

    let first_token = node_token(&tree.children[0]);
    match first_token.sql_type {
        SqlType::Join | SqlType::Inner => Ok(JoinType::Inner),
        SqlType::Left => Ok(JoinType::Left),
        SqlType::Right => Ok(JoinType::Right),
        SqlType::Full => Ok(JoinType::Full),
        _ => { Err(ParseErr { token: Some(first_token.clone()) }) }
    }
}

fn parse_source(tree: &ParseTree) -> Result<Source, ParseErr> {
    match tree.node_type {
        NodeType::SourceTable => {
            let table_name_token = node_token(&tree.children[0]);
            Ok(Source::Table(table_name_token.text.clone()))
        },
        NodeType::ParenGroup => {
            assert_tree_sql_type(&tree.children[0], SqlType::OpenParen);
            let subquery = try!(parse_source(&tree.children[1]));
            assert_tree_sql_type(&tree.children[2], SqlType::CloseParen);

            match subquery {
                Source::Table(_) => {
                    Err(ParseErr { token: find_first_token(&tree.children[1]).map(|token| token.clone()) })
                },
                _ => {
                    Ok(subquery)
                }
            }
        },
        NodeType::SourceJoin => {
            let left = try!(parse_source(&tree.children[0]));
            let join = try!(parse_join_type(&tree.children[1]));
            let right = try!(parse_source(&tree.children[2]));
            assert_tree_sql_type(&tree.children[3], SqlType::On);
            let cond = try!(parse_bool_expr(&tree.children[4]));

            Ok(Source::Join { join_type: join, left: Box::new(left), right: Box::new(right), cond: cond })
        },
        NodeType::Query => {
            let subquery = try!(parse(tree));
            Ok(Source::Query(subquery))
        },
        _ => {
            Err(ParseErr { token: find_first_token(tree).map(|token| token.clone()) })
        }
    }
}

fn parse_sources(tree: &ParseTree) -> Result<Vec<Source>, ParseErr> {
    assert_node_type(tree, NodeType::Source);

    let mut child_iter = tree.children.iter();
    assert_opt_tree_sql_type(child_iter.next(), SqlType::From);

    parse_separated_list(&mut child_iter, |_, child| parse_source(child))
}

fn parse_bool_op(tree: &ParseTree) -> Result<BoolLogic, ParseErr> {
    match node_sql_type(tree) {
        SqlType::And => Ok(BoolLogic::And),
        SqlType::Or => Ok(BoolLogic::Or),
        _ => {
            return Err(ParseErr{ token: find_first_token(&tree.children[1]).map(|token| token.clone()) })
        }
    }
}

fn bool_op_rank(op: &BoolLogic) -> u8 {
    match *op {
        BoolLogic::And => 0,
        BoolLogic::Or => 1
    }
}

fn parse_bool_expr(tree: &ParseTree) -> Result<BoolExpr, ParseErr> {
    match tree.node_type {
        NodeType::ExprBoolLogic => {
            let mut bool_exprs = vec![try!(parse_bool_expr(&tree.children[0]))];
            let mut bool_ops = vec![try!(parse_bool_op(&tree.children[1]))];

            let mut right = &tree.children[2];
            while right.node_type == NodeType::ExprBoolLogic {
                bool_exprs.push(try!(parse_bool_expr(&right.children[0])));
                bool_ops.push(try!(parse_bool_op(&right.children[1])));
                right = &right.children[2];
            }
            bool_exprs.push(try!(parse_bool_expr(right)));

            while bool_exprs.len() > 1 {
                let (i, _) = bool_ops.iter().map(|op| bool_op_rank(op)).enumerate().min_by_key(|&(_, op)| op).unwrap();

                let op = bool_ops.remove(i);
                let left = bool_exprs.remove(i);
                let right = bool_exprs.remove(i);

                bool_exprs.insert(i, BoolExpr::LogicExpr { bool_logic: op, left: Box::new(left), right: Box::new(right) });
            }

            Ok(bool_exprs.remove(0))
        },
        NodeType::ExprBoolCond => {
            let left_value = try!(parse_field_type(&tree.children[0], true));
            let bool_op = match node_sql_type(&tree.children[1]) {
                SqlType::Equal => BoolOp::Equal,
                SqlType::NotEqual => BoolOp::NotEqual,
                SqlType::GreaterThan => BoolOp::GreaterThan,
                SqlType::GreaterThanEqual => BoolOp::GreaterThanEqual,
                SqlType::LessThan => BoolOp::LessThan,
                SqlType::LessThanEqual => BoolOp::LessThanEqual,
                _ => {
                    return Err(ParseErr{ token: find_first_token(&tree.children[1]).map(|token| token.clone()) });
                }
            };
            let right_value = try!(parse_field_type(&tree.children[2], true));

            Ok(BoolExpr::Cond { left: left_value, bool_op: bool_op, right: right_value })
        },
        NodeType::ExprParenGroup => {
            assert_tree_sql_type(&tree.children[0], SqlType::OpenParen);
            let sub_expr = try!(parse_bool_expr(&tree.children[1]));
            assert_tree_sql_type(&tree.children[2], SqlType::CloseParen);
            Ok(sub_expr)
        },
        _ => {
            Err(ParseErr{ token: find_first_token(&tree.children[1]).map(|token| token.clone()) })
        }
    }
}

fn parse_filter(tree: &ParseTree) -> Result<BoolExpr, ParseErr> {
    assert_node_type(tree, NodeType::Filter);
    assert_tree_sql_type(&tree.children[0], SqlType::Where);

    parse_bool_expr(&tree.children[1])
}

fn parse_groups(tree: &ParseTree) -> Result<Vec<Grouping>, ParseErr> {
    assert_node_type(tree, NodeType::Grouping);

    let mut child_iter = tree.children.iter();
    assert_opt_tree_sql_type(child_iter.next(), SqlType::Group);
    assert_opt_tree_sql_type(child_iter.next(), SqlType::By);

    parse_separated_list(&mut child_iter, |_, child| {
        let group_token = node_token(child);
        Ok(Grouping(group_token.text.clone()))
    })
}

fn parse_having(tree: &ParseTree) -> Result<BoolExpr, ParseErr> {
    assert_node_type(tree, NodeType::Having);

    assert_tree_sql_type(&tree.children[0], SqlType::Having);
    //TODO: Verify having types
    let bool_expr = try!(parse_bool_expr(&tree.children[1]));
    Ok(bool_expr)
}

fn parse_order(tree: &ParseTree) -> Result<Vec<Order>, ParseErr> {
    assert_node_type(tree, NodeType::Sort);

    let mut child_iter = tree.children.iter();
    assert_opt_tree_sql_type(child_iter.next(), SqlType::Order);
    assert_opt_tree_sql_type(child_iter.next(), SqlType::By);

    parse_separated_list(&mut child_iter, |_, child| {
        assert_node_type(child, NodeType::SortField);

        let field_name = typed_node_token(&child.children[0], SqlType::Literal).text.clone();

        let dir = if let Some(dir_tree) = child.children.get(1) {
            let dir_token = node_token(&dir_tree);
            match dir_token.sql_type {
                SqlType::Asc => Direction::Asc,
                SqlType::Desc => Direction::Desc,
                _ => { return Err(ParseErr { token: Some(dir_token.clone()) }) }
            }
        } else {
            Direction::Asc
        };

        Ok(Order(field_name, dir))
    })
}

fn parse_limit(tree: &ParseTree) -> Result<Limit, ParseErr> {
    assert_node_type(tree, NodeType::Limit);

    assert_tree_sql_type(&tree.children[0], SqlType::Limit);

    let token = node_token(&tree.children[1]);
    match token.sql_type {
        SqlType::All => {
            Ok(Limit::All)
        },
        SqlType::Int => {
            let limit = token.text.parse::<i64>().unwrap();
            if limit >= 0 {
                Ok(Limit::Amount(limit))
            } else {
                Err(ParseErr { token: Some(token.clone()) })
            }
        },
        _ => {
            Err(ParseErr { token: Some(token.clone()) })
        }
    }
}

fn parse_offset(tree: &ParseTree) -> Result<i64, ParseErr> {
    assert_node_type(tree, NodeType::Offset);

    assert_tree_sql_type(&tree.children[0], SqlType::Offset);

    let token = node_token(&tree.children[1]);
    match token.sql_type {
        SqlType::Int => {
            let offset = token.text.parse::<i64>().unwrap();
            if offset >= 0 {
                Ok(offset)
            } else {
                Err(ParseErr { token: Some(token.clone()) })
            }
        },
        _ => {
            Err(ParseErr { token: Some(token.clone()) })
        }
    }
}

pub fn parse(tree: &ParseTree) -> Result<Query, ParseErr> {
    let mut child_iter = tree.children.iter();
    let mut current_child = child_iter.next();

    let fields = try!(parse_selection(current_child.as_ref().unwrap()));
    current_child = child_iter.next();

    let sources = if is_node_type(current_child, NodeType::Source) {
        let sources = try!(parse_sources(current_child.unwrap()));
        current_child = child_iter.next();
        sources
    } else {
        vec![]
    };

    let filter = if is_node_type(current_child, NodeType::Filter) {
        let filter = try!(parse_filter(current_child.unwrap()));
        current_child = child_iter.next();
        Some(filter)
    } else {
        None
    };

    let mut groups = vec![];
    if is_node_type(current_child, NodeType::Grouping) {
        groups = try!(parse_groups(current_child.unwrap()));
        current_child = child_iter.next();
    }

    let having = if is_node_type(current_child, NodeType::Having) {
        let having = try!(parse_having(current_child.unwrap()));
        current_child = child_iter.next();
        Some(having)
    } else {
        None
    };

    let orders = if is_node_type(current_child, NodeType::Sort) {
        let orders = try!(parse_order(current_child.unwrap()));
        current_child = child_iter.next();
        orders
    } else {
        vec![]
    };

    let limit = if is_node_type(current_child, NodeType::Limit) {
        let limit = try!(parse_limit(current_child.unwrap()));
        current_child = child_iter.next();
        limit
    } else {
        Limit::All
    };

    let offset = if is_node_type(current_child, NodeType::Offset) {
        let offset = try!(parse_offset(current_child.unwrap()));
        offset
    } else {
        0i64
    };

    Ok(Query {
        fields: fields,
        sources: sources,
        filter: filter,
        groups: groups,
        having: having,
        orders: orders,
        limit: limit,
        offset: offset
    })
}

fn find_first_token(tree: &ParseTree) -> Option<&Token> {
    let mut current_tree_opt = Some(tree);

    while let Some(current_tree) = current_tree_opt {
        match current_tree.node_type {
            NodeType::Concrete(ref token) => { return Some(token) },
            _ => {
                current_tree_opt = current_tree.children.get(0);
            }
        }
    }

    None
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

fn parse_separated_list <'a, I, F, T> (iter: &mut I, parse_fn: F) -> Result<Vec<T>, ParseErr>
    where I: Iterator<Item = &'a ParseTree>, F: Fn(usize, &ParseTree) -> Result<T, ParseErr> {
    let mut count = 0;

    let mut last_tree_opt = None;

    let parsed_list = iter.enumerate().filter_map(|(i, child)| {
        count += 1;
        last_tree_opt = Some(child);

        if i & 1 == 0 {
            Some(parse_fn(i, child))
        } else {
            assert_tree_sql_type(child, SqlType::Separator);
            None
        }
    }).collect::<Result<Vec<T>, ParseErr>>();

    if count == 0 {
        // Empty
        Err(ParseErr{ token: None })
    } else if count & 1 == 1 {
        parsed_list
    } else {
        // Dangling separator
        if let Some(last_tree) = last_tree_opt {
            Err(ParseErr{ token: find_first_token(last_tree).map(|token| token.clone()) })
        } else {
            Err(ParseErr{ token: None })
        }
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

        let query = parse(&parse_tree).unwrap();
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

        let query = parse(&parse_tree).unwrap();
        assert_eq!(query.groups, vec![Grouping("age".to_string())]);

        let function_call = ValueExpr::Function { name: "count".to_string(), args: vec![ValueExpr::Star] };
        assert_eq!(query.having, Some(BoolExpr::Cond { left: function_call, bool_op: BoolOp::GreaterThan, right: ValueExpr::Primitive("2".to_string()) }));
    }

    #[test]
    fn test_orders() {
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select * from people order by age desc, name")).unwrap();

        let query = parse(&parse_tree).unwrap();
        assert_eq!(query.orders, vec![Order("age".to_string(), Direction::Desc), Order("name".to_string(), Direction::Asc)]);
    }

    #[test]
    fn test_scoped_field_select() {
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select people.name as lolz from people")).unwrap();

        let query = parse(&parse_tree).unwrap();
        assert_eq!(&query.fields, &vec![ Field{ alias: Some("lolz".to_string()), field_type: ValueExpr::ScopedLiteral("people".to_string(), "name".to_string()) }, ]);
    }

    #[test]
    fn test_scoped_star_select() {
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select people.* from people")).unwrap();

        let query = parse(&parse_tree).unwrap();
        assert_eq!(&query.fields, &vec![ Field{ alias: None, field_type: ValueExpr::ScopedStar("people".to_string()) }, ]);
    }

    #[test]
    fn test_query_bool_precedence() {
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select * from people where age > 1 and age > 2 or age > 3 and (age > 4 or age > 5)")).unwrap();

        let query = parse(&parse_tree).unwrap();

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

        let query = parse(&parse_tree).unwrap();

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

        let query = parse(&parse_tree).unwrap();

        let func = ValueExpr::Function{ name: "count".to_string(), args: vec![ValueExpr::Star] };
        let mult = ValueExpr::Math(MathOp::Multiply, Box::new(ValueExpr::Primitive("2".to_string())), Box::new(func));
        let add = ValueExpr::Math(MathOp::Add, Box::new(mult), Box::new(ValueExpr::Primitive("1".to_string())));

        assert_eq!(query.fields, vec![Field { alias: None, field_type: add }]);
    }

    #[test]
    fn test_double_star() {
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select count(*, *) from bananas")).unwrap();

        assert!(parse(&parse_tree).is_err());
    }

    #[test]
    fn test_select_subquery() {
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select * from (select * from bananas)")).unwrap();
        parse(&parse_tree).unwrap();
    }

    #[test]
    fn test_select_join() {
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select * from (people inner join pets on people.id = pets.person_id)")).unwrap();
        parse(&parse_tree).unwrap();
    }

    #[test]
    fn test_select_table_parens() {
        let parse_tree = concrete_tree::parse(SqlTokenizer::new(&"select * from (people)")).unwrap();
        assert!(parse(&parse_tree).is_err());
    }
}
