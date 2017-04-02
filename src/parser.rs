use std::iter::Iterator;
use std::iter::Peekable;

use lexer::{Token, SqlType};

#[derive(PartialEq, Debug, Clone)]
pub enum NodeType {
    FieldDef,
    NamedFieldDef,

    Query,
    Selection,
    Source,
    Filter,
    Expression,
    Condition,
    Limit,
    Offset,
    FieldSelection,
    Grouping,
    Having,
    Sort,
    Table,

    Concrete(Token)
}

#[derive(Debug, Clone)]
pub struct ParseErr {
    pub invalid_token: Option<Token>,
    pub sql_types: Vec<SqlType>
}

impl ParseErr {
    fn new(token: Option<Token>, sql_type: SqlType) -> ParseErr {
        ParseErr { invalid_token: token, sql_types: vec![sql_type] }
    }

    fn new_multi(token: Option<Token>, sql_types: Vec<SqlType>) -> ParseErr {
        ParseErr { invalid_token: token, sql_types: sql_types }
    }
}

pub struct ParseTree {
    pub node_type: NodeType,
    pub children: Vec<ParseTree>
}

impl ParseTree {
    fn new_leaf(token: Token) -> ParseTree {
        ParseTree {
            node_type: NodeType::Concrete(token),
            children: vec![]
        }
    }
}

fn peek_sql_type <T> (lexer: &mut Peekable<T>) -> Option<SqlType>
    where T: Iterator<Item = Token> {
    lexer.peek().as_ref().map(|token| token.sql_type.clone())
}

fn parse_token <T> (lexer: &mut Peekable<T>, sql_type: SqlType) -> Result<Token, ParseErr>
    where T: Iterator<Item = Token> {
    if let Some(token) = lexer.next() {
        if token.sql_type == sql_type {
            Ok(token)
        } else {
            Err(ParseErr::new(Some(token), sql_type))
        }
    } else {
        Err(ParseErr::new(None, sql_type))
    }
}

fn parse_any_token <T> (lexer: &mut Peekable<T>, sql_types: &Vec<SqlType>) -> Result<Token, ParseErr>
    where T: Iterator<Item = Token> {
    if let Some(token) = lexer.next() {
        if sql_types.iter().any(|sql_type| &token.sql_type == sql_type) {
            Ok(token)
        } else {
            Err(ParseErr::new_multi(Some(token), sql_types.clone()))
        }
    } else {
        Err(ParseErr::new_multi(None, sql_types.clone()))
    }
}

fn parse_optional_token <T> (lexer: &mut Peekable<T>, sql_type: SqlType) -> Option<Token>
    where T: Iterator<Item = Token> {
    if peek_sql_type(lexer) == Some(sql_type) {
        Some(parse_token(lexer, sql_type).unwrap())
    } else {
        None
    }
}

fn parse_field_def <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {

    let mut children = vec![];
    let field_types = vec![SqlType::Int, SqlType::Float, SqlType::Text, SqlType::Literal, SqlType::Star];

    let token = try!(parse_any_token(lexer, &field_types));
    match token.sql_type {
        SqlType::Literal => {
            if let Some(dot_token) = parse_optional_token(lexer, SqlType::Dot) {
                let scoped_types = vec![SqlType::Star, SqlType::Literal];
                let scoped_token = try!(parse_any_token(lexer, &scoped_types));

                children.push(ParseTree::new_leaf(token));
                children.push(ParseTree::new_leaf(dot_token));
                children.push(ParseTree::new_leaf(scoped_token));
            } else if let Some(open_paren_token) = parse_optional_token(lexer, SqlType::OpenParen) {
                children.push(ParseTree::new_leaf(token));
                children.push(ParseTree::new_leaf(open_paren_token));

                if let Some(close_paren_token) = parse_optional_token(lexer, SqlType::CloseParen) {
                    children.push(ParseTree::new_leaf(close_paren_token));
                } else if let Some(star_token) = parse_optional_token(lexer, SqlType::Star) {
                    children.push(ParseTree::new_leaf(star_token));
                    let close_paren_token = try!(parse_token(lexer, SqlType::CloseParen));
                    children.push(ParseTree::new_leaf(close_paren_token));
                } else {
                    let delimiter_types = vec![SqlType::Separator, SqlType::CloseParen];

                    loop {
                        children.push(try!(parse_field_def(lexer)));

                        let next_token = try!(parse_any_token(lexer, &delimiter_types));
                        match next_token.sql_type {
                            SqlType::Separator => {
                                children.push(ParseTree::new_leaf(next_token));
                            },
                            SqlType::CloseParen => {
                                children.push(ParseTree::new_leaf(next_token));
                                break;
                            },
                            _ => {
                                panic!("Never get here");
                            }
                        }
                    }
                }
            } else {
                children.push(ParseTree::new_leaf(token));
            }
        },
        SqlType::Int | SqlType::Float | SqlType::Text | SqlType::Star => {
            children.push(ParseTree::new_leaf(token));
        },
        _ => {
            panic!("Never get here");
        }
    }

    Ok(ParseTree { node_type: NodeType::FieldSelection, children: children })
}

fn parse_field_selection <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {

    let mut children = vec![try!(parse_field_def(lexer))];

    if let Some(as_token) = parse_optional_token(lexer, SqlType::As) {
        let literal_token = try!(parse_token(lexer, SqlType::Literal));

        children.push(ParseTree::new_leaf(as_token));
        children.push(ParseTree::new_leaf(literal_token));

        Ok(ParseTree { node_type: NodeType::NamedFieldDef, children: children })
    } else {
        Ok(ParseTree { node_type: NodeType::FieldDef, children: children })
    }
}

fn parse_select <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {
    let select_token = try!(parse_token(lexer, SqlType::Select));

    let mut children = Vec::new();
    children.push(ParseTree::new_leaf(select_token));

    if let Some(sql_type) = peek_sql_type(lexer) {
        match sql_type {
            SqlType::Literal | SqlType::Int | SqlType::Float | SqlType::Text | SqlType::Star => {
                children.push(try!(parse_field_selection(lexer)));

                while peek_sql_type(lexer) == Some(SqlType::Separator) {
                    let sep_token = try!(parse_token(lexer, SqlType::Separator));
                    children.push(ParseTree::new_leaf(sep_token));
                    children.push(try!(parse_field_selection(lexer)));
                }
            },
            _ => {}
        }
    }

    Ok(ParseTree { node_type: NodeType::Selection, children: children })
}

fn parse_from_join_type <T> (lexer: &mut Peekable<T>) -> Option<Result<ParseTree, ParseErr>>
    where T: Iterator<Item = Token> {

    let mut children = Vec::new();

    if let Some(sql_type) = peek_sql_type(lexer) {
        match sql_type {
            SqlType::Left | SqlType::Right | SqlType::Full => {
                let outer_join_types = vec![SqlType::Left, SqlType::Right, SqlType::Full];
                let type_token = parse_any_token(lexer, &outer_join_types).unwrap();
                children.push(ParseTree::new_leaf(type_token));

                if let Some(outer_token) = parse_optional_token(lexer, SqlType::Outer) {
                    children.push(ParseTree::new_leaf(outer_token));
                }
            },
            SqlType::Inner => {
                let inner_token = parse_token(lexer, SqlType::Inner).unwrap();
                children.push(ParseTree::new_leaf(inner_token));
            },
            SqlType::Join => { },
            _ => {
                return None
            }
        }
    } else {
        return None
    }

    match parse_token(lexer, SqlType::Join) {
        Ok(join_token) => {
            children.push(ParseTree::new_leaf(join_token));
            Some(Ok(ParseTree { node_type: NodeType::Table, children: children }))
        },
        Err(parse_err) => {
            Some(Err(parse_err))
        }
    }
}

fn parse_from_table <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {
    let table_token = try!(parse_token(lexer, SqlType::Literal));

    let mut children = Vec::new();
    children.push(ParseTree::new_leaf(table_token));

    if let Some(parse_join_result) = parse_from_join_type(lexer) {
        children.push(try!(parse_join_result));

        let join_table_token = try!(parse_token(lexer, SqlType::Literal));
        children.push(ParseTree::new_leaf(join_table_token));

        let on_token = try!(parse_token(lexer, SqlType::On));
        children.push(ParseTree::new_leaf(on_token));

        let expr_tree = try!(parse_bool_expr(lexer));
        children.push(expr_tree);
    }

    Ok(ParseTree { node_type: NodeType::Table, children: children })
}

fn parse_from <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {
    let from_token = try!(parse_token(lexer, SqlType::From));

    let mut children = Vec::new();
    children.push(ParseTree::new_leaf(from_token));
    children.push(try!(parse_from_table(lexer)));

    while let Some(separator_token) = parse_optional_token(lexer, SqlType::Separator) {
        children.push(ParseTree::new_leaf(separator_token));
        children.push(try!(parse_from_table(lexer)));
    }

    Ok(ParseTree { node_type: NodeType::Source, children: children })
}

fn parse_value_expr <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {

    let var_types = vec![SqlType::Literal, SqlType::Int, SqlType::Float, SqlType::Text];

    let token = try!(parse_any_token(lexer, &var_types));
    let mut children = vec![];

    match token.sql_type {
        SqlType::Literal => {
            children.push(ParseTree::new_leaf(token));
            if let Some(dot_token) = parse_optional_token(lexer, SqlType::Dot) {
                children.push(ParseTree::new_leaf(dot_token));
                let qualified_types = vec![SqlType::Literal, SqlType::Star];
                let qualified_token = try!(parse_any_token(lexer, &qualified_types));
                children.push(ParseTree::new_leaf(qualified_token));
            } else if let Some(open_paren_token) = parse_optional_token(lexer, SqlType::OpenParen) {
                children.push(ParseTree::new_leaf(open_paren_token));

                if let Some(star_token) = parse_optional_token(lexer, SqlType::Star) {
                    children.push(ParseTree::new_leaf(star_token));
                } else {
                    if peek_sql_type(lexer) != Some(SqlType::CloseParen) {
                        loop {
                            let arg_tree = try!(parse_value_expr(lexer));
                            children.push(arg_tree);

                            if let Some(separator_token) = parse_optional_token(lexer, SqlType::Separator) {
                                children.push(ParseTree::new_leaf(separator_token));
                            } else {
                                break;
                            }
                        }
                    }
                }

                let close_paren_token = try!(parse_token(lexer, SqlType::CloseParen));
                children.push(ParseTree::new_leaf(close_paren_token));
            }
        },
        _ => {
            children.push(ParseTree::new_leaf(token));
        }
    }

    Ok(ParseTree { node_type: NodeType::Condition, children: children })
}

fn parse_bool_expr <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {

    let mut children = vec![];

    if let Some(open_paren_token) = parse_optional_token(lexer, SqlType::OpenParen) {
        let sub_expr = try!(parse_bool_expr(lexer));
        let close_paren_token = try!(parse_token(lexer, SqlType::CloseParen));

        children.push(ParseTree::new_leaf(open_paren_token));
        children.push(sub_expr);
        children.push(ParseTree::new_leaf(close_paren_token));
        return Ok(ParseTree { node_type: NodeType::Expression, children: children });
    }

    let bool_op_types = vec![
        SqlType::GreaterThanEqual,
        SqlType::GreaterThan,
        SqlType::LessThanEqual,
        SqlType::LessThan,
        SqlType::Equal,
        SqlType::NotEqual
    ];

    let left_value_expr = try!(parse_value_expr(lexer));
    let bool_op_token = try!(parse_any_token(lexer, &bool_op_types));
    let right_value_expr = try!(parse_value_expr(lexer));

    children.push(left_value_expr);
    children.push(ParseTree::new_leaf(bool_op_token));
    children.push(right_value_expr);

    let bool_tree = ParseTree { node_type: NodeType::Expression, children: children };

    if let Some(sql_type) = peek_sql_type(lexer) {
        match sql_type {
            SqlType::And | SqlType::Or => {
                let cond_types = vec![SqlType::And, SqlType::Or];
                let cond_token = try!(parse_any_token(lexer, &cond_types));
                let right_bool_expr = try!(parse_bool_expr(lexer));

                let cond_tree = ParseTree { node_type: NodeType::Expression, children: vec![bool_tree, ParseTree::new_leaf(cond_token), right_bool_expr] };

                return Ok(cond_tree)
            },
            _ => { }
        }
    }

    Ok(bool_tree)
}

fn parse_where <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {
    let where_token = try!(parse_token(lexer, SqlType::Where));
    let expr_tree = try!(parse_bool_expr(lexer));

    let children = vec![
        ParseTree::new_leaf(where_token),
        expr_tree
    ];

    Ok(ParseTree { node_type: NodeType::Filter, children: children})
}

fn parse_limit <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {
    let limit_token = try!(parse_token(lexer, SqlType::Limit));

    let amount_types = vec![SqlType::Int, SqlType::All];
    let amount_token = try!(parse_any_token(lexer, &amount_types));

    let children = vec![
        ParseTree::new_leaf(limit_token),
        ParseTree::new_leaf(amount_token),
    ];

    Ok(ParseTree { node_type: NodeType::Limit, children: children})
}

fn parse_offset <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {
    let offset_token = try!(parse_token(lexer, SqlType::Offset));
    let int_token = try!(parse_token(lexer, SqlType::Int));

    let children = vec![
        ParseTree::new_leaf(offset_token),
        ParseTree::new_leaf(int_token),
    ];

    Ok(ParseTree { node_type: NodeType::Offset, children: children})
}

fn parse_group_field <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {
    let field_token = try!(parse_token(lexer, SqlType::Literal));

    if let Some(separator_token) = parse_optional_token(lexer, SqlType::Separator) {
        let children = vec![ParseTree::new_leaf(field_token), ParseTree::new_leaf(separator_token), try!(parse_group_field(lexer))];
        Ok(ParseTree { node_type: NodeType::Grouping, children: children })
    } else {
        Ok(ParseTree::new_leaf(field_token))
    }
}

fn parse_grouping <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {
    let group_token = try!(parse_token(lexer, SqlType::Group));
    let by_token = try!(parse_token(lexer, SqlType::By));

    let children = vec![
        ParseTree::new_leaf(group_token),
        ParseTree::new_leaf(by_token),
        try!(parse_group_field(lexer))
    ];

    Ok(ParseTree { node_type: NodeType::Grouping, children: children})
}

//fn parse_having_field <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    //where T: Iterator<Item = Token> {

    //let field_types = vec![SqlType::Literal, SqlType::Star];
    //let assertion_types = vec![SqlType::GreaterThan, SqlType::GreaterThanEqual, SqlType::LessThan, SqlType::LessThanEqual, SqlType::Equal];
    //let compare_types = vec![SqlType::Int, SqlType::Float, SqlType::Text];
    //let bool_op_types = vec![SqlType::And, SqlType::Or];

    //let name_token = try!(parse_token(lexer, SqlType::Literal));
    //let open_paren_token = try!(parse_token(lexer, SqlType::OpenParen));
    //let arg_token = try!(parse_any_token(lexer, &field_types));
    //let close_paren_token = try!(parse_token(lexer, SqlType::CloseParen));
    //let assertion_token = try!(parse_any_token(lexer, &assertion_types));
    //let compare_token = try!(parse_any_token(lexer, &compare_types));

    //let children = vec![
        //ParseTree::new_leaf(name_token),
        //ParseTree::new_leaf(open_paren_token),
        //ParseTree::new_leaf(arg_token),
        //ParseTree::new_leaf(close_paren_token),
        //ParseTree::new_leaf(assertion_token),
        //ParseTree::new_leaf(compare_token),
    //];

    //let field_tree = ParseTree { node_type: NodeType::Having, children: children };

    //let next_sql_type = peek_sql_type(lexer);
    //if next_sql_type == Some(SqlType::And) || next_sql_type == Some(SqlType::Or) {
        //let mut expr_children = vec![
            //field_tree,
            //ParseTree::new_leaf(try!(parse_any_token(lexer, &bool_op_types))),
            //try!(parse_having_field(lexer))
        //];
        //Ok(ParseTree { node_type: NodeType::Expression, children: expr_children })
    //} else {
        //Ok(field_tree)
    //}
//}

fn parse_having <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {
    let having_token = try!(parse_token(lexer, SqlType::Having));

    let children = vec![
        ParseTree::new_leaf(having_token),
        try!(parse_bool_expr(lexer))
    ];

    Ok(ParseTree { node_type: NodeType::Having, children: children})
}

fn parse_order_field <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {
    let field_token = try!(parse_token(lexer, SqlType::Literal));

    let mut children = vec![
        ParseTree::new_leaf(field_token)
    ];

    if let Some(asc_token) = parse_optional_token(lexer, SqlType::Asc) {
        children.push(ParseTree::new_leaf(asc_token));
    } else if let Some(desc_token) = parse_optional_token(lexer, SqlType::Desc) {
        children.push(ParseTree::new_leaf(desc_token));
    }

    Ok(ParseTree { node_type: NodeType::Sort, children: children})
}

fn parse_order <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {
    let order_token = try!(parse_token(lexer, SqlType::Order));
    let by_token = try!(parse_token(lexer, SqlType::By));

    let mut children = vec![
        ParseTree::new_leaf(order_token),
        ParseTree::new_leaf(by_token),
        try!(parse_order_field(lexer))
    ];

    while let Some(separator_token) = parse_optional_token(lexer, SqlType::Separator) {
        children.push(ParseTree::new_leaf(separator_token));
        children.push(try!(parse_order_field(lexer)));
    }

    Ok(ParseTree { node_type: NodeType::Sort, children: children})
}

fn parse_query <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {
    let mut children = Vec::new();

    children.push(try!(parse_select(lexer)));

    if peek_sql_type(lexer) == Some(SqlType::From) {
        children.push(try!(parse_from(lexer)));
    }

    if peek_sql_type(lexer) == Some(SqlType::Where) {
        children.push(try!(parse_where(lexer)));
    }

    if peek_sql_type(lexer) == Some(SqlType::Group) {
        children.push(try!(parse_grouping(lexer)));

        if peek_sql_type(lexer) == Some(SqlType::Having) {
            children.push(try!(parse_having(lexer)));
        }
    }

    if peek_sql_type(lexer) == Some(SqlType::Order) {
        children.push(try!(parse_order(lexer)));
    }

    if peek_sql_type(lexer) == Some(SqlType::Limit) {
        children.push(try!(parse_limit(lexer)));
    }

    if peek_sql_type(lexer) == Some(SqlType::Offset) {
        children.push(try!(parse_offset(lexer)));
    }

    Ok(ParseTree { node_type: NodeType::Query, children: children })
}

pub fn parse <T> (lexer: T) -> Result<ParseTree, ParseErr>
  where T: Iterator<Item = Token> {
    let mut peekable_lexer = Box::new(lexer.peekable());
    let tree = try!(parse_query(&mut peekable_lexer));

    if let Some(next_token) = peekable_lexer.next() {
        Err(ParseErr { invalid_token: Some(next_token), sql_types: vec![] })
    } else {
        Ok(tree)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::{Token, SqlTokenizer};

    fn find_sql_type(tree: &ParseTree, child_indexes: &[usize]) -> SqlType {
        let mut current_tree = tree;
        for i in child_indexes {
            current_tree = &current_tree.children[*i];
        }

        match current_tree.node_type {
            NodeType::Concrete(ref token) => token.sql_type.clone(),
            _ => panic!("Not a concrete type: {:?}", current_tree.node_type)
        }
    }

    fn find_node_type(tree: &ParseTree, child_indexes: &[usize]) -> NodeType {
        let mut current_tree = tree;
        for i in child_indexes {
            current_tree = &current_tree.children[*i];
        }

        current_tree.node_type.clone()
    }

    #[test]
    fn test_simple_query() {
        let parse_tree = parse(SqlTokenizer::new(&"select a, b from people where a > 1 and a < 10")).unwrap();
        // 0 query
        //   0 selection
        //     0 select
        //     1 field_def
        //       0 field
        //         0 a
        //     2 ,
        //     3 field_def
        //       0 field
        //         0 b
        //  1 source
        //    0 from
        //    1 table
        //      0 people
        //  2 filter
        //    0 where
        //    1 expr
        //      0 expr
        //        0 cond
        //          0 a
        //        1 >
        //        2 cond
        //          0 1
        //      1 and
        //      2 expr
        //        0 cond
        //          0 a
        //        1 <
        //        2 cond
        //          0 10

        assert_eq!(find_node_type(&parse_tree, &[]), NodeType::Query);

        assert_eq!(find_node_type(&parse_tree, &[0]), NodeType::Selection);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 0]), SqlType::Select);
        assert_eq!(find_node_type(&parse_tree, &[0, 1]), NodeType::FieldDef);
        assert_eq!(find_node_type(&parse_tree, &[0, 1, 0]), NodeType::FieldSelection);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 1, 0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 2]), SqlType::Separator);
        assert_eq!(find_node_type(&parse_tree, &[0, 3]), NodeType::FieldDef);
        assert_eq!(find_node_type(&parse_tree, &[0, 3, 0]), NodeType::FieldSelection);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 3, 0, 0]), SqlType::Literal);

        assert_eq!(find_node_type(&parse_tree, &[1]), NodeType::Source);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 0]), SqlType::From);
        assert_eq!(find_node_type(&parse_tree, &[1, 1]), NodeType::Table);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 0]), SqlType::Literal);

        assert_eq!(find_node_type(&parse_tree, &[2]), NodeType::Filter);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 0]), SqlType::Where);
        assert_eq!(find_node_type(&parse_tree, &[2, 1]), NodeType::Expression);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 0]), NodeType::Expression);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 0, 0]), NodeType::Condition);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 0, 0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 0, 1]), SqlType::GreaterThan);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 0, 2]), NodeType::Condition);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 0, 2, 0]), SqlType::Int);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 1]), SqlType::And);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 2, 0]), NodeType::Condition);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 2, 0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 2, 1]), SqlType::LessThan);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 2, 2]), NodeType::Condition);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 2, 2, 0]), SqlType::Int);
    }

    #[test]
    fn test_select_function() {
        let parse_tree = parse(SqlTokenizer::new(&"select \"my age\", count(*) as num_people from people group by age having count(*) > 3 order by age desc limit 1 offset 2")).unwrap();
        // 0 query
        //   0 selection
        //     0 select
        //     1 field_def
        //       0 field
        //         0 a
        //     2 ,
        //     3 named_field_def
        //       0 field
        //         0 count
        //         1 (
        //         2 *
        //         3 )
        //       1 as
        //       2 num_people
        //  1 source
        //    0 from
        //    1 table
        //      0 people
        //  2 grouping
        //    0 group
        //    1 by
        //    2 age
        //  3 having
        //    0 having
        //    1 field
        //      0 expr
        //        0 count
        //        1 (
        //        2 *
        //        3 )
        //      4 >
        //      5 expr
        //        0 3
        //  4 sort
        //    0 order
        //    1 by
        //    2 sort
        //      0 age
        //  5 limitation
        //    0 limit
        //    1 2
        //  6 offset
        //    0 offset
        //    1 1

        assert_eq!(find_node_type(&parse_tree, &[]), NodeType::Query);

        assert_eq!(find_node_type(&parse_tree, &[0]), NodeType::Selection);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 0]), SqlType::Select);
        assert_eq!(find_node_type(&parse_tree, &[0, 1]), NodeType::FieldDef);
        assert_eq!(find_node_type(&parse_tree, &[0, 1, 0]), NodeType::FieldSelection);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 1, 0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 2]), SqlType::Separator);
        assert_eq!(find_node_type(&parse_tree, &[0, 3]), NodeType::NamedFieldDef);
        assert_eq!(find_node_type(&parse_tree, &[0, 3, 0]), NodeType::FieldSelection);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 3, 0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 3, 0, 1]), SqlType::OpenParen);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 3, 0, 2]), SqlType::Star);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 3, 0, 3]), SqlType::CloseParen);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 3, 1]), SqlType::As);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 3, 2]), SqlType::Literal);

        assert_eq!(find_node_type(&parse_tree, &[1]), NodeType::Source);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 0]), SqlType::From);
        assert_eq!(find_node_type(&parse_tree, &[1, 1]), NodeType::Table);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 0]), SqlType::Literal);

        assert_eq!(find_node_type(&parse_tree, &[2]), NodeType::Grouping);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 0]), SqlType::Group);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1]), SqlType::By);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 2]), SqlType::Literal);

        assert_eq!(find_node_type(&parse_tree, &[3]), NodeType::Having);
        assert_eq!(find_sql_type(&parse_tree,  &[3, 0]), SqlType::Having);
        assert_eq!(find_node_type(&parse_tree, &[3, 1, 0]), NodeType::Condition);
        assert_eq!(find_sql_type(&parse_tree,  &[3, 1, 0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[3, 1, 0, 1]), SqlType::OpenParen);
        assert_eq!(find_sql_type(&parse_tree,  &[3, 1, 0, 2]), SqlType::Star);
        assert_eq!(find_sql_type(&parse_tree,  &[3, 1, 0, 3]), SqlType::CloseParen);
        assert_eq!(find_sql_type(&parse_tree,  &[3, 1, 1]), SqlType::GreaterThan);
        assert_eq!(find_node_type(&parse_tree, &[3, 1, 2]), NodeType::Condition);
        assert_eq!(find_sql_type(&parse_tree,  &[3, 1, 2, 0]), SqlType::Int);

        assert_eq!(find_node_type(&parse_tree, &[4]), NodeType::Sort);
        assert_eq!(find_sql_type(&parse_tree,  &[4, 0]), SqlType::Order);
        assert_eq!(find_sql_type(&parse_tree,  &[4, 1]), SqlType::By);
        assert_eq!(find_sql_type(&parse_tree,  &[4, 2, 0]), SqlType::Literal);

        assert_eq!(find_node_type(&parse_tree, &[5]), NodeType::Limit);
        assert_eq!(find_sql_type(&parse_tree,  &[5, 0]), SqlType::Limit);
        assert_eq!(find_sql_type(&parse_tree,  &[5, 1]), SqlType::Int);

        assert_eq!(find_node_type(&parse_tree, &[6]), NodeType::Offset);
        assert_eq!(find_sql_type(&parse_tree,  &[6, 0]), SqlType::Offset);
        assert_eq!(find_sql_type(&parse_tree,  &[6, 1]), SqlType::Int);
    }

    #[test]
    fn test_simple_error() {
        let parse_tree = parse(SqlTokenizer::new(&"select a b from people"));
        assert!(parse_tree.is_err());
    }

    #[test]
    fn test_select_star_as() {
        let parse_tree = parse(SqlTokenizer::new(&"select * as logical_error from people"));
        assert!(parse_tree.is_ok());
    }

    #[test]
    fn test_select_scoped_star_as() {
        let parse_tree = parse(SqlTokenizer::new(&"select people.* as ok from people"));
        assert!(parse_tree.is_ok());
    }

    #[test]
    fn test_select_value() {
        let parse_tree = parse(SqlTokenizer::new(&"select 1 as ok from people"));
        assert!(parse_tree.is_ok());
    }

    #[test]
    fn test_select_double_star() {
        let parse_tree = parse(SqlTokenizer::new(&"select count(*, *) from people"));
        assert!(parse_tree.is_err());
    }

    #[test]
    fn test_from_cross() {
        let parse_tree = parse(SqlTokenizer::new(&"select * from people, pets")).unwrap();
        // 0 query
        //   0 selection
        //     0 select
        //     1 field_def
        //       0 field
        //         0 star
        //  1 source
        //    0 from
        //    1 table
        //      1 people
        //    2 ,
        //    3 table
        //      0 pets

        assert_eq!(find_node_type(&parse_tree, &[]), NodeType::Query);

        assert_eq!(find_node_type(&parse_tree, &[0]), NodeType::Selection);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 0]), SqlType::Select);
        assert_eq!(find_node_type(&parse_tree, &[0, 1]), NodeType::FieldDef);
        assert_eq!(find_node_type(&parse_tree, &[0, 1, 0]), NodeType::FieldSelection);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 1, 0, 0]), SqlType::Star);

        assert_eq!(find_node_type(&parse_tree, &[1]), NodeType::Source);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 0]), SqlType::From);
        assert_eq!(find_node_type(&parse_tree, &[1, 1]), NodeType::Table);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 2]), SqlType::Separator);
        assert_eq!(find_node_type(&parse_tree, &[1, 3]), NodeType::Table);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 3, 0]), SqlType::Literal);
    }

    #[test]
    fn test_from_join() {
        let parse_tree = parse(SqlTokenizer::new(&"select * from people inner join pets on people.id = person_id")).unwrap();
        // 0 query
        //   0 selection
        //     0 select
        //     1 field
        //       0 field
        //         0 star
        //  1 source
        //    0 from
        //    1 table
        //      0 people
        //      1 join
        //        0 inner
        //        1 join
        //      2 pets
        //      3 on
        //      4 expression
        //        0 expr
        //          0 people
        //          1 .
        //          2 id
        //        1 =
        //        2 expr
        //          0 person_id

        assert_eq!(find_node_type(&parse_tree, &[]), NodeType::Query);

        assert_eq!(find_node_type(&parse_tree, &[0]), NodeType::Selection);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 0]), SqlType::Select);
        assert_eq!(find_node_type(&parse_tree, &[0, 1]), NodeType::FieldDef);
        assert_eq!(find_node_type(&parse_tree, &[0, 1, 0]), NodeType::FieldSelection);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 1, 0, 0]), SqlType::Star);

        assert_eq!(find_node_type(&parse_tree, &[1]), NodeType::Source);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 0]), SqlType::From);
        assert_eq!(find_node_type(&parse_tree, &[1, 1]), NodeType::Table);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 0]), SqlType::Literal);
        assert_eq!(find_node_type(&parse_tree, &[1, 1, 1]), NodeType::Table);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 1, 0]), SqlType::Inner);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 1, 1]), SqlType::Join);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 2]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 3]), SqlType::On);
        assert_eq!(find_node_type(&parse_tree, &[1, 1, 4]), NodeType::Expression);
        assert_eq!(find_node_type(&parse_tree, &[1, 1, 4, 0]), NodeType::Condition);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 4, 0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 4, 0, 1]), SqlType::Dot);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 4, 0, 2]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 4, 1]), SqlType::Equal);
        assert_eq!(find_node_type(&parse_tree, &[1, 1, 4, 2]), NodeType::Condition);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 4, 2, 0]), SqlType::Literal);
    }

    #[test]
    fn test_from_empty() {
        let parse_tree = parse(SqlTokenizer::new(&"select")).unwrap();
        // 0 query
        //   0 selection
        //     0 select

        assert_eq!(find_node_type(&parse_tree, &[]), NodeType::Query);

        assert_eq!(find_node_type(&parse_tree, &[0]), NodeType::Selection);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 0]), SqlType::Select);
    }

    #[test]
    fn test_having_field() {
        let parse_tree = parse(SqlTokenizer::new(&"select * from people group by age having age > 4"));
        assert!(parse_tree.is_ok());
    }

    #[test]
    fn test_query_extra() {
        let parse_tree = parse(SqlTokenizer::new(&"select * from people select"));
        assert!(parse_tree.is_err());
    }
}
