use std::iter::Iterator;
use std::iter::Peekable;

use lexer::{Token, SqlType};

#[derive(PartialEq, Debug, Clone)]
pub enum NodeType {
    FieldDef,
    NamedFieldDef,

    FieldValueLiteral,
    FieldValuePrimitive,
    FieldValueFunction,
    FieldValueStar,
    FieldValueScoped,
    FieldValueParenGroup,
    FieldValueMath,

    ExprBoolCond,
    ExprBoolLogic,
    ExprParenGroup,

    SourceTable,
    SourceJoin,
    ParenGroup,
    JoinType,

    SortField,

    Query,
    Subquery,
    Selection,
    Source,
    Filter,
    Limit,
    Offset,
    Grouping,
    Having,
    Sort,

    Concrete(Token)
}

#[derive(Debug, Clone)]
pub struct ParseErr {
    pub token: Option<Token>
}

impl ParseErr {
    fn new(token: Option<Token>) -> ParseErr {
        ParseErr { token: token }
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
            Err(ParseErr::new(Some(token)))
        }
    } else {
        Err(ParseErr::new(None))
    }
}

fn parse_any_token <T> (lexer: &mut Peekable<T>, sql_types: &Vec<SqlType>) -> Result<Token, ParseErr>
    where T: Iterator<Item = Token> {
    if let Some(token) = lexer.next() {
        if sql_types.iter().any(|sql_type| &token.sql_type == sql_type) {
            Ok(token)
        } else {
            Err(ParseErr::new(Some(token)))
        }
    } else {
        Err(ParseErr::new(None))
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

fn parse_optional_math_token <T> (lexer: &mut Peekable<T>) -> Option<Token>
    where T: Iterator<Item = Token> {

    if let Some(next_token) = peek_sql_type(lexer) {
        match next_token {
            SqlType::Plus | SqlType::Minus | SqlType::Divide | SqlType::Star => {
                lexer.next()
            },
            _ => {
                None
            }
        }
    } else {
        None
    }
}

fn parse_value_expr <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {

    let mut children = vec![];
    let field_types = vec![SqlType::Int, SqlType::Float, SqlType::Text, SqlType::Literal, SqlType::Star, SqlType::OpenParen];

    let token = try!(parse_any_token(lexer, &field_types));
    let field_tree = match token.sql_type {
        SqlType::Literal => {
            if let Some(dot_token) = parse_optional_token(lexer, SqlType::Dot) {
                let scoped_types = vec![SqlType::Star, SqlType::Literal];
                let scoped_token = try!(parse_any_token(lexer, &scoped_types));

                children.push(ParseTree::new_leaf(token));
                children.push(ParseTree::new_leaf(dot_token));
                children.push(ParseTree::new_leaf(scoped_token));

                ParseTree { node_type: NodeType::FieldValueScoped, children: children }
            } else if let Some(open_paren_token) = parse_optional_token(lexer, SqlType::OpenParen) {
                children.push(ParseTree::new_leaf(token));
                children.push(ParseTree::new_leaf(open_paren_token));

                if let Some(close_paren_token) = parse_optional_token(lexer, SqlType::CloseParen) {
                    children.push(ParseTree::new_leaf(close_paren_token));
                } else {
                    let delimiter_types = vec![SqlType::Separator, SqlType::CloseParen];

                    loop {
                        children.push(try!(parse_value_expr(lexer)));

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

                ParseTree { node_type: NodeType::FieldValueFunction, children: children }
            } else {
                children.push(ParseTree::new_leaf(token));

                ParseTree { node_type: NodeType::FieldValueLiteral, children: children }
            }
        },
        SqlType::Int | SqlType::Float | SqlType::Text => {
            children.push(ParseTree::new_leaf(token));
            ParseTree { node_type: NodeType::FieldValuePrimitive, children: children }
        },
        SqlType::Star => {
            children.push(ParseTree::new_leaf(token));
            ParseTree { node_type: NodeType::FieldValueStar, children: children }
        },
        SqlType::OpenParen => {
            let value_tree = try!(parse_value_expr(lexer));
            let close_token = try!(parse_token(lexer, SqlType::CloseParen));

            children.push(ParseTree::new_leaf(token));
            children.push(value_tree);
            children.push(ParseTree::new_leaf(close_token));
            ParseTree { node_type: NodeType::FieldValueParenGroup, children: children }
        },
        _ => {
            panic!("Never get here");
        }
    };

    if let Some(math_token) = parse_optional_math_token(lexer) {
        let math_children = vec![field_tree, ParseTree::new_leaf(math_token), try!(parse_value_expr(lexer))];

        Ok(ParseTree { node_type: NodeType::FieldValueMath, children: math_children })
    } else {
        Ok(field_tree)
    }
}

fn parse_field_selection <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {

    let mut children = vec![try!(parse_value_expr(lexer))];

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
            SqlType::Literal | SqlType::Int | SqlType::Float | SqlType::Text | SqlType::Star | SqlType::OpenParen => {
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
            Some(Ok(ParseTree { node_type: NodeType::JoinType, children: children }))
        },
        Err(parse_err) => {
            Some(Err(parse_err))
        }
    }
}

fn parse_from_table <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {

    let table_token = try!(parse_token(lexer, SqlType::Literal));

    if let Some(as_token) = parse_optional_token(lexer, SqlType::As) {
        let name_token = try!(parse_token(lexer, SqlType::Literal));
        Ok(ParseTree { node_type: NodeType::SourceTable, children: vec![ParseTree::new_leaf(table_token), ParseTree::new_leaf(as_token), ParseTree::new_leaf(name_token)] })
    } else {
        Ok(ParseTree { node_type: NodeType::SourceTable, children: vec![ParseTree::new_leaf(table_token)] })
    }
}

fn parse_from_paren_group <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {

    let open_token = try!(parse_token(lexer, SqlType::OpenParen));

    let subtree = if let Some(next_token) = peek_sql_type(lexer) {
        match next_token {
            SqlType::Select => {
                try!(parse_query(lexer))
            },
            _ => {
                try!(parse_from_source(lexer))
            }
        }
    } else {
        return Err(ParseErr::new(None));
    };

    let close_token = try!(parse_token(lexer, SqlType::CloseParen));

    if let Some(as_token) = parse_optional_token(lexer, SqlType::As) {
        let name_token = try!(parse_token(lexer, SqlType::Literal));

        let children = vec![
            ParseTree::new_leaf(open_token),
            subtree,
            ParseTree::new_leaf(close_token),
            ParseTree::new_leaf(as_token),
            ParseTree::new_leaf(name_token)
        ];

        Ok(ParseTree { node_type: NodeType::ParenGroup, children: children })
    } else {
        Ok(ParseTree { node_type: NodeType::ParenGroup, children: vec![ParseTree::new_leaf(open_token), subtree, ParseTree::new_leaf(close_token)] })
    }
}

fn parse_from_source <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {

    let mut source_tree = if let Some(next_token) = peek_sql_type(lexer) {
        match next_token {
            SqlType::OpenParen => {
                try!(parse_from_paren_group(lexer))
            },
            _ => {
                try!(parse_from_table(lexer))
            }
        }
    } else {
        return Err(ParseErr::new(None));
    };

    while let Some(parse_join_result) = parse_from_join_type(lexer) {
        let mut join_children = vec![];
        join_children.push(source_tree);

        join_children.push(try!(parse_join_result));

        let join_table_token = try!(parse_token(lexer, SqlType::Literal));
        let right_join_tree = ParseTree { node_type: NodeType::SourceTable, children: vec![ParseTree::new_leaf(join_table_token)] };
        join_children.push(right_join_tree);

        let on_token = try!(parse_token(lexer, SqlType::On));
        join_children.push(ParseTree::new_leaf(on_token));

        let expr_tree = try!(parse_bool_expr(lexer));
        join_children.push(expr_tree);

        source_tree = ParseTree { node_type: NodeType::SourceJoin, children: join_children }
    }

    Ok(source_tree)
}

fn parse_from <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {
    let from_token = try!(parse_token(lexer, SqlType::From));

    let mut children = Vec::new();
    children.push(ParseTree::new_leaf(from_token));
    children.push(try!(parse_from_source(lexer)));

    while let Some(separator_token) = parse_optional_token(lexer, SqlType::Separator) {
        children.push(ParseTree::new_leaf(separator_token));
        children.push(try!(parse_from_source(lexer)));
    }

    Ok(ParseTree { node_type: NodeType::Source, children: children })
}

fn parse_bool_expr <T> (lexer: &mut Peekable<T>) -> Result<ParseTree, ParseErr>
    where T: Iterator<Item = Token> {

    let mut children = vec![];

    let bool_tree = if let Some(open_paren_token) = parse_optional_token(lexer, SqlType::OpenParen) {
        let sub_expr = try!(parse_bool_expr(lexer));
        let close_paren_token = try!(parse_token(lexer, SqlType::CloseParen));

        children.push(ParseTree::new_leaf(open_paren_token));
        children.push(sub_expr);
        children.push(ParseTree::new_leaf(close_paren_token));

        ParseTree { node_type: NodeType::ExprParenGroup, children: children }
    } else {
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

        ParseTree { node_type: NodeType::ExprBoolCond, children: children }
    };

    if let Some(sql_type) = peek_sql_type(lexer) {
        match sql_type {
            SqlType::And | SqlType::Or => {
                let cond_types = vec![SqlType::And, SqlType::Or];
                let cond_token = try!(parse_any_token(lexer, &cond_types));
                let right_bool_expr = try!(parse_bool_expr(lexer));

                let cond_tree = ParseTree { node_type: NodeType::ExprBoolLogic, children: vec![bool_tree, ParseTree::new_leaf(cond_token), right_bool_expr] };

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

    Ok(ParseTree { node_type: NodeType::SortField, children: children})
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
        Err(ParseErr::new(Some(next_token)))
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
        //       0 field_value_literal
        //         0 a
        //     2 ,
        //     3 field_def
        //       0 field_value_literal
        //         0 b
        //  1 source
        //    0 from
        //    1 table
        //      0 people
        //  2 filter
        //    0 where
        //    1 expr_bool_logic
        //      0 expr_bool_cond
        //        0 field_value_literal
        //          0 a
        //        1 >
        //        2 field_value_primitive
        //          0 1
        //      1 and
        //      2 expr_bool_cond
        //        0 field_value_literal
        //          0 a
        //        1 <
        //        2 field_value_primitive
        //          0 10

        assert_eq!(find_node_type(&parse_tree, &[]), NodeType::Query);

        assert_eq!(find_node_type(&parse_tree, &[0]), NodeType::Selection);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 0]), SqlType::Select);
        assert_eq!(find_node_type(&parse_tree, &[0, 1]), NodeType::FieldDef);
        assert_eq!(find_node_type(&parse_tree, &[0, 1, 0]), NodeType::FieldValueLiteral);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 1, 0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 2]), SqlType::Separator);
        assert_eq!(find_node_type(&parse_tree, &[0, 3]), NodeType::FieldDef);
        assert_eq!(find_node_type(&parse_tree, &[0, 3, 0]), NodeType::FieldValueLiteral);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 3, 0, 0]), SqlType::Literal);

        assert_eq!(find_node_type(&parse_tree, &[1]), NodeType::Source);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 0]), SqlType::From);
        assert_eq!(find_node_type(&parse_tree, &[1, 1]), NodeType::SourceTable);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 0]), SqlType::Literal);

        assert_eq!(find_node_type(&parse_tree, &[2]), NodeType::Filter);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 0]), SqlType::Where);
        assert_eq!(find_node_type(&parse_tree, &[2, 1]), NodeType::ExprBoolLogic);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 0]), NodeType::ExprBoolCond);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 0, 0]), NodeType::FieldValueLiteral);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 0, 0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 0, 1]), SqlType::GreaterThan);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 0, 2]), NodeType::FieldValuePrimitive);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 0, 2, 0]), SqlType::Int);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 1]), SqlType::And);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 2]), NodeType::ExprBoolCond);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 2, 0]), NodeType::FieldValueLiteral);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 2, 0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 2, 1]), SqlType::LessThan);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 2, 2]), NodeType::FieldValuePrimitive);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 2, 2, 0]), SqlType::Int);
    }

    #[test]
    fn test_select_function() {
        let parse_tree = parse(SqlTokenizer::new(&"select \"my age\", count(*) as num_people from people group by name having max(age) > 3 order by age desc limit 1 offset 2")).unwrap();
        // 0 query
        //   0 selection
        //     0 select
        //     1 field_def
        //       0 field_value_literal
        //         0 a
        //     2 ,
        //     3 named_field_def
        //       0 field_value_function
        //         0 count
        //         1 (
        //         2 field_value_star
        //           0 *
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
        //    1 expr_bool_cond
        //      0 field_value_function
        //        0 max
        //        1 (
        //        2 field_value_literal
        //          0 age
        //        3 )
        //      4 >
        //      5 field_value_primitive
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
        assert_eq!(find_node_type(&parse_tree, &[0, 1, 0]), NodeType::FieldValueLiteral);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 1, 0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 2]), SqlType::Separator);
        assert_eq!(find_node_type(&parse_tree, &[0, 3]), NodeType::NamedFieldDef);
        assert_eq!(find_node_type(&parse_tree, &[0, 3, 0]), NodeType::FieldValueFunction);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 3, 0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 3, 0, 1]), SqlType::OpenParen);
        assert_eq!(find_node_type(&parse_tree, &[0, 3, 0, 2]), NodeType::FieldValueStar);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 3, 0, 2, 0]), SqlType::Star);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 3, 0, 3]), SqlType::CloseParen);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 3, 1]), SqlType::As);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 3, 2]), SqlType::Literal);

        assert_eq!(find_node_type(&parse_tree, &[1]), NodeType::Source);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 0]), SqlType::From);
        assert_eq!(find_node_type(&parse_tree, &[1, 1]), NodeType::SourceTable);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 0]), SqlType::Literal);

        assert_eq!(find_node_type(&parse_tree, &[2]), NodeType::Grouping);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 0]), SqlType::Group);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1]), SqlType::By);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 2]), SqlType::Literal);

        assert_eq!(find_node_type(&parse_tree, &[3]), NodeType::Having);
        assert_eq!(find_sql_type(&parse_tree,  &[3, 0]), SqlType::Having);
        assert_eq!(find_node_type(&parse_tree, &[3, 1, 0]), NodeType::FieldValueFunction);
        assert_eq!(find_sql_type(&parse_tree,  &[3, 1, 0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[3, 1, 0, 1]), SqlType::OpenParen);
        assert_eq!(find_node_type(&parse_tree, &[3, 1, 0, 2]), NodeType::FieldValueLiteral);
        assert_eq!(find_sql_type(&parse_tree,  &[3, 1, 0, 2, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[3, 1, 0, 3]), SqlType::CloseParen);
        assert_eq!(find_sql_type(&parse_tree,  &[3, 1, 1]), SqlType::GreaterThan);
        assert_eq!(find_node_type(&parse_tree, &[3, 1, 2]), NodeType::FieldValuePrimitive);
        assert_eq!(find_sql_type(&parse_tree,  &[3, 1, 2, 0]), SqlType::Int);

        assert_eq!(find_node_type(&parse_tree, &[4]), NodeType::Sort);
        assert_eq!(find_sql_type(&parse_tree,  &[4, 0]), SqlType::Order);
        assert_eq!(find_sql_type(&parse_tree,  &[4, 1]), SqlType::By);
        assert_eq!(find_node_type(&parse_tree, &[4, 2]), NodeType::SortField);
        assert_eq!(find_sql_type(&parse_tree,  &[4, 2, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[4, 2, 1]), SqlType::Desc);

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
    fn test_from_cross() {
        let parse_tree = parse(SqlTokenizer::new(&"select * from people, pets")).unwrap();
        // 0 query
        //   0 selection
        //     0 select
        //     1 field_def
        //       0 field_value_star
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
        assert_eq!(find_node_type(&parse_tree, &[0, 1, 0]), NodeType::FieldValueStar);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 1, 0, 0]), SqlType::Star);

        assert_eq!(find_node_type(&parse_tree, &[1]), NodeType::Source);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 0]), SqlType::From);
        assert_eq!(find_node_type(&parse_tree, &[1, 1]), NodeType::SourceTable);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 2]), SqlType::Separator);
        assert_eq!(find_node_type(&parse_tree, &[1, 3]), NodeType::SourceTable);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 3, 0]), SqlType::Literal);
    }

    #[test]
    fn test_from_join() {
        let parse_tree = parse(SqlTokenizer::new(&"select * from people inner join pets on people.id = person_id")).unwrap();
        // 0 query
        //   0 selection
        //     0 select
        //     1 field
        //       0 field_value_star
        //         0 star
        //  1 source
        //    0 from
        //    1 source_join
        //      0 source_table
        //        0 people
        //      1 join
        //        0 inner
        //        1 join
        //      2 source_table
        //        0 pets
        //      3 on
        //      4 expr_bool_cond
        //        0 field_value_scoped
        //          0 people
        //          1 .
        //          2 id
        //        1 =
        //        2 field_value_literal
        //          0 person_id

        assert_eq!(find_node_type(&parse_tree, &[]), NodeType::Query);

        assert_eq!(find_node_type(&parse_tree, &[0]), NodeType::Selection);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 0]), SqlType::Select);
        assert_eq!(find_node_type(&parse_tree, &[0, 1]), NodeType::FieldDef);
        assert_eq!(find_node_type(&parse_tree, &[0, 1, 0]), NodeType::FieldValueStar);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 1, 0, 0]), SqlType::Star);

        assert_eq!(find_node_type(&parse_tree, &[1]), NodeType::Source);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 0]), SqlType::From);
        assert_eq!(find_node_type(&parse_tree, &[1, 1]), NodeType::SourceJoin);
        assert_eq!(find_node_type(&parse_tree, &[1, 1, 0]), NodeType::SourceTable);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 0, 0]), SqlType::Literal);
        assert_eq!(find_node_type(&parse_tree, &[1, 1, 1]), NodeType::JoinType);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 1, 0]), SqlType::Inner);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 1, 1]), SqlType::Join);
        assert_eq!(find_node_type(&parse_tree, &[1, 1, 2]), NodeType::SourceTable);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 2, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 3]), SqlType::On);
        assert_eq!(find_node_type(&parse_tree, &[1, 1, 4]), NodeType::ExprBoolCond);
        assert_eq!(find_node_type(&parse_tree, &[1, 1, 4, 0]), NodeType::FieldValueScoped);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 4, 0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 4, 0, 1]), SqlType::Dot);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 4, 0, 2]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 4, 1]), SqlType::Equal);
        assert_eq!(find_node_type(&parse_tree, &[1, 1, 4, 2]), NodeType::FieldValueLiteral);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 4, 2, 0]), SqlType::Literal);
    }

    #[test]
    fn test_where_bool_logic() {
        let parse_tree = parse(SqlTokenizer::new(&"select * from people where (age >= 5 and age < 10) or (age >= 20 and age < 25)")).unwrap();
        // 0 query
        //   0 selection
        //     0 select
        //     1 field
        //       0 field_value_star
        //         0 star
        //  1 source
        //    0 from
        //    1 table
        //      0 people
        //  2 filter
        //    0 where
        //    1 expr_bool_logic
        //      0 expr_paren_group
        //        0 (
        //        1 expr_bool_logic
        //          0 expr_bool_cond
        //            0 field_value_literal
        //              0 age
        //            1 >=
        //            2 field_value_primitive
        //              0 5
        //          1 or
        //          0 expr_bool_cond
        //            0 field_value_literal
        //              0 age
        //            1 <
        //            2 field_value_primitive
        //              0 10
        //        2 )
        //      1 or
        //      0 expr_paren_group
        //        0 (
        //        1 expr_bool_logic
        //          0 expr_bool_cond
        //            0 field_value_literal
        //              0 age
        //            1 >=
        //            2 field_value_primitive
        //              0 20
        //          1 or
        //          0 expr_bool_cond
        //            0 field_value_literal
        //              0 age
        //            1 <
        //            2 field_value_primitive
        //              0 25
        //        2 )

        assert_eq!(find_node_type(&parse_tree, &[]), NodeType::Query);

        assert_eq!(find_node_type(&parse_tree, &[0]), NodeType::Selection);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 0]), SqlType::Select);
        assert_eq!(find_node_type(&parse_tree, &[0, 1]), NodeType::FieldDef);
        assert_eq!(find_node_type(&parse_tree, &[0, 1, 0]), NodeType::FieldValueStar);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 1, 0, 0]), SqlType::Star);

        assert_eq!(find_node_type(&parse_tree, &[1]), NodeType::Source);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 0]), SqlType::From);
        assert_eq!(find_node_type(&parse_tree, &[1, 1]), NodeType::SourceTable);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 0]), SqlType::Literal);

        assert_eq!(find_node_type(&parse_tree, &[2]), NodeType::Filter);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 0]), SqlType::Where);
        assert_eq!(find_node_type(&parse_tree, &[2, 1]), NodeType::ExprBoolLogic);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 0]), NodeType::ExprParenGroup);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 0, 0]), SqlType::OpenParen);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 0, 1]), NodeType::ExprBoolLogic);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 0, 1, 0]), NodeType::ExprBoolCond);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 0, 1, 0, 0]), NodeType::FieldValueLiteral);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 0, 1, 0, 0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 0, 1, 0, 1]), SqlType::GreaterThanEqual);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 0, 1, 0, 2]), NodeType::FieldValuePrimitive);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 0, 1, 0, 2, 0]), SqlType::Int);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 0, 1, 2]), NodeType::ExprBoolCond);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 0, 1, 2, 0]), NodeType::FieldValueLiteral);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 0, 1, 2, 0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 0, 1, 2, 1]), SqlType::LessThan);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 0, 1, 2, 2]), NodeType::FieldValuePrimitive);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 0, 1, 2, 2, 0]), SqlType::Int);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 0, 2]), SqlType::CloseParen);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 1]), SqlType::Or);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 2]), NodeType::ExprParenGroup);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 2, 0]), SqlType::OpenParen);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 2, 1]), NodeType::ExprBoolLogic);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 2, 1, 0]), NodeType::ExprBoolCond);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 2, 1, 0, 0]), NodeType::FieldValueLiteral);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 2, 1, 0, 0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 2, 1, 0, 1]), SqlType::GreaterThanEqual);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 2, 1, 0, 2]), NodeType::FieldValuePrimitive);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 2, 1, 0, 2, 0]), SqlType::Int);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 2, 1, 2]), NodeType::ExprBoolCond);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 2, 1, 2, 0]), NodeType::FieldValueLiteral);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 2, 1, 2, 0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 2, 1, 2, 1]), SqlType::LessThan);
        assert_eq!(find_node_type(&parse_tree, &[2, 1, 2, 1, 2, 2]), NodeType::FieldValuePrimitive);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 2, 1, 2, 2, 0]), SqlType::Int);
        assert_eq!(find_sql_type(&parse_tree,  &[2, 1, 2, 2]), SqlType::CloseParen);
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

    #[test]
    fn test_multi_join() {
        parse(SqlTokenizer::new(&"select * from ((people inner join pets on people.id = pets.id) inner join addresses on people.address_id = address.id)")).unwrap();
    }

    #[test]
    fn test_from_subquery() {
        // 0 query
        //   0 selection
        //     0 select
        //     1 field_def
        //       0 field_value_star
        //         0 star
        //  1 source
        //    0 from
        //    1 paren_group
        //      0 (
        //      1 query
        //        0 selection
        //          0 select
        //          1 field
        //            0 field_value_star
        //              0 star
        //      2 )
        //      3 as
        //      4 people_two
        let parse_tree = parse(SqlTokenizer::new(&"select * from (select * from people) as people_two")).unwrap();

        assert_eq!(find_node_type(&parse_tree, &[]), NodeType::Query);

        assert_eq!(find_node_type(&parse_tree, &[0]), NodeType::Selection);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 0]), SqlType::Select);
        assert_eq!(find_node_type(&parse_tree, &[0, 1]), NodeType::FieldDef);
        assert_eq!(find_node_type(&parse_tree, &[0, 1, 0]), NodeType::FieldValueStar);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 1, 0, 0]), SqlType::Star);

        assert_eq!(find_node_type(&parse_tree, &[1]), NodeType::Source);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 0]), SqlType::From);
        assert_eq!(find_node_type(&parse_tree, &[1, 1]), NodeType::ParenGroup);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 0]), SqlType::OpenParen);
        assert_eq!(find_node_type(&parse_tree, &[1, 1, 1]), NodeType::Query);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 2]), SqlType::CloseParen);

        assert_eq!(find_node_type(&parse_tree, &[1, 1, 1, 0]), NodeType::Selection);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 1, 0, 0]), SqlType::Select);
        assert_eq!(find_node_type(&parse_tree, &[1, 1, 1, 0, 1]), NodeType::FieldDef);
        assert_eq!(find_node_type(&parse_tree, &[1, 1, 1, 0, 1, 0]), NodeType::FieldValueStar);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 1, 0, 1, 0, 0]), SqlType::Star);

        assert_eq!(find_node_type(&parse_tree, &[1, 1, 1, 1]), NodeType::Source);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 1, 1, 0]), SqlType::From);
        assert_eq!(find_node_type(&parse_tree, &[1, 1, 1, 1, 1]), NodeType::SourceTable);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 1, 1, 1, 0]), SqlType::Literal);

        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 2]), SqlType::CloseParen);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 3]), SqlType::As);
        assert_eq!(find_sql_type(&parse_tree,  &[1, 1, 4]), SqlType::Literal);
    }

    #[test]
    fn test_query_math() {
        // 0 query
        //   0 selection
        //     0 select
        //     1 field_def
        //       0 field_value_math
        //         0 field_value_paren_group
        //           0 (
        //           1 field_value_math
        //             0 field_value_literal
        //               0 age
        //             1 +
        //             2 field_value_primitive
        //               0 1
        //           2 )
        //         1 /
        //         2 field_value_math
        //           2 field_value_primitive
        //             0 2
        //           3 +
        //           4 field_value_primitive
        //             0 4.2
        let parse_tree = parse(SqlTokenizer::new(&"select (age + 1) / 2 + 4.2 from people")).unwrap();

        assert_eq!(find_node_type(&parse_tree, &[]), NodeType::Query);

        assert_eq!(find_node_type(&parse_tree, &[0]), NodeType::Selection);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 0]), SqlType::Select);
        assert_eq!(find_node_type(&parse_tree, &[0, 1]), NodeType::FieldDef);
        assert_eq!(find_node_type(&parse_tree, &[0, 1, 0]), NodeType::FieldValueMath);
        assert_eq!(find_node_type(&parse_tree, &[0, 1, 0, 0]), NodeType::FieldValueParenGroup);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 1, 0, 0, 0]), SqlType::OpenParen);
        assert_eq!(find_node_type(&parse_tree, &[0, 1, 0, 0, 1]), NodeType::FieldValueMath);
        assert_eq!(find_node_type(&parse_tree, &[0, 1, 0, 0, 1, 0]), NodeType::FieldValueLiteral);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 1, 0, 0, 1, 0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 1, 0, 0, 1, 1]), SqlType::Plus);
        assert_eq!(find_node_type(&parse_tree, &[0, 1, 0, 0, 1, 2]), NodeType::FieldValuePrimitive);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 1, 0, 0, 1, 2, 0]), SqlType::Int);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 1, 0, 0, 2]), SqlType::CloseParen);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 1, 0, 1]), SqlType::Divide);
        assert_eq!(find_node_type(&parse_tree, &[0, 1, 0, 2]), NodeType::FieldValueMath);
        assert_eq!(find_node_type(&parse_tree, &[0, 1, 0, 2, 0]), NodeType::FieldValuePrimitive);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 1, 0, 2, 0, 0]), SqlType::Int);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 1, 0, 2, 1]), SqlType::Plus);
        assert_eq!(find_node_type(&parse_tree, &[0, 1, 0, 2, 2]), NodeType::FieldValuePrimitive);
        assert_eq!(find_sql_type(&parse_tree,  &[0, 1, 0, 2, 2, 0]), SqlType::Float);
    }
}
