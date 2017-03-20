use std::str::Chars;
use std::iter::Iterator;
use std::iter::Peekable;
use std::collections::HashSet;

use lexer::{Token, SqlTokenizer, SqlType};

#[derive(PartialEq, Debug, Clone)]
pub enum NodeType {
    Query,
    Expression,

    Concrete(Token)
}

pub struct ParseTree {
    pub node_type: NodeType,
    pub children: Vec<ParseTree>
}

fn parse_select <T> (lexer: &mut Peekable<T>) -> ParseTree
    where T: Iterator<Item = Token> {
    let select_token = lexer.next();
    if select_token.as_ref().unwrap().sql_type != SqlType::Select {
        panic!("Expected select");
    }

    let mut children = Vec::new();
    loop {
        let column_token = lexer.next();

        children.push(ParseTree { node_type: NodeType::Concrete(column_token.unwrap()), children: vec![] });

        let has_additional = if let Some(ref next_token) = lexer.peek() {
            next_token.sql_type == SqlType::Separator
        } else {
            false
        };

        if has_additional {
            let separator_token = lexer.next().unwrap();
            children.push(ParseTree { node_type: NodeType::Concrete(separator_token), children: vec![] });
        } else {
            break;
        }
    }

    ParseTree { node_type: NodeType::Concrete(select_token.unwrap()), children: children }
}

fn parse_from <T> (lexer: &mut Peekable<T>) -> ParseTree
    where T: Iterator<Item = Token> {
    let from_token = lexer.next();
    if from_token.as_ref().unwrap().sql_type != SqlType::From {
        panic!("Expected from, received {}", from_token.as_ref().unwrap().text);
    }

    if let Some(table_token) = lexer.next() {
        if table_token.sql_type != SqlType::Literal {
            panic!("Expected literal");
        }

        let mut children = Vec::new();
        children.push(ParseTree { node_type: NodeType::Concrete(table_token), children: vec![] });

        ParseTree { node_type: NodeType::Concrete(from_token.unwrap()), children: children }
    } else {
        panic!("Missing table");
    }
}

fn parse_where <T> (lexer: &mut Peekable<T>) -> Option<ParseTree>
    where T: Iterator<Item = Token> {
    if let Some(where_token) = lexer.peek().as_ref() {
        if where_token.sql_type != SqlType::Where {
            return None;
        }
    } else {
        return None;
    }

    let where_token = lexer.next();

    let left = ParseTree { node_type: NodeType::Concrete(lexer.next().unwrap()), children: vec![] };
    let condition = ParseTree { node_type: NodeType::Concrete(lexer.next().unwrap()), children: vec![] };
    let right = ParseTree { node_type: NodeType::Concrete(lexer.next().unwrap()), children: vec![] };
    let mut children = vec![left, condition, right];

    Some(ParseTree { node_type: NodeType::Concrete(where_token.unwrap()), children: children })
}

fn parse_query <T> (lexer: &mut Peekable<T>) -> ParseTree
    where T: Iterator<Item = Token> {
    let mut children = Vec::new();

    children.push(parse_select(lexer));
    children.push(parse_from(lexer));

    if let Some(where_tree) = parse_where(lexer) {
        children.push(where_tree);
    }

    ParseTree { node_type: NodeType::Query, children: children }
}

pub fn parse <T> (mut lexer: T) -> ParseTree
  where T: Iterator<Item = Token> {
    let mut peekable_lexer = Box::new(lexer.peekable());
    parse_query(&mut peekable_lexer)
    //ParseTree { token: None, children: vec![] }
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
            _ => panic!("Not a concrete type!")
        }
    }

    #[test]
    fn test_simple_query() {
        let parse_tree = parse(SqlTokenizer::new(&"select a, b from people where a > 1"));
        // Query
        //   select
        //     a
        //     ,
        //     b
        //  from
        //    people
        //  where
        //      a
        //      >
        //      1

        assert_eq!(find_sql_type(&parse_tree, &[0]), SqlType::Select);
        assert_eq!(find_sql_type(&parse_tree, &[0, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree, &[0, 1]), SqlType::Separator);
        assert_eq!(find_sql_type(&parse_tree, &[0, 2]), SqlType::Literal);

        assert_eq!(find_sql_type(&parse_tree, &[1]), SqlType::From);
        assert_eq!(find_sql_type(&parse_tree, &[1, 0]), SqlType::Literal);

        assert_eq!(find_sql_type(&parse_tree, &[2]), SqlType::Where);
        assert_eq!(find_sql_type(&parse_tree, &[2, 0]), SqlType::Literal);
        assert_eq!(find_sql_type(&parse_tree, &[2, 1]), SqlType::GreaterThan);
        assert_eq!(find_sql_type(&parse_tree, &[2, 2]), SqlType::Int);
    }
}
