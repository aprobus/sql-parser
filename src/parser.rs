use std::str::Chars;
use std::iter::Iterator;
use std::iter::Peekable;
use std::collections::HashSet;

use lexer::{Token, SqlTokenizer, SqlType};

pub struct ParseTree {
    pub token: Option<Token>,
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

        children.push(ParseTree { token: column_token, children: vec![] });

        let has_additional = if let Some(ref next_token) = lexer.peek() {
            next_token.sql_type == SqlType::Separator
        } else {
            false
        };

        if has_additional {
            children.push(ParseTree { token: lexer.next(), children: vec![] });
        } else {
            break;
        }
    }

    ParseTree { token: select_token, children: children }
}

fn parse_from <T> (lexer: &mut Peekable<T>) -> ParseTree
    where T: Iterator<Item = Token> {
    let from_token = lexer.next();
    if from_token.as_ref().unwrap().sql_type != SqlType::From {
        panic!("Expected from, received {}", from_token.as_ref().unwrap().text);
    }

    let mut children = Vec::new();
    let table_token = lexer.next();
    if table_token.as_ref().unwrap().sql_type != SqlType::Literal {
        panic!("Expected literal");
    }
    children.push(ParseTree { token: table_token, children: vec![] });

    ParseTree { token: from_token, children: children }
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

    let left = ParseTree { token: lexer.next(), children: vec![] };
    let condition = ParseTree { token: lexer.next(), children: vec![] };
    let right = ParseTree { token: lexer.next(), children: vec![] };
    let mut children = vec![left, condition, right];

    Some(ParseTree { token: where_token, children: children })
}

fn parse_query <T> (lexer: &mut Peekable<T>) -> ParseTree
    where T: Iterator<Item = Token> {
    let mut children = Vec::new();

    children.push(parse_select(lexer));
    children.push(parse_from(lexer));

    if let Some(where_tree) = parse_where(lexer) {
        children.push(where_tree);
    }

    ParseTree { token: None, children: children }
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

        assert_eq!(parse_tree.token, None);
        assert_eq!(parse_tree.children[0].token.as_ref().unwrap().sql_type, SqlType::Select);
        assert_eq!(parse_tree.children[0].children[0].token.as_ref().unwrap().sql_type, SqlType::Literal);
        assert_eq!(parse_tree.children[0].children[1].token.as_ref().unwrap().sql_type, SqlType::Separator);
        assert_eq!(parse_tree.children[0].children[2].token.as_ref().unwrap().sql_type, SqlType::Literal);

        assert_eq!(parse_tree.children[1].token.as_ref().unwrap().sql_type, SqlType::From);
        assert_eq!(parse_tree.children[1].children[0].token.as_ref().unwrap().sql_type, SqlType::Literal);

        assert_eq!(parse_tree.children[2].token.as_ref().unwrap().sql_type, SqlType::Where);
        assert_eq!(parse_tree.children[2].children[0].token.as_ref().unwrap().sql_type, SqlType::Literal);
        assert_eq!(parse_tree.children[2].children[1].token.as_ref().unwrap().sql_type, SqlType::GreaterThan);
        assert_eq!(parse_tree.children[2].children[2].token.as_ref().unwrap().sql_type, SqlType::Int);
    }
}
