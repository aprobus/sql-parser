use std::str::Chars;
use std::iter::Iterator;
use std::iter::Peekable;
use std::collections::HashSet;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum SqlType {
    Select,
    Star,
    From,
    Literal,
    Separator,
    OpenParen,
    CloseParen,
    Where,
    Order,
    Group,
    By,
    As,
    Dot,
    Having,
    Asc,
    Desc,

    Limit,
    Offset,
    All,

    On,
    Inner,
    Outer,
    Left,
    Right,
    Join,

    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    Equal,
    And,
    Or,

    Float,
    Int,

    Whitespace,
    Text
}

#[derive(PartialEq, Debug, Clone)]
pub struct Token {
    pub sql_type: SqlType,
    pub text: String
}

trait TokenParser {
    fn parse(&self, text: &str) -> Option<Token>;
}

struct KeywordTokenParser {
    text: &'static str,
    sql_type: SqlType,
}

impl TokenParser for KeywordTokenParser {
    fn parse(&self, text: &str) -> Option<Token> {
        if self.text == text {
            Some( Token { text: text.to_string(), sql_type: self.sql_type.clone() })
        } else {
            None
        }
    }
}

struct WhitespaceTokenParser { }

impl TokenParser for WhitespaceTokenParser {
    fn parse(&self, text: &str) -> Option<Token> {
        if text.chars().all(|c| c == ' ') {
            Some(Token { text: text.to_string(), sql_type: SqlType::Whitespace })
        } else {
            None
        }
    }
}

struct LiteralTokenParser {
    letters: HashSet<char>
}

impl LiteralTokenParser {
    fn new() -> LiteralTokenParser {
        let mut literal_chars = HashSet::new();
        literal_chars.insert('a');
        literal_chars.insert('b');
        literal_chars.insert('c');
        literal_chars.insert('d');
        literal_chars.insert('e');
        literal_chars.insert('f');
        literal_chars.insert('g');
        literal_chars.insert('h');
        literal_chars.insert('i');
        literal_chars.insert('j');
        literal_chars.insert('k');
        literal_chars.insert('l');
        literal_chars.insert('m');
        literal_chars.insert('n');
        literal_chars.insert('o');
        literal_chars.insert('p');
        literal_chars.insert('q');
        literal_chars.insert('r');
        literal_chars.insert('s');
        literal_chars.insert('t');
        literal_chars.insert('u');
        literal_chars.insert('v');
        literal_chars.insert('w');
        literal_chars.insert('x');
        literal_chars.insert('y');
        literal_chars.insert('z');
        literal_chars.insert('_');

        LiteralTokenParser {
            letters: literal_chars
        }
    }
}

impl TokenParser for LiteralTokenParser {
    fn parse(&self, text: &str) -> Option<Token> {
        if text.chars().all(|c| self.letters.contains(&c)) {
            Some(Token { text: text.to_string(), sql_type: SqlType::Literal })
        } else {
            None
        }
    }
}

struct FloatTokenParser { }

impl FloatTokenParser {
    fn new() -> FloatTokenParser {
        FloatTokenParser { }
    }
}

impl TokenParser for FloatTokenParser {
    fn parse(&self, text: &str) -> Option<Token> {
        if text.parse::<f64>().is_ok() {
            Some(Token { text: text.to_string(), sql_type: SqlType::Float })
        } else {
            None
        }
    }
}

struct IntTokenParser { }

impl IntTokenParser {
    fn new() -> IntTokenParser {
        IntTokenParser { }
    }
}

impl TokenParser for IntTokenParser {
    fn parse(&self, text: &str) -> Option<Token> {
        if text.parse::<i64>().is_ok() {
            Some(Token { text: text.to_string(), sql_type: SqlType::Int })
        } else {
            None
        }
    }
}

struct TextTokenParser { }

impl TextTokenParser {
    fn new() -> TextTokenParser {
        TextTokenParser { }
    }
}

impl TokenParser for TextTokenParser {
    fn parse(&self, text: &str) -> Option<Token> {
        let mut chars = text.chars();
        if chars.next() != Some('\'') {
            return None;
        }

        let mut parsed_text = String::new();
        let mut previous_char = None;
        let mut has_end_quote = false;

        for next_char in chars {
            if has_end_quote {
                return None;
            }

            if next_char == '\'' {
                if previous_char == Some('\\') {
                    parsed_text.pop();
                } else {
                    has_end_quote = true;
                }
            }

            parsed_text.push(next_char);
            previous_char = Some(next_char);
        }

        if has_end_quote {
            parsed_text.pop();
            Some(Token { text: parsed_text, sql_type: SqlType::Text })
        } else {
            None
        }
    }
}

pub struct SqlTokenizer<'a> {
    sql_chars: Peekable<Chars<'a>>,
    token_parsers: Vec<Box<TokenParser>>
}

impl <'a> SqlTokenizer<'a> {
    pub fn new(sql: &str) -> SqlTokenizer {
        let token_parsers: Vec<Box<TokenParser>> = vec![
            Box::new(WhitespaceTokenParser { }),
            Box::new(KeywordTokenParser{ text: "select", sql_type: SqlType::Select }),
            Box::new(KeywordTokenParser{ text: "*", sql_type: SqlType::Star }),
            Box::new(KeywordTokenParser{ text: ",", sql_type: SqlType::Separator }),
            Box::new(KeywordTokenParser{ text: "(", sql_type: SqlType::OpenParen }),
            Box::new(KeywordTokenParser{ text: ")", sql_type: SqlType::CloseParen }),
            Box::new(KeywordTokenParser{ text: "from", sql_type: SqlType::From }),
            Box::new(KeywordTokenParser{ text: "and", sql_type: SqlType::And }),
            Box::new(KeywordTokenParser{ text: "or", sql_type: SqlType::Or }),
            Box::new(KeywordTokenParser{ text: "on", sql_type: SqlType::On }),
            Box::new(KeywordTokenParser{ text: ".", sql_type: SqlType::Dot }),
            Box::new(KeywordTokenParser{ text: "where", sql_type: SqlType::Where }),
            Box::new(KeywordTokenParser{ text: ">", sql_type: SqlType::GreaterThan }),
            Box::new(KeywordTokenParser{ text: ">=", sql_type: SqlType::GreaterThanEqual }),
            Box::new(KeywordTokenParser{ text: "<", sql_type: SqlType::LessThan }),
            Box::new(KeywordTokenParser{ text: "<=", sql_type: SqlType::LessThanEqual }),
            Box::new(KeywordTokenParser{ text: "=", sql_type: SqlType::Equal }),
            Box::new(KeywordTokenParser{ text: "order", sql_type: SqlType::Order }),
            Box::new(KeywordTokenParser{ text: "group", sql_type: SqlType::Group }),
            Box::new(KeywordTokenParser{ text: "having", sql_type: SqlType::Having }),
            Box::new(KeywordTokenParser{ text: "by", sql_type: SqlType::By }),
            Box::new(KeywordTokenParser{ text: "as", sql_type: SqlType::As }),
            Box::new(KeywordTokenParser{ text: "asc", sql_type: SqlType::Asc }),
            Box::new(KeywordTokenParser{ text: "desc", sql_type: SqlType::Desc }),
            Box::new(KeywordTokenParser{ text: "join", sql_type: SqlType::Join }),
            Box::new(KeywordTokenParser{ text: "inner", sql_type: SqlType::Inner }),
            Box::new(KeywordTokenParser{ text: "full", sql_type: SqlType::Inner }),
            Box::new(KeywordTokenParser{ text: "left", sql_type: SqlType::Inner }),
            Box::new(KeywordTokenParser{ text: "right", sql_type: SqlType::Inner }),
            Box::new(KeywordTokenParser{ text: "limit", sql_type: SqlType::Limit }),
            Box::new(KeywordTokenParser{ text: "all", sql_type: SqlType::All }),
            Box::new(KeywordTokenParser{ text: "offset", sql_type: SqlType::Offset }),
            Box::new(TextTokenParser::new()),
            Box::new(IntTokenParser::new()),
            Box::new(FloatTokenParser::new()),
            Box::new(LiteralTokenParser::new())
        ];

        SqlTokenizer {
            sql_chars: sql.chars().peekable(),
            token_parsers: token_parsers
        }
    }
}

impl <'a> Iterator for SqlTokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let mut text = if let Some(next_char) = self.sql_chars.next() {
            let mut text = String::new();
            text.push(next_char);
            text
        } else {
            return None;
        };

        let mut last_match = self.token_parsers.iter().filter_map(|ref token_parser| token_parser.parse(&text)).nth(0);

        while let Some(next_char) = self.sql_chars.peek().map(|c| *c) {
            text.push(next_char);

            let parsed_token = self.token_parsers.iter().filter_map(|ref token_parser| token_parser.parse(&text)).nth(0);
            if parsed_token.is_some() {
                self.sql_chars.next();
                last_match = parsed_token;
            } else if last_match.is_some() {
                break;
            } else {
                self.sql_chars.next();
            }
        }

        if let Some(ref token) = last_match {
            if token.sql_type == SqlType::Whitespace {
                return self.next();
            }
        }
        last_match
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_selects_all() {
        let mut tokenizer = SqlTokenizer::new(&"select * from bananas");
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Select, text: "select".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Star, text: "*".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::From, text: "from".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "bananas".to_string() }));
        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn it_selects_fields() {
        let mut tokenizer = SqlTokenizer::new(&"select color, size from bananas where (size > 1.2 or size <= 2) and color = 'yellow' order by size asc");
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Select, text: "select".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "color".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Separator, text: ",".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "size".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::From, text: "from".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "bananas".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Where, text: "where".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::OpenParen, text: "(".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "size".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::GreaterThan, text: ">".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Float, text: "1.2".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Or, text: "or".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "size".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::LessThanEqual, text: "<=".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Int, text: "2".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::CloseParen, text: ")".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::And, text: "and".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "color".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Equal, text: "=".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Text, text: "yellow".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Order, text: "order".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::By, text: "by".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "size".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Asc, text: "asc".to_string() }));
        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn test_grouping() {
        let mut tokenizer = SqlTokenizer::new(&"select age, count(*) from people group by age having count(*) > 5");
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Select, text: "select".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "age".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Separator, text: ",".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "count".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::OpenParen, text: "(".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Star, text: "*".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::CloseParen, text: ")".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::From, text: "from".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "people".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Group, text: "group".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::By, text: "by".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "age".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Having, text: "having".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "count".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::OpenParen, text: "(".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Star, text: "*".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::CloseParen, text: ")".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::GreaterThan, text: ">".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Int, text: "5".to_string() }));
        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn test_join() {
        let mut tokenizer = SqlTokenizer::new(&"select people.* from people inner join pets on people.id = pets.person_id limit 4 offset 2");
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Select, text: "select".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "people".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Dot, text: ".".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Star, text: "*".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::From, text: "from".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "people".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Inner, text: "inner".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Join, text: "join".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "pets".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::On, text: "on".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "people".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Dot, text: ".".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "id".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Equal, text: "=".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "pets".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Dot, text: ".".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Literal, text: "person_id".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Limit, text: "limit".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Int, text: "4".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Offset, text: "offset".to_string() }));
        assert_eq!(tokenizer.next(), Some(Token { sql_type: SqlType::Int, text: "2".to_string() }));
        assert_eq!(tokenizer.next(), None);
    }
}
