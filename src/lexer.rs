#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Bang,
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    And,
    Or,

    // Literals
    Star,
    Number(f64),
    String(String),
    Identifier(String),
    True,
    False,
    Null,
    Eof,
}

#[derive(Debug)]
pub struct Pos {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub struct Range {
    pub start: Pos,
    pub end: Pos,
}

#[derive(Debug)]
pub struct TokenWithRange {
    pub token: Token,
    pub range: Range,
}

impl TokenWithRange {
    pub fn new(token: Token, range: Range) -> Self {
        Self { token, range }
    }
}

pub struct Lexer {
    input: Vec<char>,
    input_string: String,    // Original input string for error reporting.
    start: usize,            // Start of current lexeme.
    offset: usize,           // Current offset in input.
    line: usize,             // Current line number.
    last_line_offset: usize, // Offset of last line break.
    tokens: Vec<TokenWithRange>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Self {
            input: input.chars().collect(),
            input_string: input,
            start: 0,
            offset: 0,
            line: 0,
            last_line_offset: 0,
            tokens: Vec::new(),
        }
    }

    pub fn lex(mut self) -> Vec<TokenWithRange> {
        // TODO: Validate max expression length.
        while !self.at_end() {
            self.start = self.offset;
            match self.next() {
                '(' => self.add_token(Token::LeftParen),
                ')' => self.add_token(Token::RightParen),
                '[' => self.add_token(Token::LeftBracket),
                ']' => self.add_token(Token::RightBracket),
                ',' => self.add_token(Token::Comma),
                '.' => {
                    let previous = self.previous();
                    match previous {
                        Token::Identifier(_)
                        | Token::RightBracket
                        | Token::LeftBracket
                        | Token::Star => {
                            self.consume_number();
                        }
                        _ => {
                            self.add_token(Token::Dot);
                        }
                    }
                }
                '-' | '+' => self.consume_number(),
                '!' => {
                    if self.match_char('=') {
                        self.add_token(Token::BangEqual);
                    } else {
                        self.add_token(Token::Bang);
                    }
                }
                '=' => {
                    if self.match_char('=') {
                        self.add_token(Token::EqualEqual);
                    } else {
                        // Illegal; continue reading until we hit a boundary character and return an error
                        self.consume_identifier();
                    }
                }
                '<' => {
                    if self.match_char('=') {
                        self.add_token(Token::LessEqual);
                    } else {
                        self.add_token(Token::Less);
                    }
                }
                '>' => {
                    if self.match_char('=') {
                        self.add_token(Token::GreaterEqual);
                    } else {
                        self.add_token(Token::Greater);
                    }
                }
                '&' => {
                    if self.match_char('&') {
                        self.add_token(Token::And);
                    } else {
                        // Illegal; continue reading until we hit a boundary character and return an error
                        self.consume_identifier();
                    }
                }
                '|' => {
                    if self.match_char('|') {
                        self.add_token(Token::Or);
                    } else {
                        // Illegal; continue reading until we hit a boundary character and return an error
                        self.consume_identifier();
                    }
                }
                '*' => self.add_token(Token::Star),
                ' ' | '\r' | '\t' => {
                    // Ignore whitespace.
                }
                '\n' => {
                    self.line += 1;
                    self.last_line_offset = self.offset;
                }
                '\'' => self.consume_string(),
                '0'..='9' => self.consume_number(),
                _ => self.consume_identifier(),
            }
        }

        self.tokens
            .push(TokenWithRange::new(Token::Eof, self.range()));

        self.tokens
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.at_end() {
            return false;
        }
        if self.input[self.offset] != expected {
            return false;
        }
        self.offset += 1;
        true
    }

    fn next(&mut self) -> char {
        let c = self.input[self.offset];
        self.offset += 1;
        c
    }

    fn peek(&self) -> char {
        if self.at_end() {
            return '\0';
        }
        self.input[self.offset]
    }

    fn peek_next(&self) -> char {
        if self.offset + 1 >= self.input.len() {
            return '\0';
        }
        self.input[self.offset + 1]
    }

    fn previous(&self) -> Token {
        if self.tokens.len() == 0 {
            Token::Eof
        } else {
            self.tokens[self.tokens.len() - 1].token.clone()
        }
    }

    fn at_end(&self) -> bool {
        self.offset >= self.input.len()
    }

    fn add_token(&mut self, token: Token) {
        self.tokens.push(TokenWithRange::new(token, self.range()));
    }

    fn pos(&self) -> Pos {
        return Pos {
            line: self.line,
            column: self.start - self.last_line_offset,
        };
    }

    fn end_pos(&self) -> Pos {
        return Pos {
            line: self.line,
            column: self.offset - self.last_line_offset,
        };
    }

    fn range(&self) -> Range {
        return Range {
            start: self.pos(),
            end: self.end_pos(),
        };
    }

    fn consume_number(&mut self) {
        while !self.at_end() && (!is_boundary(self.peek()) || self.peek() == '.') {
            self.next();
        }

        let lexeme = self.input_string[self.start..self.offset].to_string();
        let f = lexeme.parse::<f64>().unwrap();
        self.add_token(Token::Number(f));
    }

    fn consume_string(&mut self) {
        while (self.peek() != '\'' || self.peek_next() == '\'') && !self.at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            if self.peek() == '\'' && self.peek_next() == '\'' {
                // Escaped "'", consume
                self.next();
            }
            self.next();
        }

        if self.at_end() {
            // Unterminated string
            // TODO: Error here?
        }

        // Consume the closing "'"
        self.next();

        // Trim the surrounding quotes
        let lexeme = self.input_string[self.start + 1..self.offset - 1]
            .to_string()
            .replace("''", "'");
        // Replace every '' with '
        self.add_token(Token::String(lexeme));
    }

    fn consume_identifier(&mut self) {
        while !self.at_end() && !is_boundary(self.peek()) {
            self.next();
        }

        let lexeme: &str = &self.input_string[self.start..self.offset];
        if !is_legal_identifier(lexeme) {
            // TODO: Error here
        }

        if self.previous() != Token::Dot {
            let token = match lexeme {
                "true" => Token::True,
                "false" => Token::False,
                "null" => Token::Null,
                "NaN" => Token::Number(f64::NAN),
                "Infinity" => Token::Number(f64::INFINITY),
                _ => Token::Identifier(lexeme.to_string()),
            };
            self.add_token(token)
        }
    }
}

fn is_boundary(c: char) -> bool {
    match c {
        '(' | '[' | ')' | ']' | ',' | '.' | '!' | '>' | '<' | '=' | '&' | '|' => true,
        _ => c.is_whitespace(),
    }
}

fn is_legal_identifier(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }

    let first = s.chars().next().unwrap();
    if first.is_ascii_alphabetic() || first == '_' {
        for c in s.chars().skip(1) {
            if c.is_ascii_alphanumeric() || c == '_' || c == '-' {
                // OK
            } else {
                return false;
            }
        }

        return true;
    }

    false
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! test {
        ($name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $name() {
                assert_tokens($input, $expected);
            }
        };
    }

    fn assert_tokens(input: &str, expected: &[Token]) {
        let mut e = expected.to_vec();
        e.push(Token::Eof);

        let actual = Lexer::new(String::from(input))
            .lex()
            .iter()
            .map(|t| t.token.clone())
            .collect::<Vec<Token>>();
        assert_eq!(e, actual);
    }

    test!(test_empty, "", &[]);
    test!(test_number, "123", &[Token::Number(123.0)]);
    test!(test_number_with_dot, "123.", &[Token::Number(123.0)]);

    test!(test_less, "<", &[Token::Less]);
    test!(test_less_equal, "<=", &[Token::LessEqual]);
    test!(test_greater, ">", &[Token::Greater]);
    test!(test_greater_equal, ">=", &[Token::GreaterEqual]);
    test!(test_equal, "==", &[Token::EqualEqual]);
    test!(test_not_equal, "!=", &[Token::BangEqual]);
    test!(test_and, "&&", &[Token::And]);
    test!(test_or, "||", &[Token::Or]);

    test!(
        test_string,
        "'hello'",
        &[Token::String(String::from("hello"))]
    );

    test!(
        test_string_with_single_quote,
        "'It''s a string'",
        &[Token::String(String::from("It's a string"))]
    );

    test!(
        test_array,
        "[1, 2, 3]",
        &[
            Token::LeftBracket,
            Token::Number(1.0),
            Token::Comma,
            Token::Number(2.0),
            Token::Comma,
            Token::Number(3.0),
            Token::RightBracket
        ]
    );

    test!(
        test_expression,
        "1 == 2",
        &[Token::Number(1.0), Token::EqualEqual, Token::Number(2.0)]
    );

    test!(
        test_identifier,
        "foo",
        &[Token::Identifier(String::from("foo"))]
    );

    test!(test_true, "true", &[Token::True]);
    test!(test_false, "false", &[Token::False]);
    test!(test_null, "null", &[Token::Null]);
}
