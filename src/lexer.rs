#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
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
pub struct Token {
    pub token_type: TokenType,
    pub range: Range, // TODO: Can this be included in the TokenType enum, removing the need for a separate Token struct?
}

pub struct Lexer {
    input: Vec<char>,
    input_string: String,    // Original input string for error reporting.
    start: usize,            // Start of current lexeme.
    offset: usize,           // Current offset in input.
    line: usize,             // Current line number.
    last_line_offset: usize, // Offset of last line break.
    tokens: Vec<Token>,
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

    pub fn lex(mut self) -> Vec<Token> {
        // TODO: Validate max expression length.
        while !self.at_end() {
            self.start = self.offset;
            match self.next() {
                '(' => self.add_token(TokenType::LeftParen),
                ')' => self.add_token(TokenType::RightParen),
                '[' => self.add_token(TokenType::LeftBracket),
                ']' => self.add_token(TokenType::RightBracket),
                ',' => self.add_token(TokenType::Comma),
                '.' => {
                    let previous = self.previous();
                    match previous {
                        TokenType::Identifier(_)
                        | TokenType::RightBracket
                        | TokenType::LeftBracket
                        | TokenType::Star => {
                            self.consume_number();
                        }
                        _ => {
                            self.add_token(TokenType::Dot);
                        }
                    }
                }
                '-' | '+' => self.consume_number(),
                '!' => {
                    if self.match_char('=') {
                        self.add_token(TokenType::BangEqual);
                    } else {
                        self.add_token(TokenType::Bang);
                    }
                }
                '=' => {
                    if self.match_char('=') {
                        self.add_token(TokenType::EqualEqual);
                    } else {
                        // Illegal; continue reading until we hit a boundary character and return an error
                        self.consume_identifier();
                    }
                }
                '<' => {
                    if self.match_char('=') {
                        self.add_token(TokenType::LessEqual);
                    } else {
                        self.add_token(TokenType::Less);
                    }
                }
                '>' => {
                    if self.match_char('=') {
                        self.add_token(TokenType::GreaterEqual);
                    } else {
                        self.add_token(TokenType::Greater);
                    }
                }
                '&' => {
                    if self.match_char('&') {
                        self.add_token(TokenType::And);
                    } else {
                        // Illegal; continue reading until we hit a boundary character and return an error
                        self.consume_identifier();
                    }
                }
                '|' => {
                    if self.match_char('|') {
                        self.add_token(TokenType::Or);
                    } else {
                        // Illegal; continue reading until we hit a boundary character and return an error
                        self.consume_identifier();
                    }
                }
                '*' => self.add_token(TokenType::Star),
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

        self.tokens.push(Token {
            token_type: TokenType::Eof,
            range: self.range(),
        });

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

    fn previous(&self) -> TokenType {
        if self.tokens.len() == 0 {
            TokenType::Eof
        } else {
            self.tokens[self.tokens.len() - 1].token_type.clone()
        }
    }

    fn at_end(&self) -> bool {
        self.offset >= self.input.len()
    }

    fn add_token(&mut self, token_type: TokenType) {
        let token = Token {
            token_type,
            range: self.range(),
        };
        self.tokens.push(token);
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
        self.add_token(TokenType::Number(f));
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
        self.add_token(TokenType::String(lexeme));
    }

    fn consume_identifier(&mut self) {
        while !self.at_end() && !is_boundary(self.peek()) {
            self.next();
        }

        let lexeme: &str = &self.input_string[self.start..self.offset];
        if !is_legal_identifier(lexeme) {
            // TODO: Error here
        }

        if self.previous() != TokenType::Dot {
            let token = match lexeme {
                "true" => TokenType::True,
                "false" => TokenType::False,
                "null" => TokenType::Null,
                "NaN" => TokenType::Number(f64::NAN),
                "Infinity" => TokenType::Number(f64::INFINITY),
                _ => TokenType::Identifier(lexeme.to_string()),
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

    fn assert_tokens(input: &str, expected: &[TokenType]) {
        let mut e = expected.to_vec();
        e.push(TokenType::Eof);

        let actual = Lexer::new(String::from(input))
            .lex()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();
        assert_eq!(e, actual);
    }

    test!(test_empty, "", &[]);
    test!(test_number, "123", &[TokenType::Number(123.0)]);
    test!(test_number_with_dot, "123.", &[TokenType::Number(123.0)]);

    test!(test_less, "<", &[TokenType::Less]);
    test!(test_less_equal, "<=", &[TokenType::LessEqual]);
    test!(test_greater, ">", &[TokenType::Greater]);
    test!(test_greater_equal, ">=", &[TokenType::GreaterEqual]);
    test!(test_equal, "==", &[TokenType::EqualEqual]);
    test!(test_not_equal, "!=", &[TokenType::BangEqual]);
    test!(test_and, "&&", &[TokenType::And]);
    test!(test_or, "||", &[TokenType::Or]);

    test!(
        test_string,
        "'hello'",
        &[TokenType::String(String::from("hello"))]
    );

    test!(
        test_string_with_single_quote,
        "'It''s a string'",
        &[TokenType::String(String::from("It's a string"))]
    );

    test!(
        test_array,
        "[1, 2, 3]",
        &[
            TokenType::LeftBracket,
            TokenType::Number(1.0),
            TokenType::Comma,
            TokenType::Number(2.0),
            TokenType::Comma,
            TokenType::Number(3.0),
            TokenType::RightBracket
        ]
    );

    test!(
        test_expression,
        "1 == 2",
        &[
            TokenType::Number(1.0),
            TokenType::EqualEqual,
            TokenType::Number(2.0)
        ]
    );

    test!(
        test_identifier,
        "foo",
        &[TokenType::Identifier(String::from("foo"))]
    );

    test!(test_true, "true", &[TokenType::True]);
    test!(test_false, "false", &[TokenType::False]);
    test!(test_null, "null", &[TokenType::Null]);
}
