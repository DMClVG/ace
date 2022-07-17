use std::ops::{Deref, Range};

#[derive(PartialEq, Debug, Clone)]
pub enum TokenKind<'a> {
    Number(f64),
    String(String),
    Identifier(&'a str),
    // Coefficient(f64, &'a str),
    Assign,

    Lesser,
    Greater,
    Equal,
    Unequal,
    LesserOrEq,
    GreaterOrEq,

    Exclamation,

    LParenthese,
    RParenthese,
    LBraces,
    RBraces,
    LBrackets,
    RBrackets,

    And,
    Or,
    If,
    Else,
    While,
    For,
    Loop,
    Break,
    Continue,

    Plus,
    Minus,
    Slash,
    Asterisk,
    Percent,

    Period,
    Comma,
    Colon,

    False,
    True,
    Nil,

    Fun,
    Return,

    Invalid,
    EOF,
}

impl TokenKind<'_> {
    const ESCAPE_CHARS: [(char, char); 10] = [
        ('\\', '\\'),
        ('n', '\n'),
        ('r', '\r'),
        ('\n', '\0'), // \ followed by a new line => ignores new line in strings
        ('a', '\x07'),
        ('b', '\x08'),
        ('t', '\t'),
        ('v', '\x0B'),
        ('\'', '\''),
        ('\"', '\"'),
    ];

    fn try_from_escaped_string(s: &str) -> Result<TokenKind, String> {
        let mut was_escaped = false;
        let res = s
            .chars()
            .zip(
                s[s.chars().nth(0).map_or(0, |c| c.len_utf8())..]
                    .chars()
                    .chain("\0".chars()),
            )
            .map(|(c, f)| {
                if was_escaped {
                    was_escaped = false;
                    return None;
                }
                if c == '\\' {
                    for (escape_char, translated) in Self::ESCAPE_CHARS {
                        if f == escape_char {
                            was_escaped = true;
                            return Some(Ok(translated));
                        }
                    }
                    Some(Err(format!("Invalid escape code \\{}", f)))
                } else {
                    Some(Ok(c))
                }
            })
            .flatten() // filter out nones
            .collect::<Result<String, String>>()?;

        Ok(TokenKind::String(res))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub line: usize,
    pub col: usize,
    pub span: Range<usize>,
}

#[derive(Debug, Clone)]
pub struct LexerError<'a> {
    pub responsible: Token<'a>,
    pub cause: String,
}

struct SplitWord<'a> {
    input: &'a str,
    pos: usize,
    line: usize,
    line_col: usize,
}

impl<'a> SplitWord<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: 0,
            line: 0,
            line_col: 0,
        }
    }
}

#[derive(Clone)]
struct Word<'a> {
    body: &'a str,
    line: usize,
    col: usize,
    span: Range<usize>,
}

impl<'a> Deref for Word<'a> {
    type Target = &'a str;

    fn deref(&self) -> &Self::Target {
        &self.body
    }
}

impl<'a> Iterator for SplitWord<'a> {
    type Item = Word<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.input.len() {
            return None;
        }
        let mut word = None;
        let mut is_num = false;

        for (_, c) in self.input[self.pos..].char_indices() {
            let c_len = c.len_utf8();
            self.pos += c_len;
            self.line_col += 1;
            match c {
                _ if c.is_ascii_punctuation() && c != '_' => {
                    if word.is_none() {
                        word = Some((self.line, self.line_col, self.pos - c_len..self.pos));
                        break;
                    } else if is_num && c == '.' {
                        word.as_mut().unwrap().2.end += c_len;
                        continue;
                    } else {
                        self.line_col -= 1;
                        self.pos -= c_len;
                        break;
                    }
                }
                _ if c.is_whitespace() => {
                    if c == '\n' {
                        self.line += 1;
                        self.line_col = 0;
                    }
                    if word.is_none() {
                        // ignore
                        continue;
                    } else {
                        break; // end word if it hit a whitespace character
                    }
                }
                _ => {
                    if let Some(ref mut range) = word {
                        range.2.end += c_len;
                    } else {
                        if c.is_ascii_digit() {
                            is_num = true;
                        }
                        word = Some((self.line, self.line_col, self.pos - c_len..self.pos));
                    }
                }
            }
        }
        if let Some((line, col, span)) = word {
            let res = Word {
                body: &self.input[span.clone()],
                line,
                col: col - 1,
                span,
            };

            Some(res)
        } else {
            None
        }
    }
}

pub struct Lexer<'a> {
    input: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn from_str(input: &'a str) -> Self {
        Self { input }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();

        let words = SplitWord::new(self.input).collect::<Vec<Word>>();
        let mut iter = words.iter().peekable();

        let mut commented = None;
        while let Some(word) = iter.next() {
            let line = word.line;
            let col = word.col;
            let span = word.span.clone();

            if commented.is_some() && commented.unwrap() == line {
                continue; // skip words in commented line
            }

            let kind = match word.body {
                op @ ("<" | ">" | "=" | "!") => {
                    match iter.peek() {
                        Some(&Word { body: "=", .. }) => {
                            iter.next(); // Skip element
                            match op {
                                "<" => TokenKind::LesserOrEq,
                                ">" => TokenKind::GreaterOrEq,
                                "=" => TokenKind::Equal,
                                "!" => TokenKind::Unequal,
                                _ => unreachable!(),
                            }
                        }
                        _ => match op {
                            "<" => TokenKind::Lesser,
                            ">" => TokenKind::Greater,
                            "=" => TokenKind::Assign,
                            "!" => TokenKind::Exclamation,
                            _ => unreachable!(),
                        },
                    }
                }
                "#" => {
                    commented = Some(line);
                    continue;
                }
                "." => TokenKind::Period,
                "(" => TokenKind::LParenthese,
                ")" => TokenKind::RParenthese,
                "{" => TokenKind::LBraces,
                "}" => TokenKind::RBraces,
                "[" => TokenKind::LBrackets,
                "]" => TokenKind::RBrackets,
                "," => TokenKind::Comma,
                ":" => TokenKind::Colon,
                "and" => TokenKind::And,
                "or" => TokenKind::Or,
                "if" => TokenKind::If,
                "else" => TokenKind::Else,
                "while" => TokenKind::While,
                "for" => TokenKind::For,
                "loop" => TokenKind::Loop,
                "*" => TokenKind::Asterisk,
                "+" => TokenKind::Plus,
                "-" => TokenKind::Minus,
                "/" => TokenKind::Slash,
                "%" => TokenKind::Percent,
                "true" => TokenKind::True,
                "false" => TokenKind::False,
                "fun" => TokenKind::Fun,
                "nil" => TokenKind::Nil,
                "continue" => TokenKind::Continue,
                "break" => TokenKind::Break,
                "return" => TokenKind::Return,
                "\"" => {
                    let mut s: Option<Range<usize>> = None;
                    let mut escaped = false;
                    while let Some(t) = iter.next() { // search for unescaped double quote
                        let end = t.span.start;
                        if !escaped && t.body == "\"" {
                            s = Some(span.end..end);
                            break;
                        } else if !escaped && t.body == "\\" {
                            escaped = true;
                        } else {
                            escaped = false;
                        }
                    }
                    TokenKind::try_from_escaped_string(
                        &self.input[s.ok_or(LexerError { cause: "String never terminated".to_owned(), responsible: Token { kind: TokenKind::Invalid, col, line, span: span.clone() }})?],
                    )
                    .unwrap()
                }
                _ => {
                    if let Ok(num) = word.parse::<f64>() {
                        TokenKind::Number(num)
                    } else if word.starts_with(char::is_alphabetic) {
                        // if word.starts_with(char::is_numeric) {
                        //     let num = word.split_at(word.find(|c: char| { c.is_numeric() || c == '.'}).unwrap() + 1);
                        //     TokenKind::Coefficient(num.0.parse().unwrap(), num.1)
                        // } else {
                        TokenKind::Identifier(&word)
                        // }
                    } else {
                        Err(LexerError { cause: "Is not a valid token".to_owned(), responsible: Token { kind: TokenKind::Invalid, col, line, span: span.clone() } })?
                    }
                }
            };
            tokens.push(Token {
                kind,
                line,
                col,
                span,
            });
        }
        tokens.push(Token {
            kind: TokenKind::EOF,
            line: 0,
            col: 0,
            span: Default::default(),
        });
        Ok(tokens)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lexer() {
        let mut lexer =
            Lexer::from_str("while x != 18.32 and size() < (5+1) { wait(\"for eve\n\n\\\"r\") }");

        use TokenKind::*;
        assert_eq!(
            lexer
                .lex()
                .unwrap()
                .into_iter()
                .map(|t| { t.kind })
                .collect::<Vec<TokenKind>>(),
            vec![
                While,
                Identifier("x"),
                Unequal,
                Number(18.32),
                And,
                Identifier("size"),
                LParenthese,
                RParenthese,
                Lesser,
                LParenthese,
                Number(5.0),
                Plus,
                Number(1.0),
                RParenthese,
                LBraces,
                Identifier("wait"),
                LParenthese,
                String("for eve\n\n\"r".to_string()),
                RParenthese,
                RBraces,
                EOF,
            ]
        );
    }

    #[test]
    fn test_splitword_string() {
        let mut splitter = SplitWord::new("\"lol i am  so cool    and 3u r $+. ,[too ] \"    ");
        assert_eq!("\"", *splitter.next().unwrap());
        assert_eq!("lol", *splitter.next().unwrap());
        assert_eq!("i", *splitter.next().unwrap());
        assert_eq!("am", *splitter.next().unwrap());
        assert_eq!("so", *splitter.next().unwrap());
        assert_eq!("cool", *splitter.next().unwrap());
        assert_eq!("and", *splitter.next().unwrap());
        assert_eq!("3u", *splitter.next().unwrap());
        assert_eq!("r", *splitter.next().unwrap());
        assert_eq!("$", *splitter.next().unwrap());
        assert_eq!("+", *splitter.next().unwrap());
        assert_eq!(".", *splitter.next().unwrap());
        assert_eq!(",", *splitter.next().unwrap());
        assert_eq!("[", *splitter.next().unwrap());
        assert_eq!("too", *splitter.next().unwrap());
        assert_eq!("]", *splitter.next().unwrap());
        assert_eq!("\"", *splitter.next().unwrap());
    }

    #[test]
    fn test_splitword_math_expression() {
        let mut splitter = SplitWord::new("(4+96* (72+6/ 3) )");
        assert_eq!("(", *splitter.next().unwrap());
        assert_eq!("4", *splitter.next().unwrap());
        assert_eq!("+", *splitter.next().unwrap());
        assert_eq!("96", *splitter.next().unwrap());
        assert_eq!("*", *splitter.next().unwrap());
        assert_eq!("(", *splitter.next().unwrap());
        assert_eq!("72", *splitter.next().unwrap());
        assert_eq!("+", *splitter.next().unwrap());
        assert_eq!("6", *splitter.next().unwrap());
        assert_eq!("/", *splitter.next().unwrap());
        assert_eq!("3", *splitter.next().unwrap());
        assert_eq!(")", *splitter.next().unwrap());
        assert_eq!(")", *splitter.next().unwrap());
    }
}
