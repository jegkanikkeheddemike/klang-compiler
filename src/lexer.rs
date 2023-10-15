use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    ParenOpen,
    ParenClose,
    CurlyOpen,
    CurlyClose,
    BrackOpen,
    BrackClose,
    Plus,
    Minus,
    Mult,
    Cmp(Comparator),
    Eq,
    If,
    Else,
    For,
    From,
    To,
    Continue,
    Int,
    Integer(i32),
    Ident(&'a str),
    SemiColon,
    Or,
    And,
}

#[allow(unused)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Comparator {
    Eq,
    Gt,
    Lt,
}

pub fn lex<'a>(source: &'a str) -> Box<[Token<'a>]> {
    let mut index = 0;

    let chars: Box<[char]> = source.chars().collect();
    let mut tokens = vec![];

    while let Some(c) = chars.get(index) {
        if c.is_whitespace() {
            index += 1;
            continue;
        }
        let val = if let Some(token) = prim_lex(&chars, &mut index) {
            token
        } else {
            if c.is_ascii_digit() {
                let start = index;
                while let Some(n_char) = chars.get(index) {
                    if !n_char.is_ascii_digit() {
                        break;
                    }
                    index += 1;
                }
                index -= 1;
                Token::Integer(source[start..index + 1].parse().unwrap())
            } else {
                //Identifier or keyword
                let start = index;
                while prim_lex(&chars, &mut index) == None && !chars[index].is_ascii_whitespace() {
                    index += 1;
                }

                if *c == '#' {
                    continue;
                } else {
                    index -= 1;
                }

                let text = &source[start..index + 1];
                match text {
                    "if" => Token::If,
                    "from" => Token::From,
                    "to" => Token::To,
                    "for" => Token::For,
                    "continue" => Token::Continue,
                    "int" => Token::Int,
                    "else" => Token::Else,
                    "and" => Token::And,
                    "or" => Token::Or,
                    _ => Token::Ident(text),
                }
            }
        };
        tokens.push(val);
        index += 1;
    }

    tokens.into()
}

fn prim_lex<'a>(chars: &[char], index: &mut usize) -> Option<Token<'a>> {
    match chars[*index] {
        '(' => Some(Token::ParenOpen),
        ')' => Some(Token::ParenClose),
        '{' => Some(Token::CurlyOpen),
        '}' => Some(Token::CurlyClose),
        '+' => Some(Token::Plus),
        '-' => Some(Token::Minus),
        '*' => Some(Token::Mult),
        '[' => Some(Token::BrackOpen),
        ']' => Some(Token::BrackClose),
        '=' => match chars.get(*index + 1) {
            Some('=') => {
                *index += 1;
                Some(Token::Cmp(Comparator::Eq))
            }
            _ => Some(Token::Eq),
        },
        ';' => Some(Token::SemiColon),
        '#' => {
            while let Some(c) = chars.get(*index) {
                if *c == '\n' {
                    break;
                }
                *index += 1;
            }
            if let Some(_) = chars.get(*index) {
                prim_lex(chars, index)
            } else {
                None
            }
        }
        _ => None,
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} ",
            match self {
                Token::ParenOpen => "(".into(),
                Token::ParenClose => ")".into(),
                Token::CurlyOpen => "{".into(),
                Token::CurlyClose => "}".into(),
                Token::BrackOpen => "[".into(),
                Token::BrackClose => "]".into(),
                Token::Plus => "+".into(),
                Token::Minus => "-".into(),
                Token::Mult => "*".into(),
                Token::Cmp(cmp) => match cmp {
                    Comparator::Eq => "==",
                    Comparator::Gt => ">",
                    Comparator::Lt => "<",
                }
                .into(),
                Token::Eq => "=".into(),
                Token::For => "for".into(),
                Token::From => "from".into(),
                Token::To => "to".into(),
                Token::Int => "int".into(),
                Token::Integer(int) => int.to_string(),
                Token::Ident(str) => (*str).into(),
                Token::Continue => "continue".into(),
                Token::SemiColon => ";".into(),
                Token::If => "if".into(),
                Token::Else => "else".into(),
                Token::And => "and".into(),
                Token::Or => "or".into(),
            }
        )
    }
}
