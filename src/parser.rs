use crate::lexer::{Comparator, Token};

#[derive(Debug)]
pub struct AST<'a> {
    pub root: Box<[Command<'a>]>,
}

#[derive(Debug)]
pub enum Command<'a> {
    RawExpr(Expr<'a>),
    If(Expr<'a>, Box<[Command<'a>]>, Box<[Command<'a>]>),
    For(&'a str, (i32, i32), Box<[Command<'a>]>),
    Init(&'a str, Expr<'a>),
    VarReassign(&'a str, Expr<'a>),
    ArrIndexReassign(&'a str, Expr<'a>, Expr<'a>),
    Continue,
}

#[derive(Debug)]
pub enum Expr<'a> {
    Cmp(Box<Expr<'a>>, Box<Expr<'a>>, Comparator),
    Add(Box<Expr<'a>>, Box<Expr<'a>>),
    Sub(Box<Expr<'a>>, Box<Expr<'a>>),
    Mult(Box<Expr<'a>>, Box<Expr<'a>>),
    Ident(&'a str),
    Raw(i32),
    ArrIndex(&'a str, Box<Expr<'a>>),
    Or(Box<Expr<'a>>, Box<Expr<'a>>),
    And(Box<Expr<'a>>, Box<Expr<'a>>),
}

pub fn parse_ast<'a>(tokens: &[Token<'a>]) -> AST<'a> {
    AST {
        root: parse(tokens, &mut 0),
    }
}

fn parse<'a>(tokens: &[Token<'a>], index: &mut usize) -> Box<[Command<'a>]> {
    let mut cmds = vec![];
    while let Some(token) = tokens.get(*index) {
        let cmd = match token {
            Token::Int => parse_init(tokens, index),
            Token::For => parse_for(tokens, index),
            Token::If => parse_if(tokens, index),
            Token::Continue => {
                *index += 1;
                Command::Continue
            }
            Token::CurlyClose => {
                *index += 1;
                break;
            }
            Token::SemiColon => {
                *index += 1;
                continue;
            }
            Token::Ident(ident) => {
                //Scan hen til nærmeste semicolon,
                //hvis den finder en 'Eq' er det en reassignment
                //ellers er det en Expr
                let mut i2 = *index + 1;
                let is_assignment = loop {
                    match tokens.get(i2) {
                        Some(Token::SemiColon) | None => break false,
                        Some(Token::Eq) => break true,
                        _ => i2 += 1,
                    }
                };
                if is_assignment {
                    *index += 1;
                    if tokens[*index] == Token::BrackOpen {
                        parse_arrindex_reassign(tokens, index, ident)
                    } else {
                        parse_var_reassign(tokens, index, *ident)
                    }
                } else {
                    Command::RawExpr(parse_expr(tokens, index))
                }
            }

            _ => panic!(
                "Expected, Int, For, If or Ident. Found {:#?}{}",
                tokens[*index],
                failed_here(tokens, *index)
            ),
        };
        cmds.push(cmd);
    }
    cmds.into()
}

fn parse_expr<'a>(tokens: &[Token<'a>], index: &mut usize) -> Expr<'a> {
    parse_or(tokens, index)
}

fn parse_init<'a>(tokens: &[Token<'a>], index: &mut usize) -> Command<'a> {
    *index += 1;
    let Token::Ident(ident) = tokens[*index] else {
        panic!("Expected identifier found {:#?}", tokens[*index]);
    };
    *index += 1;
    if tokens[*index] != Token::Eq {
        panic!("Expected '=' found {:#?}", tokens[*index]);
    }
    *index += 1;

    let val = parse_expr(tokens, index);

    Command::Init(ident, val.into())
}

fn parse_or<'a>(tokens: &[Token<'a>], index: &mut usize) -> Expr<'a> {
    let mut left = parse_and(tokens, index);

    while tokens[*index] == Token::Or {
        *index += 1;
        let right = parse_and(tokens, index);
        left = Expr::Or(left.into(), right.into());
    }
    left
}

fn parse_and<'a>(tokens: &[Token<'a>], index: &mut usize) -> Expr<'a> {
    let mut left = parse_cmp(tokens, index);

    while tokens[*index] == Token::And {
        *index += 1;
        let right = parse_cmp(tokens, index);
        left = Expr::And(left.into(), right.into());
    }
    left
}

fn parse_cmp<'a>(tokens: &[Token<'a>], index: &mut usize) -> Expr<'a> {
    let mut left = parse_addition(tokens, index);
    while matches!(tokens[*index], Token::Cmp(_)) {
        let Token::Cmp(comparator) = tokens[*index] else {
            unreachable!()
        };
        *index += 1;

        let right = parse_addition(tokens, index);

        left = Expr::Cmp(left.into(), right.into(), comparator)
    }

    left
}

fn parse_addition<'a>(tokens: &[Token<'a>], index: &mut usize) -> Expr<'a> {
    let mut left = parse_subtract(tokens, index);

    while tokens[*index] == Token::Plus {
        *index += 1;
        let right = parse_subtract(tokens, index);
        left = Expr::Add(left.into(), right.into());
    }
    left
}

fn parse_subtract<'a>(tokens: &[Token<'a>], index: &mut usize) -> Expr<'a> {
    let mut left = parse_mult(tokens, index);

    while tokens[*index] == Token::Minus {
        *index += 1;
        let right = parse_mult(tokens, index);
        left = Expr::Sub(left.into(), right.into());
    }
    left
}

fn parse_mult<'a>(tokens: &[Token<'a>], index: &mut usize) -> Expr<'a> {
    let mut left = parse_paren(tokens, index);

    while tokens[*index] == Token::Mult {
        *index += 1;
        let right = parse_paren(tokens, index);
        left = Expr::Mult(left.into(), right.into());
    }
    left
}

fn parse_paren<'a>(tokens: &[Token<'a>], index: &mut usize) -> Expr<'a> {
    if tokens[*index] == Token::ParenOpen {
        *index += 1;
        let val = parse_expr(tokens, index);

        if tokens[*index] != Token::ParenClose {
            panic!(
                "Expected ')' found {:#?}{}",
                tokens[*index],
                failed_here(tokens, *index)
            )
        }
        *index += 1;
        val
    } else {
        parse_root(tokens, index)
    }
}

fn parse_root<'a>(tokens: &[Token<'a>], index: &mut usize) -> Expr<'a> {
    match tokens[*index] {
        Token::Integer(int) => {
            *index += 1;
            Expr::Raw(int)
        }
        Token::Ident(ident) => {
            *index += 1;
            if tokens.get(*index) == Some(&Token::BrackOpen) {
                *index += 1;
                let arr_index = parse_expr(tokens, index);
                if tokens[*index] != Token::BrackClose {
                    panic!("Expected ']' found {:#?}", tokens[*index])
                }
                *index += 1;
                Expr::ArrIndex(ident, arr_index.into())
            } else {
                Expr::Ident(ident)
            }
        }
        _ => panic!(
            "Expected Ident or Number, found {:#?}{}",
            tokens[*index],
            failed_here(tokens, *index)
        ),
    }
}

fn parse_for<'a>(tokens: &[Token<'a>], index: &mut usize) -> Command<'a> {
    *index += 1;
    if tokens[*index] != Token::ParenOpen {
        panic!("Expected '(' found {:#?}", tokens[*index])
    }

    *index += 1;
    let Token::Ident(ident) = tokens[*index] else {
        panic!("Expected Ident found {}:#?", tokens[*index]);
    };

    *index += 1;
    if tokens[*index] != Token::From {
        panic!("Expected 'from' found {:#?}", tokens[*index]);
    }

    *index += 1;
    let Token::Integer(start) = tokens[*index] else {
        panic!("Expected Integer found {}:#?", tokens[*index]);
    };

    *index += 1;
    if tokens[*index] != Token::To {
        panic!("Expected 'to' found {:#?}", tokens[*index]);
    }

    *index += 1;
    let Token::Integer(end) = tokens[*index] else {
        panic!("Expected Integer found {}:#?", tokens[*index]);
    };

    *index += 1;
    if tokens[*index] != Token::ParenClose {
        panic!("Expected ')' found {:#?}", tokens[*index])
    }

    *index += 1;
    if tokens[*index] != Token::CurlyOpen {
        panic!("Expected '{{' found {:#?}", tokens[*index])
    }
    *index += 1;

    let sub = parse(tokens, index);

    Command::For(ident, (start, end), sub)
}

fn parse_if<'a>(tokens: &[Token<'a>], index: &mut usize) -> Command<'a> {
    *index += 1;
    if tokens[*index] != Token::ParenOpen {
        panic!("Expected '(' found {:#?}", tokens[*index])
    }
    *index += 1;
    let cond = parse_expr(tokens, index);

    if tokens[*index] != Token::ParenClose {
        panic!("Expected ')' found {:#?}", tokens[*index])
    }

    *index += 1;
    if tokens[*index] != Token::CurlyOpen {
        panic!("Expected '{{' found {:#?}", tokens[*index])
    }

    *index += 1;
    let sub = parse(tokens, index);

    let else_sub = if tokens[*index] == Token::Else {
        *index += 1;
        if tokens[*index] != Token::CurlyOpen {
            panic!("Expected '{{' found {:#?}", tokens[*index])
        }
        *index += 1;
        parse(tokens, index)
    } else {
        [].into()
    };
    Command::If(cond, sub, else_sub)
}

fn parse_var_reassign<'a>(tokens: &[Token<'a>], index: &mut usize, ident: &'a str) -> Command<'a> {
    if tokens[*index] != Token::Eq {
        panic!("Expected '=' found {:#?}", tokens[*index])
    }
    *index += 1;
    let expr = parse_expr(tokens, index);

    Command::VarReassign(ident, expr.into())
}

fn parse_arrindex_reassign<'a>(
    tokens: &[Token<'a>],
    index: &mut usize,
    ident: &'a str,
) -> Command<'a> {
    *index += 1;
    let index_expr = parse_expr(tokens, index);

    if tokens[*index] != Token::BrackClose {
        panic!("Expected ']' found {:#?}", tokens[*index])
    }

    *index += 1;
    if tokens[*index] != Token::Eq {
        panic!(
            "Expected '=' found {:#?}, {}",
            tokens[*index],
            failed_here(tokens, *index)
        )
    }
    *index += 1;

    let val = parse_expr(tokens, index);

    Command::ArrIndexReassign(ident, index_expr.into(), val.into())
}

fn failed_here(tokens: &[Token<'_>], index: usize) -> String {
    let mut msg = String::from("\nFailed at: ");

    msg.push_str(&index.to_string());
    msg.push('\n');

    for i in 0..index {
        msg.push_str(&tokens[i].to_string())
    }
    msg.push_str("{{FAILED}} ");
    for i in index..tokens.len() {
        msg.push_str(&tokens[i].to_string())
    }

    msg
}
