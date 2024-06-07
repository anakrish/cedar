// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use crate::ast::*;
use crate::*;
use anyhow::{bail, Result};

use crate::ast::{BinOp, Expr};

pub struct Parser<'source> {
    source: Source,
    lexer: Lexer<'source>,
    tok: Token,
    line: u32,
    end: u32,
}

impl<'source> Parser<'source> {
    pub fn new(source: &'source Source) -> Result<Self> {
        let mut lexer = Lexer::new(source);
        lexer.set_unknown_char_is_symbol(true);
        lexer.set_comment_starts_with_double_slash(true);
	lexer.set_double_colon_token(true);
        let tok = lexer.next_token()?;
        Ok(Self {
            source: source.clone(),
            lexer,
            tok,
            line: 0,
            end: 0,
        })
    }

    pub fn token_text(&self) -> &str {
        match self.tok.0 {
            TokenKind::Symbol | TokenKind::Number | TokenKind::Ident | TokenKind::Eof => {
                self.tok.1.text()
            }
            TokenKind::String | TokenKind::RawString => "",
        }
    }

    pub fn next_token(&mut self) -> Result<()> {
        self.line = self.tok.1.line;
        self.end = self.tok.1.end;
        self.tok = self.lexer.next_token()?;
        Ok(())
    }

    fn expect(&mut self, text: &str, context: &str) -> Result<()> {
        if self.token_text() == text {
            self.next_token()
        } else {
            let msg = format!("expecting `{text}` {context}");
            Err(self.source.error(self.tok.1.line, self.tok.1.col, &msg))
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Policy>> {
        let mut policies = vec![];

        while self.tok.0 != TokenKind::Eof {
            policies.push(self.parse_policy()?);
        }

        Ok(policies)
    }

    fn parse_policy(&mut self) -> Result<Policy> {
        let mut span = self.tok.1.clone();
        let annotations = self.parse_annotations()?;

        let effect = match self.token_text() {
            "permit" => Effect::Permit,
            "forbid" => Effect::Forbid,
            _ => bail!(self.tok.1.error("expecting `permit` or `forbid`")),
        };
        self.next_token()?;
        let scope = self.parse_scope()?;
        let conditions = self.parse_conditions()?;

        self.expect(";", "while parsing policy")?;
        span.end = self.end;
        Ok(Policy {
            span,
            annotations,
            effect,
            scope,
            conditions,
        })
    }

    fn parse_annotations(&mut self) -> Result<Vec<Annotation>> {
        let mut annos = vec![];
        while self.tok.0 == TokenKind::Symbol && self.token_text() == "@" {
            let mut span = self.tok.1.clone();
            self.next_token()?;
            if self.tok.0 != TokenKind::Ident {
                bail!(self.tok.1.error("expecting annotation identifier"))
            }

            let key = Value::from(self.token_text());
            self.next_token()?;

            self.expect("(", "while parsing annotation")?;

            if self.tok.0 != TokenKind::String {
                bail!(self.tok.1.error("expecting annotation string"));
            }
            let value = Value::from(self.token_text());
            self.next_token()?;

            self.expect(")", "while parsing annotation")?;

            span.end = self.end;
            annos.push(Annotation { span, key, value });
        }
        Ok(annos)
    }

    fn parse_scope(&mut self) -> Result<Scope> {
        let mut span = self.tok.1.clone();
        self.expect("(", "while parsing policy scope")?;

        let principal = self.parse_principal()?;
        self.expect(",", "after principal specification")?;

        let action = self.parse_action()?;
        self.expect(",", "after action specification")?;

        let resource = self.parse_resource()?;
        self.expect(")", "while parsing policy scope")?;

        span.end = self.end;
        Ok(Scope {
            span,
            principal,
            action,
            resource,
        })
    }

    fn parse_principal(&mut self) -> Result<Principal> {
        let mut span = self.tok.1.clone();
        self.expect("principal", "")?;
        let qualifier = self.parse_qualifier(true)?;
        span.end = self.end;
        Ok(Principal { span, qualifier })
    }

    fn parse_action(&mut self) -> Result<Action> {
        let mut span = self.tok.1.clone();
        self.expect("action", "")?;

        if self.token_text() == "==" {
            self.next_token()?;
            let entity = self.parse_entity()?;
            span.end = self.end;
            Ok(Action::Equals { span, entity })
        } else if self.token_text() == "in" {
            let mut entities = vec![];
            if self.token_text() == "[" {
                self.next_token()?;
                entities.push(self.parse_entity()?);
                while self.token_text() != "]" {
                    self.expect(",", "while parsing entity list")?;
                    entities.push(self.parse_entity()?);
                }
                self.expect("]", "while parsing entity list")?;
            } else {
                entities.push(self.parse_entity()?);
            }
            span.end = self.end;
            Ok(Action::In { span, entities })
        } else {
            Ok(Action::All)
        }
    }

    fn parse_resource(&mut self) -> Result<Resource> {
        let mut span = self.tok.1.clone();
        self.expect("resource", "")?;
        let qualifier = self.parse_qualifier(true)?;
        span.end = self.end;
        Ok(Resource { span, qualifier })
    }

    fn parse_qualifier(&mut self, is_principal: bool) -> Result<Option<Qualifier>> {
        match self.token_text() {
            "," => return Ok(None),
            "in" | "is" | "==" => (),
            _ => bail!(self.tok.1.error("expecting `in`, `is` or ==")),
        }

        let mut span = self.tok.1.clone();
        Ok(Some(if self.token_text() == "is" {
            self.next_token()?;
            let path = self.parse_path()?;
            let category = if self.token_text() == "in" {
                self.next_token()?;
                Some(self.parse_entity_or_template(is_principal)?)
            } else {
                None
            };
            span.end = self.end;
            Qualifier::IsIn {
                span,
                path,
                category,
            }
        } else if self.token_text() == "in" {
            self.next_token()?;
            let category = self.parse_entity_or_template(is_principal)?;
            span.end = self.end;
            Qualifier::In { span, category }
        } else if self.token_text() == "==" {
            self.next_token()?;
            let category = self.parse_entity_or_template(is_principal)?;
            Qualifier::Equals { span, category }
        } else {
            bail!(self.tok.1.error("expecting `is`, 'in` or =="));
        }))
    }

    fn parse_path_into(&mut self, path: &mut Vec<Value>) -> Result<()> {	
        if self.tok.0 != TokenKind::Ident {
            bail!(self
                .tok
                .1
                .error("expecting identifier while parsing entity"));
        }
        path.push(Value::from(self.token_text()));
        self.next_token()?;

        while self.token_text() == "::" {
            self.next_token()?;
            if self.tok.0 == TokenKind::Ident {
                path.push(Value::from(self.token_text()));
                self.next_token()?;
            } else if self.tok.0 == TokenKind::String {
                break;
            } else {
                bail!(self
                    .tok
                    .1
                    .error("expecting identifer or string while parsing entity"));
            }
        }
        Ok(())
    }

    fn parse_path(&mut self) -> Result<Vec<Value>> {
        let mut path = vec![];
        self.parse_path_into(&mut path)?;
        Ok(path)
    }

    fn parse_entity(&mut self) -> Result<Entity> {
        let mut path = self.parse_path()?;
        if self.tok.0 != TokenKind::String {
            bail!(self.tok.1.error("expecting string entity identifier"));
        }
        path.push(Value::from(self.tok.1.text()));
        self.next_token()?;
        Ok(Entity { path })
    }

    fn parse_entity_or_template(&mut self, is_principal: bool) -> Result<EntityOrTemplate> {
        if self.tok.0 == TokenKind::Ident {
            let entity = self.parse_entity()?;
            Ok(EntityOrTemplate::Entity { entity })
        } else if self.tok.0 == TokenKind::Symbol && self.token_text() == "?" {
            let mut span = self.tok.1.clone();
            self.next_token()?;
            if self.tok.1.start == span.end + 1 {
                match self.token_text() {
                    "principal" if is_principal => (),
                    "resource" if !is_principal => (),
                    _ => {
                        let msg = if is_principal {
                            "expecting `?principal`"
                        } else {
                            "expecting `?resource`"
                        };
                        bail!(self.tok.1.error(msg));
                    }
                }
                self.next_token()?;
                span.end = self.end;
                return Ok(EntityOrTemplate::Template { span });
            } else {
                bail!(self.tok.1.error("expecting `?principal` or `?resource`"))
            }
        } else {
            bail!(self
                .tok
                .1
                .error("expecting entity, `?principal` or `?resource`"))
        }
    }

    fn parse_conditions(&mut self) -> Result<Vec<Condition>> {
        let mut conditions = vec![];
        while !matches!(self.token_text(), ";" | "permit" | "forbid") {
            conditions.push(self.parse_condition()?);
        }
        Ok(conditions)
    }

    fn parse_condition(&mut self) -> Result<Condition> {
        let mut span = self.tok.1.clone();

        let ctype = match self.token_text() {
            "when" => ConditionType::When,
            "unless" => ConditionType::Unless,
            _ => bail!(self.tok.1.error("expecting `when` or `unless`")),
        };
        self.next_token()?;

        self.expect("{", "while parsing condition body")?;
        let mut exprs = vec![];
        while self.token_text() != "}" {
            exprs.push(self.parse_expr()?);
        }
        self.expect("}", "while parsing condition body")?;
        span.end = self.end;

        Ok(Condition { span, ctype, exprs })
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        if self.token_text() == "if" {
            let mut span = self.tok.1.clone();
            self.next_token()?;
            let condition = Box::new(self.parse_expr()?);
            self.expect("then", "")?;
            let then_expr = Box::new(self.parse_expr()?);
            self.expect("else", "")?;
            let else_expr = Box::new(self.parse_expr()?);
            span.end = self.end;
            Ok(Expr::If {
                span,
                condition,
                then_expr,
                else_expr,
            })
        } else {
            self.parse_or_expr()
        }
    }

    fn parse_or_expr(&mut self) -> Result<Expr> {
        let mut span = self.tok.1.clone();
        let mut left = self.parse_and_expr()?;
        while self.token_text() == "||" {
            self.next_token()?;
            let right = self.parse_and_expr()?;
            span.end = self.end;
            left = Expr::Bin {
                span: span.clone(),
                left: Box::new(left),
                right: Box::new(right),
                op: BinOp::Or,
            };
        }
        Ok(left)
    }

    fn parse_and_expr(&mut self) -> Result<Expr> {
        let mut span = self.tok.1.clone();
        let mut left = self.parse_rel_expr()?;
        while self.token_text() == "&&" {
            self.next_token()?;
            let right = self.parse_rel_expr()?;
            span.end = self.end;
            left = Expr::Bin {
                span: span.clone(),
                left: Box::new(left),
                right: Box::new(right),
                op: BinOp::And,
            };
        }
        Ok(left)
    }

    fn is_relop(&self) -> Option<BinOp> {
        Some(match self.token_text() {
            "<" => BinOp::Less,
            "<=" => BinOp::LessEqual,
            ">=" => BinOp::GreaterEqual,
            ">" => BinOp::Greater,
            "!=" => BinOp::NotEqual,
            "==" => BinOp::Equal,
            "in" => BinOp::In,
            _ => return None,
        })
    }

    fn parse_ident(&mut self) -> Result<Expr> {
        let t = if self.tok.0 == TokenKind::Ident {
            Expr::Ident {
                span: self.tok.1.clone(),
                name: Value::String(self.tok.1.text().into()),
            }
        } else {
            bail!(self.tok.1.error("expecting identifier"))
        };
        self.next_token()?;
        Ok(t)
    }

    fn parse_str(&mut self) -> Result<Expr> {
        let t = if self.tok.0 == TokenKind::String {
            Expr::Str {
                span: self.tok.1.clone(),
                value: Value::String(self.tok.1.text().into()),
            }
        } else {
            bail!(self.tok.1.error("expecting string"))
        };
        self.next_token()?;
        Ok(t)
    }

    fn parse_rel_expr(&mut self) -> Result<Expr> {
        let mut span = self.tok.1.clone();
        let mut left = self.parse_add_expr()?;
        loop {
            let (right, op) = match self.token_text() {
                "has" => {
                    self.next_token()?;
                    let right = match self.tok.0 {
                        TokenKind::Ident => self.parse_ident()?,
                        TokenKind::String => self.parse_str()?,
                        _ => bail!(self.tok.1.error("expecting identifer or string")),
                    };
                    (right, BinOp::Has)
                }
                "like" => {
                    self.lexer.set_allow_slash_star_escape(true);
                    self.next_token()?;
                    self.lexer.set_allow_slash_star_escape(false);
                    (self.parse_str()?, BinOp::Like)
                }
                "is" => {
                    self.next_token()?;
                    let path = self.parse_path()?;
                    let in_expr = if self.token_text() == "in" {
                        self.next_token()?;
                        Some(Box::new(self.parse_add_expr()?))
                    } else {
                        None
                    };
                    span.end = self.end;
                    left = Expr::IsIn {
                        span: span.clone(),
                        left: Box::new(left),
                        path,
                        in_expr,
                    };
                    continue;
                }
                _ => {
                    if let Some(op) = self.is_relop() {
                        self.next_token()?;
                        (self.parse_add_expr()?, op)
                    } else {
                        break;
                    }
                }
            };
            span.end = self.end;
            left = Expr::Bin {
                span: span.clone(),
                left: Box::new(left),
                right: Box::new(right),
                op,
            };
        }
        Ok(left)
    }

    fn parse_add_expr(&mut self) -> Result<Expr> {
        let mut span = self.tok.1.clone();
        let mut left = self.parse_mul_expr()?;
        loop {
            let op = match self.token_text() {
                "+" => BinOp::Add,
                "-" => BinOp::Sub,
                _ => break,
            };
            self.next_token()?;
            let right = self.parse_mul_expr()?;
            span.end = self.end;
            left = Expr::Bin {
                span: span.clone(),
                left: Box::new(left),
                right: Box::new(right),
                op,
            };
        }
        Ok(left)
    }

    fn parse_mul_expr(&mut self) -> Result<Expr> {
        let mut span = self.tok.1.clone();
        let mut left = self.parse_unary_expr()?;
        while self.token_text() == "*" {
            self.next_token()?;
            let right = self.parse_unary_expr()?;
            span.end = self.end;
            left = Expr::Bin {
                span: span.clone(),
                left: Box::new(left),
                right: Box::new(right),
                op: BinOp::Mul,
            };
        }
        Ok(left)
    }

    fn parse_unary_expr(&mut self) -> Result<Expr> {
        let mut span = self.tok.1.clone();
        let op = match self.token_text() {
            "!" => UnaryOp::Not,
            "-" => UnaryOp::Minus,
            _ => return self.parse_member_expr(),
        };
        self.next_token()?;
        let expr = Box::new(self.parse_unary_expr()?);
        span.end = self.end;
        Ok(Expr::Unary { span, expr, op })
    }

    fn parse_member_expr(&mut self) -> Result<Expr> {
        let mut span = self.tok.1.clone();
        let expr = Box::new(self.parse_primary_expr()?);
        let mut access = vec![];
        loop {
            let mut span = self.tok.1.clone();
            match self.token_text() {
                "." => {
                    self.next_token()?;
                    let field = if self.tok.0 == TokenKind::Ident {
                        Value::from(self.token_text())
                    } else {
                        bail!(self.tok.1.error("expected identifier after `.`"));
                    };
                    self.next_token()?;
                    if self.token_text() == "(" {
                        let name = field;
                        self.next_token()?;
                        let args = self.parse_expr_list(")")?;
                        self.expect(")", "while parsing arguments")?;
                        access.push(Access::Call { span, name, args })
                    } else {
                        span.end = self.end;
                        access.push(Access::Field { span, field });
                    }
                }
                "[" => {
                    self.next_token()?;
                    let field = if self.tok.0 == TokenKind::String {
                        Value::from(self.tok.1.text())
                    } else {
                        bail!(self.tok.1.error("expected string after `[`"));
                    };
                    self.next_token()?;
                    self.expect("]", "while parsing index expression")?;
                    span.end = self.end;
                    access.push(Access::Field { span, field });
                }
                _ => break,
            }
        }

        span.end = self.end;
        Ok(Expr::Member { span, expr, access })
    }

    fn parse_expr_list(&mut self, delim: &str) -> Result<Vec<Expr>> {
        let mut exprs = vec![];
        if self.token_text() != delim {
            exprs.push(self.parse_expr()?);
        }
        while self.token_text() != delim {
            self.expect(",", "while parsing expression list")?;
            exprs.push(self.parse_expr()?);
        }

        Ok(exprs)
    }

    fn parse_primary_expr(&mut self) -> Result<Expr> {
        if self.tok.0 == TokenKind::Ident {
            match self.token_text() {
                "principal" | "action" | "resource" | "context" => {
                    let var = Expr::Var {
                        span: self.tok.1.clone(),
                        name: Value::from(self.token_text()),
                    };
                    self.next_token()?;
                    Ok(var)
                }
                "true" | "false" => {
                    let b = Expr::Bool {
                        span: self.tok.1.clone(),
                        value: Value::from(self.token_text() == "true"),
                    };
                    self.next_token()?;
                    Ok(b)
                }
                _ => {
                    let mut span = self.tok.1.clone();
                    let mut path = self.parse_path()?;

                    match self.tok.0 {
                        TokenKind::String => {
                            path.push(Value::from(self.tok.1.text()));
                            self.next_token()?;
                            span.end = self.end;
                            Ok(Expr::Entity { span, path })
                        }
                        TokenKind::Symbol if self.token_text() == "(" => {
                            self.expect("(", "while parsing extfun call")?;
                            let args = self.parse_expr_list(")")?;
                            span.end = self.end;
                            Ok(Expr::ExtFcnCall { span, path, args })
                        }
                        _ if path.len() == 1 => {
                            span.end = self.end;
                            Ok(Expr::Ident {
                                span,
                                name: path[0].clone(),
                            })
                        }
                        _ => {
                            span.end = self.end;
                            Ok(Expr::Entity { span, path })
                        }
                    }
                }
            }
        } else if self.tok.0 == TokenKind::String {
            self.parse_str()
        } else {
            let mut span = self.tok.1.clone();
            match self.token_text() {
                "(" => {
                    self.next_token()?;
                    let e = self.parse_expr()?;
                    self.expect(")", "while parsing parenthesized expression")?;
                    Ok(e)
                }
                "[" => {
                    self.next_token()?;
                    let exprs = self.parse_expr_list("]")?;
                    self.expect(")", "while parsing expression list")?;
                    span.end = self.end;
                    Ok(Expr::List { span, exprs })
                }
                //"{"
                _ => bail!(self.tok.1.error("expecting primary expression")),
            }
        }
    }
}
