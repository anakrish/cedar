// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use crate::*;

#[derive(Debug, Clone)]
pub struct Policy {
    pub span: Span,
    pub annotations: Vec<Annotation>,
    pub effect: Effect,
    pub scope: Scope,
    pub conditions: Vec<Condition>,
}

#[derive(Debug, Clone)]
pub enum Effect {
    Permit,
    Forbid,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub span: Span,
    pub principal: Principal,
    pub action: Action,
    pub resource: Resource,
}

#[derive(Debug, Clone)]
pub struct Principal {
    pub span: Span,
    pub qualifier: Option<Qualifier>,
}

#[derive(Debug, Clone)]
pub enum Qualifier {
    IsIn {
        span: Span,
        path: Vec<Value>,
        category: Option<EntityOrTemplate>,
    },
    In {
        span: Span,
        category: EntityOrTemplate,
    },
    Equals {
        span: Span,
        category: EntityOrTemplate,
    },
}

#[derive(Debug, Clone)]
pub enum Action {
    All,
    Equals { span: Span, entity: Entity },
    In { span: Span, entities: Vec<Entity> },
}

#[derive(Debug, Clone)]
pub struct Resource {
    pub span: Span,
    pub qualifier: Option<Qualifier>,
}

#[derive(Debug, Clone)]
pub struct Annotation {
    pub span: Span,
    pub key: regorus::Value,
    pub value: regorus::Value,
}

#[derive(Debug, Clone)]
pub struct Condition {
    pub span: Span,
    pub ctype: ConditionType,
    pub exprs: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub enum ConditionType {
    When,
    Unless,
}

#[derive(Debug, Clone)]
pub struct Entity {
    pub path: Vec<Value>,
}

#[derive(Debug, Clone)]
pub enum EntityOrTemplate {
    Entity { entity: Entity },
    Template { span: Span },
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Or,
    And,
    Has,
    Like,
    Less,
    LessEqual,
    GreaterEqual,
    Greater,
    NotEqual,
    Equal,
    In,
    Add,
    Sub,
    Mul,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Not,
    Minus,
}

#[derive(Debug, Clone)]
pub enum Expr {
    If {
        span: Span,
        condition: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    Bin {
        span: Span,
        left: Box<Expr>,
        right: Box<Expr>,
        op: BinOp,
    },
    IsIn {
        span: Span,
        left: Box<Expr>,
        path: Vec<Value>,
        in_expr: Option<Box<Expr>>,
    },
    Unary {
        span: Span,
        expr: Box<Expr>,
        op: UnaryOp,
    },
    Member {
        span: Span,
        expr: Box<Expr>,
        access: Vec<Access>,
    },
    ExtFcnCall {
        span: Span,
        path: Vec<Value>,
        args: Vec<Expr>,
    },
    List {
        span: Span,
        exprs: Vec<Expr>,
    },
    Entity {
        span: Span,
        path: Vec<Value>,
    },
    Ident {
        span: Span,
        name: Value,
    },
    Var {
        span: Span,
        name: Value,
    },
    Str {
        span: Span,
        value: Value,
    },
    Bool {
        span: Span,
        value: Value,
    },
}

#[derive(Debug, Clone)]
pub enum Access {
    Field {
        span: Span,
        field: Value,
    },
    Call {
        span: Span,
        name: Value,
        args: Vec<Expr>,
    },
}
