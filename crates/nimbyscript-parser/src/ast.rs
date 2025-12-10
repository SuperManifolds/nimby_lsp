use serde::{Deserialize, Serialize};

/// Span representing a range in source code
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

/// Root of the AST - a complete source file
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceFile {
    pub meta: Option<MetaBlock>,
    pub items: Vec<Item>,
    pub span: Span,
}

/// Script metadata block
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetaBlock {
    pub values: Vec<MetaKV>,
    pub span: Span,
}

/// Key-value pair in metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetaKV {
    pub key: String,
    pub value: MetaValue,
    pub span: Span,
}

/// Metadata value types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MetaValue {
    String(String),
    Number(f64),
    Map(Vec<MetaKV>),
    Vec(Vec<MetaValue>),
    Name(String),
}

/// Top-level items in a source file
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Item {
    Const(ConstDef),
    Struct(StructDef),
    Enum(EnumDef),
    Function(FunctionDef),
}

impl Item {
    pub fn span(&self) -> Span {
        match self {
            Item::Const(c) => c.span,
            Item::Struct(s) => s.span,
            Item::Enum(e) => e.span,
            Item::Function(f) => f.span,
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Item::Const(c) => &c.name.name,
            Item::Struct(s) => &s.name.name,
            Item::Enum(e) => &e.name.name,
            Item::Function(f) => &f.name.name,
        }
    }
}

/// Constant definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstDef {
    pub name: Identifier,
    pub ty: Type,
    pub value: Expr,
    pub span: Span,
}

/// Struct definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructDef {
    pub is_pub: bool,
    pub name: Identifier,
    pub extends: Option<Identifier>,
    pub fields: Vec<Field>,
    pub span: Span,
}

/// Field in a struct
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Field {
    pub name: Identifier,
    pub ty: Type,
    pub meta: Option<MetaBlock>,
    pub span: Span,
}

/// Enum definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnumDef {
    pub is_pub: bool,
    pub name: Identifier,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}

/// Enum variant
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnumVariant {
    pub name: Identifier,
    pub meta: Option<MetaBlock>,
    pub span: Span,
}

/// Function definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionDef {
    pub is_pub: bool,
    pub receiver: Option<Identifier>,
    pub name: Identifier,
    pub params: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Block,
    pub span: Span,
}

/// Function parameter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Parameter {
    pub name: Identifier,
    pub ty: Type,
    pub span: Span,
}

/// Type expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Type {
    pub storage: Option<StorageClass>,
    pub is_mut: bool,
    pub name: TypeName,
    pub span: Span,
}

/// Storage class modifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum StorageClass {
    Reference, // &
    Pointer,   // *
}

/// Type name with optional generic arguments
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeName {
    pub name: String,
    pub type_args: Vec<TypeName>,
    pub path: Vec<String>, // For paths like Foo::Bar
    pub span: Span,
}

/// Identifier with span
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

/// Block of statements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

/// Statement types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Stmt {
    Let(LetStmt),
    LetElse(LetElseStmt),
    Assign(AssignStmt),
    If(IfStmt),
    IfLet(IfLetStmt),
    For(ForStmt),
    Return(ReturnStmt),
    Break(Span),
    Continue(Span),
    Log(LogStmt),
    Expr(ExprStmt),
}

impl Stmt {
    pub fn span(&self) -> Span {
        match self {
            Stmt::Let(s) => s.span,
            Stmt::LetElse(s) => s.span,
            Stmt::Assign(s) => s.span,
            Stmt::If(s) => s.span,
            Stmt::IfLet(s) => s.span,
            Stmt::For(s) => s.span,
            Stmt::Return(s) => s.span,
            Stmt::Break(span) | Stmt::Continue(span) => *span,
            Stmt::Log(s) => s.span,
            Stmt::Expr(s) => s.span,
        }
    }
}

/// Let binding: let x = expr;
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LetStmt {
    pub name: Identifier,
    pub ty: Option<Type>,
    pub value: Expr,
    pub span: Span,
}

/// Let-else binding: let x = expr else { ... }
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LetElseStmt {
    pub name: Identifier,
    pub ty: Option<Type>,
    pub value: Expr,
    pub else_block: Block,
    pub span: Span,
}

/// Assignment: x.y = expr;
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AssignStmt {
    pub target: Vec<Identifier>,
    pub value: Expr,
    pub span: Span,
}

/// If statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_block: Block,
    pub else_branch: Option<ElseBranch>,
    pub span: Span,
}

/// If-let statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IfLetStmt {
    pub name: Identifier,
    pub ty: Option<Type>,
    pub value: Expr,
    pub then_block: Block,
    pub else_branch: Option<ElseBranch>,
    pub span: Span,
}

/// Else branch (can be else if, else if let, or else block)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ElseBranch {
    ElseIf(Box<IfStmt>),
    ElseIfLet(Box<IfLetStmt>),
    Else(Block),
}

/// For loop
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForStmt {
    pub var: Identifier,
    pub iter: Expr,
    pub body: Block,
    pub span: Span,
}

/// Return statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
    pub span: Span,
}

/// Log statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogStmt {
    pub message: String,
    pub args: Vec<Expr>,
    pub span: Span,
}

/// Expression statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExprStmt {
    pub expr: Expr,
    pub span: Span,
}

/// Expression types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expr {
    Literal(Literal),
    Identifier(Identifier),
    Path(PathExpr),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Call(CallExpr),
    FieldAccess(FieldAccessExpr),
    Paren(Box<Expr>),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Literal(l) => l.span(),
            Expr::Identifier(i) => i.span,
            Expr::Path(p) => p.span,
            Expr::Binary(b) => b.span,
            Expr::Unary(u) => u.span,
            Expr::Call(c) => c.span,
            Expr::FieldAccess(f) => f.span,
            Expr::Paren(e) => e.span(),
        }
    }
}

/// Literal values
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Literal {
    Int { value: i64, suffix: Option<String>, span: Span },
    Float { value: f64, suffix: Option<String>, span: Span },
    Bool { value: bool, span: Span },
    String { value: String, span: Span },
    Time { hours: u32, minutes: u32, seconds: Option<u32>, span: Span },
}

impl Literal {
    pub fn span(&self) -> Span {
        match self {
            Literal::Int { span, .. }
            | Literal::Float { span, .. }
            | Literal::Bool { span, .. }
            | Literal::String { span, .. }
            | Literal::Time { span, .. } => *span,
        }
    }
}

/// Path expression (e.g., DB::view, EnumName::Variant)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PathExpr {
    pub segments: Vec<Identifier>,
    pub span: Span,
}

/// Binary expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub op: BinaryOp,
    pub right: Box<Expr>,
    pub span: Span,
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Comparison
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    // Logical
    And,
    Or,
}

impl BinaryOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::Eq => "==",
            BinaryOp::NotEq => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Gt => ">",
            BinaryOp::LtEq => "<=",
            BinaryOp::GtEq => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
        }
    }
}

/// Unary expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub operand: Box<Expr>,
    pub span: Span,
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
    Neg,
    Not,
}

/// Function call expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub type_args: Vec<TypeName>,
    pub args: Vec<Expr>,
    pub span: Span,
}

/// Field access expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldAccessExpr {
    pub object: Box<Expr>,
    pub field: Identifier,
    pub span: Span,
}
