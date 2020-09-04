use std::borrow::Cow;
use core::marker::PhantomData;
use serde::{Deserialize, Serialize};

pub mod borrowed;
pub mod concrete;
pub mod owned;
pub mod ser;

/// Describes all the elements of a syntax.
pub trait Syntax: Sized {
  type Script<'a>: Script<Ast = Self>;

  type Stmt<'a>: Stmt<Ast = Self>;
  type BreakStmt<'a>: BreakStmt<Ast = Self>;
  /// Represents an invalid statement
  type ErrorStmt<'a>: ErrorStmt<Ast = Self>;
  type ExprStmt<'a>: ExprStmt<Ast = Self>;
  type TraceStmt<'a>: TraceStmt<Ast = Self>;
  type VarDecl<'a>: VarDecl<Ast = Self>;

  type Expr<'a>: Expr<Ast = Self>;
  type AssignExpr<'a>: AssignExpr<Ast = Self>;
  type BinExpr<'a>: BinExpr<Ast = Self>;
  type BoolLit<'a>: BoolLit;
  type CallExpr<'a>: CallExpr<Ast = Self>;
  type ErrorExpr<'a>: ErrorExpr<Ast = Self>;
  type IdentExpr<'a>: IdentExpr;
  type LogicalExpr<'a>: LogicalExpr<Ast = Self>;
  type NumLit<'a>: NumLit;
  type SeqExpr<'a>: SeqExpr<Ast = Self>;
  type StrLit<'a>: StrLit;

  type Pat<'a>: Pat<Ast = Self>;
  type IdentPat<'a>: IdentPat;
  type MemberPat<'a>: MemberPat<Ast = Self>;

  type StmtRef<'a>: core::ops::Deref<Target = Self::Stmt<'a>>;
  type BreakStmtRef<'a>: core::ops::Deref<Target = Self::BreakStmt<'a>>;
  type ErrorStmtRef<'a>: core::ops::Deref<Target = Self::ErrorStmt<'a>>;
  type ExprStmtRef<'a>: core::ops::Deref<Target = Self::ExprStmt<'a>>;
  type TraceStmtRef<'a>: core::ops::Deref<Target = Self::TraceStmt<'a>>;
  type VarDeclRef<'a>: core::ops::Deref<Target = Self::VarDecl<'a>>;

  type ExprRef<'a>: core::ops::Deref<Target = Self::Expr<'a>>;
  type AssignExprRef<'a>: core::ops::Deref<Target = Self::AssignExpr<'a>>;
  type BinExprRef<'a>: core::ops::Deref<Target = Self::BinExpr<'a>>;
  type BoolLitRef<'a>: core::ops::Deref<Target = Self::BoolLit<'a>>;
  type CallExprRef<'a>: core::ops::Deref<Target = Self::CallExpr<'a>>;
  type ErrorExprRef<'a>: core::ops::Deref<Target = Self::ErrorExpr<'a>>;
  type IdentExprRef<'a>: core::ops::Deref<Target = Self::IdentExpr<'a>>;
  type LogicalExprRef<'a>: core::ops::Deref<Target = Self::LogicalExpr<'a>>;
  type NumLitRef<'a>: core::ops::Deref<Target = Self::NumLit<'a>>;
  type SeqExprRef<'a>: core::ops::Deref<Target = Self::SeqExpr<'a>>;
  type StrLitRef<'a>: core::ops::Deref<Target = Self::StrLit<'a>>;

  type PatRef<'a>: core::ops::Deref<Target = Self::Pat<'a>>;
  type IdentPatRef<'a>: core::ops::Deref<Target = Self::IdentPat<'a>>;
  type MemberPatRef<'a>: core::ops::Deref<Target = Self::MemberPat<'a>>;
}

pub trait Script {
  type Ast: Syntax;
  type StmtIter<'a>: Iterator<Item = <Self::Ast as Syntax>::StmtRef<'a>>;

  fn stmts<'a>(&'a self) -> Self::StmtIter<'a>;
}

/// Trait representing any ActionScript statement
pub trait Stmt {
  type Ast: Syntax;

  /// Downcast the statement to its concrete type.
  fn cast(&self) -> StmtCast<Self::Ast>;
}

/// Represents the result of downcasting a statement.
pub enum StmtCast<'a, S: Syntax> {
  Break(S::BreakStmtRef<'a>),
  Error(S::ErrorStmtRef<'a>),
  Expr(S::ExprStmtRef<'a>),
  Trace(S::TraceStmtRef<'a>),
  VarDecl(S::VarDeclRef<'a>),
  /// Represents a failed cast
  ///
  /// This is mainly used as a workaround for the "unused lifetime 'a" error and will be removed in
  /// the future once GAT is improved.
  Unknown(PhantomData<&'a !>)
}

pub trait BreakStmt {
  type Ast: Syntax;
}

pub trait ErrorStmt {
  type Ast: Syntax;
}

pub trait ExprStmt {
  type Ast: Syntax;

  fn expr(&self) -> <Self::Ast as Syntax>::ExprRef<'_>;
}

pub trait TraceStmt {
  type Ast: Syntax;

  fn value(&self) -> <Self::Ast as Syntax>::ExprRef<'_>;
}

pub trait VarDecl {
  type Ast: Syntax;
}

/// Trait representing any ActionScript expression
pub trait Expr {
  type Ast: Syntax;

  /// Downcast the expression to its concrete type.
  fn cast(&self) -> ExprCast<Self::Ast>;
}

/// Represents the result of downcasting an expression.
pub enum ExprCast<'a, S: Syntax> {
  Assign(S::AssignExprRef<'a>),
  Bin(S::BinExprRef<'a>),
  Bool(S::BoolLitRef<'a>),
  Call(S::CallExprRef<'a>),
  Error(S::ErrorExprRef<'a>),
  Ident(S::IdentExprRef<'a>),
  Logical(S::LogicalExprRef<'a>),
  Num(S::NumLitRef<'a>),
  Seq(S::SeqExprRef<'a>),
  Str(S::StrLitRef<'a>),
  /// Represents a failed cast
  ///
  /// This is mainly used as a workaround for the "unused lifetime 'a" error and will be removed in
  /// the future once GAT is improved.
  Unknown(PhantomData<&'a !>)
}

pub trait AssignExpr {
  type Ast: Syntax;

  fn target(&self) -> <Self::Ast as Syntax>::PatRef<'_>;
  fn value(&self) -> <Self::Ast as Syntax>::ExprRef<'_>;
}

pub trait BinExpr {
  type Ast: Syntax;

  fn op(&self) -> BinOp;
  fn left(&self) -> <Self::Ast as Syntax>::ExprRef<'_>;
  fn right(&self) -> <Self::Ast as Syntax>::ExprRef<'_>;
}

/// Represents all the binary operators.
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum BinOp {
  /// Binary operator `+`
  #[serde(rename = "+")]
  Add,
  /// Binary operator `&`
  #[serde(rename = "&")]
  BitAnd,
  /// Binary operator `|`
  #[serde(rename = "|")]
  BitOr,
  /// Binary operator `^`
  #[serde(rename = "^")]
  BitXor,
  /// Binary operator `/`
  #[serde(rename = "/")]
  Divide,
  /// Binary operator `==`
  #[serde(rename = "==")]
  Equals,
  /// Binary operator `>`
  #[serde(rename = ">")]
  Greater,
  /// Binary operator `instanceof`
  #[serde(rename = "instanceof")]
  InstanceOf,
  /// Binary operator `add`
  #[serde(rename = "add")]
  LegacyAdd,
  /// Binary operator `<<`
  #[serde(rename = "<<")]
  LeftShift,
  /// Binary operator `<`
  #[serde(rename = "<")]
  Less,
  /// Binary operator `*`
  #[serde(rename = "*")]
  Multiply,
  /// Binary operator `!=`
  #[serde(rename = "!=")]
  NotEquals,
  /// Binary operator `!==`
  #[serde(rename = "!==")]
  NotStrictEquals,
  /// Binary operator `%`
  #[serde(rename = "%")]
  Remainder,
  /// Binary operator `>>`
  #[serde(rename = ">>")]
  SignedRightShift,
  /// Binary operator `-`
  #[serde(rename = "-")]
  Subtract,
  /// Binary operator `===`
  #[serde(rename = "===")]
  StrictEquals,
  /// Binary operator `>>>`
  #[serde(rename = ">>>")]
  UnsignedRightShift,
}

pub trait BoolLit {
  fn value(&self) -> bool;
}

pub trait CallExpr {
  type Ast: Syntax;
  type ExprIter<'a>: Iterator<Item = <Self::Ast as Syntax>::ExprRef<'a>>;

  fn callee(&self) -> <Self::Ast as Syntax>::ExprRef<'_>;
  fn args<'a>(&'a self) -> Self::ExprIter<'a>;
}

pub trait ErrorExpr {
  type Ast: Syntax;
}

pub trait IdentExpr {
  fn name(&self) -> Cow<str>;
}

pub trait LogicalExpr {
  type Ast: Syntax;

  fn op(&self) -> LogicalOp;
  fn left(&self) -> <Self::Ast as Syntax>::ExprRef<'_>;
  fn right(&self) -> <Self::Ast as Syntax>::ExprRef<'_>;
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum LogicalOp {
  /// Binary operator `&&`
  #[serde(rename = "&&")]
  And,
  /// Logical operator `||`
  #[serde(rename = "||")]
  Or,
}

pub trait NumLit {
  fn value(&self) -> f64;
}

pub trait StrLit {
  fn value(&self) -> Cow<str>;
}

pub trait SeqExpr {
  type Ast: Syntax;
  type ExprIter<'a>: Iterator<Item = <Self::Ast as Syntax>::ExprRef<'a>>;

  fn exprs<'a>(&'a self) -> Self::ExprIter<'a>;
}

/// Trait representing any ActionScript pattern
pub trait Pat {
  type Ast: Syntax;

  /// Downcast the pattern to its concrete type.
  fn cast(&self) -> PatCast<Self::Ast>;
}

/// Represents the result of downcasting a pattern.
pub enum PatCast<'a, S: Syntax> {
  Ident(S::IdentPatRef<'a>),
  Member(S::MemberPatRef<'a>),
  /// Represents a failed cast
  ///
  /// This is mainly used as a workaround for the "unused lifetime 'a" error and will be removed in
  /// the future once GAT is improved.
  Unknown(PhantomData<&'a !>)
}

pub trait IdentPat {
  fn name(&self) -> Cow<str>;
}

pub trait MemberPat {
  type Ast: Syntax;

  fn base(&self) -> <Self::Ast as Syntax>::ExprRef<'_>;
  fn key(&self) -> <Self::Ast as Syntax>::ExprRef<'_>;
}
