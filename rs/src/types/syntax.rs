/// A poor man's never type represented as an empty type so it works on stable Rust.
pub enum Empty {}

/// Describes all the elements of a syntax.
pub trait Syntax: Sized {
  // The RFC 1598 (Generic Associated Types) may help here
  // type List<T>: ExactSizeIterator<T>;
  // type ExprList: ExactSizeIterator<Item = &Self::Expr>;
  // type ExprRef: Borrow<Self::Expr>;

  type Expr: Expr<Self>;
  type SeqExpr: SeqExpr<Self>;
  type BinExpr: BinExpr<Self>;
  type StrLit: StrLit;
}

/// Trait representing any ActionScript expression
pub trait Expr<S: Syntax> {
  /// Downcast the expression to its concrete type.
  fn cast<'a>(&'a self) -> ExprCast<'a, S>;
}

/// Represents the result of downcasting an expression.
pub enum ExprCast<'a, S: Syntax> {
  StrLit(&'a S::StrLit),
  Error,
}

/// Sequence expression
///
/// Corresponds to two or more expressions separated by commas.
pub trait SeqExpr<S: Syntax> {
  type Iter<'a>: ExactSizeIterator<Item = &'a S::Expr>;

  fn exprs<'a>(&'a self) -> Self::Iter<'a>;
}

pub trait BinExpr<S: Syntax> {
  fn left(&self) -> &S::Expr;
  fn right(&self) -> &S::Expr;
}

/// Represents all the binary operators.
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum BinOp {
  /// Binary operator `+`
  Add,
  /// Binary operator `&`
  BitAnd,
  /// Binary operator `|`
  BitOr,
  /// Binary operator `^`
  BitXor,
  /// Binary operator `/`
  Divide,
  /// Binary operator `==`
  Equals,
  /// Binary operator `>`
  Greater,
  /// Binary operator `instanceof`
  InstanceOf,
  /// Binary operator `add`
  LegacyAdd,
  /// Binary operator `<<`
  LeftShift,
  /// Binary operator `<`
  Less,
  /// Binary operator `*`
  Multiply,
  /// Binary operator `!=`
  NotEquals,
  /// Binary operator `!==`
  NotStrictEquals,
  /// Binary operator `%`
  Remainder,
  /// Binary operator `>>`
  SignedRightShift,
  /// Binary operator `-`
  Subtract,
  /// Binary operator `===`
  StrictEquals,
  /// Binary operator `>>>`
  UnsignedRightShift,
}

pub trait StrLit {
  fn value(&self) -> &str;
}
