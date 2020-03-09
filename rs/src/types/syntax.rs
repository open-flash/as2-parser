use crate::types::ast::traits;
use std::convert::TryFrom;

#[derive(Debug, Eq, PartialEq, Copy, Clone, Ord, PartialOrd, Hash)]
pub enum SyntaxKind {
  /// Invalid token
  Error,

  /// Identifier
  ///
  /// In ActionScript, keywords are never valid identifiers so there is no
  /// ambiguity here.
  ///
  /// Examples:
  /// - `foo`
  /// - `$foo`
  /// - `_foo_$123`
  Identifier,

  /// `(`
  LParen,

  /// `)`
  RParen,

  /// String literal
  ///
  /// Examples:
  /// - `"foo"`
  /// - `"foo\nbar\0baz"`
  /// - `'"'`
  /// - `'\''`
  /// - `"\""`
  Str,

  /// Whitespace without any newline
  UnilineWhitespace,

  /// Whitespace containing at least one newline
  MultilineWhitespace,

  /// Trailing comment: from `//` to the end of the line
  TrailingComment,

  /// Comment between `/*` and `*/`, containing a newline
  MultilineComment,

  /// Comment between `/*` and `*/`, without a newline
  UnilineComment,

  /// Any statement
  Statement,

  /// Any expression
  Expression,

  /// Root node
  Script,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Ord, PartialOrd, Hash)]
pub struct SyntaxToken<'text> {
  pub(crate) kind: SyntaxKind,
  pub(crate) text: &'text str,
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct SyntaxNode<'text> {
  pub(crate) kind: SyntaxKind,
  pub(crate) children: Vec<SyntaxSymbol<'text>>
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub enum SyntaxSymbol<'text> {
  Token(SyntaxToken<'text>),
  Node(SyntaxNode<'text>),
}

/// Represents an identifier pattern backed by a lossless syntax node.
///
/// # Invariant
///
/// `IdentPat` maintains `identPat.syntax.kind === SyntaxKind::Identifier`.
#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd, Hash)]
pub struct IdentPat<'text> {
  syntax: SyntaxToken<'text>,
}

impl<'text> TryFrom<SyntaxToken<'text>> for IdentPat<'text> {
  type Error = ();

  fn try_from(syntax: SyntaxToken<'text>) -> Result<Self, Self::Error> {
    match syntax.kind {
      SyntaxKind::Identifier => Ok(IdentPat { syntax }),
      _ => Err(()),
    }
  }
}

impl<'text> traits::IdentPat for IdentPat<'text> {
  fn name(&self) -> &str {
    &self.syntax.text
  }
}
