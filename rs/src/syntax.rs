use std::convert::TryFrom;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
  /// Invalid token
  TokenError,

  // Trivia
  /// Whitespace without any newline
  TokenUnilineWhitespace,

  /// Whitespace containing at least one newline
  TokenMultilineWhitespace,

  /// Trailing comment: from `//` to the end of the line
  TokenTrailingComment,

  /// Comment between `/*` and `*/`, containing a newline
  TokenMultilineComment,

  /// Comment between `/*` and `*/`, without a newline
  TokenUnilineComment,

  // Keywords
  /// The keyword `delete`
  TokenDelete,

  /// The keyword `false`
  TokenFalse,

  /// The keyword `for`
  TokenFor,

  /// The keyword `throw`
  TokenThrow,

  /// The keyword `this`
  TokenThis,

  /// The keyword `true`
  TokenTrue,

  /// The keyword `try`
  TokenTry,

  /// The keyword `var`
  TokenVar,

  /// The keyword `void`
  TokenVoid,

  // Atoms
  /// Identifier name
  ///
  /// In ActionScript, keywords are never valid identifiers so there is no
  /// ambiguity here. (e.g. `throw` is _not_ an identifier)
  ///
  /// Examples:
  /// - `foo`
  /// - `$foo`
  /// - `_foo_$123`
  TokenIdent,

  /// String literal
  ///
  /// Examples:
  /// - `"foo"`
  /// - `"foo\nbar\0baz"`
  /// - `'"'`
  /// - `'\''`
  /// - `"\""`
  TokenStrLit,

  /// Number literal
  TokenNumLit,

  // Punctuators
  /// `;`
  TokenSemicolon,

  /// `:`
  TokenColon,

  /// `,`
  TokenComa,

  /// `{`
  TokenOpenBrace,

  /// `}`
  TokenCloseBrace,

  /// `[`
  TokenOpenBracket,

  /// `]`
  TokenCloseBracket,

  /// `(`
  TokenOpenParen,

  /// `)`
  TokenCloseParen,

  /// `=`
  TokenEq,

  /// `==`
  TokenEqEq,

  /// `===`
  TokenEqEqEq,

  /// `!=`
  TokenNotEq,

  /// `!==`
  TokenNotEqEq,

  /// `!`
  TokenExcl,

  /// `?`
  TokenQuestion,

  /// `<`
  TokenLt,

  /// `<<`
  TokenLtLt,

  /// `<=`
  TokenLtEq,

  /// `>`
  TokenGt,

  /// `>>`
  TokenGtGt,

  /// `>>>`
  TokenGtGtGt,

  /// `>=`
  TokenGtEq,

  /// `+`
  TokenPlus,

  /// `++`
  TokenPlusPlus,

  /// `-`
  TokenMinus,

  /// `--`
  TokenMinusMinus,

  /// `|`
  TokenPipe,

  /// `||`
  TokenPipePipe,

  /// `&`
  ///
  /// "amp" is an abbreviation for "ampersand".
  TokenAmp,

  /// `&&`
  ///
  /// "amp" is an abbreviation for "ampersand".
  TokenAmpAmp,

  /// `/`
  TokenSlash,

  /// `%`
  TokenPercent,

  /// `*`
  TokenStar,

  /// `^`
  TokenCaret,

  /// `.`
  TokenDot,

  // Simple nodes
  /// Boolean literal expression
  NodeBoolLit,

  /// Number literal expression
  NodeNumLit,

  /// Object literal expression
  NodeObjectLit,

  /// A single property from an object literal expression
  NodeObjectLitProp,

  /// String literal expression
  NodeStrLit,

  /// Identifier reference expression, or identifier pattern, or label identifier
  NodeIdent,

  /// Sequence expression
  NodeSeqExpr,

  /// Assignment expression
  NodeAssignExpr,

  /// Conditional expression
  NodeCondExpr,

  /// Binary expression
  NodeInfixExpr,

  /// Prefix expression
  NodePrefixExpr,

  /// Postfix expression
  NodePostfixExpr,

  /// Parenthesized expression
  NodeParenExpr,

  /// Identifier member expression
  ///
  /// ```as2
  /// foo.bar
  /// ```
  NodeIdentMemberExpr,

  /// Computed member expression
  ///
  /// ```as2
  /// foo["bar"]
  /// ```
  NodeComputedMemberExpr,

  /// Function call
  NodeCallExpr,

  /// Function call arguments, or constructor arguments
  NodeArgs,

  // Composite nodes
  /// Break statement
  NodeBreakStmt,

  /// Expression statement
  NodeExprStmt,

  /// Variable declaration
  NodeVarDecl,

  /// Root node
  NodeScript,
}

impl From<SyntaxKind> for rowan::SyntaxKind {
  fn from(kind: SyntaxKind) -> Self {
    Self(u16::from(kind))
  }
}

impl From<SyntaxKind> for u16 {
  fn from(value: SyntaxKind) -> Self {
    value.into_u16()
  }
}

impl TryFrom<u16> for SyntaxKind {
  type Error = ();

  fn try_from(value: u16) -> Result<Self, Self::Error> {
    if usize::from(value) < core::mem::variant_count::<SyntaxKind>() {
      Ok(unsafe { std::mem::transmute::<u16, SyntaxKind>(value) })
    } else {
      Err(())
    }
  }
}

impl SyntaxKind {
  // TODO: Move this function to the `From` trait once possible (requires const fn in traits)
  //       (and remove this function)
  pub const fn into_u16(self) -> u16 {
    self as u16
  }

  pub fn is_trivia(self) -> bool {
    use SyntaxKind::*;
    match self {
      TokenMultilineWhitespace
      | TokenUnilineWhitespace
      | TokenTrailingComment
      | TokenMultilineComment
      | TokenUnilineComment => true,
      _ => false,
    }
  }

  pub fn is_multiline_trivia(self) -> bool {
    use SyntaxKind::*;
    match self {
      TokenMultilineWhitespace | TokenTrailingComment | TokenMultilineComment => true,
      _ => false,
    }
  }

  pub fn is_token(self) -> bool {
    use SyntaxKind::*;
    match self {
      TokenError
      | TokenUnilineWhitespace
      | TokenMultilineWhitespace
      | TokenTrailingComment
      | TokenMultilineComment
      | TokenUnilineComment
      | TokenThrow
      | TokenThis
      | TokenTrue
      | TokenTry
      | TokenIdent
      | TokenStrLit
      | TokenSemicolon
      | TokenOpenParen
      | TokenCloseParen => true,
      _ => false,
    }
  }

  pub fn is_stmt(self) -> bool {
    use SyntaxKind::*;
    match self {
      NodeExprStmt | NodeVarDecl => true,
      _ => false,
    }
  }

  pub fn is_expr(self) -> bool {
    use SyntaxKind::*;
    match self {
      NodeAssignExpr | NodeComputedMemberExpr | NodeCondExpr | NodeIdent | NodeIdentMemberExpr
      | NodeInfixExpr | NodeBoolLit | NodeCallExpr | NodeNumLit | NodeParenExpr
      | NodePostfixExpr | NodePrefixExpr | NodeSeqExpr | NodeStrLit => true,
      _ => false,
    }
  }

  pub fn is_assign_op(self) -> bool {
    use SyntaxKind::*;
    match self {
      TokenEq => true,
      _ => false,
    }
  }

  pub fn is_logical_op(self) -> bool {
    use SyntaxKind::*;
    match self {
      TokenPipePipe | TokenAmpAmp => true,
      _ => false,
    }
  }

  pub fn is_bin_op(self) -> bool {
    use SyntaxKind::*;
    match self {
      TokenAmp | TokenCaret | TokenEqEq | TokenEqEqEq | TokenLt | TokenLtLt | TokenMinus | TokenPipe | TokenPlus
      | TokenSlash | TokenStar => true,
      _ => false,
    }
  }

  pub fn is_infix_op(self) -> bool {
    self.is_assign_op() || self.is_bin_op() || self.is_logical_op()
  }
}

/// Enum representing the ActionScript 2 language supported by OpenFlash
/// It is used as a bridge between Rowan's untyped node and AS2's syntax kinds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum As2Lang {}

impl rowan::Language for As2Lang {
  type Kind = SyntaxKind;

  fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
    SyntaxKind::try_from(raw.0).unwrap()
  }

  fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
    rowan::SyntaxKind(u16::from(kind))
  }
}

/// Represents an AS2 syntax token
///
/// A token is a terminal symbol of the syntax tree. It has a kind, range and text.
/// The text is owned by the token and stored as a `rowan::SmolStr`.
pub type SyntaxToken = rowan::SyntaxToken<As2Lang>;

/// Represents an AS2 syntax node
///
/// A node is a non-terminal symbol of the syntax tree. It has a kind, range and
/// child symbols. It does not own text directly: the text is retrieved by
/// iterating on the descendant children.
pub type SyntaxNode = rowan::SyntaxNode<As2Lang>;
/// Represents an AS2 syntax symbol: token (terminal) or node (non-terminal).
pub type SyntaxSymbol = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

#[cfg(test)]
mod tests {
  use super::SyntaxKind;

  #[test]
  fn test_syntax_kind_variant_count() {
    assert_eq!(core::mem::variant_count::<SyntaxKind>(), 75);
  }
}
