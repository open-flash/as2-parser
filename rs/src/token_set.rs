use crate::syntax::SyntaxKind;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TokenSet(u64);

impl TokenSet {
  pub const EMPTY: TokenSet = TokenSet(0);

  pub const fn insert_end(self) -> Self {
    Self(self.0 | TokenSet::end_to_bit())
  }

  pub const fn insert_token(self, kind: SyntaxKind) -> Self {
    Self(self.0 | TokenSet::token_to_bit(kind))
  }

  // pub const fn insert(self, kind: Option<SyntaxKind>) -> Self {
  //   match kind {
  //     Some(kind) => self.insert_token(kind),
  //     None => self.insert_end(),
  //   }
  // }

  pub const fn contains(self, kind: Option<SyntaxKind>) -> bool {
    let bit = match kind {
      Some(kind) => TokenSet::token_to_bit(kind),
      None => TokenSet::end_to_bit(),
    };
    self.0 & bit != 0
  }

  const fn end_to_bit() -> u64 {
    1
  }

  const fn token_to_bit(kind: SyntaxKind) -> u64 {
    let bit_id = kind.into_u16() + 1;
    // TODO: Use const_assert once available
    // assert!(bit_id < 64);
    // TODO: Use `u64::from` once available in const context
    1 << (bit_id as u64)
  }
}

/// Defines a token set.
///
/// The first argument is the name, the remaining arguments are the tokens in the set.
///
/// It is defined as `const` if constant tokens are supported (requires the `const_if_match`
/// feature) otherwise as `static`.
macro_rules! token_set {
  (None) => {
    crate::token_set::TokenSet::EMPTY.insert_end();
  };
  (None, $($token:expr),+) => {
    crate::token_set::TokenSet::EMPTY.insert_end() $(.insert_token($token))*;
  };
  ($($token:expr),*) => {
    crate::token_set::TokenSet::EMPTY $(.insert_token($token))*;
  };
}

#[test]
fn test_empty() {
  let set = token_set!();
  assert!(!set.contains(None));
  assert!(!set.contains(Some(SyntaxKind::TokenIdent)));
  assert!(!set.contains(Some(SyntaxKind::TokenCloseBrace)));
  assert!(!set.contains(Some(SyntaxKind::TokenSemicolon)));
}

#[test]
fn test_only_end() {
  let set = token_set!(None);
  assert!(set.contains(None));
  assert!(!set.contains(Some(SyntaxKind::TokenIdent)));
  assert!(!set.contains(Some(SyntaxKind::TokenCloseBrace)));
  assert!(!set.contains(Some(SyntaxKind::TokenSemicolon)));
}

#[test]
fn test_only_ident() {
  let set = token_set!(SyntaxKind::TokenIdent);
  assert!(!set.contains(None));
  assert!(set.contains(Some(SyntaxKind::TokenIdent)));
  assert!(!set.contains(Some(SyntaxKind::TokenCloseBrace)));
  assert!(!set.contains(Some(SyntaxKind::TokenSemicolon)));
}

#[test]
fn test_end_and_ident() {
  let set = token_set!(None, SyntaxKind::TokenIdent);
  assert!(set.contains(None));
  assert!(set.contains(Some(SyntaxKind::TokenIdent)));
  assert!(!set.contains(Some(SyntaxKind::TokenCloseBrace)));
  assert!(!set.contains(Some(SyntaxKind::TokenSemicolon)));
}

#[test]
fn test_ident_and_semicolon() {
  let set = token_set!(SyntaxKind::TokenIdent, SyntaxKind::TokenSemicolon);
  assert!(!set.contains(None));
  assert!(set.contains(Some(SyntaxKind::TokenIdent)));
  assert!(!set.contains(Some(SyntaxKind::TokenCloseBrace)));
  assert!(set.contains(Some(SyntaxKind::TokenSemicolon)));
}
