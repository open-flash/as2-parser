use crate::types::owned;

// use crate::types::cast::ExprCast;
//
// struct OwnedAstBuilder {}
//
// impl OwnedAstBuilder {
//   fn str_lit(value: String) -> owned::StrLit {
//     owned::StrLit { _loc: (), _value: value }
//   }
//
//   fn num_lit(value: f64) -> owned::NumLit {
//     owned::NumLit { _loc: (), _value: value }
//   }
// }

// struct ParseContext {
//
// }
//
// struct Parser<'i> {
//   input: &'i str,
// }
//
// #[derive(Debug, Eq, PartialEq, Copy, Clone, Ord, PartialOrd, Hash)]
// enum Token<'a> {
//   Ident(&'a str),
//   Error(&'a str),
//   End,
// }
//
// impl Parser {
//   fn parse_script() {
//
//   }
//
//   fn parse_statements(directives: bool, top_level: bool, end: Token) {
//     let mut stmts: Vec<owned::Stmt> = Vec::new();
//   }
//
//   fn peek(&self) -> Token {
//     let chars = self.input.char_indices();
//     match chars.next() {
//       None => Token::End,
//       Some((i, c @ 'a'..='z')) => {
//
//       },
//       Some((i, c)) => Token::Error(self.input[i..c.len_utf8()])
//     }
//   }
// }

pub fn parse_script(_input: &str) -> owned::StrLit {
  owned::StrLit {
    loc: (),
    value: String::new(),
  }
}

// pub fn eval_expr(input: &owned::Expr) -> f64 {
//   match input.downcast() {
//     ExprCast::BinExpr(e) => eval_bin_expr(e),
//     ExprCast::NumLit(e) => eval_num_lit(e),
//     ExprCast::StrLit(e) => unimplemented!("StrLit"),
//   }
// }
//
// pub fn eval_num_lit(input: &owned::NumLit) -> f64 {
//   input.value()
// }
//
// pub fn eval_bin_expr(input: &owned::BinExpr) -> f64 {
//   let left = eval_expr(input.left());
//   let right = eval_expr(input.right());
//   left + right
// }
