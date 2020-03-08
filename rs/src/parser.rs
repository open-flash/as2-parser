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

pub fn parse_script(_input: &str) -> owned::StrLit {
  owned::StrLit {
    _loc: (),
    _value: String::new(),
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
