//! This module provides adapters implementing serde's `Serialize` trait for AST types.

use super::traits::*;
use serde::{Serialize, Serializer};

pub struct SerializeScript<'a, Ast: Syntax>(pub &'a Ast::Script);

impl<Ast: Syntax> Serialize for SerializeScript<'_, Ast> {
  fn serialize<Ser: Serializer>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error> {
    use serde::ser::SerializeStruct;
    let mut struct_serializer = serializer.serialize_struct("Script", 1)?;
    struct_serializer.serialize_field("body", &SerializeScriptStmts::<Ast>(self.0))?;
    struct_serializer.end()
  }
}

struct SerializeScriptStmts<'a, Ast: Syntax>(pub &'a Ast::Script);

impl<'a, Ast: Syntax> Serialize for SerializeScriptStmts<'a, Ast> {
  fn serialize<Ser: Serializer>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error> {
    use serde::ser::SerializeSeq;
    let stmts = self.0.stmts();
    let len = match stmts.size_hint() {
      (min_len, Some(max_len)) if min_len == max_len => Some(min_len),
      _ => None,
    };
    let mut seq_serializer = serializer.serialize_seq(len)?;
    for stmt in stmts {
      let stmt: &Ast::Stmt = match stmt {
        MaybeOwned::Owned(ref s) => s,
        MaybeOwned::Borrowed(s) => &*s,
      };
      seq_serializer.serialize_element(&SerializeStmt::<Ast>(stmt))?;
    }
    seq_serializer.end()
  }
}

pub struct SerializeStmt<'a, Ast: Syntax>(pub &'a Ast::Stmt);

impl<Ast: Syntax> Serialize for SerializeStmt<'_, Ast> {
  fn serialize<Ser: Serializer>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error> {
    match self.0.cast() {
      StmtCast::Break(s) => SerializeBreakStmt::<Ast>(&*s).serialize(serializer),
      StmtCast::Error(s) => SerializeErrorStmt::<Ast>(&*s).serialize(serializer),
      StmtCast::Expr(s) => SerializeExprStmt::<Ast>(&*s).serialize(serializer),
      StmtCast::VarDecl(s) => SerializeVarDecl::<Ast>(&*s).serialize(serializer),
      _ => unimplemented!(),
    }
  }
}

pub struct SerializeBreakStmt<'a, Ast: Syntax>(pub &'a Ast::BreakStmt);

impl<Ast: Syntax> Serialize for SerializeBreakStmt<'_, Ast> {
  fn serialize<Ser: Serializer>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error> {
    use serde::ser::SerializeStruct;
    let mut struct_serializer = serializer.serialize_struct("BreakStmt", 1)?;
    struct_serializer.serialize_field("type", "BreakStmt")?;
    struct_serializer.end()
  }
}

pub struct SerializeErrorStmt<'a, Ast: Syntax>(pub &'a Ast::ErrorStmt);

impl<Ast: Syntax> Serialize for SerializeErrorStmt<'_, Ast> {
  fn serialize<Ser: Serializer>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error> {
    use serde::ser::SerializeStruct;
    let mut struct_serializer = serializer.serialize_struct("ErrorStmt", 1)?;
    struct_serializer.serialize_field("type", "ErrorStmt")?;
    struct_serializer.end()
  }
}

pub struct SerializeExprStmt<'a, Ast: Syntax>(pub &'a Ast::ExprStmt);

impl<Ast: Syntax> Serialize for SerializeExprStmt<'_, Ast> {
  fn serialize<Ser: Serializer>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error> {
    use serde::ser::SerializeStruct;
    let mut struct_serializer = serializer.serialize_struct("ExprStmt", 2)?;
    struct_serializer.serialize_field("type", "ExprStmt")?;
    struct_serializer.serialize_field("expr", &SerializeExpr::<Ast>(&*self.0.expr()))?;
    struct_serializer.end()
  }
}

pub struct SerializeVarDecl<'a, Ast: Syntax>(pub &'a Ast::VarDecl);

impl<Ast: Syntax> Serialize for SerializeVarDecl<'_, Ast> {
  fn serialize<Ser: Serializer>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error> {
    use serde::ser::SerializeStruct;
    let mut struct_serializer = serializer.serialize_struct("VarDecl", 1)?;
    struct_serializer.serialize_field("type", "VarDecl")?;
    struct_serializer.end()
  }
}

pub struct SerializeExpr<'a, Ast: Syntax>(pub &'a Ast::Expr);

impl<Ast: Syntax> Serialize for SerializeExpr<'_, Ast> {
  fn serialize<Ser: Serializer>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error> {
    match self.0.cast() {
      ExprCast::Assign(e) => SerializeAssignExpr::<Ast>(&*e).serialize(serializer),
      ExprCast::Bin(e) => SerializeBinExpr::<Ast>(&*e).serialize(serializer),
      ExprCast::Error(e) => SerializeErrorExpr::<Ast>(&*e).serialize(serializer),
      _ => unimplemented!(),
    }
  }
}

pub struct SerializeAssignExpr<'a, Ast: Syntax>(pub &'a Ast::AssignExpr);

impl<Ast: Syntax> Serialize for SerializeAssignExpr<'_, Ast> {
  fn serialize<Ser: Serializer>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error> {
    use serde::ser::SerializeStruct;
    let mut struct_serializer = serializer.serialize_struct("AssignExpr", 1)?;
    struct_serializer.serialize_field("type", "AssignExpr")?;
    struct_serializer.end()
  }
}

pub struct SerializeBinExpr<'a, Ast: Syntax>(pub &'a Ast::BinExpr);

impl<Ast: Syntax> Serialize for SerializeBinExpr<'_, Ast> {
  fn serialize<Ser: Serializer>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error> {
    use serde::ser::SerializeStruct;
    let mut struct_serializer = serializer.serialize_struct("BinExpr", 1)?;
    struct_serializer.serialize_field("type", "BinExpr")?;
    struct_serializer.end()
  }
}

pub struct SerializeErrorExpr<'a, Ast: Syntax>(pub &'a Ast::ErrorExpr);

impl<Ast: Syntax> Serialize for SerializeErrorExpr<'_, Ast> {
  fn serialize<Ser: Serializer>(&self, serializer: Ser) -> Result<Ser::Ok, Ser::Error> {
    use serde::ser::SerializeStruct;
    let mut struct_serializer = serializer.serialize_struct("ErrorExpr", 1)?;
    struct_serializer.serialize_field("type", "ErrorExpr")?;
    struct_serializer.end()
  }
}
