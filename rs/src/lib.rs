#![allow(incomplete_features)]
#![feature(associated_type_bounds, generic_associated_types, never_type, variant_count)]

#[macro_use]
mod token_set;

pub mod ast;
pub mod escape;
pub mod lexer;
pub mod parser;
pub mod syntax;
