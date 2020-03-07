pub mod parser;
pub mod types;

#[cfg(test)]
mod parser_tests {
  use std::path::Path;
  use ::test_generator::test_resources;
  use crate::types::owned;
  use super::*;
  use crate::parser::parse_script;

  #[test_resources("../tests/as2/[!.]*/*/")]
  fn test_parse_cfg(path: &str) {
    let path: &Path = Path::new(path);
    let _name = path
      .components()
      .last()
      .unwrap()
      .as_os_str()
      .to_str()
      .expect("Failed to retrieve sample name");

    //    if name == "hello-world" || name == "homestuck-beta2" {
    //      return;
    //    }

    let as2_path = path.join("main.as2");
    let as2_text: String = ::std::fs::read_to_string(as2_path).expect("Failed to read input");

    let _actual_tree = parse_script(&as2_text);
    let _expected_tree: owned::OwnedStrLit = owned::OwnedStrLit { _loc: (), _value: String::new() };

    // assert_eq!(actual_tree, expected_tree);
  }
}