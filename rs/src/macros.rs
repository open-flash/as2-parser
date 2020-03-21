macro_rules! maybe_gat_accessor {
  ($name: ident, $base_fn:ident, ref $gat_type: ty, ref $non_gat_type: ty) => {
    #[cfg(feature = "gat")]
    fn $name(&self) -> &$gat_type {
      self.$base_fn()
    }

    #[cfg(not(feature = "gat"))]
    fn $name<'r>(&'r self) -> Box<dyn core::ops::Deref<Target = $non_gat_type> + 'r> {
      let b: Box<&'r $non_gat_type> = Box::new(self.$base_fn());
      b
    }
  };
  ($name: ident, $base_fn:ident, $gat_type: ty, $non_gat_type: ty) => {
    #[cfg(feature = "gat")]
    fn $name(&self) -> $gat_type {
      self.$base_fn()
    }

    #[cfg(not(feature = "gat"))]
    fn $name<'r>(&'r self) -> Box<dyn core::ops::Deref<Target = $non_gat_type> + 'r> {
      let b: Box<$non_gat_type> = Box::new(self.$base_fn());
      b
    }
  };
}
