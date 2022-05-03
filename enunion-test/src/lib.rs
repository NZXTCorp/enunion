use napi_derive::napi;

#[enunion::enunion(discriminant_repr = "i64")]
pub enum Foo {
  Bar,
  Baz {
    a: i32,
    b: u32,
    c: String,
    my_multi_word_field: i32,
  },
}

#[enunion::enunion(discriminant_repr = "str")]
pub enum FooString {
  Bar,
  Baz { a: i32, b: u32, c: String },
}

#[enunion::enunion(discriminant_repr = "enum")]
pub enum FooEnum {
  Bar,
  Baz { a: i32, b: u32, c: String },
}

#[enunion::enunion(discriminant_repr = "i64", discriminant_field_name = "new_type")]
pub enum FooNew {
  Bar,
  Baz { a: i32, b: u32, c: String },
}

#[enunion::enunion(discriminant_repr = "str", discriminant_field_name = "new_type")]
pub enum FooStringNew {
  Bar,
  Baz { a: i32, b: u32, c: String },
}

#[enunion::enunion(discriminant_repr = "enum", discriminant_field_name = "new_type")]
pub enum FooEnumNew {
  Bar,
  Baz { a: i32, b: u32, c: String },
}

#[enunion::enunion]
pub enum DefaultFoo {
  Bar,
  Zam,
  Zoodle,
  Baz { a: i32, b: u32, c: String },
}

#[napi]
pub fn take_foo_enum(f: FooEnum) -> FooEnum {
  assert!(matches!(f, FooEnum::Bar));
  FooEnum::Bar
}

#[napi]
pub fn take_foo_default(f: DefaultFoo) -> DefaultFoo {
  assert!(matches!(f, DefaultFoo::Bar));
  DefaultFoo::Zoodle
}

#[napi]
pub fn take_foo_string(_f: FooString) -> FooString {
  FooString::Bar
}

#[napi]
pub fn take_foo(f: Foo) -> Foo {
  assert!(matches!(
    f,
    Foo::Baz {
      a: 3,
      b: 2,
      c: _,
      my_multi_word_field: 2
    }
  ));
  match f {
      Foo::Baz {c, ..} => assert_eq!(c, "Hello from TypeScript"),
      _ => unreachable!(),
  }
  Foo::Baz {
      a: 1,
      b: 2,
      c: String::from("Hello from Rust"),
      my_multi_word_field: 8,
  }
}
