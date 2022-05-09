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

#[enunion::enunion(discriminant_repr = "enum_str")]
pub enum FooEnumStr {
  Bar,
  Baz { a: i32, b: u32, c: String },
}

#[enunion::enunion(discriminant_repr = "none")]
pub enum FooNoDiscriminant {
  Bar { a: i32, b: i32 },
  Baz { d: i32, b: u32 },
  TransparentOneField(StringTest),
  TransparentManyStructs(FooNew, FooString),
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

#[enunion::enunion(discriminant_repr = "enum_str", discriminant_field_name = "new_type")]
pub enum FooEnumStrNew {
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

#[enunion::string_enum]
pub enum StringTest {
  Bar = "bar",
  Baz = "baz",
  Zoom,
}

#[napi]
pub fn take_string_test(t: StringTest) -> StringTest {
  assert!(matches!(t, StringTest::Bar));
  StringTest::Baz
}

#[napi]
pub fn take_foo_no_discriminant(f: FooNoDiscriminant) -> FooNoDiscriminant {
  assert!(matches!(f, FooNoDiscriminant::Bar { a: 1, b: 2 }));
  FooNoDiscriminant::Baz { d: 1, b: 2 }
}

#[napi]
pub fn take_foo_no_discriminant_transparent_one_field(f: FooNoDiscriminant) -> FooNoDiscriminant {
  assert!(matches!(
    f,
    FooNoDiscriminant::TransparentOneField(StringTest::Bar)
  ));
  FooNoDiscriminant::TransparentOneField(StringTest::Baz)
}

#[napi]
pub fn take_foo_no_discriminant_transparent_many_structs(
  f: FooNoDiscriminant,
) -> FooNoDiscriminant {
  assert!(matches!(
    f,
    FooNoDiscriminant::TransparentManyStructs(FooNew::Bar, FooString::Bar)
  ));
  FooNoDiscriminant::TransparentManyStructs(FooNew::Bar, FooString::Bar)
}

#[napi]
pub fn take_foo_enum(f: FooEnum) -> FooEnum {
  assert!(matches!(f, FooEnum::Bar));
  FooEnum::Bar
}

#[napi]
pub fn take_foo_enum_str(f: FooEnumStr) -> FooEnumStr {
  assert!(matches!(f, FooEnumStr::Bar));
  FooEnumStr::Bar
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
    Foo::Baz { c, .. } => assert_eq!(c, "Hello from TypeScript"),
    _ => unreachable!(),
  }
  Foo::Baz {
    a: 1,
    b: 2,
    c: String::from("Hello from Rust"),
    my_multi_word_field: 8,
  }
}
