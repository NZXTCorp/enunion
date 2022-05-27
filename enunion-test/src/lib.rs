use napi_derive::napi;

#[enunion::enunion(discriminant_repr = "i64")]
pub enum Foo {
  UnionVariant(TestObject, TestObjectTwo),
  Bar,
  #[enunion(discriminant_value = 4)]
  Baz {
    a: i32,
    b: u32,
    c: String,
    my_multi_word_field: i32,
  },
  UnionVariant2(TestObject, TestObjectTwo),
}

#[enunion::enunion(discriminant_repr = "bool")]
pub enum FooBool {
  #[enunion(discriminant_value = true)]
  Bar,
  #[enunion(discriminant_value = false)]
  Baz {
    a: i32,
    b: u32,
    c: String,
    my_multi_word_field: i32,
  },
}

#[enunion::enunion(discriminant_repr = "str")]
pub enum FooString {
  UnionVariant(TestObject, TestObjectTwo),
  Bar,
  #[enunion(discriminant_value = "baaz")]
  Baz {
    a: i32,
    b: u32,
    c: String,
  },
  UnionVariant2(TestObject, TestObjectTwo),
}

#[enunion::enunion(discriminant_repr = "enum")]
pub enum FooEnum {
  UnionVariant(TestObject, TestObjectTwo),
  Bar,
  #[enunion(discriminant_value = "Baaz")]
  Baz {
    a: i32,
    b: u32,
    c: String,
  },
  UnionVariant2(TestObject, TestObjectTwo),
}

#[enunion::enunion(discriminant_repr = "enum_str")]
pub enum FooEnumStr {
  UnionVariant(TestObject, TestObjectTwo),
  Bar,
  #[enunion(discriminant_value = "Baaz")]
  Baz {
    a: i32,
    b: u32,
    c: String,
  },
  UnionVariant2(TestObject, TestObjectTwo),
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
  UnionVariant(TestObject, TestObjectTwo),
  Bar,
  Baz { a: i32, b: u32, c: String },
  UnionVariant2(TestObject, TestObjectTwo),
}

#[enunion::enunion(discriminant_repr = "str", discriminant_field_name = "new_type")]
pub enum FooStringNew {
  UnionVariant(TestObject, TestObjectTwo),
  Bar,
  Baz { a: i32, b: u32, c: String },
  UnionVariant2(TestObject, TestObjectTwo),
}

#[enunion::enunion(discriminant_repr = "enum", discriminant_field_name = "new_type")]
pub enum FooEnumNew {
  UnionVariant(TestObject, TestObjectTwo),
  Bar,
  Baz { a: i32, b: u32, c: String },
  UnionVariant2(TestObject, TestObjectTwo),
}

#[enunion::enunion(discriminant_repr = "enum_str", discriminant_field_name = "new_type")]
pub enum FooEnumStrNew {
  UnionVariant(TestObject, TestObjectTwo),
  Bar,
  Baz { a: i32, b: u32, c: String },
  UnionVariant2(TestObject, TestObjectTwo),
}

#[enunion::enunion]
pub enum DefaultFoo {
  Bar,
  Zam,
  Zoodle,
  Baz { a: i32, b: u32, c: String },
}

#[enunion::string_enum]
#[derive(Debug)]
pub enum StringTest {
  Bar = "bar",
  Baz = "baz",
  Zoom,
}

#[enunion::string_enum]
#[derive(Debug)]
pub enum StringTest2 {
  Zoomier,
  Zoomin,
  Zoomiest,
}

#[napi(object)]
pub struct TestObject {
  pub test_1: i32,
  pub test_2: Option<i32>,
}

#[napi(object)]
pub struct TestObjectTwo {
  pub test_3: i32,
  pub test_4: Option<i32>,
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
pub fn take_foo_bool(f: FooBool) -> FooBool {
  assert!(matches!(f, FooBool::Bar));
  FooBool::Bar
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

#[napi]
pub fn take_foo_union(f: Foo) -> Foo {
  assert!(matches!(
    f,
    Foo::UnionVariant(
      TestObject {
        test_1: 0,
        test_2: Some(3)
      },
      TestObjectTwo {
        test_3: 2,
        test_4: None,
      }
    )
  ));
  Foo::UnionVariant(
    TestObject {
      test_1: 1,
      test_2: Some(2),
    },
    TestObjectTwo {
      test_3: 3,
      test_4: Some(4),
    },
  )
}

enunion::literal_typed_struct!(LiteralTypedStruct, foo: i64 = 1, bar: bool = true);
enunion::literal_typed_struct!(
  LiteralTypedStruct2,
  foo: String = "foolicious",
  bar: bool = false
);
enunion::literal_typed_struct!(
  LiteralTypedStruct3,
  foo: StringTest = StringTest::Zoom,
  bar: crate::StringTest = StringTest::Bar
);
enunion::literal_typed_struct!(LiteralTypedStruct4, foo: i64 = 3, bar: bool = true);

#[enunion::enunion(discriminant_repr = "none")]
#[derive(Debug)]
pub enum UnionOfStringEnums {
  StringTest(StringTest),
  StringTest2(StringTest2),
}

enunion::literal_typed_struct!(NowWithUnionOfStringEnums, foo: UnionOfStringEnums = UnionOfStringEnums::StringTest(StringTest::Bar));

#[napi]
pub fn take_literal_structs(_l: LiteralTypedStruct) -> LiteralTypedStruct2 {
  LiteralTypedStruct2
}

#[enunion::enunion(discriminant_repr = "none")]
pub enum LiteralDiscriminated {
  One(LiteralTypedStruct),
  Two(LiteralTypedStruct2),
  Three(LiteralTypedStruct3),
  Four(LiteralTypedStruct4),
}

#[napi]
pub fn take_literal_discriminated_enunion_1(l: LiteralDiscriminated) -> LiteralDiscriminated {
  assert!(matches!(l, LiteralDiscriminated::One(_)));
  LiteralDiscriminated::Two(LiteralTypedStruct2)
}

#[napi]
pub fn take_literal_discriminated_enunion_2(l: LiteralDiscriminated) -> LiteralDiscriminated {
  assert!(matches!(l, LiteralDiscriminated::Two(_)));
  LiteralDiscriminated::One(LiteralTypedStruct)
}

#[napi]
pub fn take_literal_discriminated_enunion_3(l: LiteralDiscriminated) -> LiteralDiscriminated {
  assert!(matches!(l, LiteralDiscriminated::Three(_)));
  LiteralDiscriminated::Three(LiteralTypedStruct3)
}

#[napi]
pub fn take_literal_discriminated_enunion_4(l: LiteralDiscriminated) -> LiteralDiscriminated {
  assert!(matches!(l, LiteralDiscriminated::Four(_)));
  LiteralDiscriminated::Four(LiteralTypedStruct4)
}
