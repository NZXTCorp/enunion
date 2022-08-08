import {
  DEFAULT_FOO_TYPE_BAR,
  DEFAULT_FOO_TYPE_ZOODLE,
  FOO_BOOL_TYPE_BAR,
  FOO_ENUM_STR_TYPE_BAR,
  FOO_ENUM_TYPE_BAR,
  FOO_NEW_TYPE_BAR,
  FOO_STRING_TYPE_BAR,
  FOO_TYPE_BAZ,
  FOO_TYPE_UNION_VARIANT,
  FooNoDiscriminant,
  StringTest,
  takeFoo,
  takeFooKeywordFieldName,
  takeFooBool,
  takeFooDefault,
  takeFooEnum,
  takeFooEnumStr,
  takeFooNoDiscriminant,
  takeFooNoDiscriminantTransparentManyStructs,
  takeFooNoDiscriminantTransparentOneField,
  takeFooString,
  takeFooUnion,
  takeLiteralDiscriminatedEnunion1,
  takeLiteralDiscriminatedEnunion2,
  takeLiteralDiscriminatedEnunion3,
  takeLiteralDiscriminatedEnunion4,
  takeLiteralStructs,
  takeStringTest, FOO_WITH_KEYWORD_FIELD_NAME_TYPE_BAZ,
} from "../index";

it("calls takeStringTest", () => {
  expect(takeStringTest(StringTest.Bar)).toBe(StringTest.Baz);
});

it("calls takeFooDefault", () => {
  expect(
    takeFooDefault({ defaultFooType: DEFAULT_FOO_TYPE_BAR }).defaultFooType
  ).toBe(DEFAULT_FOO_TYPE_ZOODLE);
});

it("calls takeFooEnum", () => {
  expect(takeFooEnum({ fooEnumType: FOO_ENUM_TYPE_BAR }).fooEnumType).toBe(
      FOO_ENUM_TYPE_BAR
  );
});

it("calls takeFooBool", () => {
  expect(takeFooBool({ fooBoolType: FOO_BOOL_TYPE_BAR }).fooBoolType).toBe(
      FOO_BOOL_TYPE_BAR
  );
});

it("calls takeFooEnumStr", () => {
  expect(takeFooEnumStr({ fooEnumStrType: FOO_ENUM_STR_TYPE_BAR }).fooEnumStrType).toBe(
      FOO_ENUM_STR_TYPE_BAR
  );
});

it("calls takeFooNoDiscriminant", () => {
  let r = takeFooNoDiscriminant({ a: 1, b: 2 });
  // @ts-ignore
  expect(r["d"]).toBe(1);
  // @ts-ignore
  expect(r["b"]).toBe(2);
  expect(Object.keys(r).length).toBe(2);
});

it("calls takeFooNoDiscriminantTransparentOneField", () => {
  expect(takeFooNoDiscriminantTransparentOneField(StringTest.Bar)).toBe(StringTest.Baz);
});

it("calls takeFooNoDiscriminantTransparentManyStructs", () => {
  let input: FooNoDiscriminant = { newType: FOO_NEW_TYPE_BAR, fooStringType: FOO_STRING_TYPE_BAR};
  expect(takeFooNoDiscriminantTransparentManyStructs(input)).toEqual({ newType: FOO_NEW_TYPE_BAR, fooStringType: FOO_STRING_TYPE_BAR});
});

it("calls takeFooString", () => {
  expect(
    takeFooString({ fooStringType: FOO_STRING_TYPE_BAR }).fooStringType
  ).toBe(FOO_STRING_TYPE_BAR);
});

it("calls takeFoo", () => {
  let r = takeFoo({
    a: 3,
    b: 2,
    c: "Hello from TypeScript",
    myMultiWordField: 2,
    fooType: FOO_TYPE_BAZ,
  });
  expect(r.fooType).toBe(FOO_TYPE_BAZ);
  if (r.fooType === FOO_TYPE_BAZ) {
    expect(r.a).toBe(1);
    expect(r.b).toBe(2);
    expect(r.c).toBe("Hello from Rust");
    expect(r.myMultiWordField).toBe(8);
  }
});

it("calls takeFooKeywordFieldName", () => {
  let r = takeFooKeywordFieldName({
    a: 3,
    b: 2,
    c: "Hello from TypeScript",
    myMultiWordField: 2,
    type: FOO_WITH_KEYWORD_FIELD_NAME_TYPE_BAZ,
  });
  expect(r.type).toBe(FOO_WITH_KEYWORD_FIELD_NAME_TYPE_BAZ);
  if (r.type === FOO_WITH_KEYWORD_FIELD_NAME_TYPE_BAZ) {
    expect(r.a).toBe(1);
    expect(r.b).toBe(2);
    expect(r.c).toBe("Hello from Rust");
    expect(r.myMultiWordField).toBe(8);
  }
});

it("calls takeFooUnion", () => {
  let r = takeFooUnion({
    test1: 0,
    test2: 3,
    test3: 2,
    fooType: FOO_TYPE_UNION_VARIANT,
  });
  expect(r.fooType).toBe(FOO_TYPE_UNION_VARIANT);
  if (r.fooType === FOO_TYPE_UNION_VARIANT) {
    expect(r.test1).toBe(1);
    expect(r.test2).toBe(2);
    expect(r.test3).toBe(3);
    expect(r.test4).toBe(4);
  }
});

it("calls takeLiteralStructs", () => {
  let l = takeLiteralStructs({
    foo: 1,
    bar: true,
  });
  expect(l.foo).toBe("foolicious");
  expect(l.bar).toBe(false);
})

it("check Serialization and Deserialization of LiteralDiscriminated", () => {
  let l = takeLiteralDiscriminatedEnunion1({
    foo: 1,
    bar: true,
  });
  expect(l.foo).toBe("foolicious")
  expect(l.bar).toBe(false)

  let l2 = takeLiteralDiscriminatedEnunion2({
    foo: "foolicious",
    bar: false,
  });
  expect(l2.foo).toBe(1)
  expect(l2.bar).toBe(true)

  let l3 = takeLiteralDiscriminatedEnunion3({
    foo: StringTest.Zoom,
    bar: StringTest.Bar,
  });
  expect(l3.foo).toBe(StringTest.Zoom);
  expect(l3.bar).toBe(StringTest.Bar);

  let l4 = takeLiteralDiscriminatedEnunion4({
    foo: 3,
    bar: true,
  });
  expect(l4.foo).toBe(3);
  expect(l4.bar).toBe(true);
})


it("check that you can iterate exported string enums", () => {
  let keys = Object.keys(StringTest);
  expect(keys.length).toBe(4);
  expect(keys.includes("Zoom")).toBe(true)
  expect(keys.includes("Bar")).toBe(true)
  expect(keys.includes("Baz")).toBe(true)
  expect(keys.includes("A_STRANGE_KEY")).toBe(true)
  expect(StringTest.Zoom).toBe("Zoom")
  expect(StringTest.Bar).toBe("bar")
  expect(StringTest.Baz).toBe("baz")
  expect(StringTest.A_STRANGE_KEY).toBe("AStrangeKey")
})