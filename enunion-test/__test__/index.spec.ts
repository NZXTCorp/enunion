import {
  DEFAULT_FOO_TYPE_BAR,
  DEFAULT_FOO_TYPE_ZOODLE,
  FOO_BOOL_TYPE_BAR,
  FOO_ENUM_STR_TYPE_BAR,
  FOO_ENUM_TYPE_BAR,
  FOO_STRING_TYPE_BAR,
  FOO_TYPE_BAZ, FooNew, FooNoDiscriminant, FooStringNew,
  StringTest,
  takeFoo,
  takeFooBool,
  takeFooDefault,
  takeFooEnum,
  takeFooEnumStr,
  takeFooNoDiscriminant,
  takeFooNoDiscriminantTransparentManyStructs,
  takeFooNoDiscriminantTransparentOneField,
  takeFooString,
  takeStringTest,
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
  expect(r["d"]).toBe(1);
  expect(r["b"]).toBe(2);
  expect(Object.keys(r).length).toBe(2);
});

it("calls takeFooNoDiscriminantTransparentOneField", () => {
  expect(takeFooNoDiscriminantTransparentOneField(StringTest.Bar)).toBe(StringTest.Baz);
});

it("calls takeFooNoDiscriminantTransparentManyStructs", () => {
  let input: FooNoDiscriminant = { newType: 0, fooStringType: "FOO_STRING_TYPE_BAR"};
  expect(takeFooNoDiscriminantTransparentManyStructs(input)).toEqual({ newType: 0, fooStringType: "FOO_STRING_TYPE_BAR"});
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
