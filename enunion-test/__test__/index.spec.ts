import {
  takeFoo,
  takeFooNoDiscriminant,
  takeFooEnum,
  takeFooDefault,
  takeFooString,
  takeStringTest,
  FOO_TYPE_BAZ,
  FOO_STRING_TYPE_BAR,
  DEFAULT_FOO_TYPE_BAR,
  FOO_ENUM_TYPE_BAR,
  DEFAULT_FOO_TYPE_ZOODLE,
  StringTest,
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

it("calls takeFooNoDiscriminant", () => {
  let r = takeFooNoDiscriminant({ a: 1, b: 2 });
  expect(r["d"]).toBe(1);
  expect(r.b).toBe(2);
  expect(Object.keys(r).length).toBe(2);
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
