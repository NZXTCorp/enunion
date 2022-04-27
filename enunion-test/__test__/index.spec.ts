import { takeFoo, takeFooDefault, takeFooString } from "../index";

it("calls takeFooDefault", () => {
  expect(takeFooDefault({ defaultFooType: 0 }).defaultFooType).toBe(2);
});

it("calls takeFooString", () => {
  expect(
    takeFooString({ fooStringType: "FOO_STRING_TYPE_BAR" }).fooStringType
  ).toBe("FOO_STRING_TYPE_BAR");
});

it("calls takeFoo", () => {
  let r = takeFoo({
    a: 3,
    b: 2,
    c: "Hello World",
    myMultiWordField: 2,
    fooType: 1,
  });
  expect(r.fooType).toBe(1);
  if (r.fooType === 1) {
    expect(r.a).toBe(1);
    expect(r.b).toBe(2);
    expect(r.c).toBe("yo");
    expect(r.myMultiWordField).toBe(8);
  }
});
