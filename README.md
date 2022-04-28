# enunion

Enunion (enum + union) is the combination of Rust enums and TypeScript Discriminated Unions! By combining this with [`napi`](https://napi.rs) you can define an
enum type in Rust, feed it into TypeScript, and receive instances of it from TypeScript. `enunion` will handle translating at the boundary, and neither language
has to think about the representation in the other language.

# Getting Started

This section assumes you have an existing `napi` project. 

First add `enunion` to your `Cargo.toml`

```toml
[dependencies]
enunion = { git = "https://github.com/NZXTCorp/enunion.git" }
```
<!-- TODO: Replace this with a cargo version once it's published -->


**The enunion macro will not work if `napi` and `napi_derive` are not specified in the `[dependencies]` section of the `Cargo.toml`.**

This macro has a companion program called `enunion-post-build` which must be executed in the directory after `napi build` has been called. Otherwise the resulting TypeScript will not compile.
One simple way to ensure this happens is with a `"postbuild"` script in `package.json` like so

```json
"scripts": {
  "build": "napi build --platform --release",
  "postbuild": "enunion-post-build",
},
```

To install `enunion-post-build` execute this

```
cargo install enunion-post-build
```

Sometimes if a build fails `enunion` may leave behind some of the generated files. Add these to your .gitignore like so

```
/enunion-generated-ts
```

Now you should be good to go, when you add the `enunion` trait to an enum, the macro will take care of the rest. Here's an example of what that might look like

```rust
#[enunion::enunion]
pub enum Foo {
    Bar,
    Baz {
        a: i32,
        b: u32,
        c: String,
        my_multi_word_field: i32,
    }
}

#[napi]
pub fn take_foo(f: Foo) -> Foo {
   assert!(matches!(f, Foo::Baz {a: 3, b: 2, c: _, my_multi_word_field: 2}));
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
```

And on the TypeScript side we can consume it like so

```ts
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
    // Thanks to type narrowing, TypeScript now allows us to access the "Baz" specific fields
    // that don't exist on "Bar".
    expect(r.a).toBe(1);
    expect(r.b).toBe(2);
    expect(r.c).toBe("Hello from Rust");
    expect(r.myMultiWordField).toBe(8);
  }
});
```
