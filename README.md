# Koak
## Developper guide
---

Koak is a **kaleidoscope** compiler written in haskell.

### What is kaleidoscope ?

Kaleidoscope is a simple language, compiled, statically-typed, created by llvm for the purpose of implementing their [LLVM tutorial](https://llvm.org/docs/tutorial/OCamlLangImpl1.html#the-basic-language).

### Dependencies

You will need both `stack` and `llvm 9`. For more information about llvm-hs, click [here](https://github.com/llvm-hs/llvm-hs/blob/llvm-4/README.md#installing-llvm).

---
## The Goal

The ambition is to build a JIT compiler interfaced with LLVM.

We can decline the project in 5 big steps :

- Building the AST
- Type inferrence
- Generating LLVM IR
- Adding JIT
- Bonuses


Bonuses may include :

- Handling other native types (String, Int, ...)
- Handling 
- Handling a more flexible syntax (think about `;` in JS)
- Command-line arguments (intermediate steps)
- Transpiling in an other language ?

More informations can be found in the [airtable](https://airtable.com/tblMjy6rQnNTC7BlU/viwyy98dUlRanFF85?blocks=hide).

---

## Usage

Currently, the project only parse a input file into an AST and print it in a human-readable way.

To create the binary, use `make`. It will create a `koak` binary. Use it with a kaleidoscope source file (`test.kd`) to parse it and display the ast generated.

You can find some examples of kd source file in the `kd_sources` folder.

## Testing

Unit testing is done by creating unit-modules in the `package.yaml`.

Unit-modules are defined this way :

```
module_name:
    main:              Spec.hs             # Test entrypoint, always a Spec.hs file
    source-dirs:       test/integration    # Path to directory
    ghc-options:                           # Don't touch that
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:                          # Dependencies needed for the tests
    - koak
    - hspec
    - hspec-discover
```

You may create a directory if you don't find one that already suits you.

You have to create a `Spec.hs` that contains the main function.

##### IF YOU ARE USING HSPEC

You may put this line ***AND THIS LINE ONLY*** into your `Spec.hs` file :

`{-# OPTIONS_GHC -F -pgmF hspec-discover #-}`

It will fetch unit-tests present in your module directory, making testing easier.

You may launch every tests with `make tests` or `stack test`

You an also test specific modules by using `stack test` followed by the name of the module

`$ stack test :module_name` will launch the module_name tests. 