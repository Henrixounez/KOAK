This document will hold all the information we found about LLVM.

We will follow [this](http://www.stephendiehl.com/llvm/#haskell-llvm-bindings) tutorial and explain it along the way.

# Chapter 3

---
```
double :: Type
double = FloatingPointType 64 IEEE
```

`IEEE` is jut a standard. This snippet is just mapping the representation of a 64bit double to an haskell function.

---

A `Name` is a way to link a **LLVM-IR object** address to a string name in order to create human-readable output.


---

`Symbol Table` est la table qui lie un nom à un paramètre.

---

`Operand` est un paramètre d'instruction (add, mul, ...)

---

`Word` est la représentation d'un `Int` en mémoire. C'est un unsigned Int.

---

`Names` est une map qui lie des string à des index (unnamed)

`Named` est une instruction qui peut etre référencée par un nom.

---

`Inscruction` est évidement une inscruction comme `add`, `mul`, ...

---

`Terminator` est l'instruction finale d'un block de code

---

`execState` prend un state à un état x et applique des changements via un nouveau state.

---

`emptyModule` return l'état par défaut d'un module et change son nom.

---

`Definition` c'est tout ce qui peut être au top level d'un module (metadata, globales, ...)

Example: def test(int: a, int: b) 

`test` est une définition globale.

---

`GlobalDefinition` est un objet contenant une définition globale (comme le test au dessus)

---

`Parameter` prend un `type`, un `name` et un tableau d'attributs (`unused`)

---

`externf` va emmettre une valeur nommée (`Operand`) qui va réferer à une toplevel valeur (`@test` ou `@putchar` par exemple)

---

`Named` peut représenter une instruction qui peut être nommée ou non

