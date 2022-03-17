# ACE Can Explode 🧨🧨

ACE is a dynamically typed programming language whose lexer, parser and interpreter is entirely written in rust 🦀

The interpreter is unable to throw runtime exceptions: in fact, there is no [RuntimeException](/src/interpreter.rs) type. Everything is left for the developer to deal with.

This language just can't give a *f\*\*\**

It has a very small code base ( although it still requires lots of cleaning up ), ~~making it easy to understand and perfect for first time language developers (so, me) to dive into~~

---

The syntax is quite simple, having only one SINGLE reserved keyword: fun 😊 (untrue but lets move on)

It's similar to JavaScript and Lua, or rather a combination of both

In ACE, every script should be contained with a { }, as shown here:
```
{
    message_to_readers = "🐛"
    print(message_to_readers) # >>> 🐛
}
```

This is because scripts can either be a single expression or a series of statements, hence the difference from:

``` hey
# /ezmath.ace

7 + 8 # entire script returns 7 + 8
```

In the future, scripts should be able to import other scripts and make use of that return:

```
{
    7_plus_8 = import("ezmath.ace")() # preview syntax, not final product

    print(7_plus_8) # >>> 15
}
```

As to how to declare functions, create objects, etc. please look at the [examples](/examples/)

---
## Benchmarks:
Working on it..

---

## Motivation
This is my first attempt at writing a full programming language, interpreter included.

It's far from complete, and to be frank I don't think it's a language I would be thrilled to use (its got nil ew...), but you have to get your feet wet somehow.

If there is any features I would still want to implement, it would many of those:

- [ ] For loops
- [ ] Destructuring: `` {a, b, c} = :{a: 5, b: 3, c:"yea"}``
- [ ] Imports
- [ ] Pattern Matching / Switch Statements
- [ ] Type checking: `` a typeof string``
- [ ] Combinators: ``map sum flatten...``
- [ ] IO
- [ ] Multi-line comments
- and many more...

__Logistics__

Whats really under the hood can only be described as a disaster. To keep it short, these are the many things that have to get fixed:
- [ ] Overreliance on reference counting, possible reference cycles
- [ ] Heavy amounts of cloning
- [ ] Unstructured code
- [ ] Poor error handling
- [ ] Undocumented
- [ ] Lacks testing
- [ ] Scoping issues

But enough of that, lets end with the cools things I still want to experiment with:
- [ ] Parse tree pre-evaluation
- [ ] Bytecode
- [ ] Compiling to real machine code (RISC-V..)
- [ ] Packaging

---
## Resources
I followed the book over on [Crafting Interpreters](https://craftinginterpreters.com/) for some of the parsing process which taught me A LOT, and then diverged from there. I very much dislike reading technical stuff, and tbh I felt like my head was just about to fall of after the 4th chapter. Nevertheless, I recommend reading through all of it, if you can.

From that point forward I did whatever felt right. Not much can be said, aside from the fact that you should learn about your smart pointers. They're very useful.

## How to try out
For now, you have to run your script files directly from the source code. Clone this repo and then run:

```$ cargo run --release -- --i code.ace ```

This will hopefully change very soon.


