# Zelkova

Like Elm, but different species.

Zelkova is my attempt to understand how one could build a functional programming language compiling to web assembly. It is strongly inspired by Elm, with some Haskell addition here and there.

Do not expect any production-level code here, I'm just trying to understand how a compiler works :)



## References

Here is a list of resources I consulted while working on this project.

### Articles, Documentation and Source Code

- http://dev.stephendiehl.com/fun/index.html
- https://github.com/RustPython/RustPython/blob/master/parser/src/python.lalrpop
- https://github.com/gluon-lang/gluon/blob/master/base/src/ast.rs
- https://github.com/gluon-lang/gluon/blob/master/parser/src/grammar.lalrpop
- http://smallcultfollowing.com/babysteps/blog/2016/03/02/nice-errors-in-lalrpop/
- https://github.com/lalrpop/lalrpop/issues/67
- https://github.com/elm/compiler/blob/master/compiler/src/Parse/
- https://github.com/elm/compiler/blob/master/compiler/src/AST/Source.hs
- https://elm-lang.org/docs/syntax

- https://github.com/tcr/rust-hindley-milner/blob/master/src/lib.rs
- https://github.com/0b01/tensorscript/tree/master/trsc/src/typing
- https://rickyhan.com/jekyll/update/2018/05/26/hindley-milner-tutorial-rust.html#
- Wand algorithm is a simpler version of Hindley-Milner (different trade off though)


### Papers & Books

- [_The Hindley-Milner Type Inference Algorithm_ by Ian Grant](http://steshaw.org/hm/hindley-milner.pdf)
- [_Generalizing Hindley-Milner Type Inference Algorithms_ by Bastiaan Heeren, Jurriaan Hage and Doaitse Swierstra](https://pdfs.semanticscholar.org/8983/233b3dff2c5b94efb31235f62bddc22dc899.pdf)
- [_Hindley-Milner Elaboration in Applicative Style_ (Functional pearl) by François Pottier](http://gallium.inria.fr/~fpottier/publis/fpottier-elaboration.pdf)
- [_Principled Parsing for Indentation-Sensitive Languages_ by Michael D. Adams](https://michaeldadams.org/papers/layout_parsing/LayoutParsing.pdf)
- [_Extensible records with scoped labels_ by Daan Leijen](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf)<sup>1</sup>
- [_The Essence of ML Type Inference_ by Pottier and Rémy (Advanced Topics in Types and Programming Languages)](http://gallium.inria.fr/~fpottier/publis/emlti-final.pdf) <sup>1</sup>
- [_Types and Programming Languages_ by Benjamin C. Pierce](https://mitpress.mit.edu/books/types-and-programming-languages)

<sup>1</sup> The Elm compiler (0.19) is based/inspired by those papers and books.

#### Finding cycles

- https://www.cs.tufts.edu/comp/150GA/homeworks/hw1/Johnson%2075.PDF
- https://www.youtube.com/watch?v=johyrWospv0 (nice explanation of above paper)

The above references ended up unused because of the amazing [petgraph](https://docs.rs/petgraph/) library.

## Documentation

The language itself is specified under [`docs/spec/`](docs/spec/INDEX.md), one chapter
per file, starting from that directory's index. It is normative — it describes
Zelkova as designed, not just what the compiler happens to accept today — and every
code example in it is checked against the compiler by `cargo test --test spec`, so a
chapter that drifts from the compiler is a red test rather than a stale sentence.

`cargo doc` documents the compiler's own Rust, not the language; `docs/tickets/` is
the compiler's work log, not documentation for someone writing Zelkova.

- [Greg Wilson has some idea of what a community need](https://third-bit.com/2020/09/01/what-every-community-needs.html), maybe we can use some of them.
