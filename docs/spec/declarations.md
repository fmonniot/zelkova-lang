# Declarations

A module is a [header](modules.md#the-module-header) followed by declarations. A declaration
is what puts a name into a module: a value, a function, a type, an operator, or a name
borrowed from somewhere else. There is no other way to introduce one, and there is nothing
else at the top level of a file.

There are five forms. Three of them are made of a type expression or a module name, and are
specified alongside those:

| Form | Written | Specified in |
|---|---|---|
| import | `import Widget exposing (Size)` | [Modules](modules.md#imports) |
| type annotation | `origin : Flag` | [Types](types.md#type-annotations) |
| type declaration | `type Flag = On \| Off` | [Types](types.md#type-declarations) |
| [binding](#bindings) | `origin = On`, `invert flag = …` | below |
| [`infix`](#infix-declarations) | `infix left 6 (+) = add` | below |

Every one of them begins in column 1, and a token in column 1 is what ends the declaration
before it — see [Layout](layout.md#top-level-declarations). There is no separator and no
terminator; a blank line between two declarations is conventional and means nothing.

## Bindings

A **binding** gives a name to an expression. It is the name, then zero or more parameters,
then `=`, then the expression.

```zel expect=ok
module Example exposing (Flag, origin, invert)

type Flag
  = On
  | Off

origin : Flag
origin =
  On

invert : Flag -> Flag
invert flag =
  case flag of
    On ->
      Off

    Off ->
      On
```

A value and a function are the same form, told apart only by whether any parameters were
written. There is no keyword marking one or the other, and nothing in the language treats
`origin` and `invert` differently for having been written with zero parameters and one. What a
name is bound to is a question about its type, and the type is [written down
separately](types.md#type-annotations).

The body is an [expression](expressions.md), and it may be written on the same line as the
`=` or on following lines, which must then be indented:

```zel expect=ok
module Example exposing (Flag, origin)

type Flag
  = On

origin : Flag
origin = On
```

### The name is a lowercase identifier

A binding's name is a lowercase-initial identifier — the same [identifier
rules](lexical-structure.md#identifiers) every value name follows. An uppercase-initial name
belongs to a type or a constructor, and writing one where a binding is expected is a syntax
error:

```zel expect=parse-error:UnexpectedToken
module Example exposing (f)

Flag =
  1
```

An operator is never a binding's name either. `(+) a b = a` is not a declaration:

```zel expect=parse-error:UnexpectedToken
module Example exposing (add)

(+) a b =
  a
```

An operator has no body of its own to bind. It is a name standing for a function that was
declared the ordinary way, and the [`infix` declaration](#infix-declarations) below is what
connects the two. Keeping the two separate is what lets an importing module name the operator
without the function, which is [Modules](modules.md#operators)' subject.

### Declarations are unordered

The declarations of a module are all in scope throughout it, so a binding may name one written
below it, and two bindings may name each other. Nothing has to be declared before it is used,
and reordering a file's declarations never changes what it means.

```zel expect=ok
module Example exposing (Flag, first)

type Flag
  = On

first : Flag
first =
  second

second =
  On
```

The alternative — a name being visible only after the line that declares it — would make
mutual recursion between two top-level functions impossible to write, and would give the order
of a file a meaning that a reader has to keep track of. Which name refers to which declaration
is [Name resolution](README.md#chapters)' subject; that a declaration's position never enters
into it is this rule.

### Parameters

Each parameter is a [pattern](patterns.md#where-a-pattern-may-appear), and the parameters
together are one binding position: a name may be bound [only
once](patterns.md#a-pattern-binds-each-name-once) across all of them. Juxtaposition separates
one parameter from the next, which is why a constructor pattern with arguments is
[parenthesised here](patterns.md#constructor-patterns) and is not in a `case` branch.

A binding may name **at most** as many parameters as its annotation has arrows. Naming all of
them and naming none are both ordinary:

```zel expect=ok
module Example exposing (Flag, apply)

type Flag
  = On

apply : Flag -> Flag -> Flag
apply =
  swap

swap a b =
  b
```

Naming some of them is too. `apply a` below has one parameter against two arrows, and the
`Flag -> Flag` left over is what the body produces:

```zel expect=canonical-error:BindingPatternsInvalidLen
module Example exposing (Flag, apply)

type Flag
  = On

apply : Flag -> Flag -> Flag
apply a =
  swap a

swap a b =
  b
```

**Known gap:** that block should be `expect=ok`. A binding with any parameters at all must
name exactly as many as the annotation has arrows, so the only two counts accepted today are
all of them and none ([`docs/tickets/lang-25.md`](../tickets/lang-25.md)).

An arrow is part of a type, not a promise about how a declaration is written. `Flag -> Flag ->
Flag` and `Flag -> (Flag -> Flag)` are the same type, so a rule that counted arrows would give
two spellings of one type two different sets of legal declarations.

Naming *more* parameters than the annotation has arrows is an error, and
[Types](types.md#the-annotation-and-the-declarations-parameters) specifies it along with the
rest of what an annotation and its declaration owe each other.

### A binding has a body

An annotation on its own does not declare anything. A name that is annotated and never bound
is an error at the annotation:

```zel expect=canonical-error:NoBindings
module Example exposing (Flag, f)

type Flag
  = On

f : Flag
```

A [JS interop](js-interop.md) facade is the one place this does not hold: a
`module javascript` module is annotations with no bodies, because the bodies are in the
companion `.mjs` file, and having none is what makes it a facade.

## Clauses

**Not implemented:** a binding may be written as several **clauses**, one per line, each with
its own parameters and its own body. The clauses share a name, and that is what makes them one
declaration.

```zel expect=canonical-error:MultipleBindingsUnsupported
module Example exposing (Flag, invert)

type Flag
  = On
  | Off

invert : Flag -> Flag
invert On = Off
invert Off = On
```

The compiler parses the clauses and then reports that a name declared over more than one
binding is not supported, so a declaration has exactly one clause today and any pattern that
can fail has to go in a `case`. [`docs/tickets/lang-20.md`](../tickets/lang-20.md) is the
ticket. [Patterns](patterns.md#a-pattern-that-can-fail-and-one-that-cannot) specifies the
pattern half of the construct: the clauses are tried in the order written, they must cover the
type between them, and they are one binding position each.

A declaration with one clause is the ordinary case rather than a degenerate multi-clause one.
The rules on a binding's name, its parameters and its body are all rules about a clause, and
each holds whether a declaration has one clause or several.

### Every clause names the same number of parameters

The clauses of one declaration are one function, and a function has one arity. Clauses that
disagree about it describe no function at all:

```zel expect=canonical-error:BindingPatternsInvalidLen
module Example exposing (Flag, f)

type Flag
  = On
  | Off

f On = Off
f Off On = On
```

This holds with or without an annotation, and it is checked against the clauses themselves
rather than against the annotation — which is why the block above is rejected although it has
nothing to disagree with.

### The clauses stand together

A declaration's clauses are consecutive: nothing may come between them but blank lines and
comments. A clause written apart from its siblings is not a late addition to that declaration
— it is an error.

```zel expect=canonical-error:MultipleBindingsUnsupported
module Example exposing (Flag, invert)

type Flag
  = On
  | Off

invert : Flag -> Flag
invert On =
  Off

other =
  On

invert Off =
  On
```

**Known gap:** that block is rejected today only because a declaration may not have more than
one clause at all. Clauses are gathered by name from anywhere in the module, so once several
are supported the block above would be accepted
([`docs/tickets/lang-26.md`](../tickets/lang-26.md)).

Order among the clauses decides which one is tried first, so a clause is not something a
reader can afford to find by searching. Requiring them to stand together is what makes a
declaration a thing one can read: its name appears once, in one place, with every case it
handles beneath it.

## `infix` declarations

An `infix` declaration binds an operator to a function and says how expressions written with
it group. It is the keyword `infix`, an associativity, a precedence, the operator in
parentheses, `=`, and the function's name.

```zel expect=ok
module Example exposing ((+), add)

infix left 6 (+) = add

add a b =
  a
```

The operator is then a name like any other, and `a + b` is `add` applied to `a` and `b`.
Nothing about any operator is built into the language; [Expressions](expressions.md#operators)
specifies what one means, and [Lexical
structure](lexical-structure.md#operators) which spellings are available to be given one.

An `infix` declaration is unordered along with everything else: the operator may be used above
the line that declares it, and the function it names may be declared below.

### Associativity

The associativity is one of three words — `left`, `right` or `non` — and it is written before
the precedence. The three are [soft keywords](lexical-structure.md#reserved-words): each is a
keyword in this one position and an ordinary name everywhere else. Writing them in the other
order is a syntax error:

```zel expect=parse-error:UnexpectedToken
module Example exposing ((+), add)

infix 6 left (+) = add

add a b =
  a
```

Neither part may be left out. There is no default associativity and no default precedence,
because an operator missing one would group differently in every expression that mixed it with
a different neighbour, and the reader would have to know a convention the source never states.

```zel expect=parse-error:UnexpectedToken
module Example exposing ((+), add)

infix 6 (+) = add

add a b =
  a
```

What each associativity does to an expression is
[Expressions](expressions.md#precedence-and-associativity)' subject.

### Precedence is 0 through 9

A precedence is a single decimal digit: `0` binds loosest, `9` tightest. Ten levels is enough
to place every operator a program declares against the ones it imports, and holding the whole
range to one character is what lets a table of operators be read down its columns.

```zel expect=ok
module Example exposing ((^), pow)

infix left 10 (^) = pow

pow a b =
  a
```

**Known gap:** that block should be rejected — `10` is not a precedence. Any non-negative
integer up to 255 is accepted today ([`docs/tickets/lang-24.md`](../tickets/lang-24.md)).

**Known gap:** a precedence above 255 is worse than accepted — it aborts the compiler, because
the parser narrows the integer it read to a `u8` and unwraps the result
([`docs/tickets/bug-12.md`](../tickets/bug-12.md)). The case is described here rather than
shown because a panicking example would take every other chapter's examples down with it.

A negative precedence is not a small precedence but a syntax error, since `-` is an
[operator](expressions.md#prefix-negation) and not part of the literal that follows it:

```zel expect=parse-error:UnexpectedToken
module Example exposing ((^), pow)

infix left -1 (^) = pow

pow a b =
  a
```

### The function is declared in the same module

The name after the `=` is an unqualified lowercase name, and it must be one this module
declares. A name from another module is a syntax error rather than a lookup that fails:

```zel expect=parse-error:UnexpectedToken
module Example exposing ((+))

infix left 6 (+) = Other.add
```

A name that is not declared anywhere is reported against the `infix` declaration:

```zel expect=canonical-error:InfixReferenceInvalidValue
module Example exposing (add)

infix left 6 (+) = plus

add a b =
  a
```

An operator and the function behind it therefore live in one file, which is what lets every
other module name the operator alone: an importer's `exposing ((+))` brings in a complete
meaning, with nothing left to resolve on its side. See
[Modules](modules.md#operators) for that end of it.

The function must take two arguments. That is a condition on its type rather than on how it
was written, so `infix left 6 (+) = add` is equally correct whether `add` names both of its
parameters, names one, or names none — all three are ways of writing a function of type
`a -> b -> c`.

**Not implemented:** the `infix` declaration is accepted whatever the function's type. A
function of the wrong shape is caught, if at all, where the operator is *used*, and the caret
lands on that expression instead of on the declaration that made the promise. The spec harness
stops before the type checker ([`docs/tickets/test-2.md`](../tickets/test-2.md)), so no block
above pins this.

### An operator has one `infix` declaration

A module declares each operator once. Two declarations for one operator give it two
precedences and two meanings, and no rule chooses between them:

```zel expect=ok
module Example exposing ((+), add, mul)

infix left 6 (+) = add

infix right 7 (+) = mul

add a b =
  a

mul a b =
  a
```

**Known gap:** that block should be rejected. The declarations are collected into a map keyed
by the operator, so the second silently replaces the first and `+` is `mul`, right-associative
at 7 ([`docs/tickets/lang-27.md`](../tickets/lang-27.md)).

## Where a declaration may appear

At the top level of a module, and nowhere else. A declaration's enclosing construct is always
the module.

**Not implemented:** `let … in` introduces bindings local to an expression. The construct does
not exist yet; see [Layout](layout.md#let--in) for what its declarations will be laid out
against.

A [`module javascript`](js-interop.md) facade is the one module that restricts the list: it
may hold imports and annotations, and may not hold a binding, a type declaration or an
`infix`.
