# DEC-8 · Records: nine decisions

**Settled:** 2026-09-07, by the language owner (`SPEC-21`).
**Status:** live.
**Where the rule lives:** [Records](../spec/records.md), with the pattern half also stated in
[Patterns](../spec/patterns.md#record-patterns) and the label namespace in
[Name resolution](../spec/name-resolution.md#namespaces).

Records were named by four chapters and specified by none, and one decision already rested on
them: [tuples stop at three](../spec/types.md#tuple-types) because "a tuple of four is where a
record belongs". The nine questions below are what a chapter had to commit to. The first is the
one the rest hang off — extensibility decides the shape of everything else, so it is taken first
here even though the chapter states it late.

## 1 — Records are closed: no row polymorphism

A record type names every field the record has. `{ r | name : String }` as a *type* — "any record
with a `name` field" — is not writable, and a function accepts the whole record type it is given.

The alternative is Elm's, and it is the expensive one. A variable standing for "the rest of the
fields" is a second kind of thing a type variable may be, and the checker has to solve a second
kind of equation beside the one it solves for types. [Type classes](../spec/type-classes.md) had
already declined higher-kinded variables on that reasoning; a field-set variable is the same
choice arriving a second time, and answering it the same way keeps the type system on one axis.

What is given up is real and is stated in the chapter rather than argued there: a function cannot
be written once over records that differ. What is bought is that every record type a program
mentions is written down in that program, and an annotation says exactly which records reach a
function. A function wanting to serve many records takes the fields it needs as arguments.

This is also what makes decision 3's accessor awkward, and the awkwardness is the honest price:
`.name` has no type it can stand for.

## 2 — A record type is a structural, order-insensitive set of fields

Two record types are the same type when they carry the same labels with the same field types.
Nothing about where a record type was written is part of it, so two modules that separately
annotate `{ x : Int, y : Int }` have written one type.

The two alternatives both make a record type nominal in some degree. **Order-sensitive** costs a
reader nothing to obey and buys nothing either: field order carries no meaning, so making it
significant only creates type errors between spellings that mean the same thing. **Nominal
through a `type alias`** is the more tempting one, because it gives a label a declaration site —
which decision 7 then has to invent an answer for. It loses against
[Types](../spec/types.md#type-aliases)' existing rule that an alias introduces no new type: making
record aliases the exception would mean `type alias` did one thing for tuples and another for
records, and a reader would have to know which.

An alias over a record therefore also introduces **no constructor function** of its fields. A
record is built by writing its fields out.

## 3 — `r.name` reads a field, `.name` is a function, and whitespace separates them

`.name` alone is `\r -> r.name`. A `.` written against the expression to its left is an access; a
`.` with whitespace before it, or one opening an expression, is an accessor.

The whitespace rule is forced rather than chosen. `f .name` and `f.name` produce the same token
stream, and once a bare accessor exists the two spellings have to mean different things — so
adjacency is the only signal left. It reaches the qualification dot as a consequence, which today
accepts spaces on both sides: `Widget . size` currently parses as a qualified name and must stop
([`LANG-52`](../tickets/lang-52.md)).

Dropping the bare accessor was the alternative, and it is cheaper in exactly one place — the
typer, where decision 1 leaves `.name` with no type of its own. It is rejected because the
lambda that replaces it, `\p -> p.name`, is the single most common lambda a record program
writes, and it names its parameter twice to say nothing.

Given that, an accessor is typed **from where it is written**: the record type comes from what it
is applied to or from the annotation of the position it sits in, and an accessor nothing fixes is
an error naming itself. That is a constraint solved late rather than an ordinary equation, and it
is the one place records complicate the unifier ([`LANG-51`](../tickets/lang-51.md)).

## 4 — An update has the type of the record it updates

`{ r | x = 5 }` replaces the named fields and keeps every other. Each label must already be a
field, each value must have that field's existing type, and the record before and after an update
is usable in exactly the same places.

**Letting an update change a field's type** was the alternative with a real argument behind it —
it is a natural way to write a record-shaped transformation in one step. It is declined because
it makes the *result* type of an update depend on the values written into it, which is precisely
the reading-the-whole-expression-to-know-the-type that closed records exist to avoid. A record of
a different shape is written out in full, which is what a program is doing anyway.

Dropping the update form entirely was also considered and would leave a record of many fields
copied field by field to change one, which is the case the form exists for.

## 5 — A record pattern is `{ label = pattern }`, names a subset, and has a `{ label }` shorthand

Each entry is a label and a whole pattern, so record patterns nest like every other form; `{ name }`
is shorthand for `{ name = name }`; and the fields a pattern does not mention are not matched and
not bound.

**Elm's names-only form** — `{ x, y }`, binding each field under its own name with no
sub-pattern — is the shorthand and nothing else, and it makes matching on a field's shape need a
second `case`. Zelkova's patterns nest everywhere else and there is no reason for a record to be
where that stops.

**Requiring every field to be named**, the way a constructor pattern takes all its arguments, is
the other alternative and it is a worse analogy than it looks: a constructor pattern is
positional, so an omitted argument would have nowhere to be, whereas a record pattern says which
field it means. Requiring the rest would make adding a field to a record edit every pattern that
matches one, for no gain.

The subset rule is what makes refutability the interesting half:
[a record pattern is refutable exactly when one of its sub-patterns is](../spec/records.md#record-patterns),
so a pattern of shorthand entries can never fail and is legal in a parameter.

## 6 — A record has at least one field

`{}` is neither a type nor an expression. The [unit type](../spec/types.md#the-unit-type) already
names the type with a single value, and two spellings for it is what a language does not want.

`{}` falling out of the grammar for free was the argument for allowing it. It is not free: it is
a second unit, and every question about the first one — what it means as a return type, what an
instance of it computes — then has two answers to keep in step.

## 7 — A label is a name, in a sixth namespace, put there by every record type that mentions it

Labels join the [five namespaces](../spec/name-resolution.md#namespaces) as a sixth, so a value
`x` and a field `x` never collide. What puts a label there is any record type mentioning it, and
two record types may both carry `x` without that being a clash: which record a label belongs to
is decided by the type it is read against, never by what is in scope. A label is never imported,
exposed or shadowed, because no declaration introduces one.

Treating a label as **not a name at all** — part of a record's syntax, like a tuple's comma — was
the alternative and describes the same behaviour from the outside. It is rejected because it
leaves the chapters with nowhere to say what a label *is*, and because the shorthand pattern is a
place where a label and a value genuinely meet: `{ x }` reads a label and binds a value. Two
namespaces with a stated direction between them says that; "not a name" has to hand-wave it.

Decision 2 is what makes this possible without a declaration site. A nominal record would give a
label one, and would then have to answer whether two aliases may both carry `x` — which is a
clash the structural reading never has.

## 8 — A record is walked in label order by a derivation, with no fourth binding

A [derivation](../spec/type-classes.md#a-class-says-how-it-is-derived) walks a record's field
pairs in label order, sorted by the label's characters, folding with `combine` and ending at
`matched`. `differed` is never reached: a record has one shape.

[Type classes](../spec/type-classes.md#open-questions) had named a **fourth binding** — an answer
for a labelled field — as the thing records would want, and they turn out not to want it. A field
label is data of a kind the three existing bindings have no counterpart for, and a class that
received one could only use it to make a derived member depend on a label's spelling. Decision 2
makes that unwanted: field order is not part of a record type, so nothing a derivation computes
should be able to see one spelling rather than the other. Sorting is the only order two spellings
of one type agree on, which is why it is the walk's rather than the source's.

A record is walked rather than delegated to, because
[an instance may be declared only in the module declaring its class or its type](../spec/type-classes.md#where-an-instance-may-be-declared)
and a record type is declared in no module. `instance Eq { x : Int }` names no module that could
hold it. Each field's *value* still goes through its own type's instance in the ordinary way, so
a type inside a record keeps its own definition of a member.

## 9 — The chapter specifies the three forms and the update; the rest is not a record question

The type, construction, access, update and the pattern are the whole of the language's part.
Recorded because two things a reader may expect to find are deliberately absent: a record type
cannot contain itself — it is written out in full wherever it appears, so recursion goes through
a `type` declaration — and there is no field-removal or field-addition form, which decision 1
already rules out at the type level.
