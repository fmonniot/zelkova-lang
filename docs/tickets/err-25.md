# ERR-25 · A message about an instance binding writes a member's variable and the head's alike

**Sizing:** small. A choice of spelling in one message path and its pinned test.

**Location:** `crates/zelkova-compiler/src/typer/mod.rs` — `Written::MemberSignature`, its
`ErrorKind::MissingConstraint` message and note, and `VariableNames`, the map from a type
variable to the name a message writes it with; `crates/zelkova-compiler/src/typer/classes.rs` —
`Declared::member_variables`, the set that tells a variable the member's signature binds from
one the instance's head does, and `names_for`, which builds the `VariableNames` a message
reads from the names the source wrote and `letter`.

**Depends on:** [LANG-40](README.md), which adds the instance-binding check and the wording.

**Related:** [ERR-24](err-24.md), the same shape for classes.

**Found:** while reviewing the PR for `LANG-40`. Left unfixed there because the spelling of a
variable that has no name of its own in the source is a diagnostics decision the ticket did
not make.

**Problem:** in an instance binding, a variable the *member's signature* binds and a variable
the *instance's head* binds can be spelled alike, because the class and the instance are
written independently. A message then names two different variables as one. Reproduced against
the `LANG-40` branch:

```zel
class Container a where
  has : a -> b -> b -> Bool

instance Eq b => Container (Box b) where
  has c x y =
    eq x y
```

```
error: [App] `Eq b` is required here, and the member's signature does not provide it
13 │     eq x y
   │     ^^ this use requires an instance
   = in the binding of `has` in the instance `Eq b => Container (Box b)`
   = `b` is bound by the signature of the member and not by the head of the instance, so no
     context of the instance can constrain it: …
```

The message says `Eq b` is not provided while the instance's own context, quoted in the first
note, reads `Eq b`. The note that follows explains it, but a reader meets the contradiction
first. The two `b`s are different variables and the message has no spelling for telling them
apart.

**Fix:** write the member's variable under a name the head does not use when the two collide
— a primed name, or the member's own name qualified (`has`'s `b`) — in the message and in the
note that restates the constraint. Which spelling is the one open choice, and the ticket does
not pick. `names_for` is where it goes: it takes each written name as it stands and skips a
taken one only for a variable the source did not name, so two written names that agree
are never told apart.

**Acceptance:** a test in `crates/zelkova-compiler/tests/typer.rs` for the instance above,
asserting the member's variable is written differently from the head's `b` in the message and
the note, and that goes red when the collision rule is removed. The existing
`MemberSignature` test, where the two names differ, keeps passing unchanged.
