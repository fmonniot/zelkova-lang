# BUG-41 · A union reached only transitively has no spelling, and `Spellings::spell` falls back to a name that can still collide

**Severity:** low (message-only ambiguity — the underlying type identity is correct and the
build still fails where it should; only the sentence explaining why is unwritable).

**Location:** `src/compiler/typer/mod.rs` — `Spellings::spell`'s `None` arm (currently ~line
1571) and the `Spellings::of`/`Spellings` doc comment above it (currently ~line 1525-1534),
which is where [`BUG-37`](README.md)'s rule — "a message names a type as the package being
compiled spells it" — is written down.

**Problem:** `Spellings::spell` looks a union's module up in the map `Spellings::of` builds
from `interfaces` — the modules the checked package can itself import — and writes the union
by that spelling. When the lookup misses, it falls back to the union's own bare name
(`name.to_name()`, e.g. `Size.Size`) rather than the checked package's spelling, because there
is none: the fallback exists for a module the checked package cannot import at all, only reach
indirectly through another module's signature.

That happens whenever a union arrives **transitively**. Reproduced against this tree with a
three-package chain: `tests/fixtures/dep_widgets` (`acme-widgets`, declares `Size.Size`) is
wrapped-depended-on by `tests/fixtures/dep_mid` (`acme-mid`), whose `Lib.size : AcmeWidgets.Size.Size`
re-exports that union in its own signature. A third package that depends, wrapped, only on
`acme-mid` — never on `acme-widgets` directly — has no key for `acme-widgets:Size` in its own
`interfaces` map, even though the union is reachable through `AcmeMid.Lib.size`:

```zel
module App exposing (f)

import AcmeMid.Lib
import Size


f : Size.Size
f = AcmeMid.Lib.size
```

with a local `src/Size.zel` declaring `type Size = Mine`. This produces:

```
error: [App] cannot match `Size.Size` with `Size.Size`
```

Both sides are different declarations — the local `Size.Size` and `acme-widgets`' `Size.Size`
— but the message uses one word for both, exactly the ambiguity `Spellings` exists to prevent
in the case [`BUG-37`](README.md) closed. The build still correctly fails to unify them; only
the sentence is uninformative.

**Why this was not fixed in [`BUG-37`](README.md):** that ticket's rule has no answer for a
module the checked package has no spelling for at all. The obvious fallback — the package
name, e.g. `acme-widgets` — is explicitly ruled out by the same ticket's own text: "Never the
package name itself: `acme-widgets` is a spelling no Zelkova source contains." A fallback has
to be something else, and this ticket does not pick one; see **Fix** below.

**Fix:** undecided — this is the decision the ticket exists to make. Candidates, none chosen:

- Write the union by the **path it was reached through** — e.g. `AcmeMid.Lib.Size.Size` or
  similar, naming the last module the checked package *can* spell, plus the rest of the chain.
  Correct but potentially long, and the exact notation has not been designed.
  `Spellings` would need to carry more than one hop per module, which it does not today.
- Fall back to the union's own **declaring module's name plus its declaring package's own
  namespace-style spelling** (`PackageName::namespace()`, e.g. `AcmeWidgets`) even though the
  checked package never wrote that namespace itself — this reads like a spelling that could
  exist without being one the checked package's own source contains, which is a real
  divergence from the "never a spelling no Zelkova source contains" principle and needs the
  language owner's judgment on whether it is an acceptable compromise.
- Extend `Spellings::of` to also record every module reachable transitively, not only the
  ones directly reachable via `interfaces` — changes what "spelling" means for `BUG-37`'s own
  rule and needs re-checking against that ticket's Acceptance.

Whichever is chosen, the message must stop pairing two distinct declarations under one
rendered word — the property this ticket exists to restore.

**Acceptance:**

- A `tests/pipeline.rs` test built on the three-package chain above (extending the existing
  `tests/fixtures/dep_widgets` → `tests/fixtures/dep_mid` pair with a third fixture package
  that wraps only `dep_mid`) asserts that the error message names the local `Size.Size` and the
  transitively-reached `acme-widgets` `Size.Size` with two different substrings — neither one
  the bare, ambiguous `Size.Size` twice, and neither the package name alone (`acme-widgets`
  spelled bare is still not user-writable source).
- `cargo test --workspace` is green, and `cargo run` still prints `parsed 8 modules`, lists all
  eight as checked, and exits 0.

**Related:** found in review of [PR #245](https://github.com/fmonniot/zelkova-lang/pull/245)
(`BUG-37`, "put the package in every `QualName`"). Sibling case: `BUG-37`'s own
`a_local_module_and_a_dependencys_of_one_name_declare_two_types` test pins the *directly*
wrapped case this ticket's fallback cannot yet handle.
