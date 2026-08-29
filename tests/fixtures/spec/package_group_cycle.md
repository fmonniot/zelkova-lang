# Fixture: a `package=` group whose imports form a cycle

Used by `tests/spec.rs::package_group_cycle_is_a_dependency_error`. `Left` imports
`Right` and `Right` imports `Left`, so the package has no valid module order and nothing
in it is canonicalized at all. Both blocks carry `expect=dependency-error`, which is the
only expectation that belongs to the group rather than to one module.

```zel expect=dependency-error package=fixture
module Left exposing (x)

import Right

x = 1
```

```zel expect=dependency-error package=fixture
module Right exposing (y)

import Left

y = 2
```
