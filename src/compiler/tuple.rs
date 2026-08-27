//! The tuple representation shared by the parser and canonical ASTs.
//!
//! Zelkova follows Elm here: a tuple holds two or three elements — never one,
//! never four. `Tuple` is where that rule is written down, and it is written in
//! the *shape* of the type rather than in a check, so a tuple of any other size
//! cannot be built in the first place. That is deliberate: the rule used to be
//! re-derived at every parser → canonical conversion (once for types, once for
//! patterns, once for expressions), each with its own arithmetic, and the three
//! did not agree.
//!
//! The grammar (`parser/grammar.lalrpop`) has one production per arity, so a
//! four-element tuple in a `.zel` source is a parse error and never reaches an
//! AST at all.

/// A tuple of exactly two or three elements.
///
/// The element type is a parameter so types, patterns and expressions — in both
/// the parser and the canonical AST — all reuse this one enum. `Hash`/`Eq` are
/// derived alongside `PartialEq` because the typer's `Type::Tuple(Tuple<Type>)`
/// needs both: `Type` sits in a `HashSet<Constraint>` during unification.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Tuple<T> {
    Two(Box<T>, Box<T>),
    Three(Box<T>, Box<T>, Box<T>),
}

impl<T> Tuple<T> {
    pub fn two(first: T, second: T) -> Tuple<T> {
        Tuple::Two(Box::new(first), Box::new(second))
    }

    pub fn three(first: T, second: T, third: T) -> Tuple<T> {
        Tuple::Three(Box::new(first), Box::new(second), Box::new(third))
    }

    /// The elements, in source order.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        let (first, second, third) = match self {
            Tuple::Two(a, b) => (&**a, &**b, None),
            Tuple::Three(a, b, c) => (&**a, &**b, Some(&**c)),
        };

        std::iter::once(first)
            .chain(std::iter::once(second))
            .chain(third)
    }

    /// Convert every element with `f`, keeping the arity, and stop at the first
    /// error.
    ///
    /// This is what the parser → canonical conversions use: the arity travels
    /// with the value, so they never count elements.
    pub fn try_map<U, E, F>(&self, mut f: F) -> Result<Tuple<U>, E>
    where
        F: FnMut(&T) -> Result<U, E>,
    {
        match self {
            Tuple::Two(a, b) => Ok(Tuple::two(f(a)?, f(b)?)),
            Tuple::Three(a, b, c) => Ok(Tuple::three(f(a)?, f(b)?, f(c)?)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn iter_yields_elements_in_source_order() {
        assert_eq!(Tuple::two(1, 2).iter().collect::<Vec<_>>(), vec![&1, &2]);
        assert_eq!(
            Tuple::three(1, 2, 3).iter().collect::<Vec<_>>(),
            vec![&1, &2, &3]
        );
    }

    #[test]
    fn try_map_preserves_arity() {
        let two: Result<Tuple<i32>, ()> = Tuple::two(1, 2).try_map(|i| Ok(i * 2));
        assert_eq!(two, Ok(Tuple::two(2, 4)));

        let three: Result<Tuple<i32>, ()> = Tuple::three(1, 2, 3).try_map(|i| Ok(i * 2));
        assert_eq!(three, Ok(Tuple::three(2, 4, 6)));
    }

    #[test]
    fn try_map_stops_at_the_first_error() {
        let mut seen = vec![];
        let result: Result<Tuple<i32>, i32> = Tuple::three(1, 2, 3).try_map(|i| {
            seen.push(*i);
            if *i == 2 {
                Err(*i)
            } else {
                Ok(*i)
            }
        });

        assert_eq!(result, Err(2));
        assert_eq!(seen, vec![1, 2], "the third element is never visited");
    }
}
