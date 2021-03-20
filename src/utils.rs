use std::iter::FromIterator;

/// This let use collect an iterator of result into a result of vectors.
///
/// The generic signature, assuming `I: FromIterator` would look like `Iterator<Result<T, E>> -> Result<I<T>, I<E>>`.
/// I wish this was part of the standard library, but as it is not here is my custom version. And because
/// I don't particulary need to work with something else than `Vec` at the moment, the output type is fixed :)
pub fn collect_accumulate<T, E, I>(iterator: I) -> Result<Vec<T>, Vec<E>>
where
    I: Iterator<Item = Result<T, E>>,
{
    let mut items = vec![];
    let mut errors = vec![];

    for i in iterator {
        match i {
            Ok(t) => items.push(t),
            Err(e) => errors.push(e),
        };
    }

    if errors.is_empty() {
        Ok(items)
    } else {
        Err(errors)
    }
}

// TODO Make this function the default (replace collect_accumulate by this one)
// and possibly try to remove the intermediate Vec for results
pub fn collect_acc<T, E, I, R>(iterator: I) -> Result<R, Vec<E>>
where
    I: Iterator<Item = Result<T, E>>,
    R: FromIterator<T>,
{
    collect_accumulate(iterator).map(|vec| vec.into_iter().collect())
}
