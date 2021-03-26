use std::iter::FromIterator;

/// This let use collect an iterator of result into a result of vectors.
/// This let us partition an iteration of result into an iterator of result or a vector of errors if any.
///
/// The generic signature, assuming `I: FromIterator` would look like `Iterator<Result<T, E>> -> Result<I<T>, I<E>>`.
/// I wish this was part of the standard library, but as it is not here is my custom version.
pub fn collect_accumulate<T, E, I, R>(iterator: I) -> Result<R, Vec<E>>
where
    I: Iterator<Item = Result<T, E>>,
    R: FromIterator<T>,
{
    let mut errors = vec![];

    let r = iterator
        .filter_map(|i| match i {
            Ok(t) => Some(t),
            Err(e) => {
                errors.push(e);
                None
            }
        })
        .collect();

    if errors.is_empty() {
        Ok(r)
    } else {
        Err(errors)
    }
}
