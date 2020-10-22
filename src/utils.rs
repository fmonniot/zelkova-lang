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
