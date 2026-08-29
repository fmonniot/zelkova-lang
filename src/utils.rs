use std::iter::FromIterator;

/// Levenshtein edit distance between two strings, counted in `char`s rather
/// than bytes so a suggestion built over multi-byte UTF-8 identifiers isn't
/// skewed by how many bytes each character happens to take.
fn levenshtein_distance(a: &str, b: &str) -> usize {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    let lb = b.len();

    // One row of the edit-distance matrix, updated in place: `row[j]` holds
    // the distance between the prefix of `a` seen so far and `b[..j]`.
    let mut row: Vec<usize> = (0..=lb).collect();
    for (i, &ca) in a.iter().enumerate() {
        let mut prev_diag = row[0];
        row[0] = i + 1;
        for (j, &cb) in b.iter().enumerate() {
            let temp = row[j + 1];
            let cost = if ca == cb { 0 } else { 1 };
            row[j + 1] = (row[j + 1] + 1).min(row[j] + 1).min(prev_diag + cost);
            prev_diag = temp;
        }
    }
    row[lb]
}

/// The candidate in `candidates` closest to `target`, if it is close enough
/// to be worth suggesting as a "did you mean …?".
///
/// A bad suggestion is worse than none — it sends the reader off to check
/// something irrelevant — so the threshold scales with `target`'s length
/// rather than being a flat constant: one typo'd character in a three-letter
/// name is proportionally as far as three typos in a ten-letter one, and both
/// should still suggest, while a same-length but otherwise unrelated word
/// should not. A third of the length, rounded up, with a floor of one so a
/// one- or two-character name can still get a same-distance suggestion, is
/// what `ERR-7` was tuned against (`widthDefault` → `withDefault`, distance
/// 1 over 12 characters).
///
/// Equally-distant candidates are broken by comparing the candidates
/// themselves, so the answer depends only on the *set* of candidates and not
/// on the order they arrive in. That matters because every caller in the
/// compiler feeds this `HashMap` key iteration, whose order is randomised per
/// instance: without the tie-break, `foldl`/`foldr` or two sibling
/// constructors would produce a different suggestion from one run to the next
/// for unchanged source.
pub fn suggest<'a, I>(target: &str, candidates: I) -> Option<&'a str>
where
    I: IntoIterator<Item = &'a str>,
{
    let threshold = ((target.chars().count() as f64) / 3.0).ceil().max(1.0) as usize;

    candidates
        .into_iter()
        .filter(|c| *c != target)
        .map(|c| (c, levenshtein_distance(target, c)))
        .filter(|(_, distance)| *distance <= threshold)
        .min_by(|(a, da), (b, db)| da.cmp(db).then_with(|| a.cmp(b)))
        .map(|(c, _)| c)
}

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn levenshtein_distance_identical_strings_is_zero() {
        assert_eq!(levenshtein_distance("withDefault", "withDefault"), 0);
    }

    #[test]
    fn levenshtein_distance_one_insertion() {
        // `widthDefault` has one extra `d` compared to `withDefault`.
        assert_eq!(levenshtein_distance("widthDefault", "withDefault"), 1);
    }

    #[test]
    fn levenshtein_distance_counts_chars_not_bytes() {
        // Both are single-character strings in `char`s, but multiple bytes in
        // UTF-8 — a byte-wise distance would overcount.
        assert_eq!(levenshtein_distance("é", "è"), 1);
    }

    #[test]
    fn suggest_finds_a_close_candidate() {
        let candidates = vec!["andThen", "map", "withDefault"];
        assert_eq!(
            suggest("widthDefault", candidates),
            Some("withDefault"),
            "a one-character typo should suggest the near-miss"
        );
    }

    #[test]
    fn suggest_finds_nothing_for_an_unrelated_name() {
        let candidates = vec!["andThen", "map", "withDefault"];
        assert_eq!(
            suggest("frobnicate", candidates),
            None,
            "a name resembling nothing in scope must not produce a suggestion"
        );
    }

    #[test]
    fn suggest_ignores_exact_matches() {
        // `suggest` is only ever called after a lookup by the exact name has
        // already failed, but it should not recommend the target back to
        // itself if a caller ever hands it one anyway.
        let candidates = vec!["map"];
        assert_eq!(suggest("map", candidates), None);
    }

    #[test]
    fn suggest_picks_the_closest_of_several_candidates() {
        // Distances from "withDefault" are 4, 2 and 1 respectively, and the
        // threshold for an 11-character target is 4 — so all three survive the
        // filter and only the distance comparison separates them. The nearest
        // is deliberately *last*: returning the first candidate under the
        // threshold, or dropping the comparison entirely, picks "withDef".
        let candidates = vec!["withDef", "withDefau", "withDefaults"];
        assert_eq!(suggest("withDefault", candidates), Some("withDefaults"));
    }

    #[test]
    fn suggest_breaks_ties_on_the_candidate_not_on_iteration_order() {
        // All three are distance 1 from "maq", so only the tie-break decides.
        // Callers hand `suggest` `HashMap` key iteration, whose order is
        // randomised per instance; the answer must depend on the set alone.
        assert_eq!(suggest("maq", vec!["mad", "map", "mac"]), Some("mac"));
        assert_eq!(suggest("maq", vec!["map", "mac", "mad"]), Some("mac"));
        assert_eq!(suggest("maq", vec!["mac", "mad", "map"]), Some("mac"));
    }
}
