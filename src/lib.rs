
use std::{collections::BTreeMap};
use english_dictionary_data::all_words;
use lazy_static::lazy_static;

lazy_static! {
    static ref WL: WordlistByCount = WordlistByCount::init();
}

// Letter Count. Handy for working with anagrams.
// Letters are normalized to lower case.
#[derive(Debug,PartialEq, PartialOrd, Eq, Ord)]
struct LC {
    counts: BTreeMap<char, usize>,
}

impl LC {
    fn from_word(chars: &str) -> Self {
        let mut counts = BTreeMap::new();
        let chars = chars.to_lowercase();
        for c in chars.chars() {
            *counts.entry(c).or_insert(0) += 1;
        }

        LC {
            counts
        }
    }
}

pub struct WordlistByCount {
    by_count: BTreeMap<LC, Vec<&'static str>>,
}

impl WordlistByCount {
    /// Generates a WorldlistByCount from the english dictionary dataset.
    pub fn init() -> Self {
        let mut by_count = BTreeMap::new();
        for word in all_words() {
            let lc = LC::from_word(word);
            by_count.entry(lc).or_insert(vec![]).push(word);
        }
        WordlistByCount { by_count }
    }
}

pub fn single_word_anagram(word: &str) -> Vec<&'static str> {
    let lc = LC::from_word(word.to_lowercase().as_str());
    WL.by_count.get(&lc).cloned().unwrap_or(vec![])
}

#[cfg(test)]
mod tests {
    use lazy_static::lazy_static;
    use super::*;

    lazy_static! {
        static ref WL: WordlistByCount = WordlistByCount::init();
    }

    #[test]
    fn it_works() {
        assert_eq!(vec!["BRUSH", "SHRUB"], single_word_anagram("brush"));
    }
}
