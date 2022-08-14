use english_dictionary_data::all_words;
use lazy_static::lazy_static;
use std::collections::{BTreeMap};

lazy_static! {
    static ref WL: WordlistByCount = WordlistByCount::init();
}

// Letter Count. Handy for working with anagrams.
// Filters the characters to `is_alphabetic` characters.
// Letters are normalized to lower case.
#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone)]
struct LC {
    counts: BTreeMap<char, usize>,
}

impl LC {
    fn from_word(chars: &str) -> Self {
        let mut counts = BTreeMap::new();
        let chars = chars.to_lowercase();
        for c in chars.chars().filter(|c| c.is_alphabetic()) {
            *counts.entry(c).or_insert(0) += 1;
        }

        LC { counts }
    }

    fn empty() -> Self {
        LC { counts: BTreeMap::new() }
    }

    fn remaining_letters(&self, right: &LC) -> Self {
        let mut res = self.clone();
        for (letter, right_count) in right.counts.iter() {
            if let Some(left_count) = res.counts.get_mut(letter) {
                *left_count -= right_count;
            }
        }
        res
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
    single_lc_anagram(&lc)
}

fn single_lc_anagram(letter_counts: &LC) -> Vec<&'static str> {
    WL.by_count.get(letter_counts).cloned().unwrap_or(vec![])
}

pub fn multi_word_anagram(phrase: &str) -> Vec<Vec<&'static str>> {
    multi_anagram(&LC::from_word(phrase))

}

fn multi_anagram(chars: &LC) -> Vec<Vec<&'static str>> {
    // This should get passed in.
    let mut all_paths = Vec::new();

    // TODO: this has a lot of repeat computation
    for characters_used in all_mcs(chars.clone()) {
        let remaining_letters = chars.remaining_letters(&characters_used);
        for sub_list in multi_anagram(&remaining_letters).into_iter() {
            for word in single_lc_anagram(&characters_used) {
                let mut sub_list_copy = sub_list.clone();
                sub_list_copy.push(word);
                all_paths.push(sub_list_copy);
            }
        }
    }

    all_paths
}

fn all_mcs(mut chars: LC) -> Vec<LC> {
    if let Some(first_key) = chars.counts.keys().next().cloned() {
        let mut all = Vec::new();
        let (char, count) = chars.counts.remove_entry(&first_key).unwrap();
        let sub_opts = all_mcs(chars);
        assert!(sub_opts.len() > 0);
        for sub in sub_opts {
            for to_use in (0..=count).rev() {
                let mut sub_copy = sub.clone();
                // TODO remove
                assert!(!sub_copy.counts.contains_key(&char));
                sub_copy.counts.insert(char, to_use);
                all.push(sub_copy);
            }
        }
        return all;
    }
    
    return vec![LC::empty()];
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_word_anagram_test() {
        assert_eq!(vec!["BRUSH", "SHRUB"], single_word_anagram("brush"));
    }

    #[test]
    fn multi_word_anagram_() {
        let expected: Vec<Vec<&str>> = vec![vec!["welcome", "to", "my", "kingdom"]];
        assert_eq!(expected, multi_word_anagram("two milkmen go comedy"));
    }

    impl LC {
        fn singleton(c: char, count: usize) -> Self {
            LC {
                counts: BTreeMap::from([(c, count)]),
            }
        }
    }

    #[test]
    fn all_mcs_works() {
        let counts = BTreeMap::from([('c', 2)]);
        let lc = LC { counts };
        assert_eq!(vec![LC::singleton('c', 2), LC::singleton('c', 1), LC::singleton('c', 0)], all_mcs(lc));
    }
}
