use english_dictionary_data::all_words;
use lazy_static::{lazy_static};
use std::collections::{BTreeMap};
use vgraph::{VGraph, BFSIterator, bfs_all_paths};

lazy_static! {
    static ref WL: WordlistByCount = WordlistByCount::init();
}

// Letter Count. Handy for working with anagrams.
// Filters the characters to `is_alphabetic` characters.
// Letters are normalized to lower case.
#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Hash, Clone)]
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
    
    fn is_empty(&self) -> bool {
        self.counts.values().all(|count| *count == 0)
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

struct MultiCount {
    caps: BTreeMap<char, usize>,
    key_order: Vec<char>,
    cur: BTreeMap<char, usize>,
}

impl MultiCount {
    fn new(caps: BTreeMap<char, usize>) -> Self {
        let mut key_order: Vec<char> = caps.keys().cloned().collect();
        key_order.sort();
        MultiCount { caps, key_order, cur: BTreeMap::new() }
    }
}

impl Iterator for MultiCount {
    type Item = BTreeMap<char, usize>;

    fn next(&mut self) -> Option<Self::Item> {
        let last = self.key_order.last().unwrap();
        // If we are past the cap, return none
        if self.cur.get(last).unwrap_or(&0) > self.caps.get(last).unwrap() {
            return None;
        }
        let cur = self.cur.clone();
        *self.cur.entry(self.key_order[0]).or_insert(0) += 1;

        // carry over, same as incrementing binary
        for window in self.key_order.windows(2) {
            let key = window[0];
            let next_key = window[1];

            if self.cur.get(&key) > self.caps.get(&key) {
                *self.cur.entry(next_key).or_insert(0) += 1;
                *self.cur.entry(key).or_insert(0) = 0;
            } else {
                break;
            }
        }

        Some(cur)
    }
}

/// Describes a graph to search for multi word anagrams.
/// Each node is the number of letters remaining in the anagram.
/// Each transition is using a word with a given number of characters (abstractly, using all single words that have that letter count).
struct MultiWordSearch { }

impl VGraph for MultiWordSearch {
    type Node = LC;

    type Dist = usize;

    fn out_edges(&self, node: Self::Node) -> Vec<Self::Node> {
        let mut edges = Vec::new();
        // TODO remove the case where we circle back to the same node.
        for possible in MultiCount::new(node.counts.clone()) {
            let letter_counts = LC {
                counts: possible,
            };
            if letter_counts.is_empty() {
                continue;
            }

            if letter_counts == LC::from_word("two") {
                println!("here");
            }

            if !single_lc_anagram(&letter_counts).is_empty() {
                edges.push(node.remaining_letters(&letter_counts));
            }
        }

        edges
    }

    fn dist(&self, from: Self::Node, to: Self::Node) -> Self::Dist {
        1
    }
}

// TODO: make this into an iterator
fn multi2(chars: LC, words_to_use: usize) -> Vec<Vec<Vec<&'static str>>> {
    if words_to_use == 0 {
        panic!("Invalid parameter.");
    }

    let mut answers = Vec::new();
    for path in bfs_all_paths(MultiWordSearch {}, chars, |lc| lc.is_empty()) {
        let answer = path.as_slice().windows(2).map(|window| {
            let larger = &window[0];
            let smaller = &window[1];
            let diff = larger.remaining_letters(smaller);
            single_lc_anagram(&diff)
        })
        .collect::<Vec<_>>();
        answers.push(answer);
    }

    answers
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

    // #[test]
    // fn multi_word_anagram_test() {
    //     let expected: Vec<Vec<&str>> = vec![vec!["welcome", "to", "my", "kingdom"]];
    //     assert_eq!(expected, multi_word_anagram("two milkmen go comedy"));
    // }

    #[test]
    fn multi2_test() {
        let expected: Vec<Vec<Vec<&str>>> = vec![vec![vec!["welcome"], vec!["to"], vec!["my"], vec!["kingdom"]]];
        assert_eq!(expected, multi2(LC::from_word("two milkmen go comedy"), 1));
    }

    #[test]
    fn debug() {
        let chars = LC::from_word("two milkmen go comedy");
        let expected: Vec<Vec<LC>> = vec![vec![LC::from_word("abc")]];
        assert_eq!(expected, bfs_all_paths(MultiWordSearch {}, chars, |lc| true).take(10).collect::<Vec<Vec<LC>>>());
    }

    #[test]
    fn debugtwo() {
        assert_eq!(vec!["two"], single_lc_anagram(&LC::from_word("two")))
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

    #[test]
    fn multicount_range_test() {
        let cap = BTreeMap::from([('a', 2), ('b', 2)]);
        assert_eq!(vec![
            BTreeMap::new(),
            BTreeMap::from([('a', 1)]),
            BTreeMap::from([('a', 2)]),
            BTreeMap::from([('b', 1), ('a', 0)]),
            BTreeMap::from([('b', 1), ('a', 1)]),
            BTreeMap::from([('b', 1), ('a', 2)]),
            BTreeMap::from([('b', 2), ('a', 0)]),
            BTreeMap::from([('b', 2), ('a', 1)]),
            BTreeMap::from([('b', 2), ('a', 2)]),
        ],
        MultiCount::new(cap).collect::<Vec<_>>()
        );
    }
}
