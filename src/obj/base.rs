//! Provides the basic data structures for the CCG category.

use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::ops::{Index, IndexMut};

use im::HashMap as IMHashMap;
use tree_sitter;
use tree_sitter_CategorialGrammarIconic as ts_iconic;

/// Flavors of the grammar of the CCG category.
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum GrammarFlavor {
    Iconic,
}

/// The index of a term in the term arena.
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct TermIndex(pub usize);

/// Represents a value or a variable
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum OrV<T, X>
where
    T: Eq + Clone,
    X: Eq + Clone,
{
    /// A value.
    Val(T),
    /// A variable.
    Var(X),
}

impl<T, X> std::fmt::Display for OrV<T, X>
where
    T: Eq + Clone + std::fmt::Display,
    X: Eq + Clone + std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OrV::Val(v) => write!(f, "{}", v),
            OrV::Var(v) => write!(f, "${}", v),
        }
    }
}

impl<T, X> Hash for OrV<T, X>
where
    T: Eq + Clone + Hash,
    X: Eq + Clone + Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            OrV::Val(v) => v.hash(state),
            OrV::Var(v) => v.hash(state),
        }
    }
}

impl<T, X> Default for OrV<T, X>
where
    T: Eq + Clone + Default,
    X: Eq + Clone,
{
    fn default() -> Self {
        OrV::Val(T::default())
    }
}

impl<T, X> Copy for OrV<T, X>
where
    T: Eq + Clone + Copy,
    X: Eq + Clone + Copy,
{
}

/// Represents a CCG category term.
/// # Type Parameters
/// * `T` - The type of atomic term names.
/// * `K` - The type of feature keys.
/// * `V` - The type of feature values.
/// * `X` - The type of variables.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Term<T, K, V, X>
where
    T: Eq + Clone,
    K: Eq + Hash + Clone,
    V: Eq + Clone,
    X: Eq + Hash + Clone,
{
    /// A wrapper for a core term applying features to the whole term.
    Ident {
        core_id: TermIndex,
        features: IMHashMap<OrV<K, X>, OrV<V, X>>,
    },

    /// An atomic term.
    Atomic {
        name: OrV<T, X>,
        features: IMHashMap<OrV<K, X>, OrV<V, X>>,
    },

    /// A left adjunct term.
    Left {
        ant_id: TermIndex,
        conseq_id: TermIndex,
        features: IMHashMap<OrV<K, X>, OrV<V, X>>,
    },

    //. A right adjunct term.
    Right {
        ant_id: TermIndex,
        conseq_id: TermIndex,
        features: IMHashMap<OrV<K, X>, OrV<V, X>>,
    },

    /// A middle adjunct term, which can be used as both a left and a right adjunct.
    Middle {
        ant_id: TermIndex,
        conseq_id: TermIndex,
        features: IMHashMap<OrV<K, X>, OrV<V, X>>,
    },

    /// A custom functor term.
    Custom {
        name: T,
        /// Arguments of the functor. If an argument is None, it means that the argument is not applied yet.
        arguments: Vec<Option<TermIndex>>,
        features: IMHashMap<OrV<K, X>, OrV<V, X>>,
    },
}

impl<T, K, V, X> Term<T, K, V, X>
where
    T: Eq + Clone,
    K: Eq + Hash + Clone,
    V: Eq + Clone,
    X: Eq + Hash + Clone,
{
    /// Gets the features of the term.
    pub fn get_features(&self) -> &IMHashMap<OrV<K, X>, OrV<V, X>> {
        match self {
            Term::Ident { features, .. } => features,
            Term::Atomic { features, .. } => features,
            Term::Left { features, .. } => features,
            Term::Right { features, .. } => features,
            Term::Middle { features, .. } => features,
            Term::Custom { features, .. } => features,
        }
    }

    /// Gets the mutable reference to the features of the term.
    pub fn get_features_mut(&mut self) -> &mut IMHashMap<OrV<K, X>, OrV<V, X>> {
        match self {
            Term::Ident { features, .. } => features,
            Term::Atomic { features, .. } => features,
            Term::Left { features, .. } => features,
            Term::Right { features, .. } => features,
            Term::Middle { features, .. } => features,
            Term::Custom { features, .. } => features,
        }
    }
}

/// An arena for storing CCG category terms.
pub struct TermArena<T, K, V, X>
where
    T: Eq + Clone,
    K: Eq + Hash + Clone,
    V: Eq + Clone,
    X: Eq + Hash + Clone,
{
    pub storage: Vec<Term<T, K, V, X>>,
    pub(super) parsers: HashMap<GrammarFlavor, tree_sitter::Parser>,
}

impl<T, K, V, X> Index<TermIndex> for TermArena<T, K, V, X>
where
    T: Eq + Clone,
    K: Eq + Hash + Clone,
    V: Eq + Clone,
    X: Eq + Hash + Clone,
{
    type Output = Term<T, K, V, X>;

    fn index(&self, index: TermIndex) -> &Self::Output {
        &self.storage[index.0]
    }
}

impl<T, K, V, X> IndexMut<TermIndex> for TermArena<T, K, V, X>
where
    T: Eq + Clone,
    K: Eq + Hash + Clone,
    V: Eq + Clone,
    X: Eq + Hash + Clone,
{
    fn index_mut(&mut self, index: TermIndex) -> &mut Self::Output {
        &mut self.storage[index.0]
    }
}

impl<T, K, V, X> TermArena<T, K, V, X>
where
    T: Eq + Clone,
    K: Eq + Hash + Clone,
    V: Eq + Clone,
    X: Eq + Hash + Clone,
{
    /// Creates a new arena.
    /// # Arguments
    /// * `avail_flavor` - The flavors of the grammar to be used. Only the grammars specified here will be loaded.
    pub fn new(avail_flavor: &[GrammarFlavor]) -> Self {
        let mut parser_map: HashMap<GrammarFlavor, tree_sitter::Parser> = HashMap::new();
        for fl in avail_flavor {
            let mut parser = tree_sitter::Parser::new();
            parser
                .set_language(match fl {
                    GrammarFlavor::Iconic => ts_iconic::language(),
                })
                .unwrap();
            parser_map.insert(*fl, parser);
        }

        TermArena {
            storage: Vec::new(),
            parsers: parser_map,
        }
    }

    /// Registers a term into the arena.
    pub fn register_term<U>(&mut self, term: U) -> TermIndex
    where
        U: Into<Term<T, K, V, X>>,
    {
        let term_into = term.into();
        match self.storage.iter().position(|t| *t == term_into) {
            Some(index) => TermIndex(index),
            None => {
                self.storage.push(term_into);
                TermIndex(self.storage.len() - 1)
            }
        }
    }

    /// Finds a term and get its index if found in the arena.
    pub fn find_term<U>(&self, term: U) -> Option<TermIndex>
    where
        U: Into<Term<T, K, V, X>>,
    {
        let term_into = term.into();
        self.storage
            .iter()
            .position(|t| *t == term_into)
            .map(TermIndex)
    }
}

/// A reference to an Arena with an Index (abbr. AI).
#[derive(Clone, Copy)]
pub struct Ai<'a, T, K, V, X>(pub &'a TermArena<T, K, V, X>, pub TermIndex)
where
    T: Eq + Clone,
    K: Eq + Hash + Clone,
    V: Eq + Clone,
    X: Eq + Hash + Clone;

impl<'a, T, K, V, X> Ai<'a, T, K, V, X>
where
    T: Eq + Clone,
    K: Eq + Hash + Clone,
    V: Eq + Clone,
    X: Eq + Hash + Clone,
{
    pub fn resolve(&self) -> &Term<T, K, V, X> {
        &self.0[self.1]
    }

    pub fn new_index(&self, id: TermIndex) -> Self {
        Self(self.0, id)
    }
}

impl<'a, T, K, V, X> PartialEq for Ai<'a, T, K, V, X>
where
    T: Eq + Clone,
    K: Eq + Hash + Clone,
    V: Eq + Clone,
    X: Eq + Hash + Clone,
{
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0) && self.1 == other.1
    }
}

impl<'a, T, K, V, X> Eq for Ai<'a, T, K, V, X>
where
    T: Eq + Clone,
    K: Eq + Hash + Clone,
    V: Eq + Clone,
    X: Eq + Hash + Clone,
{
}

/// A Mutable reference to Arena with an Index (abbr. Mutai).
pub struct Mutai<'a, T, K, V, X>(pub &'a mut TermArena<T, K, V, X>, pub TermIndex)
where
    T: Eq + Clone,
    K: Eq + Hash + Clone,
    V: Eq + Clone,
    X: Eq + Hash + Clone;

impl<'a, T, K, V, X> Mutai<'a, T, K, V, X>
where
    T: Eq + Clone,
    K: Eq + Hash + Clone,
    V: Eq + Clone,
    X: Eq + Hash + Clone,
{
    pub fn register_term(&'a mut self, term: Term<T, K, V, X>) -> TermIndex {
        self.0.register_term(term)
    }

    pub fn new_index(&'a mut self, id: TermIndex) -> Self {
        Self(self.0, id)
    }
}

impl<'a, T, K, V, X> Mutai<'a, T, K, V, X>
where
    T: Eq + Clone,
    K: Eq + Hash + Clone,
    V: Eq + Clone,
    X: Eq + Hash + Clone,
{
    pub fn left_adjunct(&'a mut self, features: IMHashMap<OrV<K, X>, OrV<V, X>>) -> TermIndex {
        self.register_term(Term::Left {
            ant_id: self.1,
            conseq_id: self.1,
            features,
        })
    }

    pub fn right_adjunct(&'a mut self, features: IMHashMap<OrV<K, X>, OrV<V, X>>) -> TermIndex {
        self.register_term(Term::Right {
            ant_id: self.1,
            conseq_id: self.1,
            features,
        })
    }

    pub fn middle_adjunct(&'a mut self, features: IMHashMap<OrV<K, X>, OrV<V, X>>) -> TermIndex {
        self.register_term(Term::Middle {
            ant_id: self.1,
            conseq_id: self.1,
            features,
        })
    }

    pub fn featureless(&'a mut self) -> TermIndex {
        let term = Ai(&*self.0, self.1).resolve().clone();
        match term {
            Term::Ident { core_id, .. } => Mutai(self.0, core_id).featureless(),
            Term::Atomic { name, .. } => self.register_term(Term::Atomic {
                name: name,
                features: IMHashMap::new(),
            }),
            Term::Left {
                ant_id, conseq_id, ..
            } => {
                let ant_id_new = Mutai(self.0, ant_id).featureless();
                let conseq_id_new = Mutai(self.0, conseq_id).featureless();
                self.register_term(Term::Left {
                    ant_id: ant_id_new,
                    conseq_id: conseq_id_new,
                    features: IMHashMap::new(),
                })
            }
            Term::Right {
                ant_id, conseq_id, ..
            } => {
                let ant_id_new = Mutai(self.0, ant_id).featureless();
                let conseq_id_new = Mutai(self.0, conseq_id).featureless();
                self.register_term(Term::Right {
                    ant_id: ant_id_new,
                    conseq_id: conseq_id_new,
                    features: IMHashMap::new(),
                })
            }
            Term::Middle {
                ant_id, conseq_id, ..
            } => {
                let ant_id_new = Mutai(self.0, ant_id).featureless();
                let conseq_id_new = Mutai(self.0, conseq_id).featureless();
                self.register_term(Term::Middle {
                    ant_id: ant_id_new,
                    conseq_id: conseq_id_new,
                    features: IMHashMap::new(),
                })
            }
            Term::Custom {
                name, arguments, ..
            } => {
                let mut new_args = Vec::new();
                for arg in arguments {
                    if let Some(arg_id) = arg {
                        let arg_id_new = Mutai(self.0, arg_id).featureless();
                        new_args.push(Some(arg_id_new));
                    } else {
                        new_args.push(None);
                    }
                }
                self.register_term(Term::Custom {
                    name,
                    arguments: new_args,
                    features: IMHashMap::new(),
                })
            }
        }
    }
}

impl<'a, T, K, V, X> PartialEq for Mutai<'a, T, K, V, X>
where
    T: Eq + Clone,
    K: Eq + Hash + Clone,
    V: Eq + Clone,
    X: Eq + Hash + Clone,
{
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0) && self.1 == other.1
    }
}

impl<'a, T, K, V, X> Eq for Mutai<'a, T, K, V, X>
where
    T: Eq + Clone,
    K: Eq + Hash + Clone,
    V: Eq + Clone,
    X: Eq + Hash + Clone,
{
}
