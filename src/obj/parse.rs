// # Parsing

use super::base::*;
use std::hash::Hash;

use im::HashMap as IMHashMap;
use tree_sitter;

/// Errors that can occur during converting an AST.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConvertError {
    UnsupportedNode(String),
    ConvertUniqCatName,
    ConvertFeatureKey,
    ConvertFeatureValue,
    LackOfChildren(String),
}

fn convert_feature<'src, K, V, X>(
    node: &tree_sitter::Node,
    src: &'src [u8],
    try_into_key: &mut dyn FnMut(&'src [u8]) -> Result<OrV<K, X>, ConvertError>,
    try_into_value: &mut dyn FnMut(&'src [u8]) -> Result<OrV<V, X>, ConvertError>,
    hm: &mut IMHashMap<OrV<K, X>, OrV<V, X>>,
) -> Result<(), ConvertError>
where
    K: Eq + Hash + Clone,
    V: Eq + Clone + Default,
    X: Eq + Hash + Clone,
{
    let feature_key = node
        .child_by_field_name("key")
        .ok_or(ConvertError::LackOfChildren("key: literal".to_string()))?;
    let key = try_into_key(&src[feature_key.byte_range()])
        .map_err(|_| ConvertError::ConvertFeatureKey)?;

    let feature_value = node.child_by_field_name("value");
    let value = match feature_value {
        Some(feature_value) => try_into_value(&src[feature_value.byte_range()])
            .map_err(|_| ConvertError::ConvertFeatureValue)?,
        None => Default::default(),
    };
    hm.insert(key, value);
    Ok(())
}

fn convert_feature_list<'src, K, V, X>(
    node: &tree_sitter::Node,
    src: &'src [u8],
    try_into_key: &mut dyn FnMut(&'src [u8]) -> Result<OrV<K, X>, ConvertError>,
    try_into_value: &mut dyn FnMut(&'src [u8]) -> Result<OrV<V, X>, ConvertError>,
    hm: &mut IMHashMap<OrV<K, X>, OrV<V, X>>,
) -> Result<(), ConvertError>
where
    K: Eq + Hash + Clone,
    V: Eq + Clone + Default,
    X: Eq + Hash + Clone,
{
    let mut cursor = node.walk();
    for feature in node.named_children(&mut cursor) {
        convert_feature(&feature, src, try_into_key, try_into_value, hm)?;
    }
    Ok(())
}

fn convert_feature_list_list<'src, 'iter, K, V, X, I>(
    nodes: I,
    src: &'src [u8],
    try_into_key: &mut dyn FnMut(&'src [u8]) -> Result<OrV<K, X>, ConvertError>,
    try_into_value: &mut dyn FnMut(&'src [u8]) -> Result<OrV<V, X>, ConvertError>,
) -> Result<IMHashMap<OrV<K, X>, OrV<V, X>>, ConvertError>
where
    K: Eq + Hash + Clone,
    V: Eq + Clone + Default,
    X: Eq + Hash + Clone,
    I: IntoIterator<Item = tree_sitter::Node<'iter>>,
{
    let mut feature_map = IMHashMap::new();
    for feature_list in nodes {
        convert_feature_list(
            &feature_list,
            src,
            try_into_key,
            try_into_value,
            &mut feature_map,
        )?;
    }
    Ok(feature_map)
}

impl<'src, T, K, V, X> TermArena<T, K, V, X>
where
    T: Eq + Clone,
    K: Eq + Hash + Clone,
    V: Eq + Clone + Default,
    X: Eq + Hash + Clone,
{
    /// Parse a term from a byte slice and register it in the arena.
    ///
    /// # Arguments
    /// * `flavor` - The flavor of the grammar to be used.
    /// * `src` - The byte slice to be parsed.
    /// * `try_into_term` - The function to convert a byte slice to a atomic term name object of type `T`. Decoding of the byte slice is to be implemented by users here.
    /// * `try_into_key` - The function to convert a byte slice to a feature key object of type `K`. Decoding of the byte slice is to be implemented by users here.
    /// * `try_into_value` - The function to convert a byte slice to a feature value object of type `V`. Decoding of the byte slice is to be implemented by users here.
    ///
    /// # Notes
    /// This is a helper function which calls `convert_term` subsequently.
    pub fn parse_term(
        &mut self,
        flavor: GrammarFlavor,
        src: &'src [u8],
        try_into_term: &mut dyn FnMut(&'src [u8]) -> Result<OrV<T, X>, ConvertError>,
        try_into_key: &mut dyn FnMut(&'src [u8]) -> Result<OrV<K, X>, ConvertError>,
        try_into_value: &mut dyn FnMut(&'src [u8]) -> Result<OrV<V, X>, ConvertError>,
    ) -> Result<TermIndex, ConvertError> {
        let tree = self
            .parsers
            .get_mut(&flavor)
            .unwrap()
            .parse(src, None)
            .unwrap();
        let root = tree.root_node();
        self.convert_term(&root, src, try_into_term, try_into_key, try_into_value)
    }

    /// Convert a term tree-sitter AST to a term object and register it in the arena.
    ///
    /// # Arguments
    /// * `node` - The node of the tree-sitter AST.
    /// * `src` - The byte slice of the source code.
    /// * `try_into_term` - The function to convert a byte slice to a atomic term name object of type `T`. Decoding of the byte slice is to be implemented by users here.
    /// * `try_into_key` - The function to convert a byte slice to a feature key object of type `K`. Decoding of the byte slice is to be implemented by users here.
    /// * `try_into_value` - The function to convert a byte slice to a feature value object of type `V`. Decoding of the byte slice is to be implemented by users here.
    pub fn convert_term(
        &mut self,
        node: &tree_sitter::Node,
        src: &'src [u8],
        try_into_term: &mut dyn FnMut(&'src [u8]) -> Result<OrV<T, X>, ConvertError>,
        try_into_key: &mut dyn FnMut(&'src [u8]) -> Result<OrV<K, X>, ConvertError>,
        try_into_value: &mut dyn FnMut(&'src [u8]) -> Result<OrV<V, X>, ConvertError>,
        // NOTE: https://users.rust-lang.org/t/my-recursive-code-of-cps-style-cannot-be-compiled/83001/7
    ) -> Result<TermIndex, ConvertError> {
        match node.kind() {
            "source_file" => {
                let mut cursor = node.walk();
                let mut children = node.named_children(&mut cursor);
                let category = children
                    .next()
                    .ok_or(ConvertError::LackOfChildren("category".to_string()))?;
                let category_id =
                    self.convert_term(&category, src, try_into_term, try_into_key, try_into_value)?;

                let features =
                    convert_feature_list_list(children, src, try_into_key, try_into_value)?;

                if self[category_id].get_features().is_empty() {
                    Ok(category_id)
                } else if let Term::Atomic {
                    name,
                    features: features_atom,
                } = &self[category_id]
                {
                    Ok(self.register_term(Term::Atomic {
                        name: name.clone(),
                        features: IMHashMap::unions([features_atom.clone(), features]),
                    }))
                } else {
                    Ok(self.register_term(Term::Ident {
                        core_id: category_id,
                        features: features,
                    }))
                }
            }
            "cat_uniq" => {
                let mut cursor = node.walk();
                let mut children = node.named_children(&mut cursor);
                let name = children
                    .next()
                    .ok_or(ConvertError::LackOfChildren("cat_name".to_string()))?;
                let name_converted = try_into_term(&src[name.byte_range()])?;
                let features =
                    convert_feature_list_list(children, src, try_into_key, try_into_value)?;
                Ok(self.register_term(Term::Atomic {
                    name: name_converted,
                    features: features,
                }))
            }
            "cat_left" => {
                let cursor = &mut node.walk();

                let antecedent =
                    node.child_by_field_name("antecedent")
                        .ok_or(ConvertError::LackOfChildren(
                            "antecedent: source_file".to_string(),
                        ))?;
                let ant_id = self.convert_term(
                    &antecedent,
                    src,
                    try_into_term,
                    try_into_key,
                    try_into_value,
                )?;
                let consequent =
                    node.child_by_field_name("consequent")
                        .ok_or(ConvertError::LackOfChildren(
                            "consequent: source_file".to_string(),
                        ))?;
                let conseq_id = self.convert_term(
                    &consequent,
                    src,
                    try_into_term,
                    try_into_key,
                    try_into_value,
                )?;

                let features = convert_feature_list_list(
                    node.children_by_field_name("feature_list", cursor),
                    src,
                    try_into_key,
                    try_into_value,
                )?;
                Ok(self.register_term(Term::Left {
                    ant_id: ant_id,
                    conseq_id: conseq_id,
                    features: features,
                }))
            }
            "cat_right" => {
                let cursor = &mut node.walk();
                let antecedent =
                    node.child_by_field_name("antecedent")
                        .ok_or(ConvertError::LackOfChildren(
                            "antecedent: source_file".to_string(),
                        ))?;
                let ant_id = self.convert_term(
                    &antecedent,
                    src,
                    try_into_term,
                    try_into_key,
                    try_into_value,
                )?;
                let consequent =
                    node.child_by_field_name("consequent")
                        .ok_or(ConvertError::LackOfChildren(
                            "consequent: source_file".to_string(),
                        ))?;
                let conseq_id = self.convert_term(
                    &consequent,
                    src,
                    try_into_term,
                    try_into_key,
                    try_into_value,
                )?;

                let features = convert_feature_list_list(
                    node.children_by_field_name("feature_list", cursor),
                    src,
                    try_into_key,
                    try_into_value,
                )?;
                Ok(self.register_term(Term::Right {
                    ant_id: ant_id,
                    conseq_id: conseq_id,
                    features: features,
                }))
            }
            "cat_middle" => {
                let cursor = &mut node.walk();
                let antecedent =
                    node.child_by_field_name("antecedent")
                        .ok_or(ConvertError::LackOfChildren(
                            "antecedent: source_file".to_string(),
                        ))?;
                let ant_id = self.convert_term(
                    &antecedent,
                    src,
                    try_into_term,
                    try_into_key,
                    try_into_value,
                )?;
                let consequent =
                    node.child_by_field_name("consequent")
                        .ok_or(ConvertError::LackOfChildren(
                            "consequent: source_file".to_string(),
                        ))?;
                let conseq_id = self.convert_term(
                    &consequent,
                    src,
                    try_into_term,
                    try_into_key,
                    try_into_value,
                )?;

                let features = convert_feature_list_list(
                    node.children_by_field_name("feature_list", cursor),
                    src,
                    try_into_key,
                    try_into_value,
                )?;
                Ok(self.register_term(Term::Middle {
                    ant_id: ant_id,
                    conseq_id: conseq_id,
                    features: features,
                }))
            }
            other => {
                return Err(ConvertError::UnsupportedNode(other.to_string()));
            }
        }
    }
}

pub mod converter_utf8 {
    use super::super::base::*;
    fn literal_or_var(s: &str) -> OrV<String, String> {
        if s.starts_with("$") {
            return OrV::Var(s[1..].to_string());
        } else {
            return OrV::Val(s.to_string());
        }
    }

    pub fn try_into_term<'src>(s: &'src [u8]) -> Result<OrV<String, String>, super::ConvertError> {
        std::str::from_utf8(s)
            .map_err(|_| super::ConvertError::ConvertUniqCatName)
            .map(literal_or_var)
    }
    pub fn try_into_key<'src>(s: &'src [u8]) -> Result<OrV<String, String>, super::ConvertError> {
        std::str::from_utf8(s)
            .map_err(|_| super::ConvertError::ConvertFeatureKey)
            .map(literal_or_var)
    }
    pub fn try_into_value<'src>(s: &'src [u8]) -> Result<OrV<String, String>, super::ConvertError> {
        std::str::from_utf8(s)
            .map_err(|_| super::ConvertError::ConvertFeatureValue)
            .map(literal_or_var)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("S\\S//S", GrammarFlavor::Iconic)]
    #[case("<Ant\\Conseq>//BB[ftest]", GrammarFlavor::Iconic)]
    #[case("<S\\S>//S", GrammarFlavor::Iconic)]
    #[case("<<S\\S>//S>[feat=val][feat2=val2,feat3]", GrammarFlavor::Iconic)]
    #[case("<<S\\[feat=val]S>//S>[feat2=val2,feat3]", GrammarFlavor::Iconic)]
    #[case(
        "<<S\\[feat=val]S[feat=val][feat=val]>//S>[feat2=val2,featl3]",
        GrammarFlavor::Iconic
    )]
    fn test_parse(#[case] input: &str, #[case] flavor: GrammarFlavor) -> Result<(), ConvertError> {
        let mut arena: TermArena<String, String, String, String> =
            TermArena::new(vec![GrammarFlavor::Iconic].as_slice());
        let parsed = arena.parse_term(
            flavor,
            input.as_bytes(),
            &mut converter_utf8::try_into_term,
            &mut converter_utf8::try_into_key,
            &mut converter_utf8::try_into_value,
        );
        parsed.map(|cat_id| {
            println!("{:?}", arena[cat_id]);
            ()
        })
    }
}
