use im::HashMap as IMHashmap;
use std::fmt::{Debug, Display};
use std::hash::Hash;

use super::base::*;

const TO_ESCAPE_CAT: &str = "/\\|<>[]()~,'";
const TO_ESCAPE_FEAT: &str = "[]()~,='";
const TO_ESCAPE_QUOTE: &str = "~'";

fn escape_string_cat(s: &str) -> String {
    let mut result = String::new();
    for c in s.chars() {
        if TO_ESCAPE_CAT.contains(c) {
            result.push('~');
        }
        result.push(c);
    }
    result
}
fn escape_string_feat(s: &str) -> String {
    let mut result = String::new();
    for c in s.chars() {
        if TO_ESCAPE_FEAT.contains(c) {
            result.push('~');
        }
        result.push(c);
    }
    result
}

fn quote_string(s: &str) -> String {
    let mut result = String::new();
    result.push('\'');
    for c in s.chars() {
        if TO_ESCAPE_QUOTE.contains(c) {
            result.push('~');
        }
        result.push(c);
    }
    result.push('\'');
    result
}

fn auto_quote_string_cat(s: &str) -> String {
    if s.chars().any(|c| TO_ESCAPE_CAT.contains(c)) {
        quote_string(s)
    } else {
        s.to_string()
    }
}

fn auto_quote_string_feat(s: &str) -> String {
    if s.chars().any(|c| TO_ESCAPE_FEAT.contains(c)) {
        quote_string(s)
    } else {
        s.to_string()
    }
}

fn print_features<K, V>(
    features: &IMHashmap<K, V>,
    fmt: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result
where
    K: Eq + Hash + Clone + Display,
    V: Eq + Clone + Display,
{
    if features.is_empty() {
        return Ok(());
    } else {
        write!(fmt, "[")?;
        let formatted_features: Vec<String> = features
            .iter()
            .map(|(k, v)| {
                format!(
                    "{}={}",
                    auto_quote_string_feat(&format!("{}", k)),
                    auto_quote_string_feat(&format!("{}", v)),
                )
            })
            .collect();
        write!(fmt, "{}", formatted_features.join(","))?;
        write!(fmt, "]")?;
        Ok(())
    }
}

/// Ways of how a literal should be quoted.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum QuoteStyle {
    /// Always quote the literal.
    Always,

    /// Quote the literal if it contains special characters.
    Auto,

    /// Never quote the literal and use escaping instead.
    Never,
}

impl<'a, T, K, V> Ai<'a, T, K, V>
where
    T: Eq + Clone + Display,
    K: Eq + Hash + Clone + Display,
    V: Eq + Clone + Display,
{
    /// Dump the term to a formatter.
    /// # Arguments
    /// * `f` - The formatter to write to.
    /// * `flavor` - The grammar flavor to use.
    /// * `quote_style` - The quote style to use.
    pub fn dump(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        flavor: GrammarFlavor,
        quote_style: QuoteStyle,
    ) -> std::fmt::Result {
        match self.resolve() {
            Term::Atomic { name, features } => {
                write!(
                    f,
                    "{}",
                    match quote_style {
                        QuoteStyle::Always => quote_string(&format!("{}", name)),
                        QuoteStyle::Auto => auto_quote_string_cat(&format!("{}", name)),
                        QuoteStyle::Never => escape_string_cat(&format!("{}", name)),
                    }
                )?;
                print_features(features, f)?;
            }
            Term::Ident { core_id, features } => {
                let ai = Ai(self.0, *core_id);
                write!(f, "<")?;
                ai.dump(f, flavor, quote_style)?;
                write!(f, ">")?;
                print_features(features, f)?;
            }
            Term::Left {
                ant_id,
                conseq_id,
                features,
            } => {
                let ant = Ai(self.0, *ant_id);
                let conseq = Ai(self.0, *conseq_id);
                match flavor {
                    GrammarFlavor::Iconic => {
                        write!(f, "<")?;
                        ant.dump(f, flavor, quote_style)?;
                        write!(f, "\\")?;
                        print_features(features, f)?;
                        conseq.dump(f, flavor, quote_style)?;
                        write!(f, ">")?;
                    }
                }
            }
            Term::Right {
                ant_id,
                conseq_id,
                features,
            } => {
                let ant = Ai(self.0, *ant_id);
                let conseq = Ai(self.0, *conseq_id);
                write!(f, "<")?;
                conseq.dump(f, flavor, quote_style)?;
                write!(f, "/")?;
                print_features(features, f)?;
                ant.dump(f, flavor, quote_style)?;
                write!(f, ">")?;
            }
            Term::Middle {
                ant_id,
                conseq_id,
                features,
            } => {
                let ant = Ai(self.0, *ant_id);
                let conseq = Ai(self.0, *conseq_id);
                write!(f, "<")?;
                conseq.dump(f, flavor, quote_style)?;
                write!(f, "|")?;
                print_features(features, f)?;
                ant.dump(f, flavor, quote_style)?;
                write!(f, ">")?;
            }
            Term::Custom {
                name,
                arguments,
                features,
            } => {
                todo!();
                // write!(
                //     f,
                //     "{}",
                //     match quote_style {
                //         QuoteStyle::Always => quote_string(&format!("{}", name)),
                //         QuoteStyle::Auto => auto_quote_string_cat(&format!("{}", name)),
                //         QuoteStyle::Never => escape_string_cat(&format!("{}", name)),
                //     }
                // )?;
                // print_features(features, f)?;
                // let formatted_args: Vec<String> = arguments
                //     .iter()
                //     .map(|arg| match arg {
                //         Some(a) => format!("{}", Ai(self.0, *a)),
                //         // TODO: use dump recursively
                //         // TODO: join over maps
                //         None => "".to_string(),
                //     })
                //     .collect();
                // write!(f, "({})", formatted_args.join(","))?;
            }
        }
        Ok(())
    }
}

impl<'a, T, K, V> Display for Ai<'a, T, K, V>
where
    T: Eq + Clone + Display,
    K: Eq + Hash + Clone + Display,
    V: Eq + Clone + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.dump(f, GrammarFlavor::Iconic, QuoteStyle::Auto)?;
        Ok(())
    }
}

impl<'a, T, K, V> Debug for Ai<'a, T, K, V>
where
    T: Eq + Clone + Display,
    K: Eq + Hash + Clone + Display,
    V: Eq + Clone + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TermArena#{:p},", self.0)?;
        self.dump(f, GrammarFlavor::Iconic, QuoteStyle::Always)?;
        Ok(())
    }
}
