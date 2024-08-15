use super::base::*;
use im::HashMap as IMHashMap;
use std::collections::VecDeque;
use std::hash::Hash;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum BRuleDir {
    Left,
    Right,
    Middle,
}

#[derive(PartialEq, Eq, Clone, Debug)]
struct BRule {
    dir: BRuleDir,
    depth: i64,
    functor_id_stack: Vec<TermIndex>,
}

enum RuleAppResult<'a, T, K, V, X, R>
where
    T: Eq + Clone,
    K: Eq + Hash + Clone,
    V: Eq + Clone,
    X: Eq + Hash + Clone,
    R: Clone,
{
    Success {
        rule: R,
        res_id: TermIndex,
    },
    Next {
        action: Box<dyn FnMut(&mut TermArena<T, K, V, X>) -> RuleAppResult<'a, T, K, V, X, R> + 'a>,
    },
    Fail,
}

impl<'a, T, K, V, X, R> Clone for RuleAppResult<'a, T, K, V, X, R>
where
    T: Eq + Clone,
    K: Eq + Hash + Clone,
    V: Eq + Clone,
    X: Eq + Hash + Clone,
    R: Clone,
{
    fn clone(&self) -> Self {
        match self {
            RuleAppResult::Success { rule, res_id } => RuleAppResult::Success {
                rule: rule.clone(),
                res_id: res_id.clone(),
            },
            RuleAppResult::Next { action } => panic!(),
            RuleAppResult::Fail => RuleAppResult::Fail,
        }
    }
}

impl<T, K, V, X> TermArena<T, K, V, X>
where
    T: Eq + Clone,
    K: Eq + Hash + Clone,
    V: Eq + Clone,
    X: Eq + Hash + Clone,
{
    fn reduce_b<'a>(
        &mut self,
        term_left_id: TermIndex,
        term_right_id: TermIndex,
        mut spec: BRule,
    ) -> RuleAppResult<'a, T, K, V, X, BRule> {
        let term_left = &self[term_left_id];
        let term_right = &self[term_right_id];
        match spec.dir {
            BRuleDir::Left => {
                if let Term::Left {
                    ant_id: term_right_ant_id,
                    conseq_id: term_right_conseq_id,
                    ..
                } = *term_right
                {
                    // A <A\\B> => B
                    let term_right_ant = &self[term_right_ant_id]; // A
                    if *term_right_ant == *term_left {
                        // TODO: check A features

                        let mut res_cat_id = term_right_conseq_id;
                        while let Some(functor_id) = spec.functor_id_stack.pop() {
                            res_cat_id = self.register_term(Term::Left {
                                ant_id: functor_id,
                                conseq_id: res_cat_id,
                                features: IMHashMap::new(),
                            });
                        }

                        return RuleAppResult::Success {
                            rule: BRule {
                                dir: BRuleDir::Left,
                                depth: spec.depth,
                                functor_id_stack: spec.functor_id_stack,
                            },
                            res_id: res_cat_id,
                        };
                    } else {
                        if let Term::Left {
                            ant_id: term_left_ant_id,       // C
                            conseq_id: term_left_conseq_id, // A'
                            ..
                        } = *term_left
                        // <C\\A'> <A\\B> => <C\\B'>
                        // TODO: check \\ features
                        {
                            return RuleAppResult::Next {
                                action: Box::new(move |arena| {
                                    let mut functor_id_stack = spec.functor_id_stack.clone();
                                    functor_id_stack.push(term_left_ant_id);
                                    arena.reduce_b(
                                        term_left_conseq_id, // A' of <C\\A'>
                                        term_right_id,       // <A\\B>
                                        BRule {
                                            dir: BRuleDir::Left,
                                            depth: spec.depth + 1,
                                            functor_id_stack: functor_id_stack,
                                        },
                                    )
                                }),
                            };
                        };
                    };
                }
            }
            BRuleDir::Right => {
                if let Term::Right {
                    ant_id: term_left_ant_id,
                    conseq_id: term_left_conseq_id,
                    ..
                } = *term_left
                {
                    // <B/A> A => B
                    let term_left_ant = &self[term_left_ant_id]; // A
                    if *term_left_ant == *term_right {
                        // TODO: check A features

                        let mut res_cat_id = term_left_conseq_id;
                        while let Some(functor_id) = spec.functor_id_stack.pop() {
                            res_cat_id = self.register_term(Term::Right {
                                ant_id: functor_id,
                                conseq_id: res_cat_id,
                                features: IMHashMap::new(),
                            });
                        }

                        return RuleAppResult::Success {
                            rule: BRule {
                                dir: BRuleDir::Right,
                                depth: spec.depth,
                                functor_id_stack: spec.functor_id_stack,
                            },
                            res_id: res_cat_id,
                        };
                    } else {
                        if let Term::Right {
                            ant_id: term_right_ant_id,       // C
                            conseq_id: term_right_conseq_id, // A'
                            ..
                        } = *term_right
                        // <B/A> <A'/C> => <B/C>
                        // TODO: check / features
                        {
                            return RuleAppResult::Next {
                                action: Box::new(move |arena: &mut TermArena<T, K, V, X>| {
                                    let mut functor_id_stack = spec.functor_id_stack.clone();
                                    functor_id_stack.push(term_right_ant_id);
                                    arena.reduce_b(
                                        term_left_id,         // <B/A>
                                        term_right_conseq_id, // A' of <A'/C>
                                        BRule {
                                            dir: BRuleDir::Right,
                                            depth: spec.depth + 1,
                                            functor_id_stack: functor_id_stack,
                                        },
                                    )
                                }),
                            };
                        }
                    }
                }
            }
            BRuleDir::Middle => {
                todo!()
            }
        }
        RuleAppResult::Fail
    }

    fn gen_rule_list_b<'a>(
        &mut self,
        term_left_id: TermIndex,
        term_right_id: TermIndex,
    ) -> VecDeque<
        Box<dyn FnMut(&mut TermArena<T, K, V, X>) -> RuleAppResult<'a, T, K, V, X, BRule> + 'a>,
    > {
        let mut queue: VecDeque<
            Box<dyn FnMut(&mut TermArena<T, K, V, X>) -> RuleAppResult<'a, T, K, V, X, BRule> + 'a>,
        > = VecDeque::new();

        queue.push_back(Box::new(move |arena| {
            arena.reduce_b(
                term_left_id,
                term_right_id,
                BRule {
                    dir: BRuleDir::Left,
                    depth: 0,
                    functor_id_stack: vec![],
                },
            )
        }));
        queue.push_back(Box::new(move |arena| {
            arena.reduce_b(
                term_left_id,
                term_right_id,
                BRule {
                    dir: BRuleDir::Right,
                    depth: 0,
                    functor_id_stack: vec![],
                },
            )
        }));
        // queue.push_back(Box::new(|arena| {
        //     arena.reduce_b(
        //         &term_left.clone(),
        //         &term_right.clone(),
        //         BRule::Left { depth: 0 },
        //     )
        // }));

        queue
    }

    fn reduce<'a, R>(
        &mut self,
        action_list: &mut VecDeque<
            Box<dyn FnMut(&mut TermArena<T, K, V, X>) -> RuleAppResult<'a, T, K, V, X, R> + 'a>,
        >,
    ) -> RuleAppResult<'a, T, K, V, X, R>
    where
        R: Clone,
    {
        loop {
            if let Some(mut action) = action_list.pop_front() {
                let result = action(self);
                match result {
                    RuleAppResult::Success { res_id, rule } => {
                        return RuleAppResult::Success { rule, res_id };
                    }
                    RuleAppResult::Next { action } => {
                        action_list.push_back(action);
                    }
                    RuleAppResult::Fail => {
                        continue;
                    }
                }
            } else {
                return RuleAppResult::Fail;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::parse::ConvertError;
    use super::*;
    use rstest::*;

    #[rstest]
    #[case("A", "A\\B", GrammarFlavor::Iconic, "B", BRule { dir: BRuleDir::Left, depth: 0, functor_id_stack: vec![] })]
    #[case("A\\B", "B\\C", GrammarFlavor::Iconic, "A\\C", BRule { dir: BRuleDir::Left, depth: 1, functor_id_stack: vec![] })]
    #[case("A/B", "B", GrammarFlavor::Iconic, "A", BRule {dir : BRuleDir::Right, depth: 0, functor_id_stack: vec![]})]
    #[case("A/B", "<B/C>/D", GrammarFlavor::Iconic, "<A/C>/D", BRule {dir : BRuleDir::Right, depth: 2, functor_id_stack: vec![]})]
    fn test_reduce_b(
        #[case] left_str: &str,
        #[case] right_str: &str,
        #[case] flavor: GrammarFlavor,
        #[case] result_str: &str,
        #[case] rule: BRule,
    ) {
        let mut arena = TermArena::new(&[flavor]);
        let convert_term =
            &mut |s| std::str::from_utf8(s).map_err(|_| ConvertError::ConvertUniqCatName);
        let convert_key =
            &mut |s| std::str::from_utf8(s).map_err(|_| ConvertError::ConvertFeatureKey);
        let convert_value =
            &mut |s| std::str::from_utf8(s).map_err(|_| ConvertError::ConvertFeatureValue);
        let left_id = arena
            .parse_term(
                flavor,
                left_str.as_bytes(),
                convert_term,
                convert_key,
                convert_value,
            )
            .unwrap();
        let right_id = arena
            .parse_term(
                flavor,
                right_str.as_bytes(),
                convert_term,
                convert_key,
                convert_value,
            )
            .unwrap();
        let result_expected = arena
            .parse_term(
                flavor,
                result_str.as_bytes(),
                convert_term,
                convert_key,
                convert_value,
            )
            .unwrap();
        // Perform mutable operations and store results
        let (result, res_id) = {
            let mut action_list = arena.gen_rule_list_b(left_id, right_id);
            let result = arena.reduce(&mut action_list);
            match result {
                RuleAppResult::Success { rule: r, res_id } => (r, res_id),
                _ => panic!("Failed to reduce"),
            }
        };

        // Perform immutable operations after mutable borrow is done
        assert_eq!(result, rule);
        assert_eq!(Ai(&arena, res_id), Ai(&arena, result_expected));
    }
}
