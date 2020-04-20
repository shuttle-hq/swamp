use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;

use crate::prelude::*;

pub trait Resource: Sized {
    type Type;
    type Error;
    fn get_type(&self) -> Self::Type;
    fn poll(
        &mut self,
        scope: &Scope<Self, Self::Type, Self::Error>,
    ) -> PollResult<Self::Type, Self::Error>;
}

pub struct Scope<R, T, E> {
    resources: HashMap<T, Result<R, E>>,
}

impl<R, T, E> Scope<R, T, E>
where
    T: Display + Hash + Eq + Clone,
    E: Error,
{
    fn get_within<D: DefaultExt, C: FnOnce(&R) -> D>(
        &self,
        t: &T,
        c: C,
    ) -> Result<ValueOrTug<D, T>, E> {
        let state = self
            .resources
            .get(t)
            .ok_or(E::missing(t))?
            .as_ref()
            .map_err(|e| E::dependent(t, e))?;
        let out = c(state);
        if out.is_default() {
            Ok(ValueOrTug::Tug(t.clone()))
        } else {
            Ok(ValueOrTug::Value(out))
        }
    }
}

impl<R, T, E> Scope<R, T, E>
where
    R: Resource<Type = T, Error = E>,
    T: Display + Hash + Eq + Clone,
    E: Error,
{
    fn evaluate_with<I: IntoIterator<Item = T>>(&mut self, it: I) {
        let mut queue: Vec<T> = it.into_iter().collect();
        let mut tug_count = HashMap::<T, usize>::new();
        while let Some(t) = queue.pop() {
            if let Some(r_state) = self.resources.remove(&t) {
                let new_r_state = r_state.and_then(|mut r| {
                    r.poll(&self).and_then(|res| {
                        match res {
                            PollState::Done => {}
                            PollState::Tug(tug_t) => {
                                tug_count
                                    .entry(tug_t.clone())
                                    .and_modify(|c| {
                                        c.checked_add(1).unwrap();
                                    })
                                    .or_insert(1);
                                if tug_count.get(&tug_t).unwrap() > MAX_TUGS {
                                    return Err(E::recursion(tug_t));
                                } else {
                                    queue.push(t.clone());
                                    queue.push(tug_t);
                                }
                            }
                        }
                        Ok(r)
                    })
                });
                self.resources.insert(t.clone(), new_r_state);
            }
        }
    }
}
