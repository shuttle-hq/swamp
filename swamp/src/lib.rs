#![feature(proc_macro)]
use std::fmt::Display;

pub mod prelude;

pub mod block;
pub use block::*;

pub mod compress;
pub use compress::*;

pub mod scope;
pub use scope::*;

pub mod merge;
pub use merge::*;

extern crate swamp_codegen;
pub use swamp_codegen::*;

#[macro_export]
macro_rules! block_type {
    ($ty:tt$(.$label:tt)*) => {
        BlockType::new::<&str>($ty, &[$($label,)*])
    }
}

macro_rules! value_or_tug {
    ($e:tt) => {
        match tt? {
            ValueOrTug::Value(r) => r,
            ValueOrTug::Tug(t) => return Ok(Poll::Tug(t)),
        }
    };
}

pub trait DefaultExt: Default + Eq {
    fn is_default(&self) -> bool {
        *self == Self::default()
    }
}

pub trait Error: Sized + std::error::Error + Send + Sync + Clone {
    fn missing<T: Display>(ty: T) -> Self;
    fn recursion<T: Display>(ty: T) -> Self;
    fn dependent<T: Display, D: Error>(ty: T, err: &D) -> Self;
}

pub enum ValueOrTug<R, T> {
    Value(R),
    Tug(T),
}

pub enum PollState<T> {
    Done,
    Tug(T),
}

pub type PollResult<T, E> = Result<PollState<T>, E>;
