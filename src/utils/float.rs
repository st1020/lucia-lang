//! Utilities for lucia-lang.

use std::{cmp::Ordering, hash};

use derive_more::{
    Add, AddAssign, Debug, Display, Div, DivAssign, From, FromStr, Mul, MulAssign, Neg, Rem,
    RemAssign, Sub, SubAssign,
};
use gc_arena::Collect;

// canonical raw float bit
const CANONICAL_NAN_BITS: u64 = 0x7ff8_0000_0000_0000_u64;
const CANONICAL_ZERO_BITS: u64 = 0x0_u64;

/// The f64 which impl Eq, Ord, Hash.
#[derive(
    Clone,
    Copy,
    Collect,
    From,
    FromStr,
    Display,
    Debug,
    Neg,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
)]
#[collect(require_static)]
#[from(forward)]
#[debug("{_0}")]
#[mul(forward)]
#[div(forward)]
#[rem(forward)]
#[mul_assign(forward)]
#[div_assign(forward)]
#[rem_assign(forward)]
pub struct Float(pub f64);

impl PartialEq for Float {
    fn eq(&self, other: &Self) -> bool {
        if self.0.is_nan() {
            other.0.is_nan()
        } else {
            self.0 == other.0
        }
    }
}

impl Eq for Float {}

impl PartialOrd for Float {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Float {
    fn cmp(&self, other: &Self) -> Ordering {
        if self == other {
            Ordering::Equal
        } else {
            self.0.total_cmp(&other.0)
        }
    }
}

impl hash::Hash for Float {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        if self.0.is_nan() {
            CANONICAL_NAN_BITS.hash(state);
        } else if self.0 == 0.0_f64 {
            CANONICAL_ZERO_BITS.hash(state);
        } else {
            self.0.to_bits().hash(state);
        }
    }
}
