//! Utilities for lucia-lang.

use std::{cmp::Ordering, fmt, hash, ops};

use gc_arena::Collect;

// canonical raw float bit
const CANONICAL_NAN_BITS: u64 = 0x7ff8000000000000u64;
const CANONICAL_ZERO_BITS: u64 = 0x0u64;

/// The f64 which impl Eq, Ord, Hash.
#[derive(Clone, Copy, Collect)]
#[collect(require_static)]
pub struct Float(pub f64);

impl From<f64> for Float {
    fn from(value: f64) -> Self {
        Float(value)
    }
}

impl From<Float> for f64 {
    fn from(value: Float) -> Self {
        value.0
    }
}

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
            CANONICAL_NAN_BITS.hash(state)
        } else if self.0 == 0.0f64 {
            CANONICAL_ZERO_BITS.hash(state)
        } else {
            self.0.to_bits().hash(state)
        }
    }
}

impl fmt::Debug for Float {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl fmt::Display for Float {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl ops::Add for Float {
    type Output = Float;

    fn add(self, rhs: Self) -> Self::Output {
        Float(self.0.add(rhs.0))
    }
}

impl ops::Sub for Float {
    type Output = Float;

    fn sub(self, rhs: Self) -> Self::Output {
        Float(self.0.sub(rhs.0))
    }
}

impl ops::Mul for Float {
    type Output = Float;

    fn mul(self, rhs: Self) -> Self::Output {
        Float(self.0.mul(rhs.0))
    }
}

impl ops::Div for Float {
    type Output = Float;

    fn div(self, rhs: Self) -> Self::Output {
        Float(self.0.div(rhs.0))
    }
}

impl ops::Rem for Float {
    type Output = Float;

    fn rem(self, rhs: Self) -> Self::Output {
        Float(self.0.rem(rhs.0))
    }
}

impl ops::Neg for Float {
    type Output = Float;

    fn neg(self) -> Self::Output {
        Float(self.0.neg())
    }
}
