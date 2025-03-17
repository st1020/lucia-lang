use lucia_lang::compiler::value::MetaName;
use thiserror::Error;

use crate::typing::Type;

/// Kind of TypeCheckError.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum TypeError<S: Eq + Ord> {
    #[error("{ty} is not subtype of {expected} ")]
    ExpectIsSubtypeOf {
        ty: Box<Type<S>>,
        expected: Box<Type<S>>,
    },
    #[error("unsupported operand type for {operator}: {operand}")]
    MetaUnOperator {
        operator: MetaName,
        operand: Box<Type<S>>,
    },
    #[error("unsupported operand types for {operator}: {} and {}", .operand.0, .operand.1)]
    MetaBinOperator {
        operator: MetaName,
        operand: (Box<Type<S>>, Box<Type<S>>),
    },
}
