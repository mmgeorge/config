//! Opaque identities and independent monotonic counters.

use serde::{Deserialize, Serialize};

use crate::{ContractError, MAX_COUNTER};

macro_rules! identity {
    ($($name:ident),+ $(,)?) => {$(
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
        #[serde(transparent)]
        pub struct $name(pub String);

        impl $name {
            pub fn validate(&self) -> Result<(), ContractError> {
                if self.0.is_empty() || self.0.len() > 256 || self.0.chars().any(char::is_control) {
                    return Err(ContractError("identity must contain 1..=256 bytes without control characters"));
                }
                Ok(())
            }
        }
    )+};
}

macro_rules! counter {
    ($($name:ident),+ $(,)?) => {$(
        #[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
        #[serde(transparent)]
        pub struct $name(pub u64);

        impl $name {
            pub fn validate(self) -> Result<(), ContractError> {
                if self.0 > MAX_COUNTER {
                    return Err(ContractError("counter exceeds Lua's exact integer range"));
                }
                Ok(())
            }

            pub fn next(self) -> Result<Self, ContractError> {
                self.validate()?;
                if self.0 == MAX_COUNTER {
                    return Err(ContractError("counter exhausted, replace its owning lifetime"));
                }
                Ok(Self(self.0 + 1))
            }
        }
    )+};
}

identity!(
    DocumentId, BlockId, TargetId, RegionId, FoldId, ViewId, EffectId
);
counter!(
    DocumentRevision,
    InputSequence,
    EditSequence,
    RegionRevision
);
