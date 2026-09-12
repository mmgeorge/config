use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use crate::ContractError;
use crate::block::BlockAnchor;
use crate::identity::{InputSequence, ViewId};
use crate::width::WidthProfile;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct VisibleDemand {
    pub view: ViewId,
    pub sequence: InputSequence,
    pub start: BlockAnchor,
    pub end: BlockAnchor,
    pub lookahead_rows: usize,
}

#[derive(Clone, Debug)]
struct DocumentView {
    admission: u64,
    profile: WidthProfile,
    demand: Option<VisibleDemand>,
}

#[derive(Clone, Default)]
pub struct DocumentViews {
    view: BTreeMap<ViewId, DocumentView>,
    owner: Option<ViewId>,
    next_admission: u64,
}

impl DocumentViews {
    pub fn open(&mut self, id: ViewId, profile: WidthProfile) -> Result<bool, ContractError> {
        id.validate()?;
        profile.validate()?;
        if self.view.contains_key(&id) {
            return self.resize(&id, profile);
        }
        if self.view.len() >= 64 {
            return Err(ContractError("document view admission limit exceeded"));
        }
        let admission = self.next_admission;
        self.next_admission = admission
            .checked_add(1)
            .ok_or(ContractError("view admission identity exhausted"))?;
        self.view.insert(
            id.clone(),
            DocumentView {
                admission,
                profile,
                demand: None,
            },
        );
        if self.owner.is_none() {
            self.owner = Some(id);
            return Ok(true);
        }
        Ok(false)
    }

    pub fn resize(&mut self, id: &ViewId, profile: WidthProfile) -> Result<bool, ContractError> {
        profile.validate()?;
        let view = self
            .view
            .get_mut(id)
            .ok_or(ContractError("unknown document view"))?;
        let changed = view.profile != profile;
        view.profile = profile;
        Ok(changed && self.owner.as_ref() == Some(id))
    }

    pub fn close(&mut self, id: &ViewId) -> bool {
        self.view.remove(id);
        if self.owner.as_ref() != Some(id) {
            return false;
        }
        self.owner = self
            .view
            .iter()
            .min_by_key(|(_, view)| view.admission)
            .map(|(id, _)| id.clone());
        true
    }

    pub fn profile(&self) -> Option<&WidthProfile> {
        self.owner
            .as_ref()
            .and_then(|id| self.view.get(id))
            .map(|view| &view.profile)
    }

    pub fn demand(&mut self, demand: VisibleDemand) -> Result<bool, ContractError> {
        demand.sequence.validate()?;
        demand.start.block.validate()?;
        demand.end.block.validate()?;
        if demand.lookahead_rows > 4096 {
            return Err(ContractError(
                "visible lookahead exceeds one bounded viewport",
            ));
        }
        let view = self
            .view
            .get_mut(&demand.view)
            .ok_or(ContractError("unknown demand view"))?;
        if view
            .demand
            .as_ref()
            .is_some_and(|previous| previous.sequence >= demand.sequence)
        {
            return Ok(false);
        }
        view.demand = Some(demand);
        Ok(true)
    }

    pub fn visible(&self) -> impl Iterator<Item = &VisibleDemand> {
        self.view.values().filter_map(|view| view.demand.as_ref())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn shared_buffer_width_follows_oldest_live_owner() {
        let mut views = DocumentViews::default();
        let first = ViewId("first".into());
        let second = ViewId("second".into());
        let profile = WidthProfile::default();
        assert!(views.open(first.clone(), profile.clone()).unwrap());
        assert!(
            !views
                .open(
                    second.clone(),
                    WidthProfile {
                        columns: 100,
                        ..profile.clone()
                    }
                )
                .unwrap()
        );
        assert!(
            !views
                .resize(
                    &second,
                    WidthProfile {
                        columns: 40,
                        ..profile.clone()
                    }
                )
                .unwrap()
        );
        assert_eq!(views.profile().unwrap().columns, 80);
        assert!(views.close(&first));
        assert_eq!(views.profile().unwrap().columns, 40);
        assert!(views.close(&second));
        assert!(views.profile().is_none());
    }
}
