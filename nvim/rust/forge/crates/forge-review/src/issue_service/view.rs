use super::IssueDocumentService;
use crate::issue::IssueEffect;
use anyhow::Result;
use forge_buffer::{
    identity::{DocumentId, ViewId},
    input::DocumentInput,
    patch::BufferPatch,
    width::WidthProfile,
};

impl IssueDocumentService {
    pub async fn view(
        &self,
        id: &DocumentId,
        view: ViewId,
        width: WidthProfile,
    ) -> Result<Option<BufferPatch>> {
        let owner = self.get(id)?;
        let _layout = owner.layout.lock().await;
        let capture = {
            let mut document = owner.document.lock().await;
            owner.admission.check()?;
            document.prepare_view(view, width)?
        };
        let Some(capture) = capture else {
            return Ok(None);
        };
        let (capture, projection) = self
            .analyze(owner.admission.clone(), 16 * 1024 * 1024, move || {
                let mut capture = capture;
                let fields = std::mem::take(&mut capture.fields);
                let projection =
                    crate::issue_presentation::project(&capture.detail, fields, &capture.width)?;
                Ok((capture, projection))
            })
            .await?;
        let mut document = owner.document.lock().await;
        owner.admission.check()?;
        document.adopt_view(capture, projection)
    }

    pub async fn close_view(&self, id: &DocumentId, view: &ViewId) -> Result<Option<BufferPatch>> {
        let owner = self.get(id)?;
        let _layout = owner.layout.lock().await;
        let capture = {
            let mut document = owner.document.lock().await;
            owner.admission.check()?;
            document.prepare_close_view(view)?
        };
        let Some(capture) = capture else {
            return Ok(None);
        };
        let (capture, projection) = self
            .analyze(owner.admission.clone(), 16 * 1024 * 1024, move || {
                let mut capture = capture;
                let fields = std::mem::take(&mut capture.fields);
                let projection =
                    crate::issue_presentation::project(&capture.detail, fields, &capture.width)?;
                Ok((capture, projection))
            })
            .await?;
        let mut document = owner.document.lock().await;
        owner.admission.check()?;
        document.adopt_view(capture, projection)
    }

    pub async fn act(&self, input: DocumentInput) -> Result<IssueEffect> {
        let owner = self.get(&input.document)?;
        let mut document = owner.document.lock().await;
        owner.admission.check()?;
        document.act(input)
    }
}
