#[cfg(test)]
use forge_buffer::block::BufferBlock;
#[cfg(test)]
use forge_buffer::document::BufferDocument;

#[cfg(test)]
mod tests {
    use super::*;
    use forge_buffer::block::BlockMetadata;
    use forge_buffer::identity::{BlockId, DocumentId};
    use forge_buffer::text::BufferText;

    fn block(index: usize, text: &str) -> BufferBlock {
        BufferBlock {
            id: BlockId(format!("row:{index}")),
            text: BufferText::from_rows([text]).unwrap(),
            metadata: BlockMetadata::default(),
        }
    }

    #[test]
    fn insert_delete_reorder_and_empty_replacements_match_the_projection() {
        let original = (0..6)
            .map(|index| block(index, "declaration"))
            .collect::<Vec<_>>();
        let mut document =
            BufferDocument::new(DocumentId("review".into()), original.clone()).unwrap();
        for replacement in [
            vec![
                original[3].clone(),
                block(7, "inserted"),
                original[1].clone(),
            ],
            vec![],
            original,
        ] {
            document
                .splice(0..document.block_count(), replacement.clone())
                .unwrap();
            assert_eq!(document.snapshot().block, replacement);
        }
    }
}
