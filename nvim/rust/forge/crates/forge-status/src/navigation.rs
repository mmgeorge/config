use crate::{
    BodyState, StatusSection,
    document::StatusDocument,
    protocol::{StatusLocation, file_target},
};
use anyhow::{Context, Result};
use forge_buffer::{block::TextPosition, identity::TargetId};

pub(crate) enum Destination {
    Demand(TargetId),
    Header(StatusLocation),
    End,
}

pub(crate) fn destination(
    document: &StatusDocument,
    origin: &StatusLocation,
    forward: bool,
) -> Result<Destination> {
    let origin_file = match origin {
        StatusLocation::File { id } => Some(file_target(*id)),
        StatusLocation::Body { file, .. } => Some(file_target(*file)),
        StatusLocation::Section { section } => document
            .order
            .iter()
            .find(|target| {
                let file = &document.file[*target];
                file.section == *section
                    || (*section == StatusSection::Unstaged
                        && file.section == StatusSection::Untracked)
            })
            .cloned(),
        _ => None,
    };
    let recent = matches!(origin, StatusLocation::Context { role } if role.starts_with("recent:"))
        || matches!(origin, StatusLocation::Boundary { after_files: true });
    if (origin_file.is_none() && recent && forward)
        || (origin_file.is_none() && !recent && !forward)
    {
        return Ok(Destination::End);
    }
    let start = origin_file
        .as_ref()
        .and_then(|target| document.order.iter().position(|entry| entry == target));
    let index: Box<dyn Iterator<Item = usize>> = if forward {
        Box::new(start.unwrap_or(0)..document.order.len())
    } else {
        Box::new((0..start.map_or(document.order.len(), |index| index + 1)).rev())
    };
    for index in index {
        let target = &document.order[index];
        let file = &document.file[target];
        let same = origin_file.as_ref() == Some(target);
        let incomplete = matches!(
            file.state,
            BodyState::Deferred | BodyState::Loading | BodyState::Partial
        );
        let origin_body = if same {
            match origin {
                StatusLocation::Body {
                    block, position, ..
                } => Some((block, position)),
                _ => None,
            }
        } else {
            None
        };
        if !forward && same && origin_body.is_none() {
            continue;
        }
        if !forward && !same && incomplete {
            return Ok(Destination::Demand(target.clone()));
        }
        if let Some(body) = file.body.as_ref() {
            let origin_index = origin_body.and_then(|(block, _)| body.block_index(block));
            let mut header: Vec<_> = body
                .blocks(body.revision(), 0..body.block_count())?
                .enumerate()
                .filter(|(_, block)| block.id.0.starts_with("hunk-header:"))
                .collect();
            if !forward {
                header.reverse();
            }
            for (index, block) in header {
                let eligible = match origin_index {
                    Some(origin_index) if forward => index > origin_index,
                    Some(origin_index) => {
                        index < origin_index
                            || (index == origin_index
                                && origin_body.is_some_and(|(_, position)| {
                                    position.row > 0 || position.column > 0
                                }))
                    }
                    None => true,
                };
                if eligible {
                    return Ok(Destination::Header(StatusLocation::Body {
                        file: file.id,
                        generation: file.generation,
                        revision: body.revision(),
                        block: block.id.clone(),
                        position: TextPosition { row: 0, column: 0 },
                        target: Some(
                            block
                                .metadata
                                .target
                                .first()
                                .context("hunk header target missing")?
                                .id
                                .clone(),
                        ),
                    }));
                }
            }
        }
        if incomplete {
            return Ok(Destination::Demand(target.clone()));
        }
    }
    Ok(Destination::End)
}
