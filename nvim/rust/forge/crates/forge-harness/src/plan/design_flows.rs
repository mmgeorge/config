//! Describes the plan's runtime flows, which the plan stores as the Markdown the agent wrote.
//!
//! Each flow is a `## Title` heading, a one-to-two-line description, and one fenced Mermaid
//! block. Submission checks that shape through `design_artifact::parse_flows`; review and
//! revision diffs show the text unchanged.

/// Names the Mermaid diagram types a plan flow may use, each matched to one kind of flow.
#[derive(Clone, Copy)]
enum FlowDiagramKind {
    /// Shows messages between owners for a flow that crosses ownership boundaries.
    Sequence,
    /// Shows branching control flow inside one owner.
    Flowchart,
    /// Shows a lifecycle whose transitions come from several triggers.
    State,
}

impl FlowDiagramKind {
    /// Every allowed kind, in the order diagnostics list them.
    const ALL: [Self; 3] = [Self::Sequence, Self::Flowchart, Self::State];

    /// Get the Mermaid keyword that opens a diagram of this kind.
    fn keyword(self) -> &'static str {
        match self {
            Self::Sequence => "sequenceDiagram",
            Self::Flowchart => "flowchart",
            Self::State => "stateDiagram-v2",
        }
    }
}

/// Report whether a Mermaid block's first line opens an allowed diagram type.
pub(super) fn diagram_allowed(first_line: &str) -> bool {
    let keyword = first_line.split_whitespace().next().unwrap_or_default();
    FlowDiagramKind::ALL
        .into_iter()
        .any(|kind| kind.keyword() == keyword)
}

/// List the allowed diagram keywords for diagnostics.
pub(super) fn allowed_keywords() -> String {
    let keywords = FlowDiagramKind::ALL.map(|kind| format!("`{}`", kind.keyword()));
    format!("{}, {}, or {}", keywords[0], keywords[1], keywords[2])
}

/// Count the flows in flow Markdown: its `## Title` headings outside code fences.
pub(crate) fn count(text: &str) -> usize {
    let mut fenced = false;
    let mut flows = 0;
    for line in text.lines() {
        if line.starts_with("```") {
            fenced = !fenced;
        } else if !fenced && line.starts_with("## ") {
            flows += 1;
        }
    }
    flows
}
