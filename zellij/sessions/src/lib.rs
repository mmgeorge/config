use fuzzy_matcher::skim::SkimMatcherV2;
use fuzzy_matcher::FuzzyMatcher;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SessionEntry {
    pub name: String,
    pub is_current_session: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Row {
    Session(usize),
    CreateNewSession,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Mode {
    Search,
    CreateSession,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SwitcherInput {
    Character(char),
    Backspace,
    Enter,
    Down,
    Up,
    Escape,
    Cancel,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SwitcherAction {
    Hide,
    RenamePluginPane,
    RefreshSessions,
    SwitchSession(String),
    CreateSession(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ScreenLine {
    pub row: usize,
    pub column: usize,
    pub text: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SessionSwitcher {
    sessions: Vec<SessionEntry>,
    search_term: String,
    rows: Vec<Row>,
    selected_index: usize,
    mode: Mode,
    new_session_name: String,
    error: Option<String>,
    permissions_granted: bool,
}

impl Default for Mode {
    fn default() -> Self {
        Mode::Search
    }
}

impl Default for SessionSwitcher {
    fn default() -> Self {
        let mut switcher = SessionSwitcher {
            sessions: Vec::new(),
            search_term: String::new(),
            rows: Vec::new(),
            selected_index: 0,
            mode: Mode::Search,
            new_session_name: String::new(),
            error: None,
            permissions_granted: false,
        };
        switcher.rebuild_rows();
        switcher
    }
}

impl SessionSwitcher {
    pub fn sessions(&self) -> &[SessionEntry] {
        &self.sessions
    }

    pub fn search_term(&self) -> &str {
        &self.search_term
    }

    pub fn rows(&self) -> &[Row] {
        &self.rows
    }

    pub fn selected_index(&self) -> usize {
        self.selected_index
    }

    pub fn mode(&self) -> Mode {
        self.mode
    }

    pub fn new_session_name(&self) -> &str {
        &self.new_session_name
    }

    pub fn error(&self) -> Option<&str> {
        self.error.as_deref()
    }

    pub fn permissions_granted(&self) -> bool {
        self.permissions_granted
    }

    pub fn set_sessions(&mut self, sessions: Vec<SessionEntry>) {
        self.sessions = sorted_sessions(sessions);
        self.error = None;
        self.rebuild_rows();
    }

    pub fn set_error(&mut self, error: impl Into<String>) {
        self.error = Some(error.into());
        self.rebuild_rows();
    }

    pub fn grant_permissions(&mut self) -> Vec<SwitcherAction> {
        self.permissions_granted = true;
        self.error = None;
        vec![
            SwitcherAction::RenamePluginPane,
            SwitcherAction::RefreshSessions,
        ]
    }

    pub fn deny_permissions(&mut self) {
        self.permissions_granted = false;
        self.set_error("Session permissions denied");
    }

    pub fn set_visible(&self, is_visible: bool) -> Vec<SwitcherAction> {
        if is_visible && self.permissions_granted {
            vec![SwitcherAction::RefreshSessions]
        } else {
            Vec::new()
        }
    }

    pub fn handle_input(&mut self, input: SwitcherInput) -> Vec<SwitcherAction> {
        if self.error.is_some() {
            self.error = None;
            return Vec::new();
        }

        match self.mode {
            Mode::Search => self.handle_search_input(input),
            Mode::CreateSession => self.handle_create_input(input),
        }
    }

    pub fn render_lines(&self, terminal_rows: usize, terminal_columns: usize) -> Vec<ScreenLine> {
        let width = terminal_columns.saturating_sub(4).min(86).max(32);
        let max_list_rows = terminal_rows.saturating_sub(9).min(14);
        let list_rows = self.rows.len().min(max_list_rows).max(1);
        let height = list_rows + 7;
        let column = terminal_columns.saturating_sub(width) / 2;
        let row = terminal_rows.saturating_sub(height) / 2;
        let mut lines = Vec::new();

        push_line(&mut lines, row, column, border_top(width));
        push_box_line(&mut lines, row + 1, column, width, "");
        match self.mode {
            Mode::Search => {
                let prompt = format!("Search: {}_", self.search_term);
                push_box_line(&mut lines, row + 2, column, width, &prompt);
            },
            Mode::CreateSession => {
                let prompt = format!("New session name: {}_", self.new_session_name);
                push_box_line(&mut lines, row + 2, column, width, &prompt);
            },
        }
        push_box_line(&mut lines, row + 3, column, width, "");

        match self.mode {
            Mode::Search => {
                self.push_search_rows(&mut lines, row + 4, column, width, max_list_rows);
            },
            Mode::CreateSession => {
                push_box_line(
                    &mut lines,
                    row + 4,
                    column,
                    width,
                    "Enter to create, Esc to cancel",
                );
            },
        }

        let help_row = row + height.saturating_sub(2);
        if let Some(error) = &self.error {
            push_box_line(&mut lines, help_row, column, width, error);
        } else {
            let help = match self.mode {
                Mode::Search => "Up/Down select  Enter open  Esc close",
                Mode::CreateSession => "Enter create  Esc back",
            };
            push_box_line(&mut lines, help_row, column, width, help);
        }
        push_line(
            &mut lines,
            row + height.saturating_sub(1),
            column,
            border_bottom(width),
        );
        lines
    }

    fn handle_search_input(&mut self, input: SwitcherInput) -> Vec<SwitcherAction> {
        match input {
            SwitcherInput::Character(character) => {
                if character == '\n' {
                    self.activate_selected_row()
                } else {
                    self.search_term.push(character);
                    self.rebuild_rows();
                    Vec::new()
                }
            },
            SwitcherInput::Backspace => {
                self.search_term.pop();
                self.rebuild_rows();
                Vec::new()
            },
            SwitcherInput::Enter => self.activate_selected_row(),
            SwitcherInput::Down => {
                self.move_selection_down();
                Vec::new()
            },
            SwitcherInput::Up => {
                self.move_selection_up();
                Vec::new()
            },
            SwitcherInput::Escape | SwitcherInput::Cancel => vec![SwitcherAction::Hide],
        }
    }

    fn handle_create_input(&mut self, input: SwitcherInput) -> Vec<SwitcherAction> {
        match input {
            SwitcherInput::Character(character) => {
                if character == '\n' {
                    self.create_session()
                } else {
                    self.new_session_name.push(character);
                    Vec::new()
                }
            },
            SwitcherInput::Backspace => {
                self.new_session_name.pop();
                Vec::new()
            },
            SwitcherInput::Enter => self.create_session(),
            SwitcherInput::Escape | SwitcherInput::Cancel => {
                self.mode = Mode::Search;
                self.new_session_name.clear();
                Vec::new()
            },
            SwitcherInput::Down | SwitcherInput::Up => Vec::new(),
        }
    }

    fn rebuild_rows(&mut self) {
        self.rows = visible_rows(&self.sessions, &self.search_term);
        self.selected_index = self.selected_index.min(self.rows.len().saturating_sub(1));
    }

    fn move_selection_down(&mut self) {
        if self.rows.is_empty() {
            self.selected_index = 0;
        } else {
            self.selected_index = (self.selected_index + 1) % self.rows.len();
        }
    }

    fn move_selection_up(&mut self) {
        if self.rows.is_empty() {
            self.selected_index = 0;
        } else if self.selected_index == 0 {
            self.selected_index = self.rows.len() - 1;
        } else {
            self.selected_index -= 1;
        }
    }

    fn activate_selected_row(&mut self) -> Vec<SwitcherAction> {
        if !self.permissions_granted {
            self.error = Some("Session permissions are not granted yet".to_owned());
            return Vec::new();
        }

        match self.rows.get(self.selected_index) {
            Some(Row::Session(session_index)) => {
                let session = &self.sessions[*session_index];
                if session.is_current_session {
                    self.error = Some(format!("Already in session '{}'", session.name));
                    Vec::new()
                } else {
                    self.search_term.clear();
                    vec![
                        SwitcherAction::SwitchSession(session.name.clone()),
                        SwitcherAction::Hide,
                    ]
                }
            },
            Some(Row::CreateNewSession) | None => {
                self.mode = Mode::CreateSession;
                self.new_session_name = self.search_term.trim().to_owned();
                Vec::new()
            },
        }
    }

    fn create_session(&mut self) -> Vec<SwitcherAction> {
        if !self.permissions_granted {
            self.error = Some("Session permissions are not granted yet".to_owned());
            return Vec::new();
        }

        let session_name = self.new_session_name.trim().to_owned();
        if let Some(error) = validate_new_session_name(&session_name, &self.sessions) {
            self.error = Some(error);
            return Vec::new();
        }

        self.mode = Mode::Search;
        self.search_term.clear();
        self.new_session_name.clear();
        vec![
            SwitcherAction::CreateSession(session_name),
            SwitcherAction::Hide,
        ]
    }

    fn push_search_rows(
        &self,
        lines: &mut Vec<ScreenLine>,
        first_row: usize,
        column: usize,
        width: usize,
        max_rows: usize,
    ) {
        if self.rows.is_empty() {
            push_box_line(lines, first_row, column, width, "Create new session");
            return;
        }

        let visible_range = visible_row_range(self.selected_index, self.rows.len(), max_rows);
        for (screen_row, row_index) in visible_range.enumerate() {
            let marker = if row_index == self.selected_index {
                ">"
            } else {
                " "
            };
            let label = match self.rows.get(row_index) {
                Some(Row::Session(session_index)) => {
                    let session = &self.sessions[*session_index];
                    if session.is_current_session {
                        format!("{marker} {} (current)", session.name)
                    } else {
                        format!("{marker} {}", session.name)
                    }
                },
                Some(Row::CreateNewSession) | None => {
                    format!("{marker} Create new session")
                },
            };
            push_box_line(lines, first_row + screen_row, column, width, &label);
        }
    }
}

pub fn sorted_sessions(mut sessions: Vec<SessionEntry>) -> Vec<SessionEntry> {
    sessions.sort_by(
        |left, right| match (left.is_current_session, right.is_current_session) {
            (true, false) => std::cmp::Ordering::Less,
            (false, true) => std::cmp::Ordering::Greater,
            _ => left.name.cmp(&right.name),
        },
    );
    sessions
}

pub fn border_top(width: usize) -> String {
    format!("+{}+", "-".repeat(width.saturating_sub(2)))
}

pub fn border_bottom(width: usize) -> String {
    border_top(width)
}

pub fn truncate_to_width(text: &str, width: usize) -> String {
    let mut result = String::new();
    for character in text.chars() {
        if result.chars().count() >= width {
            break;
        }
        result.push(character);
    }
    result
}

fn push_line(lines: &mut Vec<ScreenLine>, row: usize, column: usize, text: String) {
    lines.push(ScreenLine { row, column, text });
}

fn push_box_line(
    lines: &mut Vec<ScreenLine>,
    row: usize,
    column: usize,
    width: usize,
    text: &str,
) {
    let inner_width = width.saturating_sub(4);
    let text = truncate_to_width(text, inner_width);
    let padding = inner_width.saturating_sub(text.chars().count());
    push_line(
        lines,
        row,
        column,
        format!("| {}{} |", text, " ".repeat(padding)),
    );
}

pub fn visible_rows(sessions: &[SessionEntry], search_term: &str) -> Vec<Row> {
    let mut rows: Vec<Row> = if search_term.is_empty() {
        sessions
            .iter()
            .enumerate()
            .map(|(session_index, _session)| Row::Session(session_index))
            .collect()
    } else {
        let matcher = SkimMatcherV2::default().use_cache(true);
        let mut scored_sessions: Vec<(i64, usize)> = sessions
            .iter()
            .enumerate()
            .filter_map(|(session_index, session)| {
                matcher
                    .fuzzy_match(&session.name, search_term)
                    .map(|score| (score, session_index))
            })
            .collect();
        scored_sessions.sort_by(|left, right| {
            right
                .0
                .cmp(&left.0)
                .then_with(|| sessions[left.1].name.cmp(&sessions[right.1].name))
        });
        scored_sessions
            .into_iter()
            .map(|(_score, session_index)| Row::Session(session_index))
            .collect()
    };

    rows.push(Row::CreateNewSession);
    rows
}

pub fn validate_new_session_name(
    session_name: &str,
    sessions: &[SessionEntry],
) -> Option<String> {
    if session_name.is_empty() {
        return Some("Session name cannot be empty".to_owned());
    }
    if session_name.len() >= 108 {
        return Some("Session name must be shorter than 108 bytes".to_owned());
    }
    if session_name.contains('/') {
        return Some("Session name cannot contain '/'".to_owned());
    }
    if sessions.iter().any(|session| session.name == session_name) {
        return Some(format!("Session '{}' already exists", session_name));
    }
    None
}

pub fn visible_row_range(
    selected_index: usize,
    row_count: usize,
    max_rows: usize,
) -> std::ops::Range<usize> {
    if row_count <= max_rows {
        return 0..row_count;
    }

    let half_window = max_rows / 2;
    let mut start = selected_index.saturating_sub(half_window);
    start = start.min(row_count.saturating_sub(max_rows));
    start..start + max_rows
}

#[cfg(test)]
mod tests {
    use super::*;

    fn session(name: &str, is_current_session: bool) -> SessionEntry {
        SessionEntry {
            name: name.to_owned(),
            is_current_session,
        }
    }

    #[test]
    fn visible_rows_always_end_with_create_new_session() {
        let sessions = vec![session("alpha", false), session("beta", false)];

        assert_eq!(
            visible_rows(&sessions, ""),
            vec![Row::Session(0), Row::Session(1), Row::CreateNewSession]
        );
        assert_eq!(
            visible_rows(&sessions, "zzz"),
            vec![Row::CreateNewSession]
        );
    }

    #[test]
    fn visible_rows_fuzzy_match_sessions_before_create_row() {
        let sessions = vec![
            session("ferrous-main", false),
            session("config", false),
            session("ferrous-renderer", false),
        ];

        let rows = visible_rows(&sessions, "frr");

        assert_eq!(rows.last(), Some(&Row::CreateNewSession));
        assert!(rows.contains(&Row::Session(2)));
    }

    #[test]
    fn sorted_sessions_keeps_current_first_then_names() {
        let sessions = sorted_sessions(vec![
            session("zeta", false),
            session("current", true),
            session("alpha", false),
        ]);

        assert_eq!(
            sessions,
            vec![
                session("current", true),
                session("alpha", false),
                session("zeta", false)
            ]
        );
    }

    #[test]
    fn validate_new_session_name_rejects_invalid_names() {
        let sessions = vec![session("existing", false)];

        assert!(validate_new_session_name("", &sessions).is_some());
        assert!(validate_new_session_name("bad/name", &sessions).is_some());
        assert!(validate_new_session_name("existing", &sessions).is_some());
        assert_eq!(validate_new_session_name("new-session", &sessions), None);
    }

    #[test]
    fn visible_row_range_centers_selected_row_when_possible() {
        assert_eq!(visible_row_range(0, 10, 5), 0..5);
        assert_eq!(visible_row_range(5, 10, 5), 3..8);
        assert_eq!(visible_row_range(9, 10, 5), 5..10);
    }
}
