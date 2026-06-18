#[cfg(any(test, target_arch = "wasm32"))]
use fuzzy_matcher::skim::SkimMatcherV2;
#[cfg(any(test, target_arch = "wasm32"))]
use fuzzy_matcher::FuzzyMatcher;

#[cfg(any(test, target_arch = "wasm32"))]
#[derive(Clone, Debug, Eq, PartialEq)]
struct SessionEntry {
    name: String,
    is_current_session: bool,
}

#[cfg(target_arch = "wasm32")]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Mode {
    Search,
    CreateSession,
}

#[cfg(any(test, target_arch = "wasm32"))]
#[derive(Clone, Debug, Eq, PartialEq)]
enum Row {
    Session(usize),
    CreateNewSession,
}

#[cfg(target_arch = "wasm32")]
impl Default for Mode {
    fn default() -> Self {
        Mode::Search
    }
}

#[cfg(any(test, target_arch = "wasm32"))]
fn sorted_sessions(mut sessions: Vec<SessionEntry>) -> Vec<SessionEntry> {
    sessions.sort_by(
        |left, right| match (left.is_current_session, right.is_current_session) {
            (true, false) => std::cmp::Ordering::Less,
            (false, true) => std::cmp::Ordering::Greater,
            _ => left.name.cmp(&right.name),
        },
    );
    sessions
}

#[cfg(any(test, target_arch = "wasm32"))]
fn visible_rows(sessions: &[SessionEntry], search_term: &str) -> Vec<Row> {
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

#[cfg(any(test, target_arch = "wasm32"))]
fn validate_new_session_name(session_name: &str, sessions: &[SessionEntry]) -> Option<String> {
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

#[cfg(any(test, target_arch = "wasm32"))]
fn visible_row_range(
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

#[cfg(target_arch = "wasm32")]
fn border_top(width: usize) -> String {
    format!("+{}+", "-".repeat(width.saturating_sub(2)))
}

#[cfg(target_arch = "wasm32")]
fn border_bottom(width: usize) -> String {
    border_top(width)
}

#[cfg(target_arch = "wasm32")]
fn truncate_to_width(text: &str, width: usize) -> String {
    let mut result = String::new();
    for character in text.chars() {
        if result.chars().count() >= width {
            break;
        }
        result.push(character);
    }
    result
}

#[cfg(target_arch = "wasm32")]
mod plugin {
    use super::*;
    use zellij_tile::prelude::*;

    #[derive(Default)]
    struct SessionsPlugin {
        sessions: Vec<SessionEntry>,
        search_term: String,
        rows: Vec<Row>,
        selected_index: usize,
        mode: Mode,
        new_session_name: String,
        error: Option<String>,
        is_visible: bool,
    }

    register_plugin!(SessionsPlugin);

    impl ZellijPlugin for SessionsPlugin {
        fn load(&mut self, _configuration: std::collections::BTreeMap<String, String>) {
            self.is_visible = true;
            request_permission(&[PermissionType::ReadApplicationState]);
            subscribe(&[
                EventType::ModeUpdate,
                EventType::SessionUpdate,
                EventType::Key,
                EventType::Visible,
            ]);
            rename_plugin_pane(get_plugin_ids().plugin_id, "Sessions");
            self.refresh_sessions();
        }

        fn update(&mut self, event: Event) -> bool {
            match event {
                Event::Key(key) => self.handle_key(key),
                Event::SessionUpdate(sessions, _resurrectable_sessions) => {
                    self.set_sessions(sessions_to_entries(sessions));
                    true
                },
                Event::Visible(is_visible) => {
                    self.is_visible = is_visible;
                    if is_visible {
                        self.refresh_sessions();
                    }
                    true
                },
                Event::ModeUpdate(_) => true,
                Event::PermissionRequestResult(_) => {
                    self.refresh_sessions();
                    true
                },
                _ => false,
            }
        }

        fn render(&mut self, rows: usize, columns: usize) {
            self.render_centered(rows, columns);
        }
    }

    impl SessionsPlugin {
        fn refresh_sessions(&mut self) {
            match get_session_list() {
                Ok(snapshot) => self.set_sessions(sessions_to_entries(snapshot.live_sessions)),
                Err(error) => {
                    self.error = Some(format!("Could not read sessions: {error}"));
                    self.rebuild_rows();
                },
            }
        }

        fn set_sessions(&mut self, sessions: Vec<SessionEntry>) {
            self.sessions = sorted_sessions(sessions);
            self.error = None;
            self.rebuild_rows();
        }

        fn handle_key(&mut self, key: KeyWithModifier) -> bool {
            if self.error.is_some() {
                self.error = None;
                return true;
            }

            match self.mode {
                Mode::Search => self.handle_search_key(key),
                Mode::CreateSession => self.handle_create_key(key),
            }
        }

        fn handle_search_key(&mut self, key: KeyWithModifier) -> bool {
            match key.bare_key {
                BareKey::Char(character) if key.has_no_modifiers() => {
                    if character == '\n' {
                        self.activate_selected_row();
                    } else {
                        self.search_term.push(character);
                        self.rebuild_rows();
                    }
                    true
                },
                BareKey::Backspace if key.has_no_modifiers() => {
                    self.search_term.pop();
                    self.rebuild_rows();
                    true
                },
                BareKey::Enter if key.has_no_modifiers() => {
                    self.activate_selected_row();
                    true
                },
                BareKey::Down if key.has_no_modifiers() => {
                    self.move_selection_down();
                    true
                },
                BareKey::Up if key.has_no_modifiers() => {
                    self.move_selection_up();
                    true
                },
                BareKey::Esc if key.has_no_modifiers() => {
                    hide_self();
                    true
                },
                BareKey::Char('c') if key.has_modifiers(&[KeyModifier::Ctrl]) => {
                    hide_self();
                    true
                },
                _ => false,
            }
        }

        fn handle_create_key(&mut self, key: KeyWithModifier) -> bool {
            match key.bare_key {
                BareKey::Char(character) if key.has_no_modifiers() => {
                    if character == '\n' {
                        self.create_session();
                    } else {
                        self.new_session_name.push(character);
                    }
                    true
                },
                BareKey::Backspace if key.has_no_modifiers() => {
                    self.new_session_name.pop();
                    true
                },
                BareKey::Enter if key.has_no_modifiers() => {
                    self.create_session();
                    true
                },
                BareKey::Esc if key.has_no_modifiers() => {
                    self.mode = Mode::Search;
                    self.new_session_name.clear();
                    true
                },
                BareKey::Char('c') if key.has_modifiers(&[KeyModifier::Ctrl]) => {
                    self.mode = Mode::Search;
                    self.new_session_name.clear();
                    true
                },
                _ => false,
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

        fn activate_selected_row(&mut self) {
            match self.rows.get(self.selected_index) {
                Some(Row::Session(session_index)) => {
                    let session = &self.sessions[*session_index];
                    if session.is_current_session {
                        self.error = Some(format!("Already in session '{}'", session.name));
                    } else {
                        switch_session_with_focus(&session.name, None, None);
                        self.search_term.clear();
                        hide_self();
                    }
                },
                Some(Row::CreateNewSession) | None => {
                    self.mode = Mode::CreateSession;
                    self.new_session_name = self.search_term.trim().to_owned();
                },
            }
        }

        fn create_session(&mut self) {
            let session_name = self.new_session_name.trim().to_owned();
            if let Some(error) = validate_new_session_name(&session_name, &self.sessions) {
                self.error = Some(error);
                return;
            }

            switch_session(Some(&session_name));
            self.mode = Mode::Search;
            self.search_term.clear();
            self.new_session_name.clear();
            hide_self();
        }

        fn render_centered(&self, terminal_rows: usize, terminal_columns: usize) {
            let width = terminal_columns.saturating_sub(4).min(86).max(32);
            let max_list_rows = terminal_rows.saturating_sub(9).min(14);
            let list_rows = self.rows.len().min(max_list_rows).max(1);
            let height = list_rows + 7;
            let column = terminal_columns.saturating_sub(width) / 2;
            let row = terminal_rows.saturating_sub(height) / 2;

            clear_screen();
            print_line(row, column, &border_top(width));
            print_blank_line(row + 1, column, width);
            match self.mode {
                Mode::Search => {
                    let prompt = format!("Search: {}_", self.search_term);
                    print_box_line(row + 2, column, width, &prompt);
                },
                Mode::CreateSession => {
                    let prompt = format!("New session name: {}_", self.new_session_name);
                    print_box_line(row + 2, column, width, &prompt);
                },
            }
            print_blank_line(row + 3, column, width);

            match self.mode {
                Mode::Search => {
                    self.render_search_rows(row + 4, column, width, max_list_rows);
                },
                Mode::CreateSession => {
                    print_box_line(row + 4, column, width, "Enter to create, Esc to cancel");
                },
            }

            let help_row = row + height.saturating_sub(2);
            if let Some(error) = &self.error {
                print_box_line(help_row, column, width, error);
            } else {
                let help = match self.mode {
                    Mode::Search => "Up/Down select  Enter open  Esc close",
                    Mode::CreateSession => "Enter create  Esc back",
                };
                print_box_line(help_row, column, width, help);
            }
            print_line(row + height.saturating_sub(1), column, &border_bottom(width));
        }

        fn render_search_rows(&self, first_row: usize, column: usize, width: usize, max_rows: usize) {
            if self.rows.is_empty() {
                print_box_line(first_row, column, width, "Create new session");
                return;
            }

            let visible_range = visible_row_range(self.selected_index, self.rows.len(), max_rows);
            for (screen_row, row_index) in visible_range.enumerate() {
                let marker = if row_index == self.selected_index { ">" } else { " " };
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
                print_box_line(first_row + screen_row, column, width, &label);
            }
        }
    }

    fn sessions_to_entries(sessions: Vec<SessionInfo>) -> Vec<SessionEntry> {
        sessions
            .into_iter()
            .map(|session| SessionEntry {
                name: session.name,
                is_current_session: session.is_current_session,
            })
            .collect()
    }

    fn print_line(row: usize, column: usize, text: &str) {
        print!("\u{1b}[{};{}H{}", row + 1, column + 1, text);
    }

    fn print_blank_line(row: usize, column: usize, width: usize) {
        print_box_line(row, column, width, "");
    }

    fn print_box_line(row: usize, column: usize, width: usize, text: &str) {
        let inner_width = width.saturating_sub(4);
        let text = truncate_to_width(text, inner_width);
        let padding = inner_width.saturating_sub(text.chars().count());
        print_line(row, column, &format!("| {}{} |", text, " ".repeat(padding)));
    }
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
