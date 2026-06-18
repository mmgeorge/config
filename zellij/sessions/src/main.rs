#[cfg(target_arch = "wasm32")]
use sessions::{SessionEntry, SessionSwitcher, SwitcherAction, SwitcherInput};
#[cfg(target_arch = "wasm32")]
use zellij_tile::prelude::*;

#[cfg(not(target_arch = "wasm32"))]
fn main() {}

#[cfg(target_arch = "wasm32")]
#[derive(Default)]
struct SessionsPlugin {
    switcher: SessionSwitcher,
}

#[cfg(target_arch = "wasm32")]
register_plugin!(SessionsPlugin);

#[cfg(target_arch = "wasm32")]
impl ZellijPlugin for SessionsPlugin {
    fn load(&mut self, configuration: std::collections::BTreeMap<String, String>) {
        if configuration.get("rename_session").is_some_and(|value| value == "true") {
            self.switcher.open_rename_session_when_ready();
        }
        request_permission(&[
            PermissionType::ReadApplicationState,
            PermissionType::ChangeApplicationState,
        ]);
        subscribe(&[
            EventType::ModeUpdate,
            EventType::SessionUpdate,
            EventType::Key,
            EventType::Visible,
            EventType::PermissionRequestResult,
        ]);
    }

    fn update(&mut self, event: Event) -> bool {
        match event {
            Event::Key(key) => {
                if let Some(input) = input_from_key(key) {
                    let actions = self.switcher.handle_input(input);
                    self.execute_actions(actions);
                    true
                } else {
                    false
                }
            },
            Event::SessionUpdate(sessions, _resurrectable_sessions) => {
                self.switcher.set_sessions(sessions_to_entries(sessions));
                true
            },
            Event::Visible(is_visible) => {
                let actions = self.switcher.set_visible(is_visible);
                self.execute_actions(actions);
                true
            },
            Event::ModeUpdate(_) => true,
            Event::PermissionRequestResult(permission_status) => {
                match permission_status {
                    PermissionStatus::Granted => {
                        let actions = self.switcher.grant_permissions();
                        self.execute_actions(actions);
                    },
                    PermissionStatus::Denied => self.switcher.deny_permissions(),
                }
                true
            },
            _ => false,
        }
    }

    fn render(&mut self, rows: usize, columns: usize) {
        clear_screen();
        for line in self.switcher.render_lines(rows, columns) {
            print_line(line.row, line.column, &line.text);
        }
    }
}

#[cfg(target_arch = "wasm32")]
impl SessionsPlugin {
    fn execute_actions(&mut self, actions: Vec<SwitcherAction>) {
        for action in actions {
            self.execute_action(action);
        }
    }

    fn execute_action(&mut self, action: SwitcherAction) {
        match action {
            SwitcherAction::Hide => hide_self(),
            SwitcherAction::RenamePluginPane => {
                rename_plugin_pane(get_plugin_ids().plugin_id, "Sessions");
            },
            SwitcherAction::RefreshSessions => self.refresh_sessions(),
            SwitcherAction::SwitchSession(session_name) => {
                switch_session_with_focus(&session_name, None, None);
            },
            SwitcherAction::CreateSession(session_name) => {
                switch_session(Some(&session_name));
            },
            SwitcherAction::RenameSession(session_name) => {
                rename_session(&session_name);
            },
        }
    }

    fn refresh_sessions(&mut self) {
        match get_session_list() {
            Ok(snapshot) => self
                .switcher
                .set_sessions(sessions_to_entries(snapshot.live_sessions)),
            Err(error) => {
                self.switcher
                    .set_error(format!("Could not read sessions: {error}"));
            },
        }
    }
}

#[cfg(target_arch = "wasm32")]
fn input_from_key(key: KeyWithModifier) -> Option<SwitcherInput> {
    match key.bare_key {
        BareKey::Char(character) if key.has_no_modifiers() => {
            Some(SwitcherInput::Character(character))
        },
        BareKey::Backspace if key.has_no_modifiers() => Some(SwitcherInput::Backspace),
        BareKey::Enter if key.has_no_modifiers() => Some(SwitcherInput::Enter),
        BareKey::Down if key.has_no_modifiers() => Some(SwitcherInput::Down),
        BareKey::Up if key.has_no_modifiers() => Some(SwitcherInput::Up),
        BareKey::Esc if key.has_no_modifiers() => Some(SwitcherInput::Escape),
        BareKey::Char('c') if key.has_modifiers(&[KeyModifier::Ctrl]) => {
            Some(SwitcherInput::Cancel)
        },
        BareKey::Char('r') if key.has_modifiers(&[KeyModifier::Ctrl]) => {
            Some(SwitcherInput::RenameCurrentSession)
        },
        _ => None,
    }
}

#[cfg(target_arch = "wasm32")]
fn sessions_to_entries(sessions: Vec<SessionInfo>) -> Vec<SessionEntry> {
    sessions
        .into_iter()
        .map(|session| SessionEntry {
            name: session.name,
            is_current_session: session.is_current_session,
        })
        .collect()
}

#[cfg(target_arch = "wasm32")]
fn print_line(row: usize, column: usize, text: &str) {
    print!("\u{1b}[{};{}H{}", row + 1, column + 1, text);
}
