use sessions::{
    Mode, Row, SessionEntry, SessionSwitcher, SwitcherAction, SwitcherInput,
};

fn session(name: &str, is_current_session: bool) -> SessionEntry {
    SessionEntry {
        name: name.to_owned(),
        is_current_session,
    }
}

#[test]
fn permission_grant_requests_rename_and_refresh() {
    let mut switcher = SessionSwitcher::default();

    assert!(!switcher.permissions_granted());
    assert_eq!(
        switcher.grant_permissions(),
        vec![
            SwitcherAction::RenamePluginPane,
            SwitcherAction::RefreshSessions
        ]
    );
    assert!(switcher.permissions_granted());
}

#[test]
fn search_enter_switches_to_selected_session_and_hides_plugin() {
    let mut switcher = SessionSwitcher::default();
    switcher.grant_permissions();
    switcher.set_sessions(vec![
        session("config", true),
        session("ferrous-main", false),
        session("notes", false),
    ]);

    for character in "fer".chars() {
        switcher.handle_input(SwitcherInput::Character(character));
    }

    assert_eq!(switcher.search_term(), "fer");
    assert_eq!(switcher.rows(), &[Row::Session(1), Row::CreateNewSession]);
    assert_eq!(
        switcher.handle_input(SwitcherInput::Enter),
        vec![
            SwitcherAction::SwitchSession("ferrous-main".to_owned()),
            SwitcherAction::Hide
        ]
    );
    assert_eq!(switcher.search_term(), "");
}

#[test]
fn create_row_prompts_with_search_term_and_creates_session() {
    let mut switcher = SessionSwitcher::default();
    switcher.grant_permissions();
    switcher.set_sessions(vec![session("config", true)]);

    for character in "scratch".chars() {
        switcher.handle_input(SwitcherInput::Character(character));
    }

    assert_eq!(switcher.rows(), &[Row::CreateNewSession]);
    assert_eq!(switcher.handle_input(SwitcherInput::Enter), Vec::new());
    assert_eq!(switcher.mode(), Mode::CreateSession);
    assert_eq!(switcher.new_session_name(), "scratch");
    assert_eq!(
        switcher.handle_input(SwitcherInput::Enter),
        vec![
            SwitcherAction::CreateSession("scratch".to_owned()),
            SwitcherAction::Hide
        ]
    );
    assert_eq!(switcher.mode(), Mode::Search);
    assert_eq!(switcher.new_session_name(), "");
}

#[test]
fn rename_current_session_returns_rename_action_and_hides_plugin() {
    let mut switcher = SessionSwitcher::default();
    switcher.grant_permissions();
    switcher.set_sessions(vec![
        session("config", true),
        session("ferrous-main", false),
    ]);

    assert_eq!(
        switcher.handle_input(SwitcherInput::RenameCurrentSession),
        Vec::new()
    );
    assert_eq!(switcher.mode(), Mode::RenameSession);
    assert_eq!(switcher.rename_session_name(), "config");

    for _character in 0.."config".len() {
        switcher.handle_input(SwitcherInput::Backspace);
    }
    for character in "dotfiles".chars() {
        switcher.handle_input(SwitcherInput::Character(character));
    }

    assert_eq!(
        switcher.handle_input(SwitcherInput::Enter),
        vec![
            SwitcherAction::RenameSession("dotfiles".to_owned()),
            SwitcherAction::Hide
        ]
    );
    assert_eq!(switcher.mode(), Mode::Search);
}

#[test]
fn rename_mode_rejects_duplicate_session_name() {
    let mut switcher = SessionSwitcher::default();
    switcher.grant_permissions();
    switcher.set_sessions(vec![
        session("config", true),
        session("ferrous-main", false),
    ]);
    switcher.handle_input(SwitcherInput::RenameCurrentSession);
    for _character in 0.."config".len() {
        switcher.handle_input(SwitcherInput::Backspace);
    }
    for character in "ferrous-main".chars() {
        switcher.handle_input(SwitcherInput::Character(character));
    }

    assert_eq!(switcher.handle_input(SwitcherInput::Enter), Vec::new());
    assert_eq!(
        switcher.error(),
        Some("Session 'ferrous-main' already exists")
    );
}

#[test]
fn pending_rename_mode_opens_after_sessions_arrive() {
    let mut switcher = SessionSwitcher::default();
    switcher.open_rename_session_when_ready();

    switcher.grant_permissions();
    switcher.set_sessions(vec![session("config", true)]);

    assert_eq!(switcher.mode(), Mode::RenameSession);
    assert_eq!(switcher.rename_session_name(), "config");
}

#[test]
fn denied_permissions_block_session_actions() {
    let mut switcher = SessionSwitcher::default();
    switcher.set_sessions(vec![session("config", false)]);
    switcher.deny_permissions();

    assert_eq!(switcher.error(), Some("Session permissions denied"));
    switcher.handle_input(SwitcherInput::Character('x'));
    assert_eq!(switcher.error(), None);

    assert_eq!(switcher.handle_input(SwitcherInput::Enter), Vec::new());
    assert_eq!(
        switcher.error(),
        Some("Session permissions are not granted yet")
    );
}
