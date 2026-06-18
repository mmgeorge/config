use sessions::{ScreenLine, SessionEntry, SessionSwitcher, SwitcherInput};

fn session(name: &str, is_current_session: bool) -> SessionEntry {
    SessionEntry {
        name: name.to_owned(),
        is_current_session,
    }
}

fn line_texts(lines: &[ScreenLine]) -> Vec<&str> {
    lines.iter().map(|line| line.text.as_str()).collect()
}

#[test]
fn render_search_popup_contains_input_sessions_create_row_and_help() {
    let mut switcher = SessionSwitcher::default();
    switcher.grant_permissions();
    switcher.set_sessions(vec![
        session("config", true),
        session("ferrous-main", false),
        session("notes", false),
    ]);
    switcher.handle_input(SwitcherInput::Character('f'));

    let lines = switcher.render_lines(24, 80);
    let texts = line_texts(&lines);

    assert!(texts.iter().any(|line| line.contains("Search: f_")));
    assert!(texts.iter().any(|line| line.contains("> ferrous-main")));
    assert!(texts
        .iter()
        .any(|line| line.contains("  Create new session")));
    assert!(texts
        .iter()
        .any(|line| line.contains("Up/Down select  Enter open  Esc close")));
}

#[test]
fn render_create_popup_contains_new_session_prompt() {
    let mut switcher = SessionSwitcher::default();
    switcher.grant_permissions();
    for character in "scratch".chars() {
        switcher.handle_input(SwitcherInput::Character(character));
    }
    switcher.handle_input(SwitcherInput::Enter);

    let lines = switcher.render_lines(24, 80);
    let texts = line_texts(&lines);

    assert!(texts
        .iter()
        .any(|line| line.contains("New session name: scratch_")));
    assert!(texts
        .iter()
        .any(|line| line.contains("Enter to create, Esc to cancel")));
    assert!(texts
        .iter()
        .any(|line| line.contains("Enter create  Esc back")));
}
