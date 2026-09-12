use std::{env, fs, io::Write};

fn main() {
    let target = env::args().nth(1).expect("missing COMMIT_EDITMSG argument");
    fs::write(
        &target,
        "fixture: editor commit\n\nRecorded by the copied editor fixture.\n",
    )
    .expect("write COMMIT_EDITMSG");
    fs::write(
        "commit-editor-nvim",
        env::var("NVIM").expect("missing NVIM"),
    )
    .expect("record NVIM");
    fs::write("commit-editor-target", target).expect("record editor target");
    println!("editor stdout receipt");
    eprintln!("editor stderr receipt");
    std::io::stdout().flush().expect("flush stdout");
    std::io::stderr().flush().expect("flush stderr");
}
