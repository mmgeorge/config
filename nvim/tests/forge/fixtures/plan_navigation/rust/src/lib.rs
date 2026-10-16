pub mod model;
use model::{Mode, Planned};
use tiny_value::External;

pub fn run(value: Planned, external: External) -> usize {
    let _mode = Mode::Ready;
    value.count + external.count()
}
