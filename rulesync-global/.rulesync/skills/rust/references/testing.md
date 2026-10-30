# Testing

Read before adding, moving, or removing tests.

## Test owned behavior

Keep a test only when it protects a concrete runtime, safety, persistence, or ownership failure that compilation misses. State that regression when selecting coverage.

Use isolated unit tests for algorithms, parsers, codecs, state machines, numerical invariants, and error decisions. Mock external boundaries when isolation proves the owned logic.

Use real production modules for integration coverage of lifecycle, persistence, concurrent handoff, plugins, rendering, and host/library boundaries.

Do not write tests that restate assignments, metadata, constructors, or a dependency's documented derive behavior. Test the library-owned behavior that consumes those derives.

## Place tests at their boundary

Keep unit tests in the inline `#[cfg(test)]` module of the file they cover.

Keep crate-private integration workflows in the owning module's test module, so they exercise production composition without exposing implementation details. Use the crate's `tests/` directory for public contracts and multi-crate boundaries. Follow the existing fixture layout.
