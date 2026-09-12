# Forge patch to gix-path 0.12.6

This directory contains the registry source of gix-path 0.12.6 under its original
MIT or Apache-2.0 licenses. The workspace patches crates.io to use this copy.

`src/env/mod.rs` exposes `set_system_prefix` and changes the existing process cache
to `OnceLock`. Forge seeds that cache before repository reads with the previously
discovered Windows prefix. All default discovery and configuration/attribute path
derivation remain upstream behavior. The setter rejects relative paths and any
attempt to replace an initialized prefix.

When upgrading gix-path, retain this patch until upstream exposes equivalent prefix
injection. Run Forge's configuration-location and repository tests after upgrades.
