# Tutorial Profile

*Read when writing a guided tutorial in which the reader builds a working project to learn a technology.*

## Controlling Claim

Building one working project from start to finish teaches the reader how the main concepts fit together and leaves them with a pattern they can reuse.

## Sections

Write these sections in order.

### 1. What you will build

- Name the project, such as "a task queue worker in Rust that reads jobs from Redis."
- Say what the reader will be able to do afterward.

### 2. Before you start

- Give the exact versions of the tools, compilers, and images the reader needs, and any credentials.
- State the background the reader needs, such as "This tutorial assumes familiarity with async Rust and basic Redis commands."

### 3. Milestones

- Start with the smallest project that runs, and a command that proves it runs.
- Add one feature per milestone, and introduce each concept in the milestone that uses it.
- Give complete code the reader can copy, and end each milestone with a check, such as a test or a `curl` request.

### 4. Final check

- Give one command that runs the whole project, and show its exact expected output.

### 5. Next steps

- Explain how the pattern applies to a production system, and link the overviews, explanations, and references that go further.

## Leave out

- Do not offer alternative paths or configurations. Give the single simplest path that works.
- Do not interrupt the build with full API listings or long theory.

## Example

The opening of a tutorial:

> In this tutorial you build a worker that takes jobs from a Redis list and runs them, and you finish with a pattern you can reuse for any background job. It assumes familiarity with async Rust and basic Redis commands, and it uses Rust 1.80 and Redis 7.
>
> Start with a worker that only connects. Create the project with `cargo new job-worker`, add the `redis` and `tokio` crates, and open a connection to Redis in `main`. When you run `cargo run`, the worker prints `connected to redis://localhost:6379` and waits.
>
> Next, the worker reads a job. A job is a JSON string pushed onto the `jobs` list, and the worker takes one at a time with `BRPOP`, which waits until a job arrives.
