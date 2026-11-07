# Disabled slow Lua tests

The Lua runner, `.forge/checks/lua-tests.nu`, kills any fixture that runs longer than five
seconds. A fixture that is correct but cannot finish inside that budget because of launch and
sequencing overhead stays out of the default selection. Each entry in `disabled_fixture_list`
carries the comment `exceeds the 5 s test limit; tracked in nvim/tests/forge/plans/disabled-slow-tests.md`,
and an explicit path still selects the fixture, though it fails when it runs past five seconds.

The durations below come from running each fixture alone, so they exclude the contention of a
parallel run. Every wait in these fixtures is a condition wait on real host work (Harness broker
startup, `git` subprocesses, check commands). None is a fixed sleep, so the time cannot be removed
without splitting the fixture.

| Fixture | Alone | Where the time goes |
| --- | --- | --- |
| `harness_automatic_checks.lua` | 13.7 s | Sequential check scenarios, each starting a Nushell check process through the host. |
| `harness_check_queue_host.lua` | 13.8 s | Sequential check queue scenarios against a real host. |
| `status_staging_host_flows.lua` | 12.1 s | Many staging flows, each running real `git` subprocesses. |
| `harness_tasks_host.lua` | 11.3 s | Sequential task lifecycle scenarios against a real host. |
| `plan_checks_host.lua` | 8.0 s | Plan check scenarios with real check commands. |
| `harness_host.lua` | 7.4 s | Submission race scenarios, each launching its own broker. |
| `semantic_execution_host.lua` | 7.3 s | Semantic execution scenarios against a real host. |
| `harness_checks_host.lua` | 6.9 s | Check cancellation and shutdown scenarios with real processes. |
| `harness_plan_checks.lua` | 6.4 s | Plan check selection against a real host. |
| `plan_review_host.lua` | 6.0 s | Plan review scenarios against a real host. |
| `harness_check_activity_host.lua` | 5.9 s | Check activity scenarios with a compiled fixture program. |
| `harness_message_folds_host.lua` | 5.7 s | Message fold scenarios against a real host. |
| `status_staging_host_edges.lua` | 5.5 s | Staging edge cases with real `git` subprocesses. |
| `harness_replan_host.lua` | 5.4 s | Replan scenarios against a real host. |
| `harness_publication_restart_host.lua` | 4.7 s | Publication restart scenarios with two host launches; exceeds 5 s under any contention. |
| `harness_admission_host.lua` | 4.3 s | Admission scenarios against a real host; exceeds 5 s under any contention. |
| `github_api_examples.lua` | 3.5 s | Compiles two Rust fixture programs with `rustc` before running; exceeds 5 s under any contention. |
| `status_staging_source_changes.lua` | 3.8 s | Stages a 9 MiB file through real `git`; exceeds 5 s under any contention. |
| `plan_review_calls_host.lua` | 3.0 s | Plan review call scenarios against a real host; exceeds 5 s under contention. |
| `harness_approval_resume_host.lua` | 3.3 s | Approval resume scenarios against a real host; exceeds 5 s under contention. |

Re-enable a fixture by removing its entry once it is split into scenarios under five seconds, as
`plan_review_question_response_host.lua` is: the runner lists a fixture that declares
`{ name = "...", run = ... }` scenario rows once per scenario.
