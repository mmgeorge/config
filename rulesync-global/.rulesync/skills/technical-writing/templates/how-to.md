# How-To Guide Profile

*Read when writing a runbook, an incident procedure, migration steps, or another guide to one task.*

## Main Claim

A reader who meets the stated prerequisites and follows the steps in order reaches one specific result.

### Example main claims

These examples state the result a procedure must produce. Use them as alternatives to adapt, not a checklist.

- "These steps rotate production credentials without interrupting service."
- "This procedure restores the database to a selected recovery point."
- "Following these steps removes the deployment and its resources without affecting other applications."

## Sections

Write these sections in order.

### 1. Result

- Say exactly what the reader has when they finish, such as "The production database uses a new password, with no downtime."

### 2. Before you start

- List the tools, versions, and environment variables the reader needs.
- List the permissions or roles they need.
- Warn about any step that deletes data or cannot be undone, before the steps begin.

### 3. Steps

- Order the steps so each one depends only on the steps before it.
- Give each step one action, starting with a verb such as "Deploy," "Export," or "Verify."
- Give exact commands, paths, and settings the reader can copy. Put optional flags and notes under the command.

### 4. Checks

- After each important step, show the expected output, status code, or file, and a command that confirms it.

### 5. Platform differences

- Handle differences, such as macOS and Linux paths, inside the step where they occur.

### 6. Troubleshooting

- For each expected failure, give the exact error text, its cause, and the command that fixes it.

## Leave out

- Do not include design history or theory. Link an explanation instead.
- Do not offer several ways to do the task. Document the one tested procedure.

## Example

A shortened guide:

> This guide changes the production database password without downtime. You need the `dbadmin` role and version 2.4 or later of `dbctl`. Step 4 revokes the old password and cannot be undone.
>
> 1. Create the new password with `dbctl rotate app`.
> 2. Confirm that the new password works. `dbctl check app` prints `ok: app connected with new credentials`.
> 3. Restart the app servers one at a time with `deploy restart app --rolling`.
> 4. Revoke the old password with `dbctl revoke app --old`.
>
> If step 3 fails with `password authentication failed`, the servers are still reading the old password. Run `deploy restart app --rolling --refresh-secrets`, then repeat the check in step 2.
