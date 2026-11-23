# Tutorial Profile

*Read when writing a tutorial, runbook, migration guide, or other step-by-step procedure for one task.*

## Goal

Enable a reader who meets the stated prerequisites to follow the steps in order and reach one verified result without guesswork or detours.

## Sections

Write these sections in order.

### Result

- Say exactly what the reader has when they finish, such as "The production database uses a new password, with no downtime."

### Before you start

- List the tools, versions, and environment variables the reader needs.
- List the permissions or roles they need.
- Warn about any step that deletes data or cannot be undone, before the steps begin.

### Steps

- Order the steps so each one depends only on the steps before it.
- Give each step one action, starting with a verb such as "Deploy," "Export," or "Verify."
- Give exact commands, paths, and settings the reader can copy. Put optional flags and notes under the command.

### Checks

- After each important step, show the expected output, status code, or file, and a command that confirms it.

### Platform differences

- Handle differences, such as macOS and Linux paths, inside the step where they occur.

### Troubleshooting

- For each expected failure, give the exact error text, its cause, and the command that fixes it.

## Leave out

- Do not include design history or deep architecture theory. Link an architecture doc instead.
- Do not offer several ways to do the task. Document the one tested procedure.

## Example

````markdown
# Back Up a Docker Volume to a Compressed Archive

Docker stores persistent container data, such as local databases or uploaded assets, inside named volumes managed by the Docker daemon rather than inside your project directory. Upgrading an image or running a schema migration can alter or wipe volume contents with no built-in way to roll back. Archiving a volume to a compressed tar file creates a portable snapshot outside Docker that you can store or restore later. When you finish, you have a verified `app-db-backup.tar.gz` archive of the volume in your current directory.

## Before You Start

- Docker 24 or later (`docker --version`)
- Access to the local Docker daemon (`docker ps`)

Stop any container writing to the volume before running step 2. Archiving a volume while a database is writing to it can produce a corrupt backup.

## Steps

1. **Stop the container attached to the volume** so no files change during the backup:

   ```bash
   docker stop app-db
   ```

   Confirm the container stopped with `docker ps -a --filter name=app-db`. Its status should start with `Exited`.

2. **Archive the volume contents** with a temporary Alpine container that mounts the volume read-only and writes the tar file to your working directory:

   ```bash
   docker run --rm \
     -v app-db-data:/data:ro \
     -v "$(pwd):/backup" \
     alpine tar czf /backup/app-db-backup.tar.gz -C /data .
   ```

   In PowerShell on Windows, replace `"$(pwd):/backup"` with `"${PWD}:/backup"`.

3. **Verify the archive** lists the expected files without extracting them:

   ```bash
   tar tzf app-db-backup.tar.gz | head
   ```

   Confirm the command exits with status `0` and prints the top-level files from the volume.

4. **Restart the container:**

   ```bash
   docker start app-db
   ```

## Troubleshooting

- **`docker: Error response from daemon: invalid volume specification`:** On Windows PowerShell, `$(pwd)` can expand incorrectly or split on spaces. Replace it with `"${PWD}:/backup"` and rerun step 2.
- **The archive is empty (`tar tzf` prints only `./`):** The volume name in `-v` did not match an existing volume, so Docker created a new empty one. Run `docker volume ls` to find the exact name, remove the empty volume with `docker volume rm <wrong-name>`, and rerun step 2.
````
