# Portal Items and Resources

*When to read this:* before searching, resolving, inspecting, sharing, deleting,
or modifying generic Portal items and their resources.

## Search

```sh
portal item list --portal <alias> --me
portal item any list --portal <alias> --name parcels
portal item any list --portal <alias> --public --tags county,boundaries
```

List filters include `--name`, `--content`, `--owner`, `--tags`,
`--categories`, `--page`, `--me`, and `--public`. `--me` and `--owner` are
mutually exclusive.

With no filters, generic item listing authenticates and defaults to the
signed-in owner. Results paginate in groups of 100.

## Resolve identifiers

- A 32-character hexadecimal value is treated as an item ID.
- Any other value is an exact, case-sensitive title.
- Ambiguous exact titles fail.
- Missing titles can print fuzzy suggestions.
- A raw ID can bypass the item-type restriction of a typed namespace.

Use `list` to discover an ID, then use that ID for every subsequent command.

## Inspect

```sh
portal item any meta <item-id> --portal <alias>
portal item any data <item-id> --portal <alias>
portal item any about <item-id> --portal <alias>
portal item any resource list <item-id> --portal <alias>
portal item any resource list <item-id> --portal <alias> --full
portal item any open <item-id> --portal <alias>
```

`meta` prints item metadata. `data` prints the `/data` JSON. `about` prints the
description. `open` opens the Portal item page.

## Manage resources

`resource` and `file` are aliases:

```sh
portal item any resource get <item-id> path/file.json --portal <alias>
portal item any resource set <item-id> path/file.json '{"enabled":true}' \
  --portal <alias>
portal item any resource download <item-id> path/file.bin \
  --portal <alias> --out ./file.bin
portal item any resource upload <item-id> ./assets --portal <alias>
portal item any resource delete <item-id> path/file.json --portal <alias>
```

- `get` prints text or JSON and reports the byte size of binary resources.
- `set` creates or updates a text resource.
- Directory upload recurses and updates existing matching resource paths.
- Downloads can overwrite local paths without prompting.
- Resource replacement and deletion do not prompt.

List resources before and after mutation. Confirm the exact resource path
before overwrite or deletion.

## Share and delete

```sh
portal item any share <item-id> --portal <alias> --level owner
portal item any share <item-id> --portal <alias> --level org
portal item any share <item-id> --portal <alias> --level public
portal item any delete <item-id> --portal <alias>
```

Sharing defaults to `public` when `--level` is omitted. Always specify the
level.

Deletion is permanent and does not prompt. The command can print `[Failed]`
while still producing success-shaped completion output. Verify deletion with an
explicit metadata lookup or list search.

