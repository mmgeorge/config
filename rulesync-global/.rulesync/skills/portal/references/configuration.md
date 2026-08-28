# Portal Configuration

*When to read this:* before adding, switching, removing, or cleaning Portal
configuration, authenticating, or relying on a selected item.

## Commands

```sh
portal config add
portal config show
portal config switch <alias>
portal config remove <alias>
portal config path
portal config clean
```

`config add` interactively asks for the Portal URL, alias, and optional
username. A blank username configures anonymous public access. A username
triggers password authentication and token caching.

Portal URLs receive `https://` when omitted and normalize away a trailing
`/sharing/rest`. Aliases cannot contain whitespace.

## Persisted state

The CLI stores:

- Configured Portal URLs and the active alias.
- Cached authentication tokens.
- Selected items per Portal alias and item type: `any`, `feature`, `webmap`,
  and `parquet`.
- Locally tracked Feature Layer export jobs.

Configuration and token files use owner-only permissions. `portal config path`
prints the configuration path.

## Alias rules

Every `portal item` command supports:

```sh
--portal <alias>
```

The option chooses one configured Portal for that command without changing the
active alias. Top-level `portal job` commands use the active alias and do not
accept `--portal`, so switch explicitly before inspecting jobs from another
Portal.

## Authentication behavior

- Public metadata, data, and resources are attempted anonymously.
- Authentication begins after authorization failures.
- A valid cached token is reused only for its matching Portal.
- Invalid or expired cached authentication falls back to interactive sign-in.
- Noninteractive execution fails when configuration or credentials require a
  prompt.

Use `--me` for authenticated current-user searches. `--owner <name>` alone can
run anonymously and omit private items.

## Selection safety

Commands with optional `[id_or_name]` may use a persisted selection:

```sh
portal item feature select <item-id> --portal <alias>
portal item feature meta --portal <alias>
```

Selections are convenient for interactive exploration but unsafe for
automation and destructive actions. Pass the item ID directly for share,
delete, resource mutation, clone, export, cache, and multiscale commands.

## Destructive configuration

- `config remove` deletes the alias and its cached token.
- `config clean` deletes the entire configuration file and cached Portal
  tokens. This also removes Portal aliases, selections, tracked export jobs,
  Azure storage aliases, selected Azure containers, and selected local tool
  sources stored in that configuration. The separately stored Azure CDN route
  cache remains.
- Neither command prompts for confirmation.

Show the resolved configuration path and request confirmation before cleaning
state.
