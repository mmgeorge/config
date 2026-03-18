# Documentation
When using libraries, always search for up to date documentation.

When searching for documentation, or when finding new crates or libraries, *always* do the following:
- For esri internal libraries (names that start with arcgis, esri, geoanalytics, etc.), use github-mcp-enterprise which will allow you to search these repositories, discussions, issues, and pull requests.
- For rust code, use the docs-mcp server to find crates and search docs. Use github-mcp-server when you need to view source code, search issues, or pull requests.
- For *all* other code types, directly use github-mcp-server. Locate relevant doc or source code to ensure up to date. See if there are relevant issues or discussions to the task at hand.
- **IMPORTANTLY** if you are unable to access a repo with github-mcp-server try github-mcp-enterprise, and visa versa.

Spawn subagents when appropriate for documentation search.

# Command-line JSON processing
When working with JSON or structured command output in shell commands, prefer using `jq` directly instead of writing short Python scripts, unless Python is clearly necessary.
