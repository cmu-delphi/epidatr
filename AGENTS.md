# Notes for agents

## NEWS.md

Every user-visible change gets a `NEWS.md` bullet in the same PR. Skip tests,
CI, version bumps and other housekeeping.

- Collect all unreleased changes under the top heading,
  `# epidatr (development version)`. Don't add a heading per patch version:
  the `version-bump` workflow raises `DESCRIPTION` after every merge into
  `dev`, but those are development versions, not releases (the CRAN version is
  the latest release; see "Versioning" in `DEVELOPMENT.md`).
- At release time the maintainer renames that heading to the release version
  (the "polish NEWS" step in `DEVELOPMENT.md`). Don't do it in a feature PR.
- Use the sections `Breaking changes`, `Changes`, `Documentation` and `Patches`,
  and cite the issue or PR number when there is one.

To check that NEWS is complete, compare the log since the last release tag
(`git tag` / `jj tag list`) against the bullets.
