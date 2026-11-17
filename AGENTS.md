# Notes for agents

## Commits and PRs

Use [conventional-commit](https://www.conventionalcommits.org) messages for
commits and PR titles (`feat:`, `fix:`, `docs:`, `chore:`, `ci:`, ...); the
release notes are generated from them. PRs go against `dev`, and a regular
merge is preferred over a squash merge.

## Vignettes

If you change any vignette (`vignettes/*.Rmd.orig`), re-knit with `just
vignettes` and commit the regenerated `.Rmd` files and figures alongside the
`.Rmd.orig` sources. See DEVELOPMENT.md.

## NEWS.md and versions

Feature PRs don't touch `NEWS.md` or the `DESCRIPTION` version. Write a
conventional-commit PR title and a description that says what changed for
users (and flags breaking changes); those, plus the PR labels, are the raw
material for the release notes.

The release PR writes the `# epidatr X.Y.Z` section at the top of `NEWS.md`
(replacing any `# epidatr (development version)` heading) and bumps `DESCRIPTION`.
To write it:

1. Run `just news` to print GitHub's generated notes since the last tag.
2. Edit them into the NEWS style: use the sections `Breaking changes`,
   `Changes`, `Documentation` and `Patches`, drop pure housekeeping (tests, CI,
   version bumps), say what users must do about breaking changes, and cite the
   issue or PR number. Read the PR descriptions (`gh pr view N`) when a title
   isn't enough.

`just release` uses that same NEWS section as the GitHub release body, so the
heading must be exactly `# epidatr X.Y.Z`.
