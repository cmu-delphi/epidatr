### Checklist

Please:

- [ ] Make sure this PR is against "dev", not "main" (unless this is a release
      PR).
- [ ] Request a review from one of the current epidatr main reviewers:
      brookslogan, dshemetov, nmdefries, dsweber2.
- [ ] Use a conventional-commit title and describe what changed for users in
      the description; the release notes are generated from them. Don't edit
      `NEWS.md` or the `DESCRIPTION` version (the release PR does both).
- [ ] Apply a label (`enhancement`, `bug`, `documentation`, ...) so the change
      is categorized in the generated release notes. Call out breaking changes
      (backwards-incompatible changes to the documented interface) in the
      description.
- [ ] If you changed any vignette (`vignettes/*.Rmd.orig`), re-knit with `just
      vignettes` and commit the regenerated `.Rmd` files and figures alongside
      the `.Rmd.orig` sources. See DEVELOPMENT.md.
- [ ] Prefer regular merge over squash merge.

### Change explanations for reviewer

### Magic GitHub syntax to mark associated Issue(s) as resolved when this is merged into the default branch

- Resolves #{issue number}
