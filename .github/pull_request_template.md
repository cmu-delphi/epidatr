### Checklist

Please:

- [ ] Make sure this PR is against "dev", not "main" (unless this is a release
      PR).
- [ ] Request a review from one of the current epidatr main reviewers:
      brookslogan, dshemetov, nmdefries, dsweber2.
- [ ] Don't bump the version in `DESCRIPTION`. CI adds a patch bump after
      merge. Label the PR `version:minor` or `version:major` if it needs a
      bigger bump.
- [ ] Describe changes made in NEWS.md, making sure breaking changes
      (backwards-incompatible changes to the documented interface) are noted.
      Collect the changes under the heading for the next release.
- [ ] If you changed any vignette (`vignettes/*.Rmd.orig`), re-knit with `make
      vignettes` and commit the regenerated `.Rmd` files and figures alongside
      the `.Rmd.orig` sources. See DEVELOPMENT.md.

### Change explanations for reviewer

### Magic GitHub syntax to mark associated Issue(s) as resolved when this is merged into the default branch

- Resolves #{issue number}
