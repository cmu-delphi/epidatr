# Developer tasks for epidatr. Run `just` to list them.
# The release recipes follow the checklist in DEVELOPMENT.md "Release process".

# Variables are only computed by the recipes that use them (e.g. the CRAN lookup).
set lazy

pkg_version := `sed -n 's/^Version: //p' DESCRIPTION`
submitted_version := `sed -n 's/^Version: //p' CRAN-SUBMISSION`
submitted_sha := `sed -n 's/^SHA: //p' CRAN-SUBMISSION`
cran_version := ```
    Rscript --vanilla -e 'cat(available.packages(repos = "https://cloud.r-project.org")["epidatr", "Version"])' 2>/dev/null \
        || { echo "Could not look up the epidatr version on CRAN." >&2; exit 1; }
```
extras_dir := "../.epidatr-release-extras"
just := quote(just_executable())

[private]
default:
    @{{ just }} --list

# ---- Develop ----

# Install the R packages these recipes and the package itself need.
[group("develop")]
install:
    Rscript -e "install.packages(c('pak', 'devtools', 'pkgdown', 'webshot', 'textshaping', 'lintr', 'urlchecker', 'foghorn'));devtools::install_deps(dependencies = TRUE);pak::local_install_deps(dependencies = 'Config/Needs/precompile')"

[group("develop")]
lint:
    Rscript -e "devtools::load_all();lintr::lint_package()"

[group("develop")]
format:
    air format .

[group("develop")]
document:
    Rscript -e "devtools::document()"

[group("develop")]
build: document
    Rscript -e "devtools::build()"

[group("develop")]
check:
    Rscript -e "devtools::check(args = c('--no-manual', '--as-cran'), error_on = 'warning')"

# Format, lint, and check.
[group("develop")]
chores: format lint check

# Re-knit vignettes/*.Rmd.orig (live API calls) into the committed static .Rmd; all of them unless named.
[group("develop")]
vignettes *names:
    Rscript -e "devtools::install(quick = TRUE, upgrade = FALSE)"
    Rscript vignettes/precompile.R {{ names }}

# Print the R version each Imports package (and its dependencies) requires on CRAN, highest first.
[group("develop")]
min-r:
    #!/usr/bin/env Rscript
    db <- tools::CRAN_package_db()
    desc <- read.dcf("DESCRIPTION", fields = c("Depends", "Imports"))
    split_deps <- function(x) trimws(sub("\\(.*", "", strsplit(x, ",")[[1]]))
    imports <- setdiff(split_deps(desc[, "Imports"]), "")
    base <- rownames(installed.packages(priority = "base"))
    pkgs <- unique(c(imports, unlist(tools::package_dependencies(imports, db, recursive = TRUE))))
    pkgs <- setdiff(pkgs, c("R", base))
    min_r <- vapply(db$Depends[match(pkgs, db$Package)], function(d) {
      m <- regmatches(d, regexpr("R \\(>= *[0-9.]+\\)", d))
      if (length(m)) gsub("[^0-9.]", "", sub("R ", "", m)) else NA_character_
    }, "")
    out <- data.frame(package = pkgs, direct = pkgs %in% imports, min_r = unname(min_r))
    out <- out[order(numeric_version(out$min_r), decreasing = TRUE, na.last = TRUE), ]
    print(out, row.names = FALSE)
    cat("\nDESCRIPTION declares:", trimws(desc[, "Depends"]), "\n")

# ---- Test ----

[group("test")]
test:
    Rscript -e "devtools::test()"

# Run the live API tests. --pvt adds pvt_* endpoints (needs a key with private access); --cast-url points cast tests at another server.
[arg("pvt", long, value="TRUE")]
[arg("cast_url", long="cast-url")]
[group("test")]
test-live pvt="FALSE" cast_url="":
    EPIDATR_LIVE_TEST=TRUE EPIDATR_TEST_PVT={{ pvt }} EPIDATR_CAST_BASE_URL={{ quote(cast_url) }} Rscript -e "devtools::test(filter = 'live')"

# Run only the live cast-API tests, e.g. `just test-live-cast --cast-url http://localhost:8005/epidata/v5/`.
[arg("cast_url", long="cast-url")]
[group("test")]
test-live-cast cast_url="":
    EPIDATR_LIVE_TEST=TRUE EPIDATR_CAST_BASE_URL={{ quote(cast_url) }} Rscript -e "devtools::test(filter = 'live-cast')"

[group("test")]
update-fixtures:
    Rscript data-raw/update_fixtures.R

[group("test")]
coverage:
    Rscript -e "devtools::test_coverage()"

# ---- Release ----

# Bump the DESCRIPTION version: patch, minor, or major.
[group("release")]
bump level="patch":
    .github/scripts/bump-version.sh {{ level }}

# Compare the local, last-submitted, and published versions, and summarize CRAN checks.
[group("release")]
cran-status:
    @echo "DESCRIPTION:     {{ pkg_version }}"
    @echo "CRAN-SUBMISSION: {{ submitted_version }} ({{ submitted_sha }})"
    @echo "On CRAN:         {{ cran_version }}"
    @Rscript -e "options(repos = c(CRAN = 'https://cloud.r-project.org', getOption('repos'))); print(foghorn::cran_results(pkg = 'epidatr'))"
    @echo "Details: https://cran.r-project.org/web/checks/check_results_epidatr.html"

# Fail fast on the release mistakes we have made before.
[group("release")]
release-preflight:
    @test -z "$(git status --porcelain)" || { echo "Working tree is not clean; the submitted SHA would not match the tarball."; git status --short; exit 1; }
    @! grep -q '^Remotes:' DESCRIPTION || { echo "Remove Remotes from DESCRIPTION; CRAN does not support it."; exit 1; }
    @test "{{ pkg_version }}" != "{{ cran_version }}" || { echo "DESCRIPTION version {{ pkg_version }} is already on CRAN; run 'just bump'."; exit 1; }
    @echo "Preflight OK for epidatr {{ pkg_version }}."

# CRAN-like local check, including incoming feasibility checks and the PDF manual.
[group("release")]
check-cran:
    Rscript -e "devtools::check(manual = TRUE, remote = TRUE, incoming = TRUE, env_vars = c(NOT_CRAN = 'false'), error_on = 'warning')"

[group("release")]
urlcheck:
    Rscript -e "urlchecker::url_check()"

[group("release")]
readme:
    Rscript -e "devtools::build_readme()"

# Everything local that should pass before submitting.
[group("release")]
release-check: (_stashed "release-preflight document urlcheck readme check-cran")

# Remote Windows checks; results are emailed to the maintainer.
[group("release")]
check-win:
    Rscript -e "devtools::check_win_devel(quiet = TRUE); devtools::check_win_release(quiet = TRUE)"

# Remote macOS check; results are emailed to the maintainer.
[group("release")]
check-mac:
    Rscript -e "devtools::check_mac_release(quiet = TRUE)"

# Run the full OS/R-version matrix on GitHub Actions, e.g. `just check-full-ci dev`.
[group("release")]
check-full-ci branch="main":
    gh workflow run R-CMD-check-full.yaml --ref {{ branch }}

# Submit to CRAN from the tip of main. Leaves the new CRAN-SUBMISSION uncommitted.
[group("release")]
submit: (_stashed "_submit")

# submit_cran() asks confirmation questions, so R runs interactively.
_submit: release-preflight
    git fetch origin main
    @test "$(git rev-parse HEAD)" = "$(git rev-parse origin/main)" || { echo "HEAD is not origin/main; submit from the tip of main."; exit 1; }
    R --interactive --no-save --no-restore -q -e "devtools::submit_cran()"
    @echo "CRAN-SUBMISSION now records this upload. Commit it once CRAN accepts; discard it if CRAN rejects."

# Rewrite CRAN-SUBMISSION by hand, e.g. `just cran-submission 1.4.0 v1.4.0`.
[group("release")]
cran-submission version=pkg_version ref="HEAD":
    @printf 'Version: %s\nDate: %s\nSHA: %s\n' "{{ version }}" "$(date -u '+%Y-%m-%d %H:%M:%S UTC')" "$(git rev-parse {{ ref }}^{commit})" > CRAN-SUBMISSION
    @cat CRAN-SUBMISSION

# After CRAN accepts: tag the submitted commit, push the tag, and publish a GitHub release. Extra flags go to `gh release create`, e.g. `just release --draft`.
[group("release")]
release *gh_flags:
    @test "{{ submitted_version }}" = "{{ cran_version }}" || { echo "CRAN-SUBMISSION ({{ submitted_version }}) is not the version on CRAN ({{ cran_version }}); has it been accepted?"; exit 1; }
    git tag -a v{{ submitted_version }} {{ submitted_sha }} -m "epidatr {{ submitted_version }}"
    git push origin v{{ submitted_version }}
    gh release create v{{ submitted_version }} --verify-tag --generate-notes --title "epidatr {{ submitted_version }}" {{ gh_flags }}

# Open the PR that brings release fixes and CRAN-SUBMISSION from main back to dev.
[group("release")]
backmerge:
    gh pr create --base dev --head main --title "chore: merge main into dev after epidatr {{ submitted_version }}" --body "Back-merge after the {{ submitted_version }} CRAN release."

# Run recipes with untracked files (e.g. a local .Renviron) moved out of the way, so they
# don't fail the preflight clean-tree check. They are put back on exit, success or failure.
_stashed +recipes:
    #!/usr/bin/env bash
    set -euo pipefail
    trap '{{ just }} restore-extras' EXIT
    {{ just }} stash-extras
    {{ just }} {{ recipes }}

# Move untracked files out to the parent directory.
[group("release")]
stash-extras:
    #!/usr/bin/env bash
    set -euo pipefail
    git status --porcelain | grep '^?? ' | cut -c4- | while IFS= read -r f; do
        echo "Stashing untracked $f"
        mkdir -p "{{ extras_dir }}/$(dirname "$f")"
        mv "$f" "{{ extras_dir }}/$f"
    done

# Move files stashed by stash-extras back.
[group("release")]
restore-extras:
    #!/usr/bin/env bash
    set -euo pipefail
    [ -d "{{ extras_dir }}" ] || exit 0
    (cd "{{ extras_dir }}" && find . -mindepth 1 \( -type f -o -type l \)) | sed 's|^\./||' | while IFS= read -r f; do
        echo "Restoring $f"
        mkdir -p "$(dirname "$f")"
        mv "{{ extras_dir }}/$f" "$f"
    done
    find "{{ extras_dir }}" -mindepth 1 -type d -empty -delete 2>/dev/null || true
    rmdir "{{ extras_dir }}" 2>/dev/null || true
