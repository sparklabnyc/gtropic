# Contributing to gtropic

This outlines how to propose a change to gtropic. For general advice on
contributing to R packages, see the tidyverse
[development contributing guide](https://rstd.io/tidy-contrib) and the
[code review principles](https://code-review.tidyverse.org/).

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the
documentation directly using the GitHub web interface, as long as the changes
are made in the _source_ file. This generally means editing the
[roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an
`.R` file, not an `.Rd` file, `README.Rmd` rather than `README.md`, and
`vignettes/*.Rmd.orig` rather than the generated `vignettes/*.Rmd`. The first
line of each generated file says where it comes from.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue
and make sure someone from the team agrees that it's needed. If you've found a
bug, please file an issue that illustrates the bug with a minimal
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you
write a unit test, if needed). Problems in the published G-TROPIC data, rather
than in the package, have their own issue template.

**No change to the exported API without discussion.** Renaming, removing or
re-defaulting an exported function, argument or `gtropic.*` option, and any
change to the data a query returns, needs an agreed issue first.

### Development setup

*   Fork the package and clone onto your computer. If you haven't done this
    before, we recommend using
    `usethis::create_from_github("sparklabnyc/gtropic", fork = TRUE)`.

*   Install all development dependencies with
    `pak::local_install_dev_deps()`, and then make sure the package passes
    `R CMD check` by running `devtools::check()`. If `R CMD check` doesn't pass
    cleanly, it's a good idea to ask for help before continuing.

*   Install the [pre-commit](https://pre-commit.com) hooks once per clone:
    `pip install pre-commit && pre-commit install`. They format R code with
    Air, keep `man/` and `NAMESPACE` in sync with roxygen, and catch large
    files, non-portable file names and committed secrets. The same hooks run
    on every pull request.

### Pull request process

*   Create a Git branch for your pull request (PR). We recommend using
    `usethis::pr_init("brief-description-of-change")`.

*   Make your changes, run `devtools::document()` if you changed roxygen
    comments and `devtools::build_readme()` if you changed `README.Rmd`, and
    check that the package passes `R CMD check` by running `devtools::check()`
    again. The checks on GitHub treat any NOTE on Ubuntu as a failure.
    Commit the changes to git, and then create a PR by running
    `usethis::pr_push()`, and follow the prompts in your browser. The title of
    your PR should briefly describe the change. The body of your PR should
    contain `Fixes #issue-number`.

*   For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just
    below the first header). Follow the style described in
    <https://style.tidyverse.org/news.html>.

*   If you change `DESCRIPTION` or `inst/CITATION`, regenerate `CITATION.cff`
    with `cffr::cff_write(gh_keywords = FALSE, dependencies = FALSE)`; a
    workflow checks that it is up to date.

### Code style

*   New code should follow the tidyverse [style guide](https://style.tidyverse.org).
    Format it with [Air](https://posit-dev.github.io/air/) (`air format .`;
    settings in `air.toml`); the format-check workflow fails otherwise. Please
    don't restyle code that has nothing to do with your PR.

*   `lintr::lint_package()` must report nothing (settings in `.lintr`). Use
    `cli` for messages, never `print()` or `cat()`, and never pass a pasted or
    user-supplied string as the first argument of a `cli::cli_*()` function
    (`tests/testthat/test-cli-safety.R` explains why).

*   We use [roxygen2](https://cran.r-project.org/package=roxygen2), with
    [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html),
    for documentation.

### Tests

*   We use [testthat](https://cran.r-project.org/package=testthat) for unit
    tests. Contributions with test cases included are easier to accept. Tests
    must not use the network or write outside `tempdir()`:
    `tests/testthat/setup.R` sandboxes the user directories, and the Dataverse
    API is served from recordings (`tests/testthat/dv/`, made by
    `data-raw/record-http-mocks.R`) or local `webfakes` servers.

*   Snapshot tests (`tests/testthat/_snaps/`) record printed output and error
    messages. When a change is intended, review it with
    `testthat::snapshot_review()` and accept only what you meant to change.

*   `tests/testthat/test-golden-assembly.R` checks that the tables a query
    returns do not change. Its stored results are updated only for a
    deliberate change to returned data (`GTROPIC_UPDATE_GOLDEN=true`), which
    needs an agreed issue.

*   The live tests in `tests/testthat/test-live-dataverse.R` query Harvard
    Dataverse and run only with `GTROPIC_LIVE_TESTS=true`
    (`GTROPIC_LIVE_TESTS=true Rscript -e 'devtools::test(filter = "live")'`).
    They also run every week on GitHub.

### Vignettes, README and figures

The vignettes are precomputed, because their queries download data. Edit
`vignettes/*.Rmd.orig` (or `vignettes/articles/*.Rmd.orig`), never the
generated `.Rmd` files or figures, then regenerate. This needs the network,
the `optipng` command-line tool and the R packages `ragg` and `magick`; the
plot gallery's admin1 map also needs `rnaturalearthhires` from
<https://ropensci.r-universe.dev>.

```sh
R -e 'devtools::install(upgrade = FALSE)'   # precompute.R uses library(gtropic)
export GTROPIC_PRECOMPUTE_CACHE="$(mktemp -d)"  # a throwaway cache, not yours
Rscript -e 'source("vignettes/precompute.R")'
Rscript -e 'devtools::build_readme()'
Rscript data-raw/check-figure-quality.R      # every figure must be lossless
```

## Code of Conduct

Please note that the gtropic project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
