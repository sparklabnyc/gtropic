# Getting help with gtropic

Thanks for using gtropic! Before filing an issue, there are a few places to
explore and pieces to put together to make the process as smooth as possible.

## Read the documentation

The [package website](https://sparklabnyc.github.io/gtropic/) has a reference
page for every function and articles on getting started, filtering, linkage,
plotting, and versions and caching. `?gtropic_known_limitations` lists known
limitations of the published data.

## Make a reprex

Start by making a minimal **repr**oducible **ex**ample using the
[reprex](https://reprex.tidyverse.org/) package. Keep the query small (one
storm, one year) so others can run it quickly, and include the output of
`gtropic_manifest()` for the result: it lists the exact dataset versions and
files used.

## Where to ask?

*   If it's a question about using gtropic or R: start with the
    [Posit Community](https://forum.posit.co/) forum or Stack Overflow.

*   If it's a bug in the package: you're in the right place,
    [file an issue](https://github.com/sparklabnyc/gtropic/issues/new/choose).

*   If a value in the published G-TROPIC data looks wrong: use the
    **Data issue** template, so it reaches the dataset maintainers.

*   If it's a security problem: do not open an issue; see
    [SECURITY.md](SECURITY.md).

Before opening a new issue, be sure to
[search issues and pull requests](https://github.com/sparklabnyc/gtropic/issues)
to make sure the bug hasn't been reported and/or already fixed in the
development version.

## What happens next?

gtropic is maintained by a small research team, so please don't worry if you
don't get an immediate response. A good reprex makes it much more likely that
we can reproduce and fix the problem.
