# Security policy

## Supported versions

Security fixes are made for the latest release of gtropic. Until the first
release, that is the development version on the `main` branch.

## Reporting a vulnerability

Please do not report security problems in public issues. Use GitHub's private
vulnerability reporting instead: on the repository's **Security** tab, choose
**Report a vulnerability**
(<https://github.com/sparklabnyc/gtropic/security/advisories/new>).

Please include what you found, how to reproduce it (for example the manifest,
server response or cache file that triggers it), and what an attacker could
do with it.

## What to expect

We acknowledge every report within 7 days and keep you informed while we
investigate. When a fix is ready we publish it in a release, with a GitHub
security advisory that credits you unless you prefer otherwise.

## Scope

gtropic downloads files from a Dataverse server, caches them on disk, and
shows their contents in the R console and in interactive HTML maps. Relevant
reports include code execution or file access caused by a crafted manifest,
server response or cache file; disclosure of the `DATAVERSE_KEY` token; and
script injection in interactive maps. Checksums published by the server
detect corrupted or changed files but do not authenticate the publisher (see
`?gtropic_manifest`). Errors in the published data belong in the data issue
template, and problems in a dependency belong with that dependency.
