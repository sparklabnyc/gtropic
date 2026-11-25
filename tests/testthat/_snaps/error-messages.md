# a transport failure names the request and the server

    Code
      abort_dataverse(simpleError("Could not resolve host: example.invalid"), what = "file listing",
      server = "example.invalid")
    Condition
      Error:
      ! Could not complete file listing against "example.invalid".
      x Could not resolve host: example.invalid
      i Common causes: no network connection; the DOI has moved; the requested dataset version does not exist; or the server is down.
      i Check `getOption('gtropic.server')` and the DOI options (see `?gtropic_options`).

# a checksum mismatch names the file and both checksums

    Code
      verify_checksum(path, strrep("0", 32L), label = "2005.parquet")
    Condition
      Error:
      ! Checksum mismatch for '2005.parquet'.
      x Expected "00000000000000000000000000000000" but the download hashed to "900150983cd24fb0d6963f7d28e17f72".
      i The download was likely truncated. Try again; if it persists the published file may have changed.

# an unmatched filter value suggests the closest names

    Code
      match_values("Example Coastt", c("Example Coast", "Example Bay"), arg = "ADM2_NAME")
    Condition
      Error:
      ! 1 value in `ADM2_NAME` did not match any ADM2 unit.
      x "Example Coastt" - did you mean "Example Coast"?
      i See `gtropic_adm2()` for the full list of valid values.

---

    Code
      abort_empty_intersection(c("WHO_ENTITY", "ADM2_NAME"))
    Condition
      Error:
      ! No ADM2 units satisfy all of the geographic filters supplied.
      x Filters in play: `WHO_ENTITY` and `ADM2_NAME`.
      i Values are combined with OR within an argument and AND across arguments, so these conditions must hold together.
      i Use `gtropic_adm2()` to explore valid combinations.

# an unconfirmed large download states its size

    Code
      apply_guardrail(list(bytes = 1024^3, n_files = 2L, n_cached = 0L))
    Condition
      Error:
      ! This query would download about 1.0 GB across 2 files, above the 0.001 GB confirmation threshold.
      i This session is not interactive, so there is no way to ask you to confirm.
      i Pass `confirm = FALSE` to proceed anyway, or raise the threshold with `options(gtropic.confirm_threshold_gb = 1)`.
      i Narrowing `date_range` or dropping "precip" from `tables` is usually the better fix.

# a rejected manifest says which field is wrong

    Code
      gtropic_data(manifest = bad)
    Condition
      Error in `gtropic_data()`:
      ! `manifest` has an invalid label.
      x Offending value: "../outside.parquet".
      i Manifests made by `gtropic_manifest()` always pass this check; this one may have been edited.

# common argument errors in exported functions read well

    Code
      gtropic_manifest(list())
    Condition
      Error in `gtropic_manifest()`:
      ! `x` must be a <gtropic_data> object returned by `gtropic_data()`.
      x Got an empty list.

---

    Code
      gtropic_data(quiet = "yes")
    Condition
      Error in `gtropic_data()`:
      ! `quiet` must be a single `TRUE` or `FALSE` value.
      x Got "yes".

---

    Code
      gtropic_data(tables = "winds")
    Condition
      Error in `gtropic_data()`:
      ! Unknown table in `tables`: "winds".
      i Valid values are "wind", "precip", "flood", "pop", "adm2", "codebook", "zero_pairs", "tracks", "geometry", and "storm_metadata".

