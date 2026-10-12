# Plot a G-TROPIC retrieval result

Thin S3 interface to
[`gtropic_plot()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_plot.md).

## Usage

``` r
# S3 method for class 'gtropic_data'
plot(x, type = "exposure", ...)
```

## Arguments

- x:

  A `gtropic_data` object.

- type:

  Plot type, defaulting to `"exposure"`.

- ...:

  Arguments passed to
  [`gtropic_plot()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_plot.md).

## Value

The plot object returned by
[`gtropic_plot()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_plot.md).
