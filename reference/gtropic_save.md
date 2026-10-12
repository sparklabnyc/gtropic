# Save a static plot

Save a ggplot with
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).
This helper writes files;
[`gtropic_plot()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_plot.md)
itself only returns plot objects.

## Usage

``` r
gtropic_save(
  plot,
  filename,
  ...,
  dpi = 300,
  width = NULL,
  height = NULL,
  units = "in"
)
```

## Arguments

- plot:

  A ggplot, such as one returned by
  [`gtropic_plot()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_plot.md).

- filename:

  One nonempty output filename, optionally including a path. For static
  plots the extension selects the device unless `device` is supplied in
  `...`.

- ...:

  Uniquely named arguments. For static plots, passed unchanged to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html),
  including `device`, `path`, `bg`, and `scale`.

- dpi:

  Positive finite resolution in dots per inch. Defaults to 300.

- width, height:

  Positive finite output dimensions in `units`, or NULL to infer them.
  Explicit dimensions are preserved. See Details for defaults.

- units:

  Dimension units: `"in"`, `"cm"`, `"mm"`, or `"px"`. Pixel sizes are
  converted using `dpi`.

## Value

Invisibly, the output filename, combined with `path` when supplied. The
path is not normalized. Saver errors are propagated.

## Details

### Figure dimensions

Map plots carry a geographic `gtropic_bbox` attribute in the order
`c(xmin, xmax, ymin, ymax)` (named bounds may be in any order). The
estimated width-to-height ratio is longitude span times the cosine of
mean latitude, divided by latitude span. This is a geographic
approximation, not a measurement of the final projected panels, legends,
or facets.

Without usable bounds, the ratio is 7:5. With neither dimension
supplied, width defaults to 7 inches and height follows the ratio. With
one dimension supplied, the other follows the ratio. Inferred dimensions
are bounded to 3–10 inches to keep narrow, polar, or very wide extents
usable. Supplying both dimensions overrides all automatic sizing.
Defaults are converted to the requested units.

## See also

[`gtropic_plot()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_plot.md),
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)

## Examples

``` r
# \donttest{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  output <- gtropic_save(p, tempfile(fileext = ".pdf"), width = 7, height = 5)
  unlink(output)
}
# }
```
