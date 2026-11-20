# Save a plot or interactive widget

Save a ggplot with
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
or an HTML widget, including a Leaflet map, with
[`htmlwidgets::saveWidget()`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html).
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

  A ggplot or HTML widget, such as one returned by
  [`gtropic_plot()`](https://sparklabnyc.github.io/gtropic/reference/gtropic_plot.md).

- filename:

  One nonempty output filename, optionally including a path. For static
  plots the extension selects the device unless `device` is supplied in
  `...`. HTML widgets require a `.html` filename.

- ...:

  Uniquely named arguments. For static plots, passed unchanged to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html),
  including `device`, `path`, `bg`, and `scale`. For HTML widgets,
  `path` is combined with `filename`; other arguments are passed to
  [`htmlwidgets::saveWidget()`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html),
  including `selfcontained`, `libdir`, `background`, `title`, and
  `knitrOptions`.

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

### Interactive maps

Interactive maps are saved as HTML widgets. By default, their JavaScript
and CSS dependencies are written to a sibling directory rather than
embedded in the HTML file (`selfcontained = FALSE`), which does not
require Pandoc. Keep that directory with the HTML file when moving or
publishing the map. Set `selfcontained = TRUE` to request a single-file
widget when Pandoc is available.

### Figure dimensions

Map plots carry a geographic `gtropic_bbox` attribute in the order
`c(xmin, ymin, xmax, ymax)` (named bounds may be in any order), matching
[`sf::st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html).
The estimated width-to-height ratio is longitude span times the cosine
of mean latitude, divided by latitude span. This is a geographic
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
