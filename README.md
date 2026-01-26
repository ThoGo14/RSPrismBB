
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RSPrismBB

<!-- badges: start -->

[![License:
GPL-3](https://img.shields.io/badge/License-GPL%203-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R
Version](https://img.shields.io/badge/R-%E2%89%A5%204.0.0-blue.svg)](https://www.r-project.org/)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/ThoGo14/RSPrismBB/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ThoGo14/RSPrismBB/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

This Shiny application was developed to enable researchers without R
programming skills to create consistent, high-quality visualizations. It
provides an intuitive graphical interface for generating bar- and box
plots from Excel data files.

**Key Features:**

- **Two plot types**: Box plots with bee swarm overlays and bar plots
  with error bars
- **Multilingual**: Full support for German and English
- **Customizable**: Interactive color pickers, flexible grouping, and
  extensive styling options
- **Excel import**: Direct upload of `.xlsx` files (up to 100MB)
- **Export formats**: PNG, PDF, and SVG with custom dimensions and
  resolution
- **Interactive**: Drag-and-drop group ordering, dynamic data filtering
- **Standardization**: Ensures visual consistency across research groups

## Installation

### Prerequisites

- R (≥ 4.0.0)
- RStudio (recommended)

### Installation from GitHub

You can install the development version of `{RSPrismBB}` from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("ThoGo14/RSPrismBB")
```

## Run

You can launch the application by running:

``` r
RSPrismBB::run_app()
```

The app will automatically open in your default browser at
`http://127.0.0.1:XXXX`.

## Usage

### Data Format

Your Excel file should be structured as follows:

| Group | Treatment | Gene1 | Gene2 | Gene3 |
|-------|-----------|-------|-------|-------|
| A     | Control   | 1.2   | 2.3   | 3.4   |
| A     | Control   | 1.5   | 2.1   | 3.2   |
| B     | Treated   | 2.1   | 3.4   | 4.5   |

- **Metadata columns** (e.g., Group, Treatment): Text/categorical data
- **Measurement columns** (e.g., Gene1, Gene2): Numeric values

### Workflow

1.  **Upload Data**
    - Click “Browse…” and select your Excel file
    - Configure data orientation (row-wise parameters for array data)
    - Set column cutoff where numeric data begins
2.  **Configure Plot**
    - Select grouping variable
    - Choose measurement for Y-axis
    - Define dot color grouping
    - Customize axis labels
3.  **Customize Appearance**
    - Select/reorder groups to display
    - Adjust colors via interactive color picker
    - Set point size and style options
    - Configure Y-axis limits
4.  **Export**
    - Choose format (PNG, PDF, SVG)
    - Set dimensions (height and width in cm) and resolution (DPI)
    - Enter filename
    - Click “Download”

## Features in Detail

### Plot Types

#### Box Plot

- Standard box plot with quartiles
- Bee swarm overlay for individual data points
- Optional colored boxes when grouping matches dot color
- Outlier visualization

#### Bar Plot

- Mean ± standard deviation
- Bee swarm overlay
- Error bars

### Customization Options

- **Group Selection**: Include/exclude groups, drag-and-drop ordering
- **Color Management**: Per-group color selection with interactive
  picker
- **Point Styling**: Size adjustment, inverted colors (fill/stroke)
- **Axes**: Custom labels, title formatting (italic), min/max limits
- **Legend**: Toggle legend title visibility
- **Export**: Multiple formats with precise dimension control

### Multilingual Support

The application interface is available in:

- **German**
- **English**

Switch languages using the dropdown in the header. All interface
elements update dynamically.

## Technical Details

### Architecture

- **Framework**: Shiny (R web application framework)
- **Plotting**: ggplot2 with ggbeeswarm for scatter overlays
- **UI**: shinydashboard with custom CSS styling
- **Internationalization**: Reactive translation system with language
  switching
- **Data handling**: tidyverse, data.table for efficient processing

### Version History

See [changelog_de.txt](inst/changelog_de.txt) or
[changelog_en.txt](inst/changelog_en.txt) for detailed version history.

Current version: **1.6.1**

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

### Development Setup

1.  Fork the repository
2.  Create a feature branch (`git checkout -b feature/AmazingFeature`)
3.  Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4.  Push to the branch (`git push origin feature/AmazingFeature`)
5.  Open a Pull Request

## License

This project is licensed under the GNU General Public License v3.0 - see
the [LICENSE.md](LICENSE.md) file for details.

## Citation

If you use this application in your research, please cite:

``` bibtex
@software{RSPrismBB,
  author = {Thomas Goj},
  title = {RSPrismBB: A Shiny Based GUI R Package for Easy Barplots and Boxplots},
  year = {2026},
  url = {https://github.com/ThoGo14/RSPrismBB}
}
```

## Support

For questions, issues, or feature requests:

- Email: <thomas.goj@med.uni-tuebingen.de>
- Issues: [GitHub Issues](https://github.com/ThoGo14/RSPrismBB/issues)

## Acknowledgments

- Developed at University hospital of Tübingen, Germany
- Thanks to all contributors and users providing feedback
- Built with [Shiny](https://shiny.rstudio.com/) by RStudio and
  [golem](https://thinkr-open.github.io/golem/)

------------------------------------------------------------------------

**Version**: 1.6.1  
**Last Updated**: Januar 2026  
**Maintainer**: Thomas Goj

------------------------------------------------------------------------

## About

This README has been compiled on:

``` r
Sys.time()
#> [1] "2026-01-26 14:19:06 CET"
```
