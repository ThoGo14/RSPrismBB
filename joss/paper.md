---
title: 'RSPrismBB: A Shiny Based GUI R Package for Easy Barplots and Boxplots'
tags:
  - R
  - Shiny
  - barplots
  - boxplots
authors:
  - name: Thomas Goj
    orcid: 0000-0002-3075-0588
    equal-contrib: true
    affiliation: 1 
  - name: Andreas Peter
    affiliation: "1, 2, 3"
  - name: Cora Weigert
    affiliation: "1, 2, 3"
  - name: Simon I. Dreher
    equal-contrib: true
    corresponding: true 
    affiliation: "1, 2, 3"
affiliations:
 - name: Institute for Clinical Chemistry and Pathobiochemistry, Department for Diagnostic Laboratory Medicine, University Hospital Tübingen, Tübingen, Germany
   index: 1
 - name: Institute for Diabetes Research and Metabolic Diseases of the Helmholtz Zentrum München at the University of Tübingen, Tübingen, Germany
   index: 2
 - name: German Center for Diabetes Research (DZD e.V.), München-Neuherberg, Germany
   index: 3
date: 11 February 2026
bibliography: paper.bib

---

# Summary

RSPrismBB is an open-source R package that provides a Shiny-based graphical user interface (GUI) for generating publication-ready statistical plots directly from Excel files. The application is designed to enable users without programming experience to create standardized bar plots and box plots with overlaid data points, using consistent aesthetics and export settings suitable for scientific publications. RSPrismBB emphasizes reproducibility, visual consistency, and accessibility across research groups.

# Statement of need

Contrary to the believe of avid R users, coders or developers, many basic or clinician scientists do not have the time, energy or resources to learn and use coding-based programs like R for data analysis or graphical representation. At the same time many institutions refrain from providing licenses for graphical user interface (GUI)-driven, commercially available statistical programs that are arguably inferior or less versatile in some aspects, such as SPSS, JMP or GraphPad Prism. In many biomedical and life-science laboratories, experimental data are routinely stored and shared in spreadsheet formats, while figure generation is often performed manually using proprietary software or ad-hoc scripts. This workflow can lead to inconsistencies in visual style, limited reproducibility, and a high barrier for users without programming expertise. Thus, there is the need for an easy to use and navigate GUI within the open sourced and freely available R `[@RCoreTeam2025]` that enables reproducible, visually consistent and accessible generation of a graphical representation from scientific data.

RSPrismBB addresses this gap by offering a lightweight, open-source alternative that bridges spreadsheet-based data handling with reproducible, script-based visualization under the hood. By encapsulating established R visualization libraries within an intuitive and easy to use Shiny `[@Chang2012]` interface, the app allows researchers to generate consistent, high-quality figures while avoiding the need to write R code. Early iterations of this tool have been used already to generate figures in most recent publications of our lab `[@Dreher2023, @Dreher2024, @Dreher2025]`. The tool is particularly suited for collaborative environments, teaching contexts, and research groups seeking to standardize figure generation across multiple users.

# Installtion

RSPrismBB is distributed as an R package and can be installed directly from GitHub. The package requires a recent R installation and standard dependencies from the R ecosystem.

```R
# Install remotes if not already installed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("ThoGo14/RSPrismBB")
```

# Usage

The app is started using:

```R
RSPrismBB::run_app()
```

![Figure 1: Data input. After starting the app, the GUI launches in your preferred browser. Upload data by browsing your files and select the format of your data table as well as the number of columns with categorical metadata on the left. The right offers explanations and a representation of your input. Interface in English. Data from Dreher 2023 Figure 6B `[@Dreher2023]`.\label{fig:figure1}](Figure1_Input.png)

Users upload data files like Excel spreadsheets(.xlsx) containing categorical metadata columns (e.g., group or condition labels) alongside numeric measurement columns. It can be defined whether the input file contains connected data e.g. expression levels of a specific gene in a column or a row which is typically found in array or omics datasets. If your input contains the latter, ticking the box in the GUI will automatically transform your table for further use within RSPrismBB (\autoref{fig:figure1}). Through the graphical interface, users can select variables, define grouping factors, customize plot appearance, and export figures in common formats (PNG, PDF, SVG) with user-defined dimensions and resolution (\autoref{fig:figure2} and \autoref{fig:figure3}). The GUI guides the user in the left panel from top to bottom through all necessary steps to generate and modify your Boxplot or Barplot analogously, selected at the top of the right panel. To generate a plot, define which input column defines labeling and color of the individual datapoints, which column defines the groups to be compared and what you want to plot. This is followed by settings for plot axis labeling and customization of fonds, legends, coloring potions, dot size and axis scaling. In the download options setting you can customize file name, plot size that will be changing in scale in the right panel as you adjust, resolution and file format. The download button, depending on your operating system (Windows, MacOS or Linux) directly downloads your file or opens the OS-specific download-wizard. The right panel headed by plot-type selectors shows a to-scale representation of your data with currently selected settings, a context-sensitive info box, check-boxes and sliders to select and sort groups that will be displayed and an interactive color-panel for manual adjustment (\autoref{fig:figure2} and \autoref{fig:figure3}).

![Figure 2: Barplot interface. The general interface on the left here represented by the Barplot tab is accessible after uploading your data offers several customization options for your figure. The figure in actual size selected on the left as well as more data input-specific customization options are featured on the right. Data and recreated plot from Dreher 2025 Figure 6K `[@Dreher2025]`.\label{fig:figure2}](Figure2_Box_Sex_en.png)

Currently supported visualizations include bar plots with summary statistics and box plots with individual data points displayed using bee-swarm layouts. The interface supports multilingual use (German and English) and is designed to be extensible for additional plot types and workflows (\autoref{fig:figure2} and \autoref{fig:figure3}).

![Figure 3: Boxplot interface. The GUI is available in English and german, here featured by the Boxplot tab. Individual datapoint or box colors can be defined manually as shown or by inputting hex codes. Data and plot recreated from Dreher 24 Figure 10D `[@Dreher2024]`.\label{fig:figure3}](Figure3_Bar_IGF.png)

RSPrismBB is implemented in R using the Shiny framework. Data handling relies on tidyverse-compatible workflows, while visualization is based on ggplot2 and ggbeeswarm `[@Wickham2019; @Clarke2016]`. The modular structure of the codebase facilitates future extensions, including additional plot types, statistical summaries, and data validation steps.

# Acknowledgements

Special thanks go to the beta-testers within our institute in alphabetical order: Imme Behle, Kevin Elsner, Miriam Hoene, Jana Kühnle, Lara Ruoff, Vanessa Weik.

# Availability and Community Guidelines

The project is under active development, versioned releases and a changelog are provided within the repository to document ongoing improvements and feature additions. The software is available at the GitHub repository. The GitHub repository also contains the source code for this paper. Users and contributors are welcome to contribute, request features, and report bugs in this GitHub repository.

# References