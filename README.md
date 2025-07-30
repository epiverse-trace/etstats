# Epiverse-TRACE Statistics

This repository stores data and generates visualizations for public statistics about the [Epiverse-TRACE](https://github.com/epiverse-trace) project, including package downloads, GitHub metrics, and academic citations.

## Overview

The site provides comprehensive analytics and metrics for the Epiverse-TRACE ecosystem:

- **CRAN Statistics**: Download counts and trends for Epiverse-TRACE packages published on CRAN
- **GitHub Statistics**: Repository stars, followers, and engagement metrics across the organization
- **Citations**: Academic papers and publications that cite Epiverse-TRACE packages and the project

## Website

The statistics are published as a Quarto website at: <https://epiverse-trace.github.io/etstats>

## Data Sources

The repository contains automated data collection from:
- CRAN download logs via [cranlogs.r-pkg.org](https://cranlogs.r-pkg.org)
- GitHub API for repository metrics and stargazer data
- Curated bibliography files for academic citations

## Structure

- `index.qmd` - Main page with CRAN download statistics
- `github-statistics.qmd` - GitHub metrics and visualizations
- `citations.qmd` - Academic citations and references
- `data/` - Raw data files (JSON for GitHub metrics, BibTeX for citations)
- `scripts/` - Data collection and processing scripts

## Maintenance

Statistics are [automatically updated through GitHub Actions workflows](./.github/workflows) that collect fresh data from APIs and rebuild the site.
