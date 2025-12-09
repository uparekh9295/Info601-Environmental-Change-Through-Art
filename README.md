# Info601-Environmental-Change-Through-Art
A Medium and Time Based Analysis through R and Data Visualizations

Landscapes Lost & Found: Environmental Change in Museum Collections (1820–2010)

A computational analysis of how museums depict wild, mixed, and urban environments over two centuries.

Overview

This project uses computational text analysis, medium ecology, and statistical modeling to examine how artworks from The Metropolitan Museum of Art and Tate (1820–2010) shift from depicting wild landscapes to increasingly urban and industrial environments. The repository contains cleaned datasets, R scripts, and reproducible visualizations analyzing long-term cultural and environmental transitions recorded in museum collections.

Research Questions

How have artistic depictions of nature shifted from wild to urban/industrial forms over time?

How do artistic media (oil, printmaking, photography, digital, etc.) influence whether landscapes are represented as “wild” or “urban”?

How do ecological phases and institutional histories shape these environmental framings?

Data Sources

Met Museum Objects Dataset (public API; ~448k records)- https://github.com/metmuseum/openaccess

Tate Artworks Dataset (CSV; ~69k records)- https://www.kaggle.com/datasets/rtatman/the-tate-collection

Filtered to ~26k environmentally relevant artworks using:

year ≥ 1820

natural/urban keyword detection

exclusion of non-representational object types

Method Summary

The analysis is implemented entirely in R and includes:

Keyword-based environmental classification: wild, urban, mixed, other

Material ecology grouping via a custom function (oil, water-based, drawing, printmaking, photography, digital/media, sculpture, other)

Temporal structuring by decade + five ecological phases

Inferential statistics:

t-test comparing wild vs. urban dates

chi-square test of medium × depiction type

logistic regression models analyzing year, medium, phase, and museum effects

Visual analytics: ribbons, ridgelines, heatmaps, regression plots, and phase-layered index curves using a custom “Pastel Ecology” color theme

