# Info601-Environmental-Change-Through-Art
A Medium and Time Based Analysis through R and Data Visualizations

Landscapes Lost & Found: Environmental Change in Museum Collections (1820–2010)

A computational analysis of how museums depict wild, mixed, and urban environments over two centuries.

OVERVIEW

This project uses computational text analysis, medium ecology, and statistical modeling to examine how artworks from The Metropolitan Museum of Art and Tate (1820–2010) shift from depicting wild landscapes to increasingly urban and industrial environments. The repository contains cleaned datasets, R scripts, and reproducible visualizations analyzing long-term cultural and environmental transitions recorded in museum collections.

RESEARCH QUESTIONS

1. How have artistic depictions of nature shifted from wild to urban/industrial forms over time?

2. How do artistic media (oil, printmaking, photography, digital, etc.) influence whether landscapes are represented as “wild” or “urban”?

3. How do ecological phases and institutional histories shape these environmental framings?

DATA SOURCES

1. Met Museum Objects Dataset (public API; ~448k records)- https://github.com/metmuseum/openaccess
2. Tate Artworks Dataset (CSV; ~69k records)- https://www.kaggle.com/datasets/rtatman/the-tate-collection

Filtered to ~26k environmentally relevant artworks using:

- year ≥ 1820
- natural/urban keyword detection
- exclusion of non-representational object types

METHOD SUMMARY

The analysis is implemented entirely in R and includes:

- Keyword-based environmental classification: wild, urban, mixed, other
- Material ecology grouping via a custom function (oil, water-based, drawing, printmaking, photography, digital/media, sculpture, other)
- Temporal structuring by decade + five ecological phases

INFERENTIAL STATISTICS

- t-test comparing wild vs. urban dates
- chi-square test of medium × depiction type
- logistic regression models analyzing year, medium, phase, and museum effects
- Visual analytics: ribbons, ridgelines, heatmaps, regression plots, and phase-layered index curves using a custom “Pastel Ecology” color theme

  KEY FINDINGS (VERY BRIEF)

- Urban artworks appear ~27 years later on average than wild artworks.
- Medium is a strong predictor of environmental framing:
- Photography & Digital/Media → mostly urban
- Drawing & Water-based painting → mostly wild/mixed
Temporal and institutional effects matter:
- Urban imagery rises across 20th-century ecological phases
- Tate shows lower odds of urban depiction than the Met

  SCOPE & LIMITATIONS

Future work could extend this analysis to artist birthplaces, acquisition histories, or non-Western collections could deepen insights into global environmental imaginaries and to see whether these landscape shifts hold beyond Western collections .
This study relies on keyword-based classification, which may miss subtler or ambiguous landscape depictions. The Met and Tate colections differ in size, cataloging practices, and temporal coverage, introducing sampling asymmetries. Results therefore capture broad directional patterns rather than definitive measures of artistic production.


