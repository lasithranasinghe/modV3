# Global TB notifications

To create manuscript figures and tables:

1. Knit `prediction/generate_prediction_data.Rmd`
2. Knit `generate_manuscript_artefacts.Rmd`

The resulting figures and tables are saved in the `figures` directory.
This is sub-divided into a `main` and `supplement` directory.
The generated tables are embedded in `tables.docx`

## Dependencies

```r
devtools::install_github('petedodd/wbmapdata')
suppressMessages(library(tidyverse))
library(here)
library(data.table)
library(metafor)
library(forecast)
library(imputeTS)
library(patchwork)
library(flextable)
library(officer)
library(scales)
library(ggrepel)
library(ggpubr)
library(wbmapdata)
library(sf)
```
