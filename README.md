# Global TB notifications

To create manuscript figures and tables:

1. Knit `prediction/generate_prediction_data.Rmd`
2. Knit `generate_manuscript_artefacts.Rmd`

The resulting figures and tables are saved in the `figures` directory.
This is sub-divided into a `main` and `supplment` directory. Note that
the generated tables are embedded in `tables.docx`

## Dependencies

```r
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
library(wbmapdata) # devtools::install_github('petedodd/wbmapdata')
library(sf)
```

## To keep

- R folder
- `TB_notifications.csv`
- artefacts folder
- figures folder
  - main folder
  - supplement folder
