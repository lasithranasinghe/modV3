constants <- function() {
        who_regions <- c("AFR", "AMR", "EMR", "EUR", "SEA", "WPR")
        raw_vars_keep <-
                c(
                        "country",
                        "iso3",
                        "g_whoregion",
                        "year",
                        "newrel_m04",
                        "newrel_m514",
                        "newrel_m014",
                        "newrel_m15plus",
                        "newrel_f04",
                        "newrel_f514",
                        "newrel_f014",
                        "newrel_f15plus"
                )
        
        age_groups <- c("04", "514", "014", "15plus" )

        
        return(
                list(
                  who_regions = who_regions,
                  raw_vars_keep = raw_vars_keep,
                  age_groups = age_groups
                )
        )
}