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
        
        high_burden <-
                c(
                        "AGO",
                        "BGD",
                        "BRA",
                        "CHN",
                        "PRK",
                        "COD",
                        "ETH",
                        "IND",
                        "IDN",
                        "KEN",
                        "MOZ",
                        "MMR",
                        "NGA",
                        "PAK",
                        "PHL",
                        "ZAF",
                        "THA",
                        "UGA",
                        "TZA",
                        "VNM",
                        "CAF",
                        "COG",
                        "GAB",
                        "LSO",
                        "LBR",
                        "MNG",
                        "NAM",
                        "PNG",
                        "SLE",
                        "ZMB"
                )

        
        return(
                list(
                  who_regions = who_regions,
                  raw_vars_keep = raw_vars_keep,
                  age_groups = age_groups,
                  high_burden = high_burden
                )
        )
}