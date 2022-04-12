read_data <- function(file = c("notifications")) {
        file <- match.arg(file)

        name <- dplyr::case_when(
                file == "notifications" ~ "TB_notifications.csv",
                TRUE ~ ""
        )
        readr::read_csv(here::here("data", name))
}

prepare_data <- function(raw, const = constants()) {
        wide <- list(
                country = raw[raw$year > 2012, const$raw_vars_keep]
        )

        wide$region <- wide$country %>%
                select(-country, -iso3) %>%
                group_by(g_whoregion, year) %>%
                summarise(across(starts_with("newrel_"), ~ sum(.x, na.rm = TRUE)),
                        .groups = "drop"
                )

        long <- list(
                country = pivot_longer(
                        data = wide$country,
                        cols = starts_with("newrel_"),
                        values_to = "cases",
                        names_to = "group"
                ) %>%
                        mutate(group = str_remove(
                                group,
                                "^newrel_"
                        )) %>%
                        separate(
                                col = "group",
                                into = c("sex", "age_group"),
                                sep = 1
                        )
        )

        long$region <- long$country %>%
                select(-country, -iso3) %>%
                group_by(g_whoregion, year, sex, age_group) %>%
                summarise(
                        cases = sum(cases, na.rm = TRUE),
                        .groups = "drop"
                )

        long$global <- long$region %>%
                group_by(year, sex, age_group) %>%
                summarise(cases = sum(cases), .groups = "drop") %>%
                mutate(id = "Global") %>%
                select(id, everything())

        return(list(
                wide = wide,
                long = long
        ))
}