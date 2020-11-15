library(readabs)
library(dplyr)
library(purrr)

# WPI ----
wpi <- read_abs("6345.0",
  tables = "all",
  path = tempdir(),
  show_progress_bars = FALSE
)


# Reduce data size ---
datasets <- list(wpi)

data <- purrr::map_dfr(
  datasets,
  ~ dplyr::select(
    .x,
    table_no, table_title, date, series, value, series_type,
    series_id
  ) %>%
    dplyr::mutate(across(where(is.character), as.factor)) %>%
    dplyr::filter(
      series_type != "Trend",
      !is.na(value)
    )
)

data <- data %>%
  split(.$series_id)

usethis::use_data(data, internal = TRUE, overwrite = TRUE)
