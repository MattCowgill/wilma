library(readabs)
library(dplyr)
library(purrr)

# WPI ----
wpi <- read_abs("6345.0",
  tables = "all",
  path = tempdir(),
  show_progress_bars = FALSE
)

# Monthly LFS ----
lfs <- read_abs("6202.0",
                tables = "all",
                path = tempdir(),
                show_progress_bars = FALSE)

# Reduce data size ---
datasets <- list(wpi)

prep_df <- function(df) {
  df %>%
    filter(series_type != "Trend") %>%
    # For now, collapse series_type into series
    dplyr::mutate(series = paste0(series, " ;  ", series_type)) %>%
    dplyr::select(
    table_no, table_title, date, series, value,
    series_id
  ) %>%
    dplyr::mutate(data_provider = "ABS",
                  across(where(is.character), as.factor)) %>%
    dplyr::filter(!is.na(value))
}

data_df <- purrr::map_dfr(
  datasets,
  prep_df
)

available_data <- data_df %>%
  dplyr::group_by(series, series_id, data_provider) %>%
  dplyr::summarise()

data <- data_df %>%
  split(.$series_id)

usethis::use_data(data, available_data,
                  internal = TRUE, overwrite = TRUE)
