library(readabs)
library(dplyr)
library(purrr)

# WPI ----
wpi <- read_abs("6345.0",
  tables = c("1", "2b", "5b")
) %>%
  mutate(collection = "Wage Price Index")

# Monthly LFS ----
lfs <- read_abs("6202.0",
                tables = c("1", "12", "19") )

lfs <- read_abs("6202.0",
         tables = "22") %>%
  bind_rows(lfs) %>%
  mutate(collection = "Labour Force")

# CPI ----
cpi <- read_abs("6401.0",
                tables = c(1, 3, 8) ) %>%
  mutate(collection = "Consumer Price Index")

# Reduce data size ---
datasets <- list(wpi, cpi, lfs)

prep_df <- function(df) {
  df %>%
    filter(series_type != "Trend") %>%
    # For now, collapse series_type into series
    dplyr::mutate(series = paste0(series, " ;  ", series_type)) %>%
    dplyr::select(
      table_title, date, series, value,
      series_id, collection,
    ) %>%
    dplyr::mutate(
      data_provider = "ABS",
      across(where(is.character), as.factor)
    ) %>%
    dplyr::filter(!is.na(value))
}

data <- purrr::map_dfr(
  datasets,
  prep_df
)

data <- data %>%
  distinct() %>%
  group_by(series_id, date) %>%
  arrange(series) %>%
  filter(row_number() == 1) %>%
  ungroup()

available_data <- data %>%
  dplyr::group_by(collection, series, series_id, data_provider) %>%
  dplyr::summarise()

usethis::use_data(data, available_data,
  internal = TRUE, overwrite = TRUE,
  version = 3
)
