context("Test functions")
library(testthat)
library(earthquake.dc)

fpath <- system.file("exdata/signif.txt", package = "earthquake.dc")
df <- readr::read_delim(fpath, delim = "\t")

test_that("eq_clean_data returns a data frame", {
  expect_is(eq_clean_data(df), "data.frame")
})

test_that("eq_clean_data$datetime is Date type", {
  expect_is(eq_clean_data(df)$datetime, "Date")
})

test_that("eq_clean_data returns numeric coordinates", {
  expect_is(eq_clean_data(df)$latitude, "numeric")
  expect_is(eq_clean_data(df)$longitude, "numeric")
})


test_that("geom_timeline returns ggplot object", {
  g_df <- df %>% eq_clean_data() %>%
    dplyr::filter(country %in% c("USA", "MEXICO"), year > 2000) %>%
    ggplot2::ggplot(ggplot2::aes(x = datetime, y = country,
                                 color = as.numeric(deaths),
                                 size = as.numeric(eq_primary))) +
    geom_timeline()
  expect_is(g_df, "ggplot")
})

test_that("geom_timeline_label returns ggplot object", {
  g_df <- eq_clean_data(df) %>%
    dplyr::filter(country %in% c("USA", "MEXICO"), year > 2000) %>%
    ggplot2::ggplot(ggplot2::aes(x = datetime, y = country,
                                 color = as.numeric(deaths),
                                 size = as.numeric(eq_primary))) +
    geom_timeline_label(ggplot2::aes(label = location_name))
  expect_is(g_df, "ggplot")
})

test_that("theme_timeline returns ggplot object", {
  g_df <- eq_clean_data(df) %>%
    dplyr::filter(country %in% c("USA", "MEXICO"), year > 2000) %>%
    ggplot2::ggplot(ggplot2::aes(x = datetime, y = country,
                                 color = as.numeric(deaths),
                                 size = as.numeric(eq_primary))) +
    theme_timeline()
  expect_is(g_df, "ggplot")
})


test_that("eq_map returns leaflet and htmlwidget object", {
  label <- eq_clean_data(df) %>%
    dplyr::filter(country == "MEXICO" & lubridate::year(datetime) >= 2000) %>%
    dplyr::mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annotation_col = "popup_text")
  expect_is(label, "leaflet")
  expect_is(label, "htmlwidget")
})

test_that("eq_create_label returns character vector", {
  expect_is(eq_create_label(eq_clean_data(df)), "character")
})

test_that("eq_create_label has the same length as input df", {
  expect_equal(length(eq_create_label(eq_clean_data(df))), 
               dim(eq_clean_data(df))[1])
})