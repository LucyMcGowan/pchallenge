library(tidyverse)
library(glue)

## source: https://touringplans.com/walt-disney-world/crowd-calendar?m=0&utm_expid=.9ZFNELOQQu6UCDsJpk231w.1&utm_referrer=https%3A%2F%2Fblog.touringplans.com%2F2018%2F06%2F25%2Fdisney-world-wait-times-available-for-data-science-and-machine-learning%2F%3Futm_campaign%3Dtwitter%26utm_medium%3Dtwitter%26utm_source%3Dtwitter#DataSets

read_disney_data <- function(name) {
  read_csv(glue("data-raw/{name}.csv")) %>%
    mutate(attraction = name)
}

files <- list.files("data-raw", pattern = ".csv") %>%
  gsub(".csv", "", .)

## drop metadata
files <- files[files != "metadata"]

df <- map(files, read_disney_data) %>%
  bind_rows() %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

metadata <- read_csv("data-raw/metadata.csv") %>%
  mutate(date = as.Date(DATE, "%m/%d/%Y"))

df <- df %>%
  left_join(metadata, by = "date")

save(df, file = "data-raw/df.rda")

df <- df %>%
  filter(YEAR == "2018", MONTHOFYEAR == "6")

save(df, file = "data/df.rda")
