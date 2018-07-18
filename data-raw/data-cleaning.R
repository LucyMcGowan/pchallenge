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

df_all <- df %>%
  left_join(metadata, by = "date")

save(df_all, file = "data-raw/df_all.rda")

df <- df_all %>%
  filter(attraction == "pirates_of_caribbean") %>%
  mutate(hour = lubridate::hour(datetime)) %>%
  group_by(date, hour) %>%
  filter(row_number() == 1) %>%
  filter(YEAR %in% c(2017, 2018)) %>%
  select(- attraction) %>%
  ungroup()

set.seed(926)
rand <- runif(length(unique(df$date)))
df <- tibble(
  date = unique(df$date),
  MERCHANDISE = round(rbeta(length(unique(df$date)), 10, 2) * 1500)
)  %>%
  mutate(MERCHANDISE = case_when(
    rand < 0.01 ~ NA_real_,
    TRUE ~ MERCHANDISE
  )) %>%
  left_join(df)

df <- set_names(df, toupper)
df <- df[, -1] ## Drop extra date column

# library(rms)
# df_ <- df %>%
# select(- HSPRDDT1, - HSPRDDN, - AKPRDDT1, - AKPRDDT2, - AKPRDDN, - AKFIREN)
# dd <- datadist(df_)
# options(datadist = "dd")
#
# ols(SPOSTMIN ~ MERCHANDISE, data = df_)
#
# ols(SPOSTMIN ~ MERCHANDISE, data = df_[df_$SPOSTMIN != -999, ])
#
#
# ols(
#   SPOSTMIN ~ rcs(MERCHANDISE, 9),
#   data = df_[df_$SPOSTMIN != -999, ]
#   ) %>%
#   anova()
#
# df_ %>%
#   filter(SPOSTMIN != -999) %>%
#   mutate(p = ols(SPOSTMIN ~ rcs(MERCHANDISE, 9), data = .) %>%
#            predict()) %>%
#   arrange(MERCHANDISE) %>%
#   ggplot(aes(x = MERCHANDISE, y = p)) + geom_line()
#
# ols(
#    SPOSTMIN ~ rcs(MERCHANDISE, 9),
#    data = df
#    ) %>%
#    anova()
#
# df__ <- df_ %>%
#   filter(WDW_TICKET_SEASON == "value", SPOSTMIN != -999)
# ols(SPOSTMIN ~ rcs(MERCHANDISE), df__) %>%
#   anova()

save(df, file = "data/df.rda")
