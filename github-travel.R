library(tidyverse)
library(httr)
library(lubridate)
library(ggbeeswarm)

# First we need to figure out the time zone for each trip. Luckily
# there's an API for this!

timezone_ <- function(lat, lon) {
  r <- GET(
    "http://api.geonames.org/timezoneJSON",
    query = list(
      lat = lat,
      lng = lon,
      username = "hadley"
    ))
  stop_for_status(r)

  json <- content(r, "parsed", "application/json")
  json$timezoneId
}
timezone <- memoise::memoise(
  timezone_,
  cache = memoise::cache_filesystem("cache")
)

trips <- read_csv("trips.csv")

recent_trips <- trips %>%
  filter(end >= ymd("2016-04-25")) %>%
  mutate(tz = map2_chr(lat, lon, timezone)) %>%
  arrange(start)

recent_trips

# Possible more elegant way to solve this using data.table because it
# supports more types of join (non-equi) than dplyr. My approach is
# to "summarise" each trip with a tibble which has one row for each day
# and then unnest it.

travel <- recent_trips %>%
  group_by(id, tz) %>%
  summarise(days = list(tibble(
    date = seq(start, end, by = 1),
    trip_day = seq_along(date),
    travel = TRUE
  ))) %>%
  unnest(days) %>%
  arrange(date)
travel

# Now we can join on to commit data, filling in non-matches with
# my local timezone

hadley <- read_csv("github-commits.csv")

local_time <- function(dttm, tz) {
  dttm <- lubridate::with_tz(dttm, tz)
  as.integer(difftime(dttm, lubridate::floor_date(dttm, "day"), unit = "secs"))
}

hadley_tz <- hadley %>%
  left_join(travel) %>%
  mutate(
    time = with_tz(time, "America/Chicago"),
    tz = coalesce(tz, "America/Chicago"),
    travel = ifelse(coalesce(travel, FALSE), "travelling", "at home"),
    local_time = map2_int(hadley_tz$datetime, hadley_tz$tz, local_time) %>% hms::hms()
  )

# Did it work???
ggplot(hadley_tz, aes(time, local_time, colour = tz)) +
  geom_point()
# YES

hadley_tz %>%
  mutate(week = floor_date(date, "week")) %>%
  ggplot(aes(week, local_time, colour = tz)) +
  geom_quasirandom()

hadley_tz %>%
  mutate(wday = wday(datetime, label = TRUE) %>% fct_shift(1) %>% fct_rev()) %>%
  ggplot(aes(local_time, wday, colour = tz)) +
  geom_quasirandom() +
  facet_wrap(~ travel, ncol = 1)

hadley_tz %>%
  mutate(wday = wday(datetime, label = TRUE) %>% fct_shift(1) %>% fct_rev()) %>%
  ggplot(aes(local_time, wday)) +
  geom_quasirandom() +
  facet_wrap(~ travel, ncol = 1) +
  labs(
    title = "GitHub commits by @hadley",
    x = "Day of week",
    y = "Local time"
  )
ggsave("github-commits.png", dpi = 96 * 2, width = 8, height = 6)

round_time <- function(x, sec) {
  hms::hms(as.numeric(x) %/% sec * sec)
}

hadley_tz %>%
  group_by(
    wday = wday(datetime, label = TRUE) %>% fct_shift(1) %>% fct_rev(),
    local_time = round_time(local_time, 15 * 60),
    travel
  ) %>%
  summarise(n = n()) %>%
  ggplot(aes(local_time, wday, fill = n)) +
    geom_raster() +
    facet_wrap(~ travel, ncol = 1)

hadley_tz %>%
  mutate(wday = wday(datetime, label = TRUE) %>% fct_shift(1) %>% fct_rev()) %>%
  ggplot(aes(local_time, wday)) +
  geom_quasirandom() +
  facet_wrap(~ travel, ncol = 1)
