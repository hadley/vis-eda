library(tidyverse)
library(lubridate)
library(httr)
source("cache.R")

# ENDLESS SCREAMING -------------------------------------------------------

auth <- authenticate(
  "h.wickham@gmail.com",
  rstudioapi::askForPassword("tripit password"),
  "basic"
)

GET_tripit <- function(url, query = list(), ...) {
  default_query <- list(
    format = "json",
    page_size = 500
  )
  query <- modifyList(default_query, query)

  r <- GET(url, auth, query = query, ...)
  stop_for_status(r)
  content(r)
}

list_trips <- function(page_num = 1) {
  GET_tripit(
    "https://api.tripit.com/v1/list/trip/past/true",
    query = list(page_num = page_num)
  )
}

# Data rectangling -------------------------------------------------------

trips_json <- cache("trips-json", list_trips()$Trip)
str(trips_json[[1]])

address <- trips_json %>% map("PrimaryLocationAddress")

trips <- tibble(
  id =      trips_json %>% map_chr("id"),
  start =   trips_json %>% map_chr("start_date") %>% parse_date(),
  end =     trips_json %>% map_chr("end_date") %>% parse_date(),
  lat =     address %>% map_chr("latitude", .default = NA) %>% parse_double(),
  lon =     address %>% map_chr("longitude", .default = NA) %>% parse_double(),
  city =    address %>% map_chr("city", .default = NA),
  country = address %>% map_chr("country", .default = NA)
)
trips
trips %>% write_csv("trips.csv")

# Visualisation -------------------------------------------------------

ggplot(trips, aes(y = country)) +
  geom_segment(aes(x = start, xend = end, yend = country))

trips2 <- trips %>%
  mutate(
    start_day = update(start, year = 2010),
    end_day = update(end, year = 2010),
    year = year(start)
  )
ggplot(trips2) +
  geom_segment(aes(x = start_day, xend = end_day, y = year, yend = year))

trips2 %>%
  filter(start_day < end_day) %>%
  ggplot() +
  geom_segment(aes(x = start_day, xend = end_day, y = year, yend = year))

trips2 %>%
  filter(start_day < end_day) %>%
  ggplot() +
  geom_segment(aes(x = start_day, xend = end_day, y = year, yend = year), size = 10) +
  scale_y_continuous(breaks = 2000 + seq(7, 17, by = 2)) +
  scale_x_date(date_labels = "%b")

trips2 %>%
  filter(start_day < end_day) %>%
  ggplot() +
  geom_segment(aes(x = start_day, xend = end_day, y = year, yend = year, colour = country), size = 10) +
  scale_y_continuous(breaks = 2000 + seq(7, 17, by = 2)) +
  scale_x_date(date_labels = "%b")

trips2 %>%
  filter(start_day < end_day) %>%
  ggplot() +
  geom_segment(aes(x = start_day, xend = end_day, y = year, yend = year, colour = forcats::fct_lump(country, 8)), size = 10) +
  scale_y_continuous(breaks = 2000 + seq(7, 17, by = 2)) +
  scale_x_date(date_labels = "%b")

# I wish there was a way to set the colour of one country
# Would be nice to show continent instead?

# Exploration vs. exposition ----------------------------------------------

us_totals <- trips %>%
  filter(country == "US") %>%
  group_by(city, lat, lon) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

ggplot(us_totals, aes(lon, lat)) +
  borders("state") +
  geom_point(aes(size = n))

ggplot(us_totals, aes(lon, lat)) +
  borders("state", fill = "grey90", colour = "white") +
  geom_point(aes(size = n, colour = n)) +
  ggrepel::geom_text_repel(aes(label = city)) +
  scale_size_area(breaks = c(1, 5, 10, 14)) +
  viridis::scale_color_viridis(breaks = c(1, 5, 10, 14), guide = "legend") +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_quickmap() +
  labs(
    title = "Places I’ve visited in the US",
    caption = "As captured by 'primary' address in TripIt",
    colour = "Number of\nvisits",
    size = "Number of\nvisits"
  ) +
  hrbrthemes::theme_ipsum()

ggsave("travel-example.png", width = 8, height = 5.5)
