library(tidyverse)
library(gh) # devtools::install_github("gaborcsardi/gh")
library(lubridate)
library(ggbeeswarm)
source("cache.R")
source("progress.R")

# gh helpers --------------------------------------------------------------

my_repos <- function(type = c("all", "owner", "public", "private", "member"),
                     limit = 100) {
  type <- match.arg(type)

  gh(
    "GET /user/repos",
    type = type,
    sort = "updated",
    .limit = limit
  )
}

repo_commits <- function(full_name, since = "2016-01-01") {
  gh("GET /repos/:full_name/commits",
    full_name = full_name,
    since = since,
    .limit = Inf
  )
}

# Download and cache my data ----------------------------------------------

# I think this gets the 100 mostly recently updated repos that I've
# contributed to. Looking at the names, I'm reasonably certain this
# should capture everything I've been working on in the last 18 months

repos <- cache("github-repos", my_repos("all", limit = 00))
full_name <- repos %>% map_chr("full_name")
head(full_name, 20)

commits_json <- cache("github-commits",
  full_name %>%
    set_names() %>%
    map_progress(possibly(repo_commits, NULL)) %>%
    compact()
)

# Rectangling -------------------------------------------------------------

commits_flat <- commits_json %>% flatten() %>% unname()

commits <- tibble(
  repo = full_name %>% rep(map_int(commits_json, length)),
  sha = commits_flat %>% map_chr("sha", .default = NA),
  author = commits_flat %>% map_chr(c("author", "login"), .default = NA),
  datetime = commits_flat %>% map_chr(c("commit", "author", "date"), .default = NA),
  message = commits_flat %>% map_chr(c("commit", "message"), .default = NA)
)

commits <- commits %>% mutate(
  datetime = parse_datetime(datetime) %>% with_tz("America/Chicago"),
  date = as.Date(floor_date(datetime, "day")),
  time = update(datetime, year = 2016, month = 1, mday = 1, second = 0)
)

commits %>% count(sha, sort = TRUE) %>% filter(n > 1)
commits %>% count(repo, sort = TRUE) %>% print(n = 10)
commits %>% count(author, sort = TRUE)

hadley <- commits %>%
  filter(author == "hadley") %>%
  filter(date > ymd(20160423, tz = "America/Chicago")) %>%
  arrange(desc(datetime))
hadley

# Always a good idea to save a csv too
write_csv(hadley, "github-commits.csv")



# Now for some visualisation!! --------------------------------------------

# What have I been working on?
hadley %>%
  ggplot(aes(date, repo)) +
  geom_quasirandom(size = 0.5)

hadley %>%
  mutate(repo = repo %>% fct_lump(15)) %>%
  ggplot(aes(date, repo)) +
  geom_quasirandom(size = 0.5)

hadley %>%
  mutate(repo = repo %>% fct_reorder(date) %>% fct_rev() %>% fct_lump(15)) %>%
  ggplot(aes(date, repo)) +
  geom_quasirandom(size = 0.5)

# When have I been working?
hadley %>%
  mutate(week = date) %>%
  ggplot(aes(week, time)) +
  geom_quasirandom()

hadley %>%
  mutate(week = floor_date(date, "week")) %>%
  ggplot(aes(week, time)) +
  geom_quasirandom()

# Hypothesis: the majority of commits <6am or >6pm
# are because I'm in a different time zone

# What does my average week look like?
hadley %>%
  mutate(wday = wday(datetime, label = TRUE)) %>%
  ggplot(aes(time, wday)) +
  geom_quasirandom()

hadley %>%
  mutate(wday = wday(datetime, label = TRUE) %>% fct_shift(1) %>% fct_rev()) %>%
  filter(hour(time) >= 6, hour(time) <= 18) %>%
  ggplot(aes(time, wday)) +
  geom_quasirandom()

