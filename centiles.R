library(tidyverse)

dat <- read.csv("data/centile.dat.csv")

dat %>% count(group, Setting)

perc.rank <- function(x) trunc(rank(x))/length(x)

ref <- data.frame(
  es = seq(-3, 10, by = 0.01)
)

# named group split
named_group_split <- function(.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::inject(paste(!!!group_keys(grouped), sep = " / "))

  grouped %>%
    group_split() %>%
    rlang::set_names(names)
}

dat  <-
  dat %>% rename(
  n = PreN,
  es = EffectSize,
  var = Sample.Variance,
  setting = Setting
) %>%
mutate(
  setting = str_to_lower(setting),
  ci.lb = es - 1.96*sqrt(var),
  ci.ub = es + 1.96*sqrt(var)
) %>% select(-c(X, Key, measure.used)) %>%
  mutate(across(where(is.numeric), round, 2))

dat <-
  dat %>% named_group_split(group)

names(dat)[1] <- "anx"
names(dat)[2] <- "dep"
names(dat)[3] <- "gen"


norm.datsets <-
  list(
    dep.all = dat$dep %>% mutate(setting  = "all"),
    anx.all = dat$anx %>% mutate(setting  = "all"),
    gen.all = dat$gen %>% mutate(setting  = "all"),
    dep.outpatient = dat$dep %>% filter(setting == "outpatient") %>% mutate(setting  = "outpatient"),
    anx.outpatient = dat$anx %>% filter(setting == "outpatient") %>% mutate(setting  = "outpatient"),
    gen.outpatient = dat$gen %>% filter(setting == "outpatient") %>% mutate(setting  = "outpatient"),
    dep.inpatient = dat$dep %>%  filter(setting == "residential")  %>% mutate(setting = "inpatient"),
    anx.inpatient = dat$anx %>%  filter(setting == "residential")  %>% mutate(setting = "inpatient"),
    gen.inpatient = dat$gen %>%  filter(setting == "residential")  %>% mutate(setting = "inpatient")
  ) %>%
    map(arrange, es) %>%
    imap(~ mutate(.x, centile = round(
                    perc.rank(es), 4)*100)
                  )

look.up <-
  norm.datsets %>%
  map(full_join, ref, by = "es") %>%
  map(select, -c(measure.type, n, var, ci.lb, ci.ub)) %>%
  map(arrange, es) %>%
  map(fill, centile, group, setting, .direction = "updown") %>%
  imap(~mutate(.x, rank = nombre::nom_ord(centile, cardinal = F))) %>%
  imap(~ mutate(.x, final.rank =
                  case_when(
                    centile == min(centile) ~ paste("≤", min(centile), " percentile rank", sep = ""),
                    centile == max(centile) ~ paste("≥", max(centile), " percentile rank", sep = ""),
                    centile >= 1.01 ~ paste(as.character(rank), "percentile rank")
                            ))) %>%
  imap(~ mutate(.x, effective.group =
                  case_when(
                    centile < 25 ~ "lower (≤25th percentile)",
                    centile > 75 ~ "upper (≥75th percentile)",
                    centile > 25 ~ "middle (between 25th and 75th percentile)"
                  ))) %>%
    imap(~ mutate(.x, rank = as.character(rank))) %>%
    reduce(full_join, by = c("setting", "es", "group", "centile", "rank", "final.rank", "effective.group")) %>%
  mutate(across(where(is.numeric), round, 2))

norm.datsets <-
  norm.datsets %>% reduce(full_join, by = c("measure.type", "setting", "n", "es", "var", "group", "ci.lb", "ci.ub", "centile")) %>%
  relocate(group, setting, n, es, var, ci.lb, ci.ub, centile) %>%
  mutate(centile = round(centile, 2))


write_csv(norm.datsets, "data/norm.datsets.csv")
write_csv(look.up, "data/look.up.csv")

