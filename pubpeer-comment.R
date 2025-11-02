library(tidyverse)
library(haven)
library(knitr)

# load data
dat <- read_dta("PREVENT-TAHA8-Figshare.dta")

# select lab panel vars
lab_panel_vars <- c("WBC", "Hb", "Plt", "BUN", "Cr", "Na", "BS", 
                    "TOTALCHO", "LDL", "HDL", "TG")

# create subset "complete clones"
# which consists of rows with identical matches across all lab panel vars
complete_clones <- dat |>
  select(obs_id, all_of(lab_panel_vars)) |>
  filter(!is.na(TOTALCHO)) |>
  group_by(across(-obs_id)) |>
  mutate(
    clone_group_size = n()
  ) |>
  filter(clone_group_size > 1) |>
  ungroup() |>
  group_by(across(c(-obs_id, -clone_group_size))) |>
  mutate(clone_group_id = cur_group_id()) |>
  ungroup() |>
  arrange(clone_group_id, obs_id)

# note all rows with non-missing cholesterol are clones
id1 <- dat |> 
  filter(!is.na(TOTALCHO)) |>
  pull(obs_id)

id2 <- complete_clones$obs_id

setdiff(id1, id2)

# example: clone group 1
complete_clones |>
  filter(clone_group_id==1) |>
  select(-clone_group_size) |>
  kable(format="markdown")

# summarize the clone groups
clone_groups <- complete_clones |>
  group_by(clone_group_id) |>
  summarise(
    n_clones = n(),
    obs_ids = paste(obs_id, collapse = ", ")
  ) 

# plain text
clone_groups |>
  as.data.frame() |>
  print(row.names = FALSE, right = FALSE)

# markdown
clone_groups |>
  as.data.frame() |>
  kable(format="markdown")

# one group of 3 that would have a 4th member: 398
# but dataset only goes up to 396
dim(dat)
max(dat$obs_id)
