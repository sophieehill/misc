#################################
## Set up ----
#################################

library(tidyverse)
library(haven)
library(knitr)
library(openxlsx)

# load data
dat <- read_dta("PREVENT-TAHA8-Figshare.dta")

# reformat for clarity
dat <-
  dat |>
  mutate(HFdevel = as.numeric(HFdevel),
         HF_rehosp = as.numeric(HF_rehosp),
         ReMI = as.numeric(ReMI),
         Death = as.numeric(Death),
         group = as.numeric(group))

#################################
## Analysis ----
#################################

# function to identify sequential rows with the same age (sorted by obs_id)
identify_age_patterns <- function(data, id_col = "obs_id", age_col = "Age") {
  data <- data[order(data[[id_col]]), ]
  age_rle <- rle(as.numeric(as.character(data[[age_col]])))
  
  cumsum_len <- cumsum(age_rle$lengths)
  runs_df <- data.frame(
    age = age_rle$values,
    run_length = age_rle$lengths,
    start_id = data[[id_col]][c(1, cumsum_len[-length(cumsum_len)] + 1)],
    end_id = data[[id_col]][cumsum_len]
  )
  
  runs_df[runs_df$run_length > 1, ][order(-runs_df$run_length[runs_df$run_length > 1]), ]
}

# find runs of 2 or more
age_runs <- identify_age_patterns(dat)

# markdown table output
age_runs |> select(start_id, end_id, run_length, age) |>
  arrange(-run_length, start_id) |>
  kable(output="markdown")

# extract corresponding obs_id values
all_ids <- lapply(1:nrow(age_runs), function(i) {
  age_runs$start_id[i]:age_runs$end_id[i]
})

# now cross-check: 
# for each group of sequential obs_id values, 
# is HFdevel_days also constant?
keep_ids <- unlist(all_ids[sapply(all_ids, function(ids) {
  length(unique(dat$HFdevel_days[dat$obs_id %in% ids])) == 1
})])

# for 46/67 of these rows, HFdevel_days is constant within group
length(unlist(all_ids))
length(keep_ids)

# filter to those rows
evidence <-
  dat |>
  filter(obs_id %in% keep_ids) |>
  select(
    obs_id,
    Age,
    HFdevel,
    HFdevel_days,
    HF_rehosp,
    HF_rehosp_Days,
    ReMI,
    ReMI_Days,
    Death,
    Death_Days,
    group
  ) |>
  arrange(obs_id)

# note: Height/Weight all missing in this subset
# vs low missingness elsewhere
dat |>
  mutate(duplicate_subset = obs_id %in% keep_ids) |>
  group_by(duplicate_subset) |>
  summarize(n = n(), 
            pct_Height_NA = round(100*sum(is.na(Height))/n(), 1), 
            pct_Weight_NA = round(100*sum(is.na(Weight))/n(), 1)) |>
  kable(output="markdown")

########################################
## Create spreadsheet to visualize ----
########################################

# Create workbook
wb <- createWorkbook()
addWorksheet(wb, "Evidence")
writeData(wb, "Evidence", evidence)

# ncols
ncol <- dim(evidence)[2]

# Add borders
# Outer border - do edges separately
top_style <- createStyle(border = "top", borderStyle = "thin")
bottom_style <- createStyle(border = "bottom", borderStyle = "thin")
left_style <- createStyle(border = "left", borderStyle = "thin")
right_style <- createStyle(border = "right", borderStyle = "thin")

addStyle(
  wb,
  "Evidence",
  top_style,
  rows = 1,
  cols = 1:ncol,
  gridExpand = TRUE,
  stack = TRUE
)
addStyle(
  wb,
  "Evidence",
  bottom_style,
  rows = nrow(evidence) + 1,
  cols = 1:ncol,
  gridExpand = TRUE,
  stack = TRUE
)
addStyle(
  wb,
  "Evidence",
  left_style,
  rows = 1:(nrow(evidence) + 1),
  cols = 1,
  gridExpand = TRUE,
  stack = TRUE
)
addStyle(
  wb,
  "Evidence",
  right_style,
  rows = 1:(nrow(evidence) + 1),
  cols = ncol,
  gridExpand = TRUE,
  stack = TRUE
)


header_style <- createStyle(
  textDecoration = "bold",
  fgFill = "#D9D9D9",
  halign = "right",
  border = "TopBottom",
  borderStyle = "thin"
)
addStyle(
  wb,
  "Evidence",
  header_style,
  rows = 1,
  cols = 1:ncol,
  gridExpand = TRUE,
  stack = TRUE
)
setColWidths(wb, "Evidence", cols = 1:ncol, widths = "auto")
setRowHeights(wb, "Evidence", rows = 1:(nrow(evidence) + 1), heights = 16)



# Define colors for each age group
# need 17


colors <- c(
  "#FFB3B3", "#B3D9FF", "#FFE6B3", "#D9B3FF", 
  "#B3FFB3", "#FFB3E6", "#B3FFFF", "#FFD9B3",
  "#FFC9C9", "#C9E6FF", "#FFF0C9", "#E6C9FF",
  "#C9FFC9", "#FFC9F0", "#C9FFFF", "#FFE6C9",
  "#DAB9FF"
)




# modify to account for one age run that splits into two HFdevel_days groups
# Group by both Age AND HFdevel_days
age_days_group <- with(evidence, paste(Age, HFdevel_days))
age_days_runs <- rle(age_days_group)

current_row <- 2
color_idx <- 1

for (i in seq_along(age_days_runs$lengths)) {
  if (age_days_runs$lengths[i] > 1) {
    style <- createStyle(fgFill = colors[color_idx])
    rows <- current_row:(current_row + age_days_runs$lengths[i] - 1)
    addStyle(
      wb,
      "Evidence",
      style,
      rows = rows,
      cols = 1:ncol,
      gridExpand = TRUE,
      stack = TRUE
    )
    color_idx <- color_idx + 1
  }
  current_row <- current_row + age_days_runs$lengths[i]
}


saveWorkbook(wb, "duplicate_outcomes.xlsx", overwrite = TRUE)
