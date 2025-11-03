## Set up ----

library(tidyverse)
library(haven) # load .dta files
library(knitr) # output markdown tables
library(gridExtra) # arrange plots


dat <- read_dta("PREVENT-TAHA8-Figshare.dta")

## Examining K ----

# show K is determined by Gender
dat |>
  group_by(Gender, K) |>
  tally() |>
  kable(format="markdown")

## Formulas ----

# What formula did the authors use to calculate eGFR?

# authors say they use MDRD formula for eGFR
# but data correlates better with the 2009 CKD-EPI formula (though not perfect)
# with some reverse engineering we can see they authors used the CKD-EPI 2009 formula with the wrong kappa for men (1.0 instead of 0.9). when adjusted it matches perfectly.

dat <- dat |>
  mutate(
    eGFR_MDRD = case_when(
      Gender==1 ~ 175 * Cr^(-1.154) * Age^(-0.203) * (0.742),
      Gender==0 ~ 175 * Cr^(-1.154) * Age^(-0.203)
    ),
    eGFR_2009 = case_when(
      Gender == 1 ~ 141 * pmin(Cr/0.7, 1)^(-0.329) * pmax(Cr/0.7, 1)^(-1.209) * 0.993^Age * 1.018,
      Gender == 0 ~ 141 * pmin(Cr/0.9, 1)^(-0.411) * pmax(Cr/0.9, 1)^(-1.209) * 0.993^Age
    ),
    eGFR_2009_k1 = case_when(
      Gender == 1 ~ 141 * pmin(Cr/0.7, 1)^(-0.329) * pmax(Cr/0.7, 1)^(-1.209) * 0.993^Age * 1.018,
      Gender == 0 ~ 141 * pmin(Cr/1.0, 1)^(-0.411) * pmax(Cr/1.0, 1)^(-1.209) * 0.993^Age
    )
  )


## Comparing ----

dat |>
  sample_n(5) |>
  select(obs_id, Gender, Age, Cr, eGFR, eGFR_MDRD, eGFR_2009, eGFR_2009_k1) |>
  kable(output="markdown")


dat |>
  mutate(diff = abs(eGFR - eGFR_2009_k1)) |>
  summarize(mean_diff = mean(diff), max_diff = max(diff)) |>
  kable(output="markdown")
  
  
dat |>
  filter(!near(eGFR, eGFR_2009_k1, tol = 0.00001)) |>
  select(obs_id, eGFR, eGFR_2009_k1)

## Scatterplots ----

theme_set(theme_minimal())
# axis settings
common <- list(coord_fixed(), xlim(20, 145), ylim(20, 145))


p1 <- dat |>
  ggplot(aes(x = eGFR, y=eGFR_MDRD, color=factor(Gender))) +
  geom_point(size=1) +
  common +
  labs(title = "Author's eGFR vs MDRD formula\n") +
  theme(legend.position = "none") +
  annotate("text", x = 110, y = 50, label = "Female", color = "#00BFC4", hjust = 0) +
  annotate("text", x = 110, y = 56, label = "Male", color = "#F8766D", hjust = 0) +
  geom_abline(slope=1, intercept=0, lty="dashed")
p1

p2 <- dat |>
  ggplot(aes(x = eGFR, y=eGFR_2009, color=factor(Gender))) +
  geom_point() + 
  common +
  labs(title = "Author's eGFR vs CKD-EPI 2009 formula\n") +
  theme(legend.position = "none") +
  annotate("text", x = 110, y = 50, label = "Female", color = "#00BFC4", hjust = 0) +
  annotate("text", x = 110, y = 56, label = "Male", color = "#F8766D", hjust = 0) +
  geom_abline(slope=1, intercept=0, lty="dashed")
p2


p3 <- dat |>
  ggplot(aes(x = eGFR, y=eGFR_2009_k1, color=factor(Gender))) +
  geom_point() + 
  common +
  labs(title = "Author's eGFR vs CKD-EPI 2009 formula\nwith incorrect kappa for men") +
  theme(legend.position = "none") +
  annotate("text", x = 110, y = 50, label = "Female", color = "#00BFC4", hjust = 0) +
  annotate("text", x = 110, y = 56, label = "Male", color = "#F8766D", hjust = 0) +
  geom_abline(slope=1, intercept=0, lty="dashed")
p3

grid.arrange(p1, p2, p3, ncol = 1)

# correlations
dat |>
  summarize(cor_MDRD = cor(eGFR, eGFR_MDRD), 
            cor_CKD_EPI_2009 = cor(eGFR, eGFR_2009), 
            cor_CKD_EPI_2009_wrongkappa = cor(eGFR, eGFR_2009_wrongkappa)) |>
  kable(output="markdown")

