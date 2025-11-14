library(tidyverse)
library(wbstats) # World Bank data
library(psych) # for summary stats

# "EN.ATM.CO2E.KT" - CO2 emmissions no longer available
vars <- c("NY.GDP.MKTP.KD", "EG.FEC.RNEW.ZS", 
          "SP.URB.TOTL", "SP.POP.TOTL", "SP.POP.GROW")

countries <- c(
  "Ghana",
  "Burkina Faso",
  "Cabo Verde",
  "Gambia, The",
  "Benin",
  "Guinea",
  "Guinea-Bissau",
  #"Ivory Coast",
  "Cote d'Ivoire",
  "Liberia",
  "Mali",
  "Mauritania",
  "Niger",
  "Nigeria",
  "Senegal",
  "Sierra Leone",
  "Togo"
)
countries <- c("BF", "BJ", "CI", "CV", "GH", "GM","GN", "GW", 
               "LR", "ML", "MR", "NE","NG", "SL", "SN", "TG")

wdi <- wb_data(vars, start_date = 1990, end_date = 2018) 

summary_stats <- wdi |> 
  filter(iso2c %in% countries) |>
  mutate(URB = SP.URB.TOTL / SP.POP.TOTL) |>
  mutate(lnGDP = log(NY.GDP.MKTP.KD),
         lnREC = log(EG.FEC.RNEW.ZS),
         lnURB = log(URB),
         lnPOP = log(SP.POP.GROW)) |>
  select(lnGDP:lnPOP) |>
  psych::describe()

summary_stats |> 
  select(-vars, -trimmed, -mad, -range, -se) |>
  round(2) |> kable(output="markdown")
