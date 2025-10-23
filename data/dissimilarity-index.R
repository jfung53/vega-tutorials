# based on https://walker-data.com/census-r/modeling-us-census-data.html

setwd("/Users/jocelyn/Documents/Pratt/616-interactive-viz/vega-tutorials")
library(tidyverse)
library(tidycensus)
library(segregation)
library(dplyr)
library(stringr)

census_api_key("e765fd46add9ecff24174462fb36873559ba91e7")

# get new york city race/ethnicity data by census tract
fiveBoroughs <- c("Kings", "Queens", "New York", "Bronx", "Richmond")

race <- c(
  white = "B03002_003",
  black = "B03002_004",
  asian = "B03002_006",
  hispanic = "B03002_012")

nyc_data <- get_acs(geography = "tract",
                    state = "New York",
                    # geometry = TRUE,
                    county = fiveBoroughs, 
                    variables = race, 
                    year = 2022)

# median household income data per census tract
mhhincome <- get_acs(geography = "tract",
                    state = "New York",
                    county = fiveBoroughs,
                    variables = "B19013_001E",
                    year = 2022,
                    output = "wide")

# remove NA's
mhhincome2 <- na.omit(mhhincome)

# new column for borough names using dplyr and stringr
nyc_data <- nyc_data %>%
  mutate(borough = case_when(
    str_detect(NAME, "Bronx") ~ "Bronx",
    str_detect(NAME, "Kings") ~ "Brooklyn",
    str_detect(NAME, "New York County") ~ "Manhattan",
    str_detect(NAME, "Queens") ~ "Queens",
    str_detect(NAME, "Richmond") ~ "Staten Island",
    TRUE ~ NA_character_
  ))

# adding borough names to income df
mhhincome2 <- mhhincome2 %>%
  mutate(borough = case_when(
    str_detect(NAME, "Bronx") ~ "Bronx",
    str_detect(NAME, "Kings") ~ "Brooklyn",
    str_detect(NAME, "New York County") ~ "Manhattan",
    str_detect(NAME, "Queens") ~ "Queens",
    str_detect(NAME, "Richmond") ~ "Staten Island",
    TRUE ~ NA_character_
  ))

# testing dissimilarity index between two groups
nyc_data %>%
  filter(variable %in% c("white", "black")) %>%
  dissimilarity(
    group = "variable",
    unit = "GEOID",
    weight = "estimate"
  )

# testing dissimilarity by borough
nyc_data %>%
  filter(variable %in% c("white", "black")) %>%
  group_by(borough) %>%
  group_modify(~
  dissimilarity(.x,
                group = "variable",
                unit = "GEOID",
                weight = "estimate")) %>%
  arrange(desc(est))


# calculate multi-group segregation indices per census tract

local_seg <- 
  mutual_local(
    data = nyc_data,
    group = "variable",
    unit = "GEOID",
    weight = "estimate",
    wide = TRUE
)

# ls = unit-level segregation scores
# ls = 1  average segregation
# ls > 1  more segregated than average
# ls < 1  less segregated than average
# ls = 0  no segregation

# add borough column 
local_seg_export <- local_seg %>%
  left_join(mhhincome2 %>% select(GEOID, borough),
            by = "GEOID")

head(local_seg_export)

# add income column
local_seg_export <- local_seg_export %>%
  left_join(mhhincome2 %>% select(GEOID, B19013_001E), 
            by = "GEOID")

local_seg_export <- local_seg_export %>%
  rename(
    income = B19013_001E
  )

write.csv(local_seg_export, "dissimilarity_scores.csv", row.names=FALSE)
