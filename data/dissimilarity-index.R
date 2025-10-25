# based on https://walker-data.com/census-r/modeling-us-census-data.html

setwd("/Users/jocelyn/Documents/Pratt/616-interactive-viz/vega-tutorials/data")
library(tidyverse)
library(tidycensus)
library(segregation)
library(dplyr)
library(stringr)
library(jsonlite)

census_api_key("e765fd46add9ecff24174462fb36873559ba91e7")

# set up race variables
race <- c(
  asian = "B03002_006",
  black = "B03002_004",
  hispanic = "B03002_012",
  white = "B03002_003")

# nyc's counties, aka the five boroughs
nyc_counties <- c("Kings", "Queens", "New York", "Bronx", "Richmond")

# get nyc race data
nyc_race <- get_acs(geography = "tract",
                    state = "New York",
                    # geometry = TRUE,
                    county = nyc_counties, 
                    variables = race, 
                    year = 2022)

# get nyc median household income
nyc_income <- get_acs(geography = "tract",
                    state = "New York",
                    county = nyc_counties,
                    variables = "B19013_001E",
                    year = 2022,
                    output = "wide")

# make new column for borough names using dplyr and stringr
nyc_race <- nyc_race %>%
  mutate(borough = case_when(
    str_detect(NAME, "Bronx") ~ "Bronx",
    str_detect(NAME, "Kings") ~ "Brooklyn",
    str_detect(NAME, "New York County") ~ "Manhattan",
    str_detect(NAME, "Queens") ~ "Queens",
    str_detect(NAME, "Richmond") ~ "Staten Island",
    TRUE ~ NA_character_
  ))

# adding borough names to income df
nyc_income <- nyc_income %>%
  mutate(borough = case_when(
    str_detect(NAME, "Bronx") ~ "Bronx",
    str_detect(NAME, "Kings") ~ "Brooklyn",
    str_detect(NAME, "New York County") ~ "Manhattan",
    str_detect(NAME, "Queens") ~ "Queens",
    str_detect(NAME, "Richmond") ~ "Staten Island",
    TRUE ~ NA_character_
  ))

# testing dissimilarity index between two groups
nyc_race %>%
  filter(variable %in% c("white", "black")) %>%
  dissimilarity(
    group = "variable",
    unit = "GEOID",
    weight = "estimate"
  )

# testing dissimilarity by borough
nyc_race %>%
  filter(variable %in% c("white", "black")) %>%
  group_by(borough) %>%
  group_modify(~
  dissimilarity(.x,
                group = "variable",
                unit = "GEOID",
                weight = "estimate")) %>%
  arrange(desc(est))


# calculate multi-group segregation indices per census tract
# ls = unit-level segregation scores
# ls = 1  average segregation
# ls > 1  more segregated than average
# ls < 1  less segregated than average
# ls = 0  no segregation
nyc_localseg <- 
  mutual_local(data = nyc_race,
               group = "variable",
               unit = "GEOID",
               weight = "estimate",
               wide = TRUE)

# add borough column 
nyc_localseg <- nyc_localseg %>%
  left_join(nyc_income %>% select(GEOID, borough),
            by = "GEOID")

head(nyc_localseg)

# add income column
nyc_localseg <- nyc_localseg %>%
  left_join(nyc_income %>% select(GEOID, B19013_001E), 
            by = "GEOID")

# rename census table name to income
nyc_localseg <- nyc_localseg %>%
  rename(
    income = B19013_001E
  )

# remove rows with no income data
nyc_localseg <- na.omit(nyc_localseg)


# export to csv
# write.csv(nyc_localseg, "nyc_censustracts.csv", row.names=FALSE)

# export to json for vega
write_json(nyc_localseg, "nyc_censustracts.json", pretty=TRUE)

