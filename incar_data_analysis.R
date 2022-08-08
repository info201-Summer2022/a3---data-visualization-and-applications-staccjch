# load libraries
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyverse)
setwd('~/info201/a3---data-visualization-and-applications-staccjch/incarceration-trends')

# Load datasets
jailpop_year_df <- read.csv('year-end-prison-2021.csv')
incar_trend_df <- read.csv('incarceration_trends.csv')


# Which state has the highest jail population each year?
jail_pop_by_state <- jailpop_year_df[8:57, 2:5]
max_2019_state <- jail_pop_by_state[jail_pop_by_state$total_prison_pop_2019
                                   == max(jail_pop_by_state$total_prison_pop_2019), 
                                   'state_name', drop = FALSE]
max_2020_state <- jail_pop_by_state[jail_pop_by_state$total_prison_pop_2020
                                   == max(jail_pop_by_state$total_prison_pop_2020), 
                                   'state_name', drop = FALSE]
max_2021_state <- jail_pop_by_state[jail_pop_by_state$total_prison_pop_2021
                                    == max(jail_pop_by_state$total_prison_pop_2021), 
                                    'state_name', drop = FALSE]
# All Texas

# Which state has the max difference from 2019-2021
jail_pop_by_state <- jail_pop_by_state %>%
  mutate(pop_growth = total_prison_pop_2021 - total_prison_pop_2019)

max_growth <- jail_pop_by_state[jail_pop_by_state$pop_growth
                                    == max(jail_pop_by_state$pop_growth), 
                                    'state_name', drop = FALSE]
print(max_growth)  # Alaska = 159

min_growth <- jail_pop_by_state[jail_pop_by_state$pop_growth
                                == min(jail_pop_by_state$pop_growth), 
                                'state_name', drop = FALSE]
print(min_growth) # Texas = -25369

# Information in 2018
incar_trend_2018 <- incar_trend_df %>%
  filter(year == 2018) %>%
  select(state, total_pop:male_pop_15to64, aapi_pop_15to64:white_pop_15to64) %>%
  group_by(state) %>%
  summarise_each(funs(sum))
# Which state has the highest Black adult imprisonment in 2018? Texas = 2439277
max_black_2018 <- incar_trend_2018[incar_trend_2018$black_pop_15to64
                                   == max(incar_trend_2018$black_pop_15to64), 
                                   'state', drop = FALSE]

# Which state has the highest Latin adult imprisonment in 2018? CA = 10477829
max_Latin_2018 <- incar_trend_2018[incar_trend_2018$latinx_pop_15to64
                                   == max(incar_trend_2018$latinx_pop_15to64), 
                                   'state', drop = FALSE]
# Which state has the highest female adult imprisonment in 2018? CA = 13092077
max_female_2018 <- incar_trend_2018[incar_trend_2018$female_pop_15to64
                                   == max(incar_trend_2018$female_pop_15to64), 
                                   'state', drop = FALSE]


# Trend over time chart: top 10 states Black imprisonment population over 1990-2018
# First get top 10 states names
top_10_black_prison <- incar_trend_2018 %>%
  select(state, black_pop_15to64) %>%
  arrange(desc(black_pop_15to64)) %>%
  slice(1:10)
top_10_black_vector <- top_10_black_prison $state
print(top_10_black_vector)
# filter out top 10 Black imprisonment states' data
black_trend_df <- incar_trend_df %>%
  filter(year%in% 1990:2018) %>%
  select(state, year, county_name, black_pop_15to64) %>%
black_trend_df <- black_trend_df[black_trend_df$state %in% top_10_black_vector, ]
# get the sum of all counties
Black <- aggregate(black_pop_15to64~state+year, data = black_trend_df, FUN = sum)

# Plot
Black %>%
  ggplot( aes(x=year, y=black_pop_15to64, group=state, color=state)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Top 10 Black Imprisonment States Incarceration Trend") +
  theme_ipsum() +
  ylab("Black imprisonment population")



# Variable Comparison Chart: Relationship between jail rated capacity and black admissions rate
jail_capacity_df <- incar_trend_df %>%
  select(year, state, black_prison_pop, jail_rated_capacity) %>%
  na.omit()

# plotly Plot
library(reshape2)
library(plotly)
library(tidymodels)

x <- jail_capacity_df$jail_rated_capacity
y <- jail_capacity_df$black_prison_pop

lm_model <- linear_reg() %>%
  set_engine('lm') %>%
  set_mode('regression') %>%
  fit(jail_rated_capacity ~ black_prison_pop, data = jail_capacity_df) 

fit <- lm(jail_rated_capacity ~ black_prison_pop, data = jail_capacity_df)

jail_capacity_df %>%
  plot_ly(x = ~jail_rated_capacity) %>%
  add_markers( y = ~black_prison_pop) 
  # %>% add_lines(x = ~jail_rated_capacity, y = fitted(fit))

admissions <- incar_trend_df %>%
  select(year, state, fips, county_name, total_prison_adm) %>%
  na.omit()
admissions_2016 <- admissions %>% filter(year == 2016)
#admissions <- aggregate(total_prison_adm~state+year, data = admissions, FUN = sum)

# Map
library(usmap)
plot_usmap(data = admissions_2016, values = 'total_prison_adm', color = 'grey') +
  scale_fill_continuous(low = 'lightblue2', high = 'navy', name = 'prison admission(2016)', 
                        label = scales::comma) +
  labs(title = 'Counties Prison Admissions', subtitle = 'This map shows prison admission in 2016') +
  theme(legend.position = "right")


# State map
admissions_by_state <- aggregate(total_prison_adm~state+year, data = admissions_2016, FUN = sum)
# Load a shapefile of U.S. states using ggplot's `map_data()` function
state_shape <- map_data("state")
  
# 1. Load a shapefile of U.S. states using ggplot's `map_data()` function
state_shape <- map_data("state")
# 2.Create a blank map of U.S. states
ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) +
  coord_map() # use a map-based coordinate system

# 3. Plot map with data
ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = admissions_by_state$total_prison_adm),
    color = "white", # show state outlines
    size = .1        # thinly stroked
  ) +
  coord_map() + # use a map-based coordinate system
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(fill = "Prison Admissions") +
  blank_theme

# 4. define a minimalist theme for maps
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )
  