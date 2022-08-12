# Load Shiny
library(shiny)
library(shinythemes)

# load libraries
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
setwd('~/info201/a3---data-visualization-and-applications-staccjch/data')

# Load datasets
jailpop_year_df <- read.csv('year-end-prison-2021.csv')
incar_trend_df <- read.csv('incarceration_trends.csv')

# Data Cleasing
incar_trend_2018 <- incar_trend_df %>%
  filter(year == 2018) %>%
  select(state, total_pop:male_pop_15to64, aapi_pop_15to64:white_pop_15to64) %>%
  group_by(state) %>%
  summarise_each(funs(sum))
# Trend over time chart: top 10 states Black imprisonment population over 1990-2018
# First get top 10 states names
top_10_black_prison <- incar_trend_2018 %>%
  select(state, black_pop_15to64) %>%
  arrange(desc(black_pop_15to64)) %>%
  slice(1:10)
top_10_black_vector <- top_10_black_prison$state
# filter out top 10 Black imprisonment states' data
black_trend_df <- incar_trend_df %>%
  filter(year%in% 1990:2018) %>%
  select(state, year, county_name, black_pop_15to64)
  black_trend_df <- black_trend_df[black_trend_df$state %in% top_10_black_vector, ]
# get the sum of all counties
Black <- aggregate(black_pop_15to64~state+year, data = black_trend_df, FUN = sum)

# UI: page one info
page_one <- tabPanel(
  titlePanel("Introduction"),
  mainPanel(
    h1("Problem of Domain & Description of Variables"),
    p('For this dataset I want to look at the trend of prison population in each 
    state and their proportion of the country. I also want to explore the racial 
    disparities in prison, which race is being admitted into prison more, and 
    whether this disparity lowered or increased in recent years. Where the 
    imprisonment occured also relate to racial disparaties since many suburban 
    areas are largely gentrified.I want to look at which area have a higher 
    imprisonment number and if thereis a common similarity among all states.'), 
    br(), 
    h1('Summary Values'), 
    h4('(State with Highest Prison Population 2019-2021: Texas'),
    p('Texas 2019: 158820, Texas 2020: 136302, Texas 2021: 133424)'),
    h4('State with the MOST Population Change: Alaska: 159'),
    h4('State with the LEAST Population Change: Texas: -25329'),
    h4('Which state has the highest Black adult imprisonment in 2018?  Texas: 2439277'),
    h4('Which state has the highest Latin adult imprisonment in 2018?  California: 10477829'),
    h4('Which state has the highest female adult imprisonment in 2018?  California: 13092077')
  )
) 

# UI page two info
page_two <- tabPanel(
  titlePanel("Trend over Time Chart"),
  mainPanel(
    h1("Trend over Time Chart"),
    # create a input of chart:
    plotOutput('time_chart'),
    p('I included the chart of top 10 Black Imprisonment States incarceration trend 
    from 1990 to 2018. This chart illustrated a high amount of incarceration trend on Black, 
    showing trend of racism. While some States have lowered their Black incarceration rate over time, 
    like New York and Illinois, other States like Texas and California, Florida rised over time.')
  )
)

# UI page three info
page_three <- tabPanel(
  titlePanel("Variable Comparison Chart"),
  mainPanel(
    h1("Variable Comparison Chart"),
    p('My variable comparison chart showed the relevance between Black imprisonment 
      population and jail rate capacity.')
  )
)

# UI page four info
page_four <- tabPanel(
  titlePanel("Map"),
  mainPanel(
    h1("Map"),
    p('My map showed the total prison admissions population in 2016 in each US counties. 
      Through this map, we can see which State and County have higher imprisonment rate, 
      which might also implies the crime rate trend of the country in general.')
  )
)


# UI page
ui <- navbarPage(
  theme = shinytheme("flatly"),
  "Incarceration Data Visualization",
  page_one,
  page_two,
  page_three,
  page_four
)


# server page
server <- function(input, output) {
  # Viz 1
  output$time_chart <- renderPlot({
    # Plot
    Black %>%
      ggplot( aes(x=year, y=black_pop_15to64, group=state, color=state)) +
      geom_line() +
      scale_color_viridis(discrete = TRUE) +
      ggtitle("Top 10 Black Imprisonment States Incarceration Trend") +
      theme_ipsum() +
      ylab("Black imprisonment population")
  }) 
  # Viz 2
  # output$variable_chart <- renderPlotly({})
  # # Viz 3
  # output$states_map <- renderPlot({})
}




# Run the application
shinyApp(ui = ui, server = server)