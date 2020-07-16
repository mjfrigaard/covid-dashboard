# ref: 
# https://glin.github.io/reactable/articles/examples.html#embedding-html-widgets

# packages ----------------------------------------------------------------
library(shiny)
library(tidyverse)
library(sparkline)
library(reactable)

# create example data ----
# str(chickwts)
# data <- chickwts %>%
#   as_tibble() %>% 
#   group_by(feed) %>%
#   summarise(weight = list(weight)) %>%
#   mutate(boxplot = NA, sparkline = NA)
# 
# application ----
# ui <- fluidPage(
#   reactableOutput("table")
# )
# server <- function(input, output) {
#   output$table <- renderReactable({
# reactable(data, columns = list(
#   
#   weight = colDef(cell = function(values) {
#     sparkline(values, type = "bar", 
#               chartRangeMin = 0, 
#               chartRangeMax = max(chickwts$weight))
#    }),
#   boxplot = colDef(cell = function(value, index) {
#     
#     sparkline(data$weight[[index]], type = "box")
#    }),
#   sparkline = colDef(cell = function(value, index) {
#     
#     sparkline(data$weight[[index]])
#     })
#   ))
#  
#     })
#   
# }
# shinyApp(ui, server)

# # data --------------------------------------------------------------------
# source("01.0-import-csse-time-series.R")
# source("02.0-wrangle-csse-time-series.R")
sumdata <- WorldTSDataAll %>%
            dplyr::select(Country = country_region,
                          Confirmed = confirmed) %>%
  dplyr::arrange(desc(Confirmed)) %>%
  dplyr::group_by(Country) %>%
  dplyr::summarize(Confirmed = list(Confirmed)) %>%
  dplyr::mutate(boxplot = NA, sparkline = NA)
sumdata

# application  --------------------------------------------------------------
ui <- fluidPage(
  reactableOutput("table")
)

server <- function(input, output) {
  
  output$table <- renderReactable({
    
    reactable::reactable(data = sumdata,
                         
              columns = list(Confirmed = colDef(cell = function(values) {

                       sparkline(values, type = "bar", chartRangeMin = 0,
                               # this uses the actual chickwts$weight data
                               chartRangeMax = max(WorldTSDataAll$confirmed))

                          }),
                              # here we add the index argument to the function
                              # and include the new data$weight[[index]]
                             boxplot = colDef(cell = function(value, index) {

                       sparkline(sumdata$Confirmed[[index]], type = "box")

                          }),
                              # this is identical to the function above
                           sparkline = colDef(cell = function(value, index) {

                      sparkline(sumdata$Confirmed[[index]])

                          })
                    ))
    })

}

shinyApp(ui, server)