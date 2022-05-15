library(httr)
library(tidyverse)
library(lubridate)
library(dplyr)
library(shiny)
# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Cleaning"),

  # Sidebar with a slider input
  sidebarPanel(
    textInput("link", label = h3("Enter a link"), value = "https://data.iowa.gov/resource/m3tr-qhgy.json")
  ),

  # Show a numeric
  mainPanel(
    plotOutput("cc$date"),
  )
)
# Define server logic required to show
server <- function(input, output) {
  output$cc$date <- renderPlot({
    html = GET(input$link)
    a = content(html)
    b = unlist(unlist(a))
    c = as.data.frame(do.call(cbind, a))
    aa = as.data.frame(t(c))
    cc$sale_dollars = as.numeric(cc$sale_dollars)
    cc$date = gsub("T00:00:00.000", "", cc$date)
    cc$date = ymd(cc$date)
    cc$store_location = sub(
      pattern = ".*list\\((.*))\\).*",
      replacement = "\\1",
      x = cc$store_location
    )
    cc$stor_long_location = as.numeric(sub(
      pattern = "(.*)\\,.*",
      replacement = "\\1",
      x = cc$store_location
    ))
    cc$stor_lati_location = as.numeric(sub(
      pattern = ".*\\,(.*)",
      replacement = "\\1",
      x = cc$store_location
    ))
    cc$na = as.numeric(cc$store_location)
    dd = na.omit(cc)
    w = which(!is.na(cc$na))
    cc <- cc[-c(w), ]
    return(plot(cc$date))
  })
}
# Bind ui and server together
shinyApp(ui, server)
