# Global variables can go here
library(shiny)
library(ggplot2)
# library(lubridate)
library(tidyverse)
# library(readxl)
# library(googlesheets)
# library(reshape2)
library(scales)
sell_list <- read.csv("sell_list.csv", fileEncoding = "UTF-8")

## Make sure chinese word can show in shinyapp.io
font_home <- function(path = '') file.path('~', '.fonts', path)
if (Sys.info()[['sysname']] == 'Linux' &&
      system('locate wqy-zenhei.ttc') != 0 &&
      !file.exists(font_home('wqy-zenhei.ttc'))) {
  if (!file.exists('wqy-zenhei.ttc'))
    shiny:::download(
      'https://github.com/rstudio/shiny-examples/releases/download/v0.10.1/wqy-zenhei.ttc',
      'wqy-zenhei.ttc'
    )
  dir.create(font_home())
  file.copy('wqy-zenhei.ttc', font_home())
  system2('fc-cache', paste('-f', font_home()))
}


# Define the UI
ui <- 
  shinyUI(fluidPage(
  # Application title
  navbarPage(
    "Sendo E-commerce selling dashboard",
    tabPanel(
      "Historical_Trend",
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "year",
            label = "Choose the year",
            choices = c("All", unique((sell_list$year)))
          ),
          selectInput(
            inputId = "month",
            label = "Choose the month",
            choices = unique(sell_list$month)
          ),
          selectInput(
            inputId = "ranking",
            label = "Choose the ranking",
            choices = 1:length(unique(sell_list$product_name)),
            selected = 10
          )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("his_trend"),
           tags$blockquote("This plot describe the frequency of selling by each products. Last update is 10-Sep-2018")
        )
      )
    ),
    tabPanel("Monthly_Trend",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "product",
            label = "Choose the product",
            choices = unique(sell_list$product_name)
          )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("prod_mon"),
           tags$blockquote("This plot describe the frequency of selling by each products. Last update is 10-Sep-2018")
        )
      )
    )
    #,tabPanel("Under_Construction")
  )
))

# Define the server code
server <- 
  shinyServer(function(input, output) {
  # Navbar1: Historical_Trend
  output$his_trend <- renderPlot({
    yr <- input$year
    mt <- input$month
    ranking <- input$ranking
    
    uidata <- 
      if (input$year == "All") {sell_list} else {subset(sell_list, year == yr & month == mt)} # subset data(year, month) by ui
    sell_aggr <- data.frame(table(gsub(" ", "", uidata$product_name))) %>% arrange(., desc(Freq)) # count the length of data sell, and order it desc.
    names(sell_aggr) <- c("product_name", "frequency")
    
    ggplot(sell_aggr[1:ranking,], aes(x = reorder(product_name, -frequency), y = frequency)) +
      geom_bar(stat = "identity") +
      theme(axis.title = element_text(size = 20, face = "bold"),
            axis.text.x = element_text(size = 18, angle = 45, hjust = 1, vjust = 1.2), 
            axis.text.y = element_text(size = 18, hjust = 1)
            ) + 
      labs(x = "Product Name", y = "Count") + 
      geom_text(aes(product_name, frequency + 10, label = frequency), size = 5)
  })
  
  # Navbar2: Monthly_Trend
  output$prod_mon <- renderPlot({
    prod_mon_data <- subset(sell_list, product_name == input$product) %>% 
      aggregate(product_name ~ month + year, ., length)
    names(prod_mon_data) <- c("month", "year", "frequency")
    
    # draw the histogram with the specified number of bins
    ggplot(prod_mon_data, aes(x = as.factor(month), y = frequency, fill = as.factor(year))) +
      geom_bar(stat = "identity", position = "dodge") +
      theme(axis.title = element_text(size = 20, face = "bold"),
            axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 18),
            legend.title = element_text(size = 20, face = "bold"),
            legend.text = element_text(size = 18)
            ) + 
      labs(x = "Month", y = "Count", fill = "Year") + 
      scale_y_continuous(breaks = pretty_breaks(4))
  })
})

# Return a Shiny app object
shinyApp(ui = ui, server = server)