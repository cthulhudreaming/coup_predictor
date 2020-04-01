#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
coup_df <- read.csv("coup_data.csv", stringsAsFactors = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Coups"),
    sidebarLayout(
        sidebarPanel(
            selectInput("typeInput", "type_of_coup",
                        choices = c("Attempt", "Conspiracy", "Actual coup"))
        ),
        mainPanel(
            imageOutput("coups")
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$coups <- renderImage({
        
        filename = normalizePath(file.path("coups1.gif"))
        
        list(src = filename,
             contentType = 'image/gif')
    }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
