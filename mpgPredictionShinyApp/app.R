#################
# Load Packages #
#################

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)

#########################
# Load And Prepare Data #
#########################

## Load data
data(mtcars)

## Change appropriate variables to factor class
mtcars <- within(mtcars, {
        vs <- factor(vs, labels = c("V-Shaped", "Straight"))
        am <- factor(am, labels = c("Automatic", "Manual"))
        cyl  <- ordered(cyl)
        gear <- ordered(gear)
        carb <- ordered(carb)
})

#############################
# Build Dashboard Interface #
#############################

ui <- dashboardPage(
        
        ## Build header
        dashboardHeader(title = "MPG Prediction"),
        
        ## Build sidebar
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Enter your specifications", ## Dropdown menu name
                                 tabName = "inputs",
                                 icon = icon("arrow-down"),
                                 selectInput("transmission", ## Drop down box
                                              "Transmission type",
                                              c("Automatic",
                                                "Manual")),
                                 radioButtons("cylinders", ## Radio buttons
                                              "Number of cylinders",
                                              c("Four" = 4,
                                                "Six" = 6,
                                                "Eight" = 8)),
                                 sliderInput("displacement", ## Slider
                                             "Displacement (cu. in)",
                                             min = 50,
                                             max = 500,
                                             value = 275,
                                             step = 5),
                                 sliderInput("weight", ## Slider
                                             "Vehicle Weight in lbs ('000s)",
                                             min = 1,
                                             max = 6,
                                             value = 3.5,
                                             step = 0.05))
                )
        ),
        
        ## Build body
        dashboardBody(h1(tags$b('Prediction Results'), ## Main title, tags make it bold
                         align = "center",
                         div(style="height:40px")),
                      h3(tags$b('Specifications'), ## Reiterates selections to user, bold
                         align = "center",
                         div(style = "height:30px")),
                      fluidRow(
                              column("", ## For spacing
                                     align = "center",
                                     width =2),
                              column(tags$i("Transmission type:"),
                                     align = "center",
                                     width = 2,
                                     textOutput("transmission")),
                              column(tags$i("Number of cylinders:"),
                                     align = "center",
                                     width = 2,
                                     textOutput("cylinders")),
                              column(tags$i("Displacement (cu. in):"),
                                     align = "center",
                                     width = 2,
                                     textOutput("displacement")),
                              column(tags$i("Vehicle Weight in lbs ('000s):"),
                                     align = "center",
                                     width = 2,
                                     textOutput("weight"),
                                     div(style = "height:60px")),
                              column("", ## For spacing
                                     align = "center",
                                     width =2,
                                     div(style = "height:60px"))),
                      fluidRow(
                              h3(tags$b("Model Prediction"), ## Displays model output
                                 align = "center",
                                 width = 12,
                                 div(style="height:30px")),
                              column(
                                     tags$i("Miles per gallon"),
                                     textOutput("prediction"),
                                     align = "center",
                                     width = 12)))
)

###########################
# Define Dashboard Server #
###########################

server <- function(input, output) {   
        
        ## Show users their inputs
        output$transmission <- renderText({input$transmission})
        output$cylinders <- renderText({input$cylinders})
        output$displacement <- renderText({input$displacement})
        output$weight <- renderText({input$weight})
        
        ## Make prediction based off user inputs
        output$prediction <- renderText({
                
                ## Define model
                mpgModel <- lm(mpg ~ am + cyl + disp + wt, data = mtcars) ## Model used in assignment
                
                prediction <- round(predict(mpgModel, (newData = data.frame(am = input$transmission,
                                                                      cyl = input$cylinders,
                                                                      disp = input$displacement,
                                                                      wt = input$weight) %>% 
                                                         mutate(across(c("cyl"), ~ ordered(.))))), 2) ## Two decimal places, encode cylinder as factor variable
        })

}

###########
# Run App #
###########

shinyApp(ui = ui, server = server)
