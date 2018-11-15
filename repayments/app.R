#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
source('repay.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Repayments for MPsych vs MSc"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        h4("Assumptions"),
        p("Student borrows to cover fees only, and completes an MPsych or BSc + MSc"),
        sliderInput("starting.salary",
                    "Starting salary (defaults is UK average for psychology):",
                    min = 18000,
                    max = 30000,
                    value = 19032),
        sliderInput("rpi",
                    "Inflation rate (RPI):",
                    min = 0,
                    max = 20,
                    value = 3.3),
        sliderInput("payrise",
                    "Average annual pay rise (%):",
                    min = -5,
                    max = 10,
                    value = 2)
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("paymentPlot", width = "90%"),
        plotOutput("totalPlot", width = "90%")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  
  currentdata <- reactive({ 
    mpsych <- payoff.ug(amount=9500*4, 
                        income=input$starting.salary, 
                        rpi=input$rpi, 
                        payrise=input$payrise)
    
    msc <- bind_rows(
      payoff.pg(amount=8500, 
                income=input$starting.salary, 
                rpi=input$rpi,
                payrise=input$payrise),
      payoff.ug(amount=9500*3, 
                income=input$starting.salary, 
                rpi=input$rpi, 
                payrise=input$payrise)
      
    ) %>% group_by(year) %>% 
      summarise(salary=first(salary),payments=sum(payments))
    
    bind_rows(mpsych %>% mutate(route="mpsych"),
              msc %>% mutate(route="msc")) %>% 
      filter(year < 31)
  })
  
   output$paymentPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
     
     currentdata() %>% 
       ggplot(aes(year, payments, color=route)) + 
       geom_line(position=position_dodge(width=.2))  + geom_point(position=position_dodge(width=.2)) + xlab("Year after graduating") +
       ylab("Repayment this year (£)") 
     
   }, height = 300, width = 400 )
   
   
   output$totalPlot <- renderPlot({
     
     currentdata() %>%  
       group_by(route) %>% 
       filter(year < 31) %>% 
       arrange(year) %>% 
       mutate(cumpay = cumsum(payments)) %>% 
       ggplot(aes(year, cumpay, color=route)) + 
       geom_point(position=position_dodge(width=.2)) + geom_line(position=position_dodge(width=.2)) + 
       ylab("Total repayed (£)") + xlab("Year after graduation")
   }, height = 300, width = 400 )
}

# Run the application 
shinyApp(ui = ui, server = server)

