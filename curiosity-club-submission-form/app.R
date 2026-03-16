# Curiosity Club Submission Form 
# 
# This shiny app is a submission form for people who wish to present at the MN Brocialize Curiosity Club.
#
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(lubridate)
library(dplyr)
library(glue)
library(rsconnect)

################################################################################

# Curiosity club meets on the first and third Friday of the month
# Find the next 6 dates of Curiosity Club

today_dte <- today()
one_year_away <- today_dte + 365
date_list <- seq(as.Date(today_dte), as.Date(one_year_away), by = "day") # df of all dates for the next year

date_df <- as.data.frame(date_list)  %>% 
  rename(
    date = date_list
  ) %>%
  mutate(
    day_label = wday(this_year, label = TRUE), # get weekday label
    day_of_month = day(date_list)              # get day of month
  )

# pull out first friday of each month
first_fridays <- date_df %>% 
  filter(day_label == "Fri" & day_of_month <= 7) %>% 
  pull(date)

# pull out third friday of each month
third_fridays <- date_df %>% 
  filter(day_label == "Fri" & day_of_month <= 21 & day_of_month > 14) %>% 
  pull(date)

# put list of first and third fridays together, then pull the first six to display in app
next_fridays_year <- c(first_fridays, third_fridays) %>% sort()
next_six_friday_dates <- next_fridays_year %>% head(6)
next_six_fridays_label <- glue("Friday, {month(next_six_friday_dates, label = TRUE)} {day(next_six_friday_dates)} from 7-8:30pm")

################################################################################

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Curiosity Club Submission Form"),
    br(),
    h4("Please submit this form if you wish to present at Brocialize Curiosity Club."),
    # br(),
    p("Curiosity Club meets on the first and third Friday of each month, from 7-8:30pm. You must be a member of the Brocialize 
      discord server to attend and to present. Please message a moderator if you need help or have any questions."),
    br(),
    
    textInput(inputId = "first_name",label = "First Name:"),
    
    textInput(inputId = "discord_name", label = "Discord Handle:"),
    
    # p("Info about your presentation"),
    
    selectizeInput(inputId = "presentation_topic_group",
                   label = "Select the type of presentation:",
                   choices = c("PowerPoint/Slides Presentation",
                               "Whiteboard Presentation/Discussion",
                               "Oral only Presentation",
                               "Interactive Game/Quiz",
                               "Other")),
    selectInput(inputId = "length_of_presentation",
                label = "Select the approximate length of your presentation:",
                choices = c("5 min",
                            "5-10 min",
                            "10-15 min",
                            "15-20 min",
                            "20-30 min",
                            "30-45 min")),
    
    textInput(inputId = "title", 
              label = "Presentation Title"),
    
    textAreaInput(inputId = "description", 
                  label = "Please describe your presentation in a few sentences.",
                  width = "75%"),
    
    textInput(inputId = "note", label = "Anything else we should know?"),
    
    checkboxGroupInput(inputId = "date_available", 
                       label = "Select which date(s) you are available to present:",
                       choices = next_six_fridays_label),
    
    actionButton(inputId = "submission", 
                 label = "Submit Here!"),
    
    br(),
    br()
    
    
    
    
    # textInput(inputId = "pronouns", label = "Pronouns")
    # textInput(inputId = "email", label = "Email address")
    
              
    # sidebarLayout(
    #     sidebarPanel(
    #         sliderInput("bins",
    #                     "Number of bins:",
    #                     min = 1,
    #                     max = 50,
    #                     value = 30)
    #     ),
    # 
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #        plotOutput("distPlot")
    #     )
    # )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
