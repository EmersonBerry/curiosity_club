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

### to do ###
# link to google sheet to track
# exclude friday's within two days
# add more helper text
# colors etc make it look nicer

library(shiny)
library(lubridate)
library(dplyr)
library(glue)
library(blastula)
library(rsconnect)
library(shinyalert)
library(googlesheets4)

################################################################################
# Gmail and google sheets setup

# link to google sheet
gs_link <- "https://docs.google.com/spreadsheets/d/1lnVsoWWl3y-KrtjkLuOg1WNq7nuDYYMx1TbTb8qhvvw/edit?usp=sharing"

# authenticate to google sheets
googlesheets4::gs4_auth(token = Sys.getenv("CC_KEY"), email = Sys.getenv("MY_GMAIL_ACCOUNT"))

# set email credentials
my_email_creds <- creds_envvar(
  user = Sys.getenv('MY_GMAIL_ACCOUNT'),
  pass_envvar = 'SMTP_PASSWORD', 
  provider = 'gmail'
)

# Read in current google sheet
gs_current <- read_sheet(gs_link, sheet = 1)

################################################################################
# Date setup

# Curiosity club meets on the first and third Friday of the month
# Find the next 6 dates of Curiosity Club

# create df of all dates for the next year
today_dte <- today()
one_year_away <- today_dte + 365
date_list <- seq(as.Date(today_dte), as.Date(one_year_away), by = "day") 

# get day of week and day of month
date_df <- as.data.frame(date_list)  %>% 
  rename(
    date = date_list
  ) %>%
  mutate(
    day_label = wday(date, label = TRUE), # get weekday label
    day_of_month = day(date)              # get day of month
  )

# pull out first Friday of each month
first_fridays <- date_df %>% 
  filter(day_label == "Fri" & day_of_month <= 7) %>% 
  pull(date)

# pull out third Friday of each month
third_fridays <- date_df %>% 
  filter(day_label == "Fri" & day_of_month <= 21 & day_of_month > 14) %>% 
  pull(date)

# put list of first and third Fridays together, then pull the first six to display in app
next_fridays_year <- c(first_fridays, third_fridays) %>% sort()
next_six_friday_dates <- next_fridays_year %>% head(6)
next_six_fridays_label <- glue("Friday, {month(next_six_friday_dates, label = TRUE, abbr = FALSE)} {day(next_six_friday_dates)} from 7-8:30pm")

################################################################################
### App Code ###

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Curiosity Club Submission Form"),
    br(),
    h4("Please submit this form if you want to present at Brocialize Curiosity Club."),
    # br(),
    p("Curiosity Club meets on the first and third Friday of each month, from 7-8:30pm in the Brocialize Discord server curiosity-club meeting channel. You must be a member of the Brocialize 
      discord server to attend and to present. Please message a moderator if you need help or have any questions."),
    br(),
    
    textInput(inputId = "first_name",label = "First Name:"),
    
    textInput(inputId = "discord_name", label = "Discord Username:"),
    
    # p("Info about your presentation"),
    
    selectizeInput(inputId = "presentation_type",
                   label = "Select the type of presentation:",
                   choices = c("PowerPoint/Slides Presentation",
                               "Whiteboard Presentation",
                               "Oral only Presentation",
                               "Interactive Quiz/Game",
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
                  width = "100%"),
    
    checkboxGroupInput(inputId = "date_available", 
                       label = "Select which date(s) you are available to virtually present:",
                       choices = next_six_fridays_label),
    
    textInput(inputId = "note", label = "Anything else we should know?"),
    
    br(),
    
    actionButton(inputId = "submission", 
                 label = "Click here to Submit!"),
    
    br(),
    br(),
    br()
)


# 
server <- function(input, output) {

  
  observeEvent(input$submission, {
    
    # if(input$discord_name != "" & input$title != "" & input$date_availabe != ""){
    msg <-  
      compose_email(
        body = md(
          glue::glue(
            "Hello,
        
        {input$first_name} ({input$discord_name}) has submitted an idea for a {input$presentation_type}, 
        
        Title: {input$title}
        
        Length: {input$length_of_presentation}
        
        Description:
        {input$description}
        
        They are available to present on the following dates:
        
          {paste(input$date_available, collapse = ', <br>')}
        
        And they would like to note (if any):
        
          {input$note}
        
        ")))
    
    msg %>% 
      smtp_send(
        from = Sys.getenv('MY_GMAIL_ACCOUNT'),
        to = Sys.getenv('MY_GMAIL_ACCOUNT'),
        subject = "Testing the `smtp_send()` function",
        credentials = my_email_creds
      )
    
    shinyalert(title = "Thank you!", type = "success")
    
    new_submission <- data.frame(
      "date_submitted" = now(),
      "name" = input$first_name,
      "username" = input$discord_name,
      "type" = input$presentation_type,
      "length" = input$length_of_presentation,
      "title" = input$title,
      "description" = input$description,
      "dates_available" = paste(input$date_available, collapse = ', '),
      "note" = input$note
    )
    
    # append new row to existing sheet
    sheet_append(ss = gs_link,
                 data = new_submission,
                 sheet = 1)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
