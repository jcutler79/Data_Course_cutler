# Shiny code


# ui.r:

### Shiny tutorial #3 - radio button input

library(shiny)

getwd()
# dir.create("/Users/jamescutler/Desktop/Data_Science/radioButtons/")
# setwd("/Users/jamescutler/Desktop/Data_Science/radioButtons/")

shinyUI(fluidPage(
  headerPanel(title = "Demo of shiny widgets"),
  sidebarLayout(
    sidebarPanel("Personal information",
                 textInput("name","First and last name:",""),
                 textInput("age","Age:",""),
                 radioButtons("gender","Select gender:",list("Male","Female","Gender Queer"),"")),
    mainPanel("Personal Information",
              textOutput("myname"),
              textOutput("myage"),
              textOutput("mygender"))
  )
))


##################

### shiny tutorial #3 - radiobuttons server.R

shinyServer(
  function(input, output){
    output$myname = renderText(input$name) # Whatever you type in after the input$ has to be the same as the inputID of the textInput function in the shinyUI
    output$myage = renderText(input$age)
    output$mygender = renderText(input$gender)
  }
)


