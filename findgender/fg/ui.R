library(shiny)


shinyUI(fluidPage(
  titlePanel("Identify Gender"),
  fluidRow(
    column(4, wellPanel(
      textInput("n", label = h3("Enter First Name"), value = ""),
      actionButton("goButton", "Find Gender!"),
      br(),
      br(),
      p("Enter firstname in the text box and click button to identify the gender.",
        br(),
        "The gender will be displayed below:")
    )),
    column(8,
           textOutput(outputId = "gender"),
           textOutput(outputId = "message")
    )
  )
))