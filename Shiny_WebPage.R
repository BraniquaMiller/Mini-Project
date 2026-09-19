library(shiny)
library(vetiver)
library(dplyr)

reference_row  <- readRDS("model_artifacts/reference_row.rds")
factor_choices <- readRDS("model_artifacts/factor_choices.rds")

endpoint <- vetiver_endpoint("http://127.0.0.1:8080/predict")

ui <- fluidPage(
  titlePanel("Fargo-Moorhead Home Sold Price Estimator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("total_sq_ft", "Total Square Feet",
                   value = round(reference_row$total_sq_ft), min = 200, max = 10000, step = 50),
      numericInput("total_bedrooms", "Bedrooms",
                   value = round(reference_row$total_bedrooms), min = 0, max = 10, step = 1),
      numericInput("total_bathrooms", "Bathrooms",
                   value = round(reference_row$total_bathrooms), min = 0, max = 10, step = 0.5),
      numericInput("garage_stalls", "Garage Stalls",
                   value = round(reference_row$garage_stalls), min = 0, max = 6, step = 1),
      numericInput("year_built", "Year Built",
                   value = round(reference_row$year_built), min = 1900, max = 2026, step = 1),
      selectInput("city", "City", choices = factor_choices$city,
                  selected = reference_row$city),
      selectInput("style", "Style", choices = factor_choices$style,
                  selected = reference_row$style),
      selectInput("garage_type", "Garage Type", choices = factor_choices$garage_type,
                  selected = reference_row$garage_type),
      actionButton("predict_btn", "Get Predicted Price", class = "btn-primary")
    ),
    mainPanel(
      h3("Predicted Sold Price"),
      verbatimTextOutput("prediction_output"),
      helpText(
        "All other model inputs not shown here are filled in using the ",
        "median (numeric) or most common value (categorical) from the ",
        "training data."
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$predict_btn, {
    
    new_row <- reference_row
    new_row$total_sq_ft     <- input$total_sq_ft
    new_row$total_bedrooms  <- input$total_bedrooms
    new_row$total_bathrooms <- input$total_bathrooms
    new_row$garage_stalls   <- input$garage_stalls
    new_row$year_built      <- input$year_built
    new_row$city            <- input$city
    new_row$style           <- input$style
    new_row$garage_type     <- input$garage_type
    
    result <- tryCatch(
      predict(endpoint, new_row),
      error = function(e) {
        showNotification(
          paste("Prediction failed - is the API running? Error:", conditionMessage(e)),
          type = "error"
        )
        NULL
      }
    )
    
    output$prediction_output <- renderText({
      req(result)
      paste0("$", format(round(result$.pred, 0), big.mark = ","))
    })
  })
}

shinyApp(ui, server)