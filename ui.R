library(shiny)

ui <- fluidPage(
    titlePanel("CHURN PREDICTION USING LOGISTIC REGRESSION"),
    
    sidebarLayout(
        sidebarPanel(
           numericInput("cust_id",label = "Enter the Customer ID for which you would like to Predict Churn",
                       value = 100)
           ),
    
        mainPanel(
            textOutput("selected_cust_id")
        )    
    )
    
)

server <- function(input,output)
{
    output$selected_cust_id <- renderText(
        {
        paste("You have selected : ",input$cust_id)  
        }
        
    )
}

shinyApp(ui = ui, server = server)

