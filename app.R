library(shiny)

ui <- fluidPage(
    titlePanel("CHURN PREDICTION USING LOGISTIC REGRESSION"),
    
    sidebarLayout(
        sidebarPanel(
         #   numericInput("cust_id",label = "Enter the Customer ID for which you would like to Predict Churn",
              #           value = 15590876),
            numericInput("creditscore",label = "Enter Credit Score", 
                         value = 772),
            textInput("loc",label="Enter Country",value="Germany"),
            #selectInput(inputId = "gender",label = "Choose Gender",
            #           choices = list("Female" = 0, 
             #                          "Male" = 1),
              #          selected = 1),
            textInput("gender",label = "Enter Gender", value = "Male"),
            numericInput("age",label = "Enter Age", value=42,min = 18),
            numericInput("balance",label = "Enter Balance", value=75075.31),
            numericInput("prods",label = "Enter Number Of Products", value=1,min = 1),
            numericInput("active",label = "Active Member ? 
                        1 - Yes 
                        2- No",
                        value =  0),
            submitButton("PREDICT!")
        ),
        
        mainPanel(
          #uiOutput("tb")
            textOutput("model_predict"),
            plotOutput("conclusion")
        )    
    )
    
)

data <- read.csv("Churn_Modelling2.csv")
data_train <- data[2:8000,]
data_test <- data[8001:10001,]
server <- function(input,output)
{
      #  output$tb <- renderUI({
       #     tabsetPanel(
               # tabPanel("About Project",),
        #        tabPanel("LOG REG",textOutput("model_predict")))
        #})
        
        output$model_predict <- renderText({
            LogModel <- glm(Exited ~ CreditScore+Geography+Gender+Age+Balance+NumOfProducts+IsActiveMember,
                            family = binomial('logit'),
                            data = data_train)
            
            CreditScore <- input$creditscore
            Geography <- input$loc
            Gender <- input$gender
            Age <- input$age
            Balance <- input$balance
            NumOfProducts <- input$prods
            IsActiveMember <- input$active
            paste(input$creditscore)
            toPredict <- data.frame(CreditScore,Geography,Gender,Age,Balance,NumOfProducts,IsActiveMember)
            paste(toPredict)
            result <- predict(object = LogModel,newdata = toPredict,type = 'response')
            paste(result)
            
            result2 <- ifelse(result > 0.5,1,0)
            paste("Exited : ", result2)
            #paste("Threshold is at 50%")
            
            # result <- predict(LogModel,newdata = toPredict, type = 'response')
        })
        
        output$conclusion <- renderPlot({
            LogModel <- glm(Exited ~ CreditScore+Geography+Gender+Age+Balance+NumOfProducts+IsActiveMember,
                            family = binomial('logit'),
                            data = data_train)
                            
        LogModel2 <- glm(Exited ~ Geography,
                                            family = binomial('logit'),
                                            data = data_train)
            
            CreditScore <- input$creditscore
            Geography <- input$loc
            Gender <- input$gender
            Age <- input$age
            Balance <- input$balance
            NumOfProducts <- input$prods
            IsActiveMember <- input$active
            #paste(input$creditscore)
            #toPredict <- data.frame(CreditScore,Geography,Gender,Age,Balance,NumOfProducts,IsActiveMember)
            #paste(toPredict)
            result <- predict(object = LogModel,newdata = data_test,type = 'response')
            result2 <- ifelse(result > 0.5,1,0)
            error <- mean (result2!=data_test$Exited)
            accu <- 1-error
           # paste(data_test$Exited, result2 > 0.5)
           # paste("Accuracy is : ",accu)
            plot(result2)
            cm <- table(data_test$Exited, result2 > 0.5)
            
            result3 <- predict(object = LogModel2,newdata = data_test,type = 'response')
            result4 <- ifelse(result3 > 0.5,1,0)
            plot(x = data_test$Geography, y = result4)
           
             plot(cm)
        })
        
    
}

shinyApp(ui = ui, server = server)


