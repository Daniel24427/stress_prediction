# ----------------- Load Required Libraries -----------------
#install.packages("shinythemes")
library(shiny)
library(readr)
library(dplyr)
library(shinythemes)


# ----------------- Load and Score the Cleaned Data -----------------

data <- read_csv("student_depression_cleaned.csv")

data <- data %>%
  mutate(
    Gender_Score = ifelse(Gender == "Female", 1, 0),
    Suicidal_Score = ifelse(Suicidal_Thoughts == "Yes", 3, 0),
    Family_History_Score = ifelse(Family_Mental_History == "Yes", 2, 0),
    Sleep_Score = case_when(
      Sleep_Duration == "Less than 5 hours" ~ 4,
      Sleep_Duration == "5-6 hours" ~ 3,
      Sleep_Duration == "7-8 hours" ~ 1,
      Sleep_Duration == "More than 8 hours" ~ 2,
      TRUE ~ NA_real_
    ),
    Dissatisfaction_Score = 5 - Study_Satisfaction,
    Academic_Score = Academic_Pressure * 1.5,
    Financial_Score = Financial_Stress * 2
  )


# ----------------- Train the Logistic Regression Model -----------------

model <- glm(Depression ~ Gender_Score + Age + Academic_Score + Dissatisfaction_Score +
               Sleep_Score + Suicidal_Score + Study_Hours +
               Financial_Score + Family_History_Score,
             data = data, family = binomial)


# ----------------- Define UI -----------------

ui  <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel(div(h1("🧠 Student Stress Level Predictor", style = "color: #00FFFF; text-align: center;"))),
  
  fluidRow(
    column(12,
           h4("Developer: Daniel Habtu Gebrai", align = "center"),
           h5("Digital Health – Global Public Health", align = "center"),
           h5("Deggendorf Institute of Technology", align = "center")
    )
  ),
  hr(),
  
  fluidRow(
    column(
      width = 6,
      div(class = "input-panel",
          fluidRow(
            column(6,
                   sliderInput("Age", "Age:", min = 18, max = 40, value = 25),
                   selectInput("Gender_Score", "Gender:", choices = c("Female" = 1, "Male" = 0)),
                   selectInput("Academic_Score", "How Much Academic Pressure Do You Feel?",
                              choices = c(
                                "None" = 1.5,
                                "Mild" = 3,
                                "Moderate" = 4.5,
                                "High" = 6,
                                "Extreme" = 7.5
                              ),
                              selected = 3),
                   selectInput("Dissatisfaction_Score", "How Satisfied Are You With Your Studies?",
                              choices = c(
                                "Very Satisfied" = 1,
                                "Satisfied" = 2,
                                "Neutral" = 3,
                                "Dissatisfied" = 4,
                                "Very Dissatisfied" = 5
                              ),
                              selected = 3)
            ),
            column(6,
                  selectInput("Sleep_Score", "How Much Sleep Do You Typically Get?",
                              choices = c(
                                "More than 8 hours" = 2,
                                "7–8 hours" = 1,
                                "5–6 hours" = 3,
                                "Less than 5 hours" = 4
                              ),
                              selected = 2),
                  selectInput("Suicidal_Score", "Have You Ever Had Suicidal Thoughts?", 
                               choices = c("No" = 0, "Yes" = 3), selected = 0),
                  selectInput("Study_Hours", "How Many Hours Do You Study Per Day?",
                              choices = c(
                                "0–2 hours" = 3,
                                "3–5 hours" = 1,
                                "6–8 hours" = 2,
                                "9+ hours" = 4
                              ),
                              selected = 1),
                  selectInput("Financial_Score", "How Much Financial Stress Do You Experience?",
                              choices = c(
                                "None" = 0,
                                "Mild" = 1,
                                "Moderate" = 2,
                                "High" = 3,
                                "Very High" = 4,
                                "Extreme" = 5
                              ),
                              selected = 2),
                  selectInput("Family_History_Score", "Do You Have a Family History of Mental Illness?",
                               choices = c("No" = 0, "Yes" = 2), selected = 0)
                   
            )
          )
      )
    ),
    column(
      width = 6,
      div(class = "output-panel",
          h3("Prediction Output"),
          verbatimTextOutput("risk_text"),
          plotOutput("risk_bar", height = "300px"),
          h4("Recommendation:"),
          verbatimTextOutput("recommendation")
      )
    )
  )
)


# ----------------- Define Server Logic -----------------

server <- function(input, output) {
  
  depression_risk <- reactive({
    new_input <- data.frame(
      Gender_Score = as.numeric(input$Gender_Score),
      Age = input$Age,
      Academic_Score = as.numeric(input$Academic_Score),
      Suicidal_Score = as.numeric(input$Suicidal_Score),
      Family_History_Score = as.numeric(input$Family_History_Score),
      Dissatisfaction_Score = as.numeric(input$Dissatisfaction_Score),
      Sleep_Score = as.numeric(input$Sleep_Score),
      Study_Hours = as.numeric(input$Study_Hours),
      Financial_Score = as.numeric(input$Financial_Score)
     
     )
    
    prob <- predict(model, new_input, type = "response")
    return(prob * 100)
  })
  
  risk_category <- reactive({
    risk <- depression_risk()
    if (risk < 25) {
      return("Low Risk")
    } else if (risk < 60) {
      return("Moderate Risk")
    } else if (risk < 85) {
      return("High Risk")
    } else {
      return("Critical Risk")
    }
  })
  
  risk_color <- reactive({
    if (depression_risk() < 25) {
      return("green")
    } else if (depression_risk() < 60) {
      return("yellow")
    } else if (depression_risk() < 85) {
      return("orange")
    } else {
      return("red")
    }
  })
  
  output$risk_text <- renderText({
    paste("Predicted Risk of Stress:", round(depression_risk(), 2), "% —", risk_category())
  })
  
  output$recommendation <- renderText({
    risk <- depression_risk()
    if (risk < 25) {
      return("You're at low risk. Maintain healthy study habits, sleep well, and stay socially connected.")
    } else if (risk < 60) {
      return("You may be experiencing mild stress. Consider talking to someone and keeping a balanced routine.")
    } else if (risk < 85) {
      return("You are at high risk. Please reach out to your university counselor or mental health services.")
    } else {
      return("Critical risk detected. Immediate professional support is recommended. You are not alone.")
    }
  })
  
  output$risk_bar <- renderPlot({
    barplot(height = depression_risk(),
            names.arg = "Stress Risk",
            col = risk_color(),
            ylim = c(0, 100),
            ylab = "Probability (%)",
            main = "Predicted Stress Risk")
  })
}

# ----------------- Run the App -----------------
shinyApp(ui = ui, server = server)
