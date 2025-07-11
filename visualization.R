# ----------------- Load Required Libraries -----------------
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("plotly")
install.packages("GGally")
install.packages("corrplot")

library(tidyverse)
library(ggplot2)
library(plotly)
library(GGally)
library(corrplot)

# ----------------- Load Cleaned Data -----------------

student_data <- read_csv("student_depression_cleaned.csv")

# ----------------- Apply Risk-Based Scoring -----------------
student_data <- student_data %>%
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

# ----------------- Combine Scored Data -----------------
scored_data <- student_data %>%
  select(Gender_Score, Age, Academic_Score, Dissatisfaction_Score, 
         Sleep_Score, Suicidal_Score, Study_Hours, 
         Financial_Score, Family_History_Score, Depression)

# ----------------- Boxplot: Distribution by Depression -----------------
melted_data <- scored_data %>%
  pivot_longer(cols = -Depression, names_to = "Variable", values_to = "Value")

ggplot(melted_data, aes(x = Variable, y = Value, fill = factor(Depression))) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  labs(title = "Distribution of Risk Factors by Depression",
       x = "Variable", y = "Score", fill = "Depression") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ----------------- Correlation Heatmap -----------------
cor_matrix <- cor(scored_data %>% select(-Depression), use = "complete.obs")
corrplot(cor_matrix, method = "color", addCoef.col = "black", 
         title = "Correlation Heatmap of Risk Factors", mar = c(0, 0, 1, 0))

# ----------------- Pairwise Scatter Plots -----------------
ggpairs(scored_data, aes(color = factor(Depression), alpha = 0.6)) +
  ggtitle("Pairwise Relationships of Risk Factors") +
  theme_minimal()

# ----------------- Parallel Coordinates Plot -----------------
plot_ly(data = scored_data, type = "parcoords",
        line = list(color = ~Depression,
                    colorscale = list(c(0, 1), c("blue", "red"))),
        dimensions = list(
          list(label = "Gender", values = ~Gender_Score),
          list(label = "Age", values = ~Age),
          list(label = "Academic Pressure", values = ~Academic_Score),
          list(label = "Study Dissatisfaction", values = ~Dissatisfaction_Score),
          list(label = "Sleep Score", values = ~Sleep_Score),
          list(label = "Suicidal History", values = ~Suicidal_Score),
          list(label = "Study Hours", values = ~Study_Hours),
          list(label = "Financial Stress", values = ~Financial_Score),
          list(label = "Family History", values = ~Family_History_Score)
        )) %>%
  layout(title = "Parallel Coordinates Plot: Risk Profiles by Depression")