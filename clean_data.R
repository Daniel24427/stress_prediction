# Install and Load necessary libraries
install.packages("dplyr")
install.packages("readr")

library(dplyr)
library(readr)

# Load dataset

student_data <- read_csv("Student Depression Dataset.csv")

# View data structure
print(dim(student_data))
print(head(student_data))
print(summary(student_data))
print(str(student_data))

# Rename columns (for easier access and consistent formatting)
colnames(student_data) <- c("ID", "Gender", "Age", "City", "Profession", "Academic_Pressure",
                            "Work_Pressure", "CGPA", "Study_Satisfaction", "Job_Satisfaction",
                            "Sleep_Duration", "Dietary_Habits", "Degree", "Suicidal_Thoughts",
                            "Study_Hours", "Financial_Stress", "Family_Mental_History", "Depression")

# Select relevant columns
selected_data <- student_data %>%
  select(Gender, Age, Academic_Pressure, Study_Satisfaction, Sleep_Duration,
         Suicidal_Thoughts, Study_Hours, Financial_Stress, Family_Mental_History, Depression)

# Remove rows with missing values in critical variables
clean_data <- selected_data %>%
  filter(
    !is.na(Gender),
    !is.na(Age),
    !is.na(Academic_Pressure),
    !is.na(Study_Satisfaction),
    !is.na(Sleep_Duration),
    !is.na(Suicidal_Thoughts),
    !is.na(Study_Hours),
    !is.na(Financial_Stress),
    !is.na(Family_Mental_History),
    !is.na(Depression)
  )

# Filter out clearly invalid entries (e.g., Age outside typical student range)
clean_data <- clean_data %>%
  filter(Age >= 15 & Age <= 40)

# Print summary after cleaning
print(dim(clean_data))
print(summary(clean_data))

# Save cleaned dataset
write.csv(clean_data, "student_depression_cleaned.csv", row.names = FALSE)

