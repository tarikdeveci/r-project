

# Install necessary libraries 
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("caTools")  

# Load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caTools)



# Read the CSV file
data <- read.csv("C:/Users/Lenovo/Desktop/uygulamalar/r-project/r-project/StudentPerformanceFactors.csv", stringsAsFactors = TRUE)

# Preview the data
head(data)
str(data)
summary(data)


data <- data %>%
  filter(Teacher_Quality != "", 
         Parental_Education_Level != "", 
         Distance_from_Home != "")
 


# Select only the relevant columns for analysis
selected_data <- data %>%
  select(Hours_Studied, Attendance, Sleep_Hours, Previous_Scores, Tutoring_Sessions,
         Parental_Involvement, Access_to_Resources, Motivation_Level, Family_Income,
         Teacher_Quality, Peer_Influence, Parental_Education_Level,
         Exam_Score)



ggplot(selected_data, aes(x = Hours_Studied, y = Exam_Score)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Exam Score vs Hours Studied",
       x = "Hours Studied",
       y = "Exam Score") +
  theme_minimal()


ggplot(selected_data, aes(x = Motivation_Level, y = Exam_Score)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Exam Score by Motivation Level",
       x = "Motivation Level",
       y = "Exam Score") +
  theme_minimal()


ggplot(selected_data, aes(x = Previous_Scores, y = Exam_Score)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Exam Score vs Previous Scores",
       x = "Previous Scores",
       y = "Exam Score") +
  theme_minimal()


ggplot(selected_data, aes(x = Family_Income, y = Exam_Score)) +
  geom_boxplot(fill = "plum") +
  labs(title = "Exam Score by Family Income",
       x = "Family Income Level",
       y = "Exam Score") +
  theme_minimal()


ggplot(selected_data, aes(x = Tutoring_Sessions, y = Exam_Score)) +
  geom_jitter(alpha = 0.4, color = "darkorange") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Exam Score vs Tutoring Sessions",
       x = "Tutoring Sessions",
       y = "Exam Score") +
  theme_minimal()


ggplot(selected_data, aes(x = Teacher_Quality, y = Exam_Score)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Exam Score by Teacher Quality",
       x = "Teacher Quality",
       y = "Exam Score") +
  theme_minimal()


ggplot(selected_data, aes(x = Attendance, y = Exam_Score)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Exam Score vs Attendance",
       x = "Attendance (%)",
       y = "Exam Score") +
  theme_minimal()





# ANOVA test
anova_result <- aov(Exam_Score ~ Parental_Involvement, data = selected_data)
summary(anova_result)
# The ANOVA test shows a highly significant effect (p < 2e-16), indicating that students’ exam scores vary meaningfully based on their level of parental involvement.<0.05


summary(aov(Exam_Score ~ Family_Income, data = selected_data))
#(p = 3.6e-13), indicating that family income has a meaningful impact on student performance.<0.05

summary(aov(Exam_Score ~ Teacher_Quality, data = selected_data))
#exam scores differ meaningfully depending on the perceived quality of the teacher.<0.05

summary(aov(Exam_Score ~ Motivation_Level, data = selected_data))
#(p = 9e-12), showing that students' exam scores differ significantly based on their motivation level.

summary(aov(Exam_Score ~ Access_to_Resources, data = selected_data))
#(p < 2e-16), indicating that access to educational resources has a strong impact on students’ exam performance.

selected_data$School_Type <- data$School_Type
summary(aov(Exam_Score ~ School_Type, data = selected_data))
#(p = 0.385), indicating that attending a public or private school does not meaningfully affect exam scores in this dataset. 


summary(aov(Exam_Score ~ Peer_Influence, data = selected_data))
#(p = 1.24e-14), suggesting that students’ exam scores are meaningfully influenced by the type of peer interactions they experience.

summary(aov(Exam_Score ~ Parental_Education_Level, data = selected_data))
# (p = 2.99e-16), indicating that students’ exam scores vary considerably depending on their parents’ education level.

selected_data$Distance_from_Home <- data$Distance_from_Home
summary(aov(Exam_Score ~ Distance_from_Home, data = selected_data))
# (p = 1.63e-11), indicating that students’ exam scores vary considerably depending on their distance from home.


# numeric columns
numeric_cols <- c("Hours_Studied", "Attendance", "Sleep_Hours", "Previous_Scores", "Tutoring_Sessions")

# Normalization
selected_data[numeric_cols] <- scale(selected_data[numeric_cols])

head(selected_data[numeric_cols])


#Lineer
model <- lm(Exam_Score ~ Hours_Studied + Attendance + Sleep_Hours + Previous_Scores + Tutoring_Sessions + Motivation_Level + Access_to_Resources, 
            data = selected_data)

summary(model)

#The linear regression model explains approximately 63.15% of the variance in exam scores (R² = 0.6315), indicating a strong predictive power. 
#Key predictors include Hours_Studied, Attendance, Previous_Scores, Tutoring_Sessions, Motivation_Level, and Access_to_Resources, all of which have statistically significant effects (p < 0.001).
#Interestingly, Sleep_Hours does not appear to have a significant effect on exam performance (p = 0.656). 


# Predicted exam scores from the regression model
predicted <- predict(model)

# Actual exam scores from the dataset
actual <- selected_data$Exam_Score

# Scatter plot comparing predicted and actual exam scores
# Each point represents a student; points closer to the red line indicate better predictions
plot(predicted, actual,
     xlab = "Predicted Exam Score",
     ylab = "Actual Exam Score",
     main = "Predicted vs Actual Exam Scores",
     pch = 16, col = "blue")

# Reference line (y = x) shows perfect prediction
# The closer the points are to this line, the better the model fits
abline(0, 1, col = "red", lwd = 2)






# Residuals represent the difference between actual and predicted scores
residuals <- model$residuals

# This plot helps visualize the distribution of prediction errors
# Ideally, residuals should be randomly scattered around zero
plot(residuals,
     main = "Model Residuals (Errors)",
     ylab = "Residuals",
     xlab = "Index",
     col = "purple", pch = 16)

# Horizontal red line at zero for reference
# Any visible pattern in residuals could indicate model bias or missing variables
abline(h = 0, col = "red", lwd = 2)



#KNN
# Create score classes: Low (<=65), Medium (66–75), High (>75)
selected_data$Score_Class <- cut(selected_data$Exam_Score,
                                 breaks = c(-Inf, 65, 75, Inf),
                                 labels = c("Low", "Medium", "High"))

# Convert to factor
selected_data$Score_Class <- as.factor(selected_data$Score_Class)

# Optional: See class distribution
table(selected_data$Score_Class)




# Load the caTools package for data splitting
library(caTools)
#We used set.seed(123) to ensure reproducibility in our train/test split. The value is arbitrary but guarantees consistent results.
set.seed(123)  # Set seed for reproducibility

# Split the data: 70% training, 30% testing
split <- sample.split(selected_data$Score_Class, SplitRatio = 0.7)
train_data <- subset(selected_data, split == TRUE)
test_data  <- subset(selected_data, split == FALSE)


# Select numerical features for KNN (they must be scaled beforehand)
train_features <- train_data[, c("Hours_Studied", "Attendance", "Sleep_Hours", "Previous_Scores", "Tutoring_Sessions")]
test_features  <- test_data[, c("Hours_Studied", "Attendance", "Sleep_Hours", "Previous_Scores", "Tutoring_Sessions")]

# Define target labels for classification
train_labels <- train_data$Score_Class
test_labels  <- test_data$Score_Class


# Load the class package to apply KNN
library(class)

# Apply the K-Nearest Neighbors algorithm (k = 5)
knn_pred <- knn(train = train_features, test = test_features, cl = train_labels, k = 5)


# Create a confusion matrix to evaluate classification performance
conf_mat <- table(Actual = test_labels, Predicted = knn_pred)
print(conf_mat)


# Calculate overall classification accuracy
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
cat("Accuracy:", round(accuracy, 4), "\n")  # Print accuracy

