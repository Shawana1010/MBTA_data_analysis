
library(tidyverse)
library(ggplot2)

MBTA <- read.csv("C:/Users/maxma/Documents/CS555/term_project/MBTA_Ridership.csv")
colnames(MBTA)
str(MBTA)

any_na <- any(is.na(MBTA))
any_na

table(MBTA$stop_name)
unique(MBTA$season)
unique(MBTA$route_id)
unique(MBTA$route_name)
unique(MBTA$direction_id)
unique(MBTA$day_type_id)
unique(MBTA$day_type_name)
unique(MBTA$time_period_id)
unique(MBTA$time_period_name)
unique(MBTA$stop_name)
unique(MBTA$stop_id)
unique(MBTA$total_ons)
unique(MBTA$number_service_days)


# Remove specified columns
MBTA <- MBTA %>% 
  select(-c(mode, route_name, day_type_id, time_period_id, stop_id, ObjectId))

colnames(MBTA)
str(MBTA)


library(scales)

ggplot(MBTA, aes(x = time_period_name, y = total_ons)) +
  geom_boxplot() +
  scale_y_continuous(labels = label_comma()) +  
  labs(title = "Distribution of Total Ons by Time of Day",
       x = "Time of Day",
       y = "Total Ons") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
########################################################################
#ridership by route id


library(dplyr)

# Group the data by route_id and summarize the total and average ridership for each route
ridership_by_route <- MBTA %>%
  group_by(route_id) %>%
  summarize(
    Total_Ridership = sum(total_ons, na.rm = TRUE),  # Total riders per route
    Average_Ridership = mean(total_ons, na.rm = TRUE) # Average riders per route
  ) %>%
  arrange(desc(Total_Ridership)) # Arrange in descending order of total ridership


print(ridership_by_route)


# Define colors for each route_id
route_colors <- c("Red" = "red4", "Orange" = "orange3", "Green" = "green4", "Blue" = "blue4")

# Total Ridership plot with colors matched to route_id
ggplot(ridership_by_route, aes(x = reorder(route_id, -Total_Ridership), y = Total_Ridership, fill = route_id)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = route_colors) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total Ridership by Route ID",
       x = "Route ID",
       y = "Total Ridership") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank()) 

# Average Ridership plot with colors matched to route_id
ggplot(ridership_by_route, aes(x = reorder(route_id, -Average_Ridership), y = Average_Ridership, fill = route_id)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = route_colors) +
  labs(title = "Average Ridership by Route ID",
       x = "Route ID",
       y = "Average Ridership") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank()) 


############################################################################

# Fit the linear model for ANOVA
anova_model <- lm(total_ons ~ time_period_name, data = MBTA)
summary(anova_model)

# Perform ANOVA
anova_results <- anova(anova_model)
anova_results



library(dplyr)

MBTA %>%
  group_by(time_period_name) %>%
  summarize(average_ons = mean(total_ons, na.rm = TRUE)) %>%
  ggplot(aes(x = time_period_name, y = average_ons, fill = time_period_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Total Ons by Time of Day",
       x = "Time of Day",
       y = "Average Total Ons") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#######################################################################
#data division

library(caTools)

set.seed(555)

# Splitting the data into training (70%) and test (30%) sets
split <- sample.split(MBTA$direction_id, SplitRatio = 0.7)
train_data <- subset(MBTA, split == TRUE)
test_data <- subset(MBTA, split == FALSE)

#####################################################################
#SLR
# Scatter plot without scientific notation on the y-axis
ggplot(MBTA, aes(x = average_flow, y = total_ons)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Scatter Plot of Total Ons vs Average Flow",
       x = "Average Flow",
       y = "Total Ons")


slr_model <- lm(total_ons ~ average_flow, data = train_data)
summary(slr_model)

# Predict 'total_ons' on the test data using the model
test_data$predicted_ons <- predict(slr_model, newdata = test_data)

# Calculate the residuals (differences between actual and predicted values)
test_data$residuals <- test_data$total_ons - test_data$predicted_ons

# Calculate the RMSE
RMSE <- sqrt(mean(test_data$residuals^2))
print(paste("RMSE on test data:", RMSE))


#########################################################################
#Correlation Matrix

# Select only numeric columns for the correlation matrix
numeric_data <- MBTA %>% select_if(is.numeric)

# Create the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs") # 'use' argument handles missing values

# View the correlation matrix
print(cor_matrix)

install.packages("corrplot")
library(corrplot)

# Plot the correlation matrix
corrplot(cor_matrix, method = "circle")


library(dplyr)

MBTA <- MBTA %>% select(-average_ons)


#################################################################################

#MLR
full_model <- lm(total_ons ~ ., data = train_data)

# Perform backward elimination
reduced_model <- step(full_model, direction = "backward")

# Display the summary of the reduced model
summary(reduced_model)




# Calculate predictions on training data
train_data$predicted_ons <- predict(reduced_model, newdata = train_data)

# Calculate residuals for training data
train_data$residuals <- train_data$total_ons - train_data$predicted_ons

# Calculate RMSE, MAE for training data
train_RMSE <- sqrt(mean(train_data$residuals^2))
train_MAE <- mean(abs(train_data$residuals))
train_R2 <- summary(reduced_model)$r.squared

# Calculate predictions on test data
test_data$predicted_ons <- predict(reduced_model, newdata = test_data)

# Calculate residuals for test data
test_data$residuals <- test_data$total_ons - test_data$predicted_ons

# Calculate RMSE, MAE for test data
test_RMSE <- sqrt(mean(test_data$residuals^2))
test_MAE <- mean(abs(test_data$residuals))
test_R2 <- 1 - sum(test_data$residuals^2) / sum((test_data$total_ons - mean(test_data$total_ons))^2)

# Prepare the summary table
performance_summary <- data.frame(
  Data = c("Training", "Testing"),
  RMSE = c(train_RMSE, test_RMSE),
  MAE = c(train_MAE, test_MAE),
  R2 = c(train_R2, test_R2)
)

print(performance_summary)

######################################################################

#logistic Regression

# Create a binary variable for high vs. low ridership
median_ons <- median(MBTA$total_ons, na.rm = TRUE)
MBTA$high_ridership <- as.factor(ifelse(MBTA$total_ons > median_ons, 1, 0))

# Convert categorical variables to factors
MBTA$route_id <- as.factor(MBTA$route_id)
MBTA$time_period_name <- as.factor(MBTA$time_period_name)

set.seed(555)


split <- sample.split(MBTA$high_ridership, SplitRatio = 0.7)
train_set <- subset(MBTA, split == TRUE)
test_set <- subset(MBTA, split == FALSE)


# Fit logistic regression model on the training data
logistic_model <- glm(high_ridership ~ route_id + time_period_name + average_flow,
                      family = binomial(link = "logit"), data = train_set)

# Summary of the model to check for significance and overall fit
summary(logistic_model)


# Diagnostic plots for the logistic regression model
par(mfrow = c(2, 2))
plot(logistic_model)


# Predict on the test data
test_set$predicted_probs <- predict(logistic_model, newdata = test_set, type = "response")

# Create a prediction classification based on the probability threshold
test_set$predicted_class <- ifelse(test_set$predicted_probs > 0.5, 1, 0)

# Evaluate model performance using a confusion matrix
confusion_matrix <- table(TestObserved = test_set$high_ridership, TestPredicted = test_set$predicted_class)
print(confusion_matrix)


accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))


################################################################################
#ROC and AUC

if (!require(pROC)) install.packages("pROC")
library(pROC)

# For the training set
train_probs <- predict(logistic_model, newdata = train_set, type = "response")
roc_train <- roc(train_set$high_ridership, train_probs)
plot(roc_train, main="ROC Curve for Training Set")
auc_train <- auc(roc_train)
print(paste("AUC for Training Set:", auc_train))

# For the test set
test_probs <- predict(logistic_model, newdata = test_set, type = "response")
roc_test <- roc(test_set$high_ridership, test_probs)
plot(roc_test, main="ROC Curve for Test Set")
auc_test <- auc(roc_test)
print(paste("AUC for Test Set:", auc_test))


plot(roc_train, col="blue4", main="ROC Curves for Training and Test Sets")
plot(roc_test, add=TRUE, col="pink")
legend("bottomright", legend=c("Training", "Test"), col=c("blue4", "pink"), lwd=2)


####################################################################
#knn

library(dplyr)
library(caret)

# Normalize/scale numeric predictors
preProcess_range_model <- preProcess(MBTA[, sapply(MBTA, is.numeric)], method = 'scale')
MBTA_norm <- predict(preProcess_range_model, MBTA)

# Select only the numeric features for distance calculation
features <- MBTA_norm %>% select_if(is.numeric)


babcock_index <- which(MBTA$stop_name == "Babcock Street")


babcock_features <- features[babcock_index, ]

# Compute the distances from Babcock Street to all other stops
distances <- apply(features, 1, function(row) {
  sqrt(sum((row - babcock_features)^2))
})

# Create a data frame with distances and stop names
distance_data <- data.frame(stop_name = MBTA$stop_name, distance = distances, row.names = NULL)

# Order the stops by distance and take the top 7 closest stops (excluding Babcock Street itself)
nearest_neighbors <- distance_data %>%
  filter(stop_name != "Babcock Street") %>%
  arrange(distance) %>%
  head(7)

# Print the 7 nearest neighbors
print(nearest_neighbors)

