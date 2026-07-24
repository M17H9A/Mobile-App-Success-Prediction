#---------------------------------------------Logistic Regression(Oversampled and Undersampled)----------------------------------------------------------------------
library(caret)
library(pROC)

model_name <- 'LOGISTIC REGRESSION-KFold(oversampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(oversampled_data$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initializing variables to store evaluation metrics
accuracy_lr <- numeric(num_folds)
precision_lr <- numeric(num_folds)
recall_lr <- numeric(num_folds)
f1_score_lr <- numeric(num_folds)
auc_roc_lr <- numeric(num_folds)

# Iterating through folds
for (fold in seq_along(folds)) {
  
# Extracting the test set for the current fold
  test_indices <- unlist(folds[fold])
  test_data <- oversampled_data[test_indices, ]
  train_data <- oversampled_data[-test_indices, ]
  
# Fitting logistic regression model
  logistic_model <- glm(successful_app ~ price+rating_count_tot+rating_count_ver+user_rating_ver+
                          cont_rating+prime_genre+sup_devices.num+ipadSc_urls.num+lang.num+vpp_lic
                        +size_mb+paid_status+older_ver_rating, data = train_data, family = binomial)
  
# Predictions on the test set
  predictions <- predict(logistic_model, newdata = test_data, type = "response")
  
# Converting predicted probabilities to binary predictions (0 or 1)
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)
  
# Evaluating the model
  confusion_matrix <- table(test_data$successful_app, predicted_classes)
  
# Calculating metrics for the current fold
  accuracy_lr[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_lr[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_lr[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_lr[fold] <- 2 * (precision_lr[fold] * recall_lr[fold]) / (precision_lr[fold] + recall_lr[fold])
  
# Calculation of AUC-ROC
  roc_curve <- roc(test_data$successful_app, predictions)
  auc_roc_lr[fold] <- auc(roc_curve)
}

# Calculating average metrics over all folds
avg_accuracy_lr <- mean(accuracy_lr)
avg_precision_lr <- mean(precision_lr)
avg_recall_lr <- mean(recall_lr)
avg_f1_score_lr <- mean(f1_score_lr)
avg_auc_roc_lr <- mean(auc_roc_lr)

# Printing or storing the average metrics
print(paste("Average Accuracy:", avg_accuracy_lr))
print(paste("Average Precision:", avg_precision_lr))
print(paste("Average Recall:", avg_recall_lr))
print(paste("Average F1-Score:", avg_f1_score_lr))
print(paste("Average AUC-ROC:", avg_auc_roc_lr))

# Plotting ROC curve
plot(roc_curve, main = "LR-OverSampled Data-ROC Curve", col = "blue", lwd = 2)


append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy =avg_accuracy_lr,
    Precision = avg_precision_lr,
    Recall = avg_recall_lr,
    F1_Score = avg_f1_score_lr,
    AUC_ROC = avg_auc_roc_lr
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)

# Printing the updated dataframe
print(results_df)

#=======================================================================================================================================================================

model_name <- 'LOGISTIC REGRESSION-KFold(undersampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(undersampled_data$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initializing variables to store evaluation metrics
accuracy_lr <- numeric(num_folds)
precision_lr <- numeric(num_folds)
recall_lr <- numeric(num_folds)
f1_score_lr <- numeric(num_folds)
auc_roc_lr <- numeric(num_folds)

# Iterating through folds
for (fold in seq_along(folds)) {
  # Extract the test set for the current fold
  test_indices <- unlist(folds[fold])
  test_data <- undersampled_data[test_indices, ]
  train_data <- undersampled_data[-test_indices, ]
  
# Fitting logistic regression model
  logistic_model <- glm(successful_app ~ price+rating_count_tot+rating_count_ver+user_rating_ver+
                          cont_rating+prime_genre+sup_devices.num+ipadSc_urls.num+lang.num+vpp_lic
                        +size_mb+paid_status+older_ver_rating, data = train_data, family = binomial)
  
# Making predictions on the test set
  predictions <- predict(logistic_model, newdata = test_data, type = "response")
  
# Converting predicted probabilities to binary predictions (0 or 1)
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)
  
# Evaluating the model
  confusion_matrix <- table(test_data$successful_app, predicted_classes)
  
# Calculating metrics for the current fold
  accuracy_lr[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_lr[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_lr[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_lr[fold] <- 2 * (precision_lr[fold] * recall_lr[fold]) / (precision_lr[fold] + recall_lr[fold])
  
# Calculation of AUC-ROC
  roc_curve <- roc(test_data$successful_app, predictions)
  auc_roc_lr[fold] <- auc(roc_curve)
}

# Calculating average metrics over all folds
avg_accuracy_lr <- mean(accuracy_lr)
avg_precision_lr <- mean(precision_lr)
avg_recall_lr <- mean(recall_lr)
avg_f1_score_lr <- mean(f1_score_lr)
avg_auc_roc_lr <- mean(auc_roc_lr)

# Printing or storing the average metrics
print(paste("Average Accuracy:", avg_accuracy_lr))
print(paste("Average Precision:", avg_precision_lr))
print(paste("Average Recall:", avg_recall_lr))
print(paste("Average F1-Score:", avg_f1_score_lr))
print(paste("Average AUC-ROC:", avg_auc_roc_lr))

# Plotting ROC curve
plot(roc_curve, main = "LR-UnderSampled Data-ROC Curve", col = "blue", lwd = 2)


append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy =avg_accuracy_lr,
    Precision = avg_precision_lr,
    Recall = avg_recall_lr,
    F1_Score = avg_f1_score_lr,
    AUC_ROC = avg_auc_roc_lr
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)

# Printing the updated dataframe
print(results_df)
#======================================================================================================================================================================#
#---------------------------------------------KNN(Oversampled and Undersampled)----------------------------------------------------------------------
library(caret)
library(pROC)
library(class)

model_name <- 'KNN-KFold(oversampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(oversampled_data_scaled$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initializing variables to store evaluation metrics
accuracy_knn <- numeric(num_folds)
precision_knn <- numeric(num_folds)
recall_knn <- numeric(num_folds)
f1_score_knn <- numeric(num_folds)
auc_roc_knn <- numeric(num_folds)

# Iterating through folds
for (fold in seq_along(folds)) {
  test_indices <- unlist(folds[fold])
  test_data <- oversampled_data_scaled[test_indices, ]
  train_data <- oversampled_data_scaled[-test_indices, ]
  
# Converting labels to factors (if not already)
  train_data$successful_app <- as.factor(train_data$successful_app)
  test_data$successful_app <- as.factor(test_data$successful_app)
  
# Fitting KNN model
  knn_model <- knn(train = as.matrix(train_data[, -ncol(train_data)]),
                   test = as.matrix(test_data[, -ncol(test_data)]),
                   cl = train_data$successful_app, k = 5)  # Set k as needed
  
# Evaluate the model
  confusion_matrix <- table(test_data$successful_app, knn_model)
  
# Calculating metrics for the current fold
  accuracy_knn[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_knn[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_knn[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_knn[fold] <- 2 * (precision_knn[fold] * recall_knn[fold]) / (precision_knn[fold] + recall_knn[fold])
  
# Calculation of AUC-ROC
  roc_curve <- roc(as.numeric(test_data$successful_app) - 1, as.numeric(knn_model) - 1)
  auc_roc_knn[fold] <- auc(roc_curve)
}

# Calculating average metrics over all folds
avg_accuracy_knn <- mean(accuracy_knn)
avg_precision_knn <- mean(precision_knn)
avg_recall_knn <- mean(recall_knn)
avg_f1_score_knn <- mean(f1_score_knn)
avg_auc_roc_knn <- mean(auc_roc_knn)

# Printing or storing the average metrics
print(paste("Average Accuracy:", avg_accuracy_knn))
print(paste("Average Precision:", avg_precision_knn))
print(paste("Average Recall:", avg_recall_knn))
print(paste("Average F1-Score:", avg_f1_score_knn))
print(paste("Average AUC-ROC:", avg_auc_roc_knn))

plot(roc_curve, main = "KNN-OverSampled Data-ROC Curve", col = "blue", lwd = 2)

append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy =avg_accuracy_knn,
    Precision = avg_precision_knn,
    Recall = avg_recall_knn,
    F1_Score = avg_f1_score_knn,
    AUC_ROC = avg_auc_roc_knn
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)
print(results_df)
#=======================================================================================================================================================================
library(caret)
library(pROC)
library(class)

model_name <- 'KNN-KFold(undersampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(undersampled_data_scaled$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initializing variables to store evaluation metrics
accuracy_knn <- numeric(num_folds)
precision_knn <- numeric(num_folds)
recall_knn <- numeric(num_folds)
f1_score_knn <- numeric(num_folds)
auc_roc_knn <- numeric(num_folds)

# Iterating through folds
for (fold in seq_along(folds)) {
  test_indices <- unlist(folds[fold])
  test_data <- undersampled_data_scaled[test_indices, ]
  train_data <- undersampled_data_scaled[-test_indices, ]
  
# Converting labels to factors 
  train_data$successful_app <- as.factor(train_data$successful_app)
  test_data$successful_app <- as.factor(test_data$successful_app)
  
# Fitting KNN model
  knn_model <- knn(train = as.matrix(train_data[, -ncol(train_data)]),
                   test = as.matrix(test_data[, -ncol(test_data)]),
                   cl = train_data$successful_app, k = 5)  # Set k as needed
  
# Evaluating the model
  confusion_matrix <- table(test_data$successful_app, knn_model)
  
# Calculating metrics for the current fold
  accuracy_knn[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_knn[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_knn[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_knn[fold] <- 2 * (precision_knn[fold] * recall_knn[fold]) / (precision_knn[fold] + recall_knn[fold])
  
# Calculation of  AUC-ROC
  roc_curve <- roc(as.numeric(test_data$successful_app) - 1, as.numeric(knn_model) - 1)
  auc_roc_knn[fold] <- auc(roc_curve)
}

# Calculating average metrics over all folds
avg_accuracy_knn <- mean(accuracy_knn)
avg_precision_knn <- mean(precision_knn)
avg_recall_knn <- mean(recall_knn)
avg_f1_score_knn <- mean(f1_score_knn)
avg_auc_roc_knn <- mean(auc_roc_knn)

# Printing or storing the average metrics
print(paste("Average Accuracy:", avg_accuracy_knn))
print(paste("Average Precision:", avg_precision_knn))
print(paste("Average Recall:", avg_recall_knn))
print(paste("Average F1-Score:", avg_f1_score_knn))
print(paste("Average AUC-ROC:", avg_auc_roc_knn))

plot(roc_curve, main = "KNN-UnderSampled Data-ROC Curve", col = "blue", lwd = 2)

append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy =avg_accuracy_knn,
    Precision = avg_precision_knn,
    Recall = avg_recall_knn,
    F1_Score = avg_f1_score_knn,
    AUC_ROC = avg_auc_roc_knn
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)

print(results_df)
#======================================================================================================================================================================#
#---------------------------------------------Naive Bayes(Oversampled and Undersampled)----------------------------------------------------------------------
library(caret)
library(pROC)
library(e1071)

model_name <- 'NaiveBayes-KFold(undersampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(undersampled_data$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initializing variables to store evaluation metrics
accuracy_nb <- numeric(num_folds)
precision_nb <- numeric(num_folds)
recall_nb <- numeric(num_folds)
f1_score_nb <- numeric(num_folds)
auc_roc_nb <- numeric(num_folds)

# Iterating through folds
for (fold in seq_along(folds)) {
  test_indices <- unlist(folds[fold])
  test_data <- undersampled_data[test_indices, ]
  train_data <- undersampled_data[-test_indices, ]
  
# Converting labels to factors 
  train_data$successful_app <- as.factor(train_data$successful_app)
  test_data$successful_app <- as.factor(test_data$successful_app)
  
# Fitting Naive Bayes model
  nb_model <- naiveBayes(successful_app ~ ., data = train_data)
  
# Making predictions on the test set
  predictions <- predict(nb_model, newdata = test_data)
  
# Evaluating the model
  confusion_matrix <- table(test_data$successful_app, predictions)
  
# Calculating metrics for the current fold
  accuracy_nb[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_nb[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_nb[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_nb[fold] <- 2 * (precision_nb[fold] * recall_nb[fold]) / (precision_nb[fold] + recall_nb[fold])
  
# Calculation of AUC-ROC
  roc_curve <- roc(as.numeric(test_data$successful_app) - 1, as.numeric(predictions) - 1)
  auc_roc_nb[fold] <- auc(roc_curve)
}

# Calculating average metrics over all folds
avg_accuracy_nb <- mean(accuracy_nb)
avg_precision_nb <- mean(precision_nb)
avg_recall_nb <- mean(recall_nb)
avg_f1_score_nb <- mean(f1_score_nb)
avg_auc_roc_nb <- mean(auc_roc_nb)

# Printing or storing the average metrics
print(paste("Average Accuracy:", avg_accuracy_nb))
print(paste("Average Precision:", avg_precision_nb))
print(paste("Average Recall:", avg_recall_nb))
print(paste("Average F1-Score:", avg_f1_score_nb))
print(paste("Average AUC-ROC:", avg_auc_roc_nb))

#Plotting the ROC curve
plot(roc_curve, main = "NaiveBayes-UnderSampled Data-ROC Curve", col = "blue", lwd = 2)


# Function to append results to a dataframe
append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy = avg_accuracy_nb,
    Precision = avg_precision_nb,
    Recall = avg_recall_nb,
    F1_Score = avg_f1_score_nb,
    AUC_ROC = avg_auc_roc_nb
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

# Example usage:
# Replace 'model_name', 'precision', 'recall', 'f1_score', 'auc_roc' with actual values
results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)

# Print the updated dataframe
print(results_df)
#=======================================================================================================================================================================

library(caret)
library(pROC)
library(e1071)

# Assuming 'final_data' is your original dataset
model_name <- 'NaiveBayes-KFold(oversampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(oversampled_data$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initialize variables to store evaluation metrics
accuracy_nb <- numeric(num_folds)
precision_nb <- numeric(num_folds)
recall_nb <- numeric(num_folds)
f1_score_nb <- numeric(num_folds)
auc_roc_nb <- numeric(num_folds)

# Iterating through folds
for (fold in seq_along(folds)) {
  test_indices <- unlist(folds[fold])
  test_data <- oversampled_data[test_indices, ]
  train_data <- oversampled_data[-test_indices, ]
  
# Converting labels to factors (if not already)
  train_data$successful_app <- as.factor(train_data$successful_app)
  test_data$successful_app <- as.factor(test_data$successful_app)
  
# Fitting Naive Bayes model
  nb_model <- naiveBayes(successful_app ~ ., data = train_data)
  
# Making predictions on the test set
  predictions <- predict(nb_model, newdata = test_data)
  
# Evaluating the model
  confusion_matrix <- table(test_data$successful_app, predictions)
  
# Calculating metrics for the current fold
  accuracy_nb[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_nb[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_nb[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_nb[fold] <- 2 * (precision_nb[fold] * recall_nb[fold]) / (precision_nb[fold] + recall_nb[fold])
  
# Calculating AUC-ROC
  roc_curve <- roc(as.numeric(test_data$successful_app) - 1, as.numeric(predictions) - 1)
  auc_roc_nb[fold] <- auc(roc_curve)
}

# Calculating average metrics over all folds
avg_accuracy_nb <- mean(accuracy_nb)
avg_precision_nb <- mean(precision_nb)
avg_recall_nb <- mean(recall_nb)
avg_f1_score_nb <- mean(f1_score_nb)
avg_auc_roc_nb <- mean(auc_roc_nb)

# Printing or storing the average metrics
print(paste("Average Accuracy:", avg_accuracy_nb))
print(paste("Average Precision:", avg_precision_nb))
print(paste("Average Recall:", avg_recall_nb))
print(paste("Average F1-Score:", avg_f1_score_nb))
print(paste("Average AUC-ROC:", avg_auc_roc_nb))

#Plotting the ROC curve
plot(roc_curve, main = "NaiveBayes-OverSampled Data-ROC Curve", col = "blue", lwd = 2)

append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy = avg_accuracy_nb,
    Precision = avg_precision_nb,
    Recall = avg_recall_nb,
    F1_Score = avg_f1_score_nb,
    AUC_ROC = avg_auc_roc_nb
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}


results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)

# Printing the updated dataframe
print(results_df)
#======================================================================================================================================================================#
#---------------------------------------------Decision Tree(Oversampled and Undersampled)----------------------------------------------------------------------
library(caret)
library(pROC)
library(rpart)

model_name <- 'DECISION TREE-KFold(oversampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(oversampled_data$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initializing variables to store evaluation metrics
accuracy_dt <- numeric(num_folds)
precision_dt <- numeric(num_folds)
recall_dt <- numeric(num_folds)
f1_score_dt <- numeric(num_folds)
auc_roc_dt <- numeric(num_folds)

# Iterating through folds
for (fold in seq_along(folds)) {
    test_indices <- unlist(folds[fold])
    test_data <- oversampled_data[test_indices, ]
    train_data <- oversampled_data[-test_indices, ]
  
# Fitting Decision Tree model
  decision_tree_model <- rpart(successful_app ~ price + rating_count_tot + rating_count_ver +
                                 user_rating_ver + cont_rating + prime_genre + sup_devices.num +
                                 ipadSc_urls.num + lang.num + vpp_lic + size_mb + paid_status +
                                 older_ver_rating, data = train_data, method = "class")
  
# Making predictions on the test set
  predictions <- predict(decision_tree_model, newdata = test_data, type = "class")
  
# Evaluating the model
  confusion_matrix <- table(test_data$successful_app, predictions)
  
# Calculating metrics for the current fold
  accuracy_dt[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_dt[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_dt[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_dt[fold] <- 2 * (precision_dt[fold] * recall_dt[fold]) / (precision_dt[fold] + recall_dt[fold])
  
# Calculation of AUC-ROC
  roc_curve <- roc(test_data$successful_app, as.numeric(predictions))
  auc_roc_dt[fold] <- auc(roc_curve)
}

# Calculating average metrics over all folds
avg_accuracy_dt <- mean(accuracy_dt)
avg_precision_dt <- mean(precision_dt)
avg_recall_dt <- mean(recall_dt)
avg_f1_score_dt <- mean(f1_score_dt)
avg_auc_roc_dt <- mean(auc_roc_dt)

# Printing or storing the average metrics
print(paste("Average Accuracy:", avg_accuracy_dt))
print(paste("Average Precision:", avg_precision_dt))
print(paste("Average Recall:", avg_recall_dt))
print(paste("Average F1-Score:", avg_f1_score_dt))
print(paste("Average AUC-ROC:", avg_auc_roc_dt))

# Plot of ROC curve for the last fold 
plot(roc_curve, main = "DT-OverSampled Data-ROC Curve", col = "blue", lwd = 2)


append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy =avg_accuracy_dt,
    Precision = avg_precision_dt,
    Recall = avg_recall_dt,
    F1_Score = avg_f1_score_dt,
    AUC_ROC = avg_auc_roc_dt
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)

# Printing the updated dataframe
print(results_df)

#=======================================================================================================================================================================

model_name <- 'DECISION TREE-KFold(undersampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(undersampled_data$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initializing variables to store evaluation metrics
accuracy_dt <- numeric(num_folds)
precision_dt <- numeric(num_folds)
recall_dt <- numeric(num_folds)
f1_score_dt <- numeric(num_folds)
auc_roc_dt <- numeric(num_folds)

# Iterating through folds
for (fold in seq_along(folds)) {
  test_indices <- unlist(folds[fold])
  test_data <- undersampled_data[test_indices, ]
  train_data <- undersampled_data[-test_indices, ]
  
# Fitting decision Tree model
  decision_tree_model <- rpart(successful_app ~ price + rating_count_tot + rating_count_ver +
                                 user_rating_ver + cont_rating + prime_genre + sup_devices.num +
                                 ipadSc_urls.num + lang.num + vpp_lic + size_mb + paid_status +
                                 older_ver_rating, data = train_data, method = "class")
  
# Making predictions on the test set
  predictions <- predict(decision_tree_model, newdata = test_data, type = "class")
  
# Evaluating the model
  confusion_matrix <- table(test_data$successful_app, predictions)
  
# Calculation of metrics for the current fold
  accuracy_dt[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_dt[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_dt[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_dt[fold] <- 2 * (precision_dt[fold] * recall_dt[fold]) / (precision_dt[fold] + recall_dt[fold])
  
# Calculating AUC-ROC
  roc_curve <- roc(test_data$successful_app, as.numeric(predictions))
  auc_roc_dt[fold] <- auc(roc_curve)
}

# Calculation of average metrics over all folds
avg_accuracy_dt <- mean(accuracy_dt)
avg_precision_dt <- mean(precision_dt)
avg_recall_dt <- mean(recall_dt)
avg_f1_score_dt <- mean(f1_score_dt)
avg_auc_roc_dt <- mean(auc_roc_dt)

# Printing or storing the average metrics
print(paste("Average Accuracy:", avg_accuracy_dt))
print(paste("Average Precision:", avg_precision_dt))
print(paste("Average Recall:", avg_recall_dt))
print(paste("Average F1-Score:", avg_f1_score_dt))
print(paste("Average AUC-ROC:", avg_auc_roc_dt))

# Plot ROC curve for the last fold 
plot(roc_curve, main = "DT-UnderSampled Data-ROC Curve", col = "blue", lwd = 2)


append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy =avg_accuracy_dt,
    Precision = avg_precision_dt,
    Recall = avg_recall_dt,
    F1_Score = avg_f1_score_dt,
    AUC_ROC = avg_auc_roc_dt
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)

# Printing the updated dataframe
print(results_df)
#======================================================================================================================================================================#
#---------------------------------------------Random Forest(Oversampled and Undersampled)----------------------------------------------------------------------
library(randomForest)
library(pROC)

model_name <- 'RANDOM FOREST(oversampled)'
set.seed(123)
train_indices <- sample(1:nrow(oversampled_data), 0.7 * nrow(oversampled_data))
train_data <- oversampled_data[train_indices, ]
test_data <- oversampled_data[-train_indices, ]

# Fitting Random Forest model

random_forest_model <- randomForest(successful_app ~ price + rating_count_tot + rating_count_ver +
                                      user_rating_ver + cont_rating + prime_genre + sup_devices.num +
                                      ipadSc_urls.num + lang.num + vpp_lic + size_mb + paid_status +
                                      older_ver_rating, data = train_data)

# Making predictions on the test set
predictions <- predict(random_forest_model, newdata = test_data)

# Evaluating the model
confusion_matrix <- table(test_data$successful_app, predictions)
print("Confusion Matrix:")
print(confusion_matrix)

# Calculating the metrics
accuracy_rf <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision_rf <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall_rf <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)

print(paste("Accuracy:", accuracy_rf))
print(paste("Precision:", precision_rf))
print(paste("Recall:", recall_rf))
print(paste("F1-Score:", f1_score_rf))

# Plotting ROC curve

roc_curve <- roc(test_data$successful_app, as.numeric(predictions))
auc_roc_rf <- auc(roc_curve)
print(paste("AUC-ROC:", auc_roc_rf))
plot(roc_curve, main = "RF-OverSampled Data-ROC Curve", col = "blue", lwd = 2)

append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy =accuracy_rf,
    Precision = precision_rf,
    Recall = recall_rf,
    F1_Score = f1_score_rf,
    AUC_ROC = auc_roc_rf
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)

# Printting the updated dataframe
print(results_df)


#=======================================================================================================================================================================
# Splitting the data into training and testing sets
model_name <- 'RANDOM FOREST(undersampled)'
set.seed(123)
train_indices <- sample(1:nrow(undersampled_data), 0.7 * nrow(undersampled_data))
train_data <- undersampled_data[train_indices, ]
test_data <- undersampled_data[-train_indices, ]

# Fitting Random Forest model
library(randomForest)
random_forest_model <- randomForest(successful_app ~ price + rating_count_tot + rating_count_ver +
                                      user_rating_ver + cont_rating + prime_genre + sup_devices.num +
                                      ipadSc_urls.num + lang.num + vpp_lic + size_mb + paid_status +
                                      older_ver_rating, data = train_data)

# Making predictions on the test set
predictions <- predict(random_forest_model, newdata = test_data)

# Evaluating the model
confusion_matrix <- table(test_data$successful_app, predictions)
print("Confusion Matrix:")
print(confusion_matrix)

# Calculating metrics
accuracy_rf <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision_rf <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall_rf <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)

print(paste("Accuracy:", accuracy_rf))
print(paste("Precision:", precision_rf))
print(paste("Recall:", recall_rf))
print(paste("F1-Score:", f1_score_rf))

# Plotting ROC curve
library(pROC)
roc_curve <- roc(test_data$successful_app, as.numeric(predictions))
auc_roc_rf <- auc(roc_curve)
print(paste("AUC-ROC:", auc_roc_rf))
plot(roc_curve, main = "RF-UnderSampled Data-ROC Curve", col = "blue", lwd = 2)

append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy =accuracy_rf,
    Precision = precision_rf,
    Recall = recall_rf,
    F1_Score = f1_score_rf,
    AUC_ROC = auc_roc_rf
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)

# Printing the updated dataframe
print(results_df)
#======================================================================================================================================================================#
#---------------------------------------------SVM(Oversampled and Undersampled)----------------------------------------------------------------------
library(caret)
library(pROC)
library(e1071)

model_name <- 'SVM-KFold(oversampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(oversampled_data_scaled$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initializing variables to store evaluation metrics
accuracy_svm <- numeric(num_folds)
precision_svm <- numeric(num_folds)
recall_svm <- numeric(num_folds)
f1_score_svm <- numeric(num_folds)
auc_roc_svm <- numeric(num_folds)

# Iterating through folds
for (fold in seq_along(folds)) {
  test_indices <- unlist(folds[fold])
  test_data <- oversampled_data_scaled[test_indices, ]
  train_data <- oversampled_data_scaled[-test_indices, ]
  
# Converting labels to factors (if not already)
  train_data$successful_app <- as.factor(train_data$successful_app)
  test_data$successful_app <- as.factor(test_data$successful_app)
  
# Fitting SVM model
  svm_model <- svm(successful_app ~ ., data = train_data, kernel = "radial")
  
# Making predictions on the test set
  predictions <- predict(svm_model, newdata = test_data)
  
# Evaluating the model
  confusion_matrix <- table(test_data$successful_app, predictions)
  
# Calculating metrics for the current fold
  accuracy_svm[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_svm[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_svm[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_svm[fold] <- 2 * (precision_svm[fold] * recall_svm[fold]) / (precision_svm[fold] + recall_svm[fold])
  
# Calculation of AUC-ROC
  roc_curve <- roc(as.numeric(test_data$successful_app) - 1, as.numeric(predictions) - 1)
  auc_roc_svm[fold] <- auc(roc_curve)
}

# Calculating average metrics over all folds
avg_accuracy_svm <- mean(accuracy_svm)
avg_precision_svm <- mean(precision_svm)
avg_recall_svm <- mean(recall_svm)
avg_f1_score_svm <- mean(f1_score_svm)
avg_auc_roc_svm <- mean(auc_roc_svm)

# Printing or storing the average metrics
print(paste("Average Accuracy:", avg_accuracy_svm))
print(paste("Average Precision:", avg_precision_svm))
print(paste("Average Recall:", avg_recall_svm))
print(paste("Average F1-Score:", avg_f1_score_svm))
print(paste("Average AUC-ROC:", avg_auc_roc_svm))
plot(roc_curve, main = "SVM-OverSampled Data-ROC Curve", col = "blue", lwd = 2)


append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy =avg_accuracy_svm,
    Precision = avg_precision_svm,
    Recall = avg_recall_svm,
    F1_Score = avg_f1_score_svm,
    AUC_ROC = avg_auc_roc_svm
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)

# Printing the updated dataframe
print(results_df)

#=========================================================================================================================================================================#
library(caret)
library(pROC)
library(e1071)

model_name <- 'SVM-KFold(undersampled)'
set.seed(123)

# Stratified K-Fold Cross-Validation
num_folds <- 5
folds <- createFolds(undersampled_data_scaled$successful_app, k = num_folds, list = TRUE, returnTrain = FALSE)

# Initializing variables to store evaluation metrics
accuracy_svm <- numeric(num_folds)
precision_svm <- numeric(num_folds)
recall_svm <- numeric(num_folds)
f1_score_svm <- numeric(num_folds)
auc_roc_svm <- numeric(num_folds)

# Iterating through folds
for (fold in seq_along(folds)) {
  test_indices <- unlist(folds[fold])
  test_data <- undersampled_data_scaled[test_indices, ]
  train_data <- undersampled_data_scaled[-test_indices, ]
  
# Converting labels to factors 
  train_data$successful_app <- as.factor(train_data$successful_app)
  test_data$successful_app <- as.factor(test_data$successful_app)
  
# Fitting SVM model
  svm_model <- svm(successful_app ~ ., data = train_data, kernel = "radial")
  
# Making predictions on the test set
  predictions <- predict(svm_model, newdata = test_data)
  
# Evaluating the model
  confusion_matrix <- table(test_data$successful_app, predictions)
  
# Calculating metrics for the current fold
  accuracy_svm[fold] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision_svm[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall_svm[fold] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  f1_score_svm[fold] <- 2 * (precision_svm[fold] * recall_svm[fold]) / (precision_svm[fold] + recall_svm[fold])
  
# Calculation of AUC-ROC
  roc_curve <- roc(as.numeric(test_data$successful_app) - 1, as.numeric(predictions) - 1)
  auc_roc_svm[fold] <- auc(roc_curve)
}

# Calculating average metrics over all folds
avg_accuracy_svm <- mean(accuracy_svm)
avg_precision_svm <- mean(precision_svm)
avg_recall_svm <- mean(recall_svm)
avg_f1_score_svm <- mean(f1_score_svm)
avg_auc_roc_svm <- mean(auc_roc_svm)

# Printing or storing the average metrics
print(paste("Average Accuracy:", avg_accuracy_svm))
print(paste("Average Precision:", avg_precision_svm))
print(paste("Average Recall:", avg_recall_svm))
print(paste("Average F1-Score:", avg_f1_score_svm))
print(paste("Average AUC-ROC:", avg_auc_roc_svm))

plot(roc_curve, main = "SVM-UnderSampled Data-ROC Curve", col = "blue", lwd = 2)


append_to_results <- function(results_df, model_name, precision, recall, f1_score, auc_roc) {
  result_row <- data.frame(
    Model = model_name,
    Accuracy =avg_accuracy_svm,
    Precision = avg_precision_svm,
    Recall = avg_recall_svm,
    F1_Score = avg_f1_score_svm,
    AUC_ROC = avg_auc_roc_svm
  )
  results_df <- rbind(results_df, result_row)
  return(results_df)
}

results_df <- append_to_results(results_df, model_name, precision, recall, f1_score, auc_roc)

# Printing the updated dataframe
print(results_df)

#=========================================================================================================================================================================#



