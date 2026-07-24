# PBA Group 03 — AppleStore Business Analytics

A business analytics project applying data cleaning, feature engineering, and classification modeling to the **AppleStore App Store dataset**. The project builds and compares multiple machine learning models to predict app outcomes, using K-Fold cross-validation for robust evaluation.

## 📊 Dataset

- `AppleStore.csv` — main dataset (Apple App Store listings)
- `appleStore_description.csv` — supplementary app description data

## 🧰 Project Structure

| File | Description |
|---|---|
| `DATA CLEANING.R` | Cleans and prepares the raw AppleStore dataset |
| `PBA_EDA.R` | Exploratory data analysis |
| `FEATURE ENGINEERING.R` | Creates and transforms features for modeling |
| `DATA PREPARATION FOR MODELLING.R` | Final preprocessing before model training |
| `IMPORTANT FEATURES - GRAPH.R` | Visualizes key feature importance |
| `BINARY CLASSIFICATION FULL.R` | Full binary classification pipeline |
| `MULTICLASS CLASSIFICATION FILE.R` | Multiclass classification extension |
| `Decision Tree-KFold.R` | Decision Tree model with K-Fold CV |
| `DECISION TREE - IMPORTANT FEATURES.R` | Feature importance from Decision Tree |
| `Random Forest.R` | Random Forest model |
| `RANDOM FOREST - IMPORTANT FEATURES.R` | Feature importance from Random Forest |
| `KNN-KFold.R` | K-Nearest Neighbors model with K-Fold CV |
| `SVM-KFold.R` | Support Vector Machine model with K-Fold CV |
| `Logistic Regression-KFold.R` | Logistic Regression model with K-Fold CV |
| `Naive Bayes-KFold.R` | Naive Bayes model with K-Fold CV |
| `EVALUATION TABLE.R` | Consolidated evaluation metrics |
| `MODEL PERFORMANCE COMPARISON.R` | Compares performance across all models |
| `Group03 Business Analytics Report.pdf` | Final written report |

## 🔍 Methodology

1. **Data Cleaning** — handling missing values, duplicates, and inconsistent formatting
2. **EDA** — understanding distributions, correlations, and trends in app data
3. **Feature Engineering** — creating predictive features from raw app attributes
4. **Modeling** — training and validating multiple classifiers:
   - Decision Tree
   - Random Forest
   - K-Nearest Neighbors (KNN)
   - Support Vector Machine (SVM)
   - Logistic Regression
   - Naive Bayes
5. **Evaluation** — K-Fold cross-validation, accuracy, and metric comparison across all models to identify the best performer

## 🛠️ Tech Stack

- **R** (primary language for analysis and modeling)
- Common R libraries used for data wrangling, visualization, and ML (e.g. `caret`, `randomForest`, `e1071`, `class`, `rpart`, `ggplot2` — update to match your actual imports)

## ▶️ How to Run

```r
# 1. Clone the repo

# 2. Open the .R files in RStudio

# 3. Run scripts in order:
#    DATA CLEANING.R → PBA_EDA.R → FEATURE ENGINEERING.R →
#    DATA PREPARATION FOR MODELLING.R → individual model scripts →
#    MODEL PERFORMANCE COMPARISON.R
```
📈 Results

See Group03 Business Analytics Report.pdf for full findings, model comparison table, and business recommendations.


