# Rent-Price-Analysis
Rent Price Prediction in Madrid

This project aims to predict apartment rental prices in Madrid using various Machine Learning (ML) models. The data was collected through a web scraping project from the popular Spanish real estate platform Fotocasa.com.

🔍 Project Overview

The goal of this project is to develop and compare different ML models to understand which features most influence apartment rental prices and to create a model capable of making accurate predictions.

📊 Data Description

The dataset includes information on apartments in Madrid with the following key features:

Price: The rental price of the apartment (target variable).

Square Meters: The surface area of the apartment in square meters.

Rooms: The number of rooms in the apartment.

Bathrooms: The number of bathrooms in the apartment.

Neighborhood: The neighborhood (Zona) where the apartment is located.

The data was scraped from Fotocasa.com, one of the most popular property listing websites in Spain.

📈 Methodology

Data Collection:

Web scraping was used to extract apartment information from Fotocasa.com.

Cleaning and preprocessing were applied to remove missing values and standardize the data.

Exploratory Data Analysis (EDA):

Statistical summaries and visualizations were used to understand the relationships between features and rental prices.

Modeling:

Several machine learning models were built and evaluated, including:

Linear Regression

Ridge Regression

Lasso Regression

Elastic Net

Random Forest

Gradient Boosting Machines (GBM)

XGBoost

Generalized Additive Models (GAM)

Model Evaluation:

Models were evaluated using key metrics:

Root Mean Squared Error (RMSE)

R-squared (R²)

Performance comparisons were made to identify the best-performing model.

🚀 Results

The XGBoost model had the highest R-squared, but only by a small margin.

The Linear Regression model was chosen due to its simplicity, interpretability, and efficiency.

The models faced challenges in capturing the variability in rental prices, which may be due to missing features like the condition or quality of the apartments.
