# Advanced-Clustering-Analysis-for-Marketing-Segmentation-Strategy
The dataset used for this study can be found in here:
https://www.kaggle.com/datasets/hunter0007/ecommerce-dataset-for-predictive-marketing-2023

## 1. Introduction:

This study aims to utilize advanced clustering techniques to analyze essential product-specific variables such as customer consumption frequency, timing of purchases, and specific types of products purchased. This approach seeks to develop a robust and effective marketing segmentation strategy. By refining our understanding of customer segmentation and purchase behaviors through the comprehensive examination and integration of these variables, we aim to provide valuable insights for marketing practitioners and decision-makers in the grocery retail industry. Ultimately, this study will contribute to enhancing marketing intelligence, leading to improved business performance and customer satisfaction. 

Research questions are: 
1) Does the frequency of purchasing specific types of products influence customer buying habits and decisions at the Supermarket? 
2) Does the timing of purchases (hours of the day and days of the week) impact the types of products consumers choose to buy at the Supermarket?

## 2. Overflow:

- Data Cleaning: Conducted data cleaning to ensure data quality and accuracy, resolving inconsistencies and missing values in the dataset. The cleaned dataset contains the following variables:
  - [ ] order_id – (A unique number to identity the order)
  - [ ] user_id - (A unique number to identify the user)
  - [ ] order_number – (The number of times a user purchased at hunter) 
  - [ ] order_dow – (Day of the Week the order was made)
  - [ ] order_hour_of_day – (Time of the order) 
  - [ ] days_since_prior_order - (History of the order) 
  - [ ] product_id – (Id of the product) 
  - [ ] add_to_cart_order – (Number of items added to cart)
  - [ ] reordered – (If the reorder took place)
  - [ ] department_id - (Unique number allocated to each department)
  - [ ] department – (Names of the departments)
  - [ ] product_name – (Name of the products)
  - [ ] frequency - (# of time a user purchased at hunter)

- Suitability analysis: Used the ggcorrplot function from the ggplot2 package to create a correlation heatmap of the selected features and evaluates the suitability of factor analysis using the KMO and Bartlett tests.

- Factor analysis: By plotting a scree plot and applying the method for determining the number of factors, we determined that 3 factors should be chosen for factor analysis based on the selected features. Factor analysis was then conducted, and the explained variance and communality of each factor are calculated.

- Principal component analysis: Using the FactoMineR and factoextra packages, principal component analysis was performed, and eigenvalue and variance contribution plots of the features were created.

- Cluster analysis: K-means algorithm was used for cluster analysis. The optimal number of clusters was determined as 4 by calculating the total within-cluster sum of squares and the ratio of dispersion at different cluster numbers. An elbow plot was also generated.

- Prediction using regression after clustering: Based on research question 1 and research question 2, the dataset was divided into training and testing sets, and linear regression models were used to predict specific target variables. Then, the data was split into different subsets based on the cluster results, and linear regression models were applied to each subset for prediction. Finally, the prediction performance of the overall model and the subset models was compared.

- Delivered a high-quality presentation to the decision team, showcasing the comprehensive analysis, insightful findings, and strategic recommendations of the project.
