# The West Nile Virus Prediction Model

This model is used to predict the risk of West Nile Virus being present two consecutive weeks in a row. 

## Data and Model

The model relies on the presence of past West Nile Virus, weather data, and trap locations with a Generalized Linear Mixed Model to develop predictions of whether a trap will have WNV present on during a particular collection week.  The models used rely on R’s `arm` package written by (by Yu-Sung Su, Daniel Lee, and Andrew Gelman). 

## Model Performance

The overall performance of the model depends highly upon the choice of the cutoff for what is considered a positive prediction.  We carefully chose a cutoff to balance capturing positive results without projecting too many positive results, which would result in unnecessary treatment. In machine learning terms, we sought to balance the precision and recall of the model.

Based on previous results we chose a cutoff of 39%, which accurately predicts the positive results 78% of the time in the test case (94 / 120), and these predictions were correct 65% of the time (94 / 144).

# How run the model

The R programming language was used to develop the model, which is free and available for download at https://cran.r-project.org.  The preferred IDE for this project is R Studio, which is also free and available for download at https://www.rstudio.com/.  

You can open the project file in R Studio ` WNV_R_Model.Rproj `, and step through the code in the .\R directory.  The files are organized sequentially.  Data will be downloaded to a `./data` directory, which will be created if it does not already exist. 
