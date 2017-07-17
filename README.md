# The West Nile Virus Prediction Model

This model is used to predict the risk of West Nile Virus being present two consecutive weeks in a row. 

Most of the work in this project is data wrangling to get the data into a format where each row represents the results for a week / location collection, and the interval between the dates is regular (i.e. a week apart). Once this is accomplished, a model is fit that is very similar to a generalized linear model to predict the likelihood that WNV will be present if it was present last week. This allows our teams to go out and spray for mosquitos a week earlier than they would have otherwise.

## Data and Model

The model relies on the presence of past West Nile Virus, weather data, and trap locations with a Generalized Linear Mixed Model to develop predictions of whether a trap will have WNV present on during a particular collection week.  The models used rely on R’s `arm` package written by (by Yu-Sung Su, Daniel Lee, and Andrew Gelman). 

## Model Performance

The overall performance of the model depends highly upon the choice of the cutoff for what is considered a positive prediction.  We carefully chose a cutoff to balance capturing positive results without projecting too many positive results, which would result in unnecessary treatment. In machine learning terms, we sought to balance the precision and recall of the model.

Based on previous results we chose a cutoff of 39%, which accurately predicts the positive results 78% of the time in the test case (94 / 120), and these predictions were correct 65% of the time (94 / 144).

# How run the model

The R programming language was used to develop the model, which is free and available for download at https://cran.r-project.org.  The preferred IDE for this project is R Studio, which is also free and available for download at https://www.rstudio.com/.  

You can open the project file in R Studio ` WNV_R_Model.Rproj `, and step through the code in the `.\R` directory.  The files are organized sequentially.  Data will be downloaded to a `./data` directory, which will be created if it does not already exist. 

# System Requirements

We recommend using R Studio as the IDE along with R the programming language. The user can then open the `.Rproj` file and explore / run the code. However, it is not possible to run the code in its entirety from end to end without access to our databases. The West Nile Virus results on the data portal contain inexact locations for the traps, but exact locations are used in the production model. Because of the threat of vandalism / etc. we do not make exact trap locations available. 

Within R you will need to install several libraries, which are declared at the beginning of each script. The libraries are loaded using a function called `loadinstall_libraries` from the `geneorama` package, which is a convenience function that attempts to install libraries if they are not installed. `geneorama` can be installed by using the devtools package (available on CRAN) and running `devtools::install_github("geneorama/geneorama")`

You will also need to store a token to access the NOAA api.  This token is easy to obtain by registering at the NOAA website: https://www.ncdc.noaa.gov/cdo-web/token.  Store the token in a plain text file: `untracked\weather_noaa_token.txt` with no other text.
