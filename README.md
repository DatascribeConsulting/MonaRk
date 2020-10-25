# MonaRk
Monark R App for comparing flight path variability

#### Data Sources & Parsing
The data for this project comes from OpenSky, SIGMET reports from the Aviation Weather Center, storm events from the National Climatic Data Center, 
and flight delay data from the Bureau of Transportation Statistics. There are functions to parse these data types in the R file that may be of help to other data scientists. 
Download the MonaRk folder and follow the "Installing Package" instructions within vignettes/monaRk_instructions.html (or .Rmd) to use the code like an R package with documentation. 
Functions can be used individually as well.

#### Running the App
A version of the app is at https://datascribe.shinyapps.io/MonaRk/. If launching the app for the first time, go to the help tab and watch the tutorial video.

#### Updating Data
Every few months, the AddMondays() function will be run to update the data used in the app. Instructions are in vignettes/monaRk_instructions.html. 
