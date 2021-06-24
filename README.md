- 👋 Hi, I’m @JuanESuarez. Find here some information about this project:

# BigFive5q Predictive Test Project

This project uses prediction techniques to simplify the well-known "Big Five" personality traits determination test. The goal is to predict most of the answers from a person so we can reduce dramatically his/her input whilst getting to a final score as accurate as possible. Thus, less length, effort and time of test resolution will make it easier to be integrated in other processes where estimation of personality characteristics of an individual can be useful.

One of the key aspects of the project is to define clear and meaningful indicators of accuracy that take into account complexity to measure the multi-outcome challenge. Related with this, a Montecarlo approach has been used to simulate a base-reference of random results.

Project originality roots on the fact of taking advantage of a well-known standard recommendation model, adapting some concepts to use classical "User x Item" as inspiration for a "User x Question/Trait" structure. 

Apart of de Exploratory Data Analysis and training of a operational prediction model, an interactive web app (https://juanesuarez.shinyapps.io/BigFive5q) has been created for illustrative purpose, where user can play with the answers and even interactively see the hidden predictions obtained for unanswered questions.


## Files included:

report_JES_BigFive5q.R: includes the downloading and construction of the data sets, the prediction script that develops the model and applies it to the validation list, and the report text and figures.  It is self-contained.

report_JES_BigFive5q.pdf: is the final report

report_JES_BigFive5q.Rmd: is the R Markdown source to generate de report_JES_BigFive5q.pdf report

app.R: is the code for the web app (shinnyapp interactive application)

load_data.R: is an pre-execution / optional code to load the original Kaggle dataset downloaded and create the .rds input data files is /data_source 

There are directories for ./images and ./data_source 

Other remaining files and directories were used in development and testing and are only saved for reference.