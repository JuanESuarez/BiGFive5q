if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(recommenderlab)) install.packages("recommenderlab", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) tinytex::install_tinytex()  # For RMarkdown .PDF generation

library(tidyverse)
library(patchwork)
library(lubridate)
library(ggrepel)
library(ggplot2)
library(patchwork)
library(ggthemes)
library(gridExtra)
library(caret)
library(corrplot)

library(recommenderlab)
library(psych)

options(digits=5)

Sys.setlocale("LC_TIME", "english")

## =======================================================
## =======================================================
## DATA PREPARATION FOR MODELLING
## =======================================================
## =======================================================


##########################################################
# Reading data files previosly saved locally 
##########################################################
df <- readRDS("BFtests.rds")
dictionary <- readRDS("dictionary.rds")
head(df)
dictionary


##########################################################
# Dataset size reduction
##########################################################
# Let reduce dataset size to facilitate development
# This step is intended to allow agile analysis in locale environment
# Previous EDA was done using all observations 
nObservsDevelopment <- 5000
set.seed(1, sample.kind="Rounding")
df <- df[sample(nrow(df), nObservsDevelopment), ]



##########################################################
# Reverted questions treatment
##########################################################
# Our data contains answers to questions considered "reverted". This means that the question is written in a negative way and, for global analysis,  must be scored reverting results recorded.
# This fact is already covered by the generic scoring analysis used for package PSYCH, and just identifying with a minus sign before question Id, function internally reverts answers value. This was used in the EDA part above.
# However, we are going to use functions based in recommendation methods (recommenderLab) based in linear algebra and distances that use "ratings" allways meaning "positive", never "reverted". For this reason, we will, before analysis, revert results of "reverse" questions and when when necessary keep using rgular PSYCH functions, but marking those questions as regular/positive (removing the minus sign)

# Revert negative questions of a Likert Scale: (n+1)-x
df <- df %>% mutate(
  OPN2 = abs(6-OPN2), OPN4 = abs(6-OPN4), OPN6 = abs(6-OPN6), 
  CSN2 = abs(6-CSN2), CSN4 = abs(6-CSN4), CSN6 = abs(6-CSN6), CSN8 = abs(6-CSN8), 
  EXT2 = abs(6-EXT2), EXT4 = abs(6-EXT4), EXT6 = abs(6-EXT6), EXT8 = abs(6-EXT8), EXT10 = abs(6-EXT10), 
  AGR1 = abs(6-AGR1), AGR3 = abs(6-AGR3), AGR5 = abs(6-AGR5), AGR7 = abs(6-AGR7), 
  EST2 = abs(6-EST2), EST4 = abs(6-EST4))

# We score AFTER change of "reverted questions"
# Removing minus sign of the list. All questions will be "positive" 
keys.list.allPositive <- list(openess = c("OPN1","OPN2","OPN3","OPN4","OPN5","OPN6","OPN7","OPN8","OPN9","OPN10"), 
                              conscienciousness = c("CSN1","CSN2","CSN3","CSN4","CSN5","CSN6","CSN7","CSN8","CSN9","CSN10"), 
                              extroversion = c("EXT1","EXT2","EXT3","EXT4","EXT5", "EXT6", "EXT7", "EXT8", "EXT9", "EXT10"), 
                              agreeability = c("AGR1", "AGR2", "AGR3", "AGR4", "AGR5", "AGR6", "AGR7", "AGR8", "AGR9", "AGR10"), 
                              natural_reactions = c("EST1","EST2","EST3","EST4","EST5", "EST6", "EST7", "EST8", "EST9", "EST10"))


##########################################################
## Save input data for shiny app
##########################################################
BFdata <- as(data.matrix(df[,2:51]), "realRatingMatrix")
saveRDS(BFdata, "BFdata.rds")
saveRDS(dictionary, "dictionary.rds")

## get some information
dimnames(BFdata)
rowCounts(BFdata) ## number of ratings per user
rownames(BFdata)
colCounts(BFdata) ## number of ratings per item
colnames(BFdata)
colMeans(BFdata) ## average item rating
nratings(BFdata) ## total number of ratings
hasRating(BFdata) ## user-item combinations with ratings
rownames(BFdata)
## histogram of ratings
hist(getRatings(BFdata), breaks="FD")
## inspect a subset
image(BFdata[1:5,1:5])



##########################################################
# Separate data in partitions
##########################################################
# Create Train and Test partitions: Validation (Test) set will be 20% of data
devReduction <- 0.2 # Percentage of original data we extract for development
set.seed(1, sample.kind="Rounding")
test_index <- sample(c(1:nrow(df)), (nrow(df) * devReduction), replace = FALSE)
BF_train <- df[-test_index,]
BF_test <- df[test_index,]

totRowsValidation <- nrow(BF_test) # number of observations to predict

# Now, using only training partition, we separate again a train&test sub-partitions to estimate parameters without using validation data
set.seed(1, sample.kind="Rounding")
test_index_p2 <- sample(c(1:nrow(BF_train)), (nrow(BF_train) * devReduction), replace = FALSE)
BF_train_p2 <- BF_train[-test_index_p2,]
BF_test_p2 <- BF_train[test_index_p2,]

rm(test_index, test_index_p2)


## =======================================================
## =======================================================
## TRAINING AND PREDICTION
## =======================================================
## =======================================================


##########################################################
# Train
##########################################################
# Train a recommender model


##########################################################
# Montecarlo estimation of theoretical accuracy
##########################################################
n <- 1000
B <- 10000
S <- replicate(B,{
  x <- sample(c(-1,1), n, replace = TRUE, prob = c(18/38, 20/30))
  sum(x)})
S

# Create a dataframe to store results of the analysis
analysis_results <- data_frame(Trait="All", Score = 0.55, Accuracy_type = "3+ hits HighLow", Algorithm = "Montecarlo")
# Show results
analysis_results %>% knitr::kable(digits = 4)


##########################################################
# Loop available recommendation algorithms
##########################################################

# STARTS FOR-LOOP OF ALGORITHMS TO TEST
# >>>>>>>>>>>>
# >>>>>>>>>>>>

# methods_choice <- list(
#   list("ALS", "ALS" = list(NULL)),
#   list("RANDOM", "random items" = list(NULL)),
#   list("POPULAR", "popular items" = list(NULL)),
#   list("UBCF", "user-based CF" = list(nn=50)),
#   list("IBCF", "item-based CF" = list(k=5000)),
#   list("SVD", "SVD approximation" = list(k = 10))
# )

methods_choice <- list(
  list("RANDOM", "random items" = list(NULL)),
  list("POPULAR", "popular items" = list(NULL)),
  list("UBCF", "user-based CF" = list(nn=50)),
  list("IBCF", "item-based CF" = list(k=5000)),
  list("SVD", "SVD approximation" = list(k = 10))
)


for (a in 1:length(methods_choice)) {
  chosenAlgorithm <- methods_choice[[a]][[1]]
  chosenAlgorithmParams <- methods_choice[[a]][[2]]
##########################################################
# Train model
##########################################################
  recom <- Recommender(
    as(data.matrix(BF_train[,2:51]), "realRatingMatrix"), 
    method = chosenAlgorithm, 
    parameter=chosenAlgorithmParams)

##########################################################
# Test (validate) model
##########################################################

################################################
# Select 5 questions and predict the other 45 to obtain a full matrix
# Pick one question of each group
  questionsList <- dictionary$Question
  names(questionsList) <- dictionary$ID
  set.seed(1, sample.kind="Rounding")
  chosen_questions <- seq(1, 41, by=10) + sample(c(0:9), 5)

# Prepare known ratings (5 ot of 50) to send to the model - take from VALIDATION
ratings <- matrix(NA, nrow = totRowsValidation, ncol = 50)
ratings[, chosen_questions[1]] <- BF_test[,2:51][,chosen_questions[1]]
ratings[, chosen_questions[2]] <- BF_test[,2:51][,chosen_questions[2]]
ratings[, chosen_questions[3]] <- BF_test[,2:51][,chosen_questions[3]]
ratings[, chosen_questions[4]] <- BF_test[,2:51][,chosen_questions[4]]
ratings[, chosen_questions[5]] <- BF_test[,2:51][,chosen_questions[5]]

ratings <- as(ratings, "realRatingMatrix")

# create (predict) recommendations (45) based on known 5 answers ('ratings') 
pred <- predict(recom, ratings, n=45)

# Predicted answers from the model (still "rough")
if (length(unlist(getList(pred)))/45 == totRowsValidation) {
  # when all users got a prediction
  matrixNamesPredicted <- matrix(unlist(getList(pred)), ncol = 45, byrow = TRUE)
  matrixScoresPredicted <- matrix(unlist(getRatings(pred)), ncol = 45, byrow = TRUE)
} else {
  # populate if pending users to predict (IBCF method)
  anyListNames <- getList(pred)[[1]]
  matrixNamesPredicted <- matrix(NA, nrow=totRowsValidation, ncol=45)
  matrixScoresPredicted <- matrix(NA, nrow=totRowsValidation, ncol=45)
  for (i in c(1:totRowsValidation)) {
    if (length(getList(pred)[[i]]) != 0) {
      matrixNamesPredicted[i,] <- getList(pred)[[i]]
      matrixScoresPredicted[i,] <- getRatings(pred)[[i]]
    } else {
      matrixNamesPredicted[i,] <- anyListNames
      matrixScoresPredicted[i,] <- colMeans(BFdata[2:51])[anyListNames]
    }
  }
}


# This matrices contains all predictions for validation dataset row by row (matrixScoresPredicted), BUT each row follows a different order, based on corresponding row of previous (matrixNamesPredicted) matrix
# So, we need to rearrange all lines to any, but the same, column structure
# We use list of first row as template for this arrangement:
tmpPatternColumnsReference <- matrixNamesPredicted[1,]
tmpPatternColumnsReference
# This loop performs rearrange row by row
for (r in c(1:totRowsValidation)) {
  matrixScoresPredicted[r,] <- matrixScoresPredicted[r,][match(tmpPatternColumnsReference, matrixNamesPredicted[r,])]
}
# We assign column names of first row (used as reference to new global matrix)
colnames(matrixScoresPredicted) <- tmpPatternColumnsReference
rm(matrixNamesPredicted)  # Remove this mixed matrix to prevent confusion, clarity in next steps

# Inspect obtained matrix
dim(matrixScoresPredicted)
class(matrixScoresPredicted)
colnames(matrixScoresPredicted)
head(matrixScoresPredicted)
matrixScoresPredicted[1,] # First row
matrixScoresPredicted[totRowsValidation,] # Last row


##########################################################
# Collect together entered and predicted ratings to prepare results calculation
##########################################################
realRatings <- BF_test[,2:51] # real nx50 answers in the validation dataset
realRatings
dim(realRatings)
enteredQuestions <- dictionary[chosen_questions[1:5],1] # names of 5 "known"
enteredRatings <- as(ratings, "matrix")[, chosen_questions[1:5]]  # answers of nx5 "known" questions
colnames(enteredRatings) <- enteredQuestions
# Build a matrix with the union of 5 real answers + 45 predicted answers for all validation raws
dim(enteredRatings) # n x 5 questions entered
dim(matrixScoresPredicted) # n x 45 questions predicted 
# Ready to prepare a single matrix
matrixAllRatings <- cbind(enteredRatings, matrixScoresPredicted)
dim(matrixAllRatings) # n x 50 (all questions)
rm(matrixScoresPredicted) # For clarity to prevent confusion
colnames(matrixAllRatings)
# Reorder "mixed" columns as in dataset
matrixAllRatings <- matrixAllRatings[,colnames(BFdata)]
colnames(matrixAllRatings)


##########################################################
# Score real and predicted matrices
##########################################################
# We have now both matrices (real data and predicted data) so we score them individually
# Score adding last (predicted) row to validation data  
scoresfile_real <- scoreFast(keys.list.allPositive, as(BF_test[,2:51], "matrix"))
scoresfile_pred <- scoreFast(keys.list.allPositive, as(matrixAllRatings, "matrix"))
# Put a suffix in predicted-based scores columns to distinguish from real-based
colnames(scoresfile_pred) <- paste(colnames(scoresfile_pred), "_pred", sep = "")
dim(scoresfile_real)
dim(scoresfile_pred)


##########################################################
# Convert just calculated scores to ranking (percentile)
##########################################################
# Results are presented in percentile for each user/trait 
# Calculate percentile of each score
# With all data + prediction scored together (n+1 rows matrix), calculate percentiles of the last row (results for the user) using eCDF
# For real data percentiles
O_score <- round(ecdf(scoresfile_real[,"openess-A"])(scoresfile_real[,"openess-A"])*100,0)
C_score <- round(ecdf(scoresfile_real[,"conscienciousness-A"])(scoresfile_real[,"conscienciousness-A"])*100,0)
E_score <- round(ecdf(scoresfile_real[,"extroversion-A"])(scoresfile_real[,"extroversion-A"])*100,0)
A_score <- round(ecdf(scoresfile_real[,"agreeability-A"])(scoresfile_real[,"agreeability-A"])*100,0)
N_score <- round(ecdf(scoresfile_real[,"natural_reactions-A"])(scoresfile_real[,"natural_reactions-A"])*100,0)
traits_percentiles_real <- cbind(O_score, C_score, E_score, A_score, N_score)
# For predicted data percentiles
O_score_pred <- round(ecdf(scoresfile_real[,"openess-A"])(scoresfile_pred[,"openess-A_pred"])*100,0)
C_score_pred <- round(ecdf(scoresfile_real[,"conscienciousness-A"])(scoresfile_pred[,"conscienciousness-A_pred"])*100,0)
E_score_pred <- round(ecdf(scoresfile_real[,"extroversion-A"])(scoresfile_pred[,"extroversion-A_pred"])*100,0)
A_score_pred <- round(ecdf(scoresfile_real[,"agreeability-A"])(scoresfile_pred[,"agreeability-A_pred"])*100,0)
N_score_pred <- round(ecdf(scoresfile_real[,"natural_reactions-A"])(scoresfile_pred[,"natural_reactions-A_pred"])*100,0)
traits_percentiles_predicted <- cbind(O_score_pred, C_score_pred, E_score_pred, A_score_pred, N_score_pred)

################################################
# Here we have all resulting data
names(chosenAlgorithm) = "Algorithm"
userOtherData <- BF_test %>% select(userId, Year, Month, country) # Collect other user related data for final results context
# Other files generated during process
dim(realRatings)
dim(scoresfile_real)
dim(matrixAllRatings)
dim(scoresfile_pred)
dim(userOtherData)
chosenAlgorithm# Algorithm used
dim(traits_percentiles_real)
dim(traits_percentiles_predicted)
head(traits_percentiles_real)
head(traits_percentiles_predicted)


##########################################################
# Compose high level result in a data frame
##########################################################
accuracyPerQuartile <- colMeans((1+floor(abs((traits_percentiles_real-1))/25)) == (1+floor(abs((traits_percentiles_predicted-1))/25)))
accuracyPerQuartileMean <- as(cbind("All", mean(accuracyPerQuartile),"Hits quartile"),"matrix")
accuracyPerQuartile <- cbind(as(accuracyPerQuartile,"matrix"), Accuracy_type = "Hits quartile")

accuracyPerHalf <- colMeans((1+floor(abs((traits_percentiles_real-1))/50)) == (1+floor(abs((traits_percentiles_predicted-1))/50)))
accuracyPerHalfMean <- as(cbind("All", mean(accuracyPerHalf),"Hits HighLow"),"matrix")
accuracyPerHalf <- cbind(as(accuracyPerHalf,"matrix"), Accuracy_type = "Hits HighLow")

accuracySameHalf <- (1+floor(abs((traits_percentiles_real-1))/50)) == (1+floor(abs((traits_percentiles_predicted-1))/50))
accuracySameHalf <- mean(rowSums(accuracySameHalf[,1:5]) >= 3)
accuracySameHalf <- as(cbind("All", accuracySameHalf,"3+ hits HighLow"),"matrix")
accuracySameHalf

# Collect results already obtained in a temporary matrix
tmpResultsPrediction <- rbind(
  accuracyPerQuartile, 
  accuracyPerHalf)
tmpResultsPrediction <- cbind(
  "Trait" = rownames(tmpResultsPrediction), tmpResultsPrediction)
colnames(tmpResultsPrediction)[2] <- "Score"
rownames(tmpResultsPrediction) <- NULL
tmpResultsPrediction <- tmpResultsPrediction %>% 
  rbind(accuracySameHalf, accuracyPerQuartileMean, accuracyPerHalfMean) %>% 
  cbind("Algorithm" = chosenAlgorithm)

tmpResultsPrediction

################################################
# Create de data frame
df_resultsPrediction <- 
  as.data.frame(tmpResultsPrediction)
df_resultsPrediction <- df_resultsPrediction %>% 
  mutate(Score = as.double(as.character(df_resultsPrediction$Score)))
df_resultsPrediction
rm(tmpResultsPrediction)
# Store and show results
analysis_results <- bind_rows(
  analysis_results, 
  df_resultsPrediction)
analysis_results %>% knitr::kable(digits = 4)

}
# ENDS FOR-LOOP OF ALGORITHMS TO TEST
# <<<<<<<<<<
# <<<<<<<<<<


##########################################################
# Show final results
##########################################################


table_results <- analysis_results %>% 
  spread(Trait, Score, fill = "") %>% 
  select(1,2,4,8,5,6,3,7,4)

table_results %>% 
  group_by(Accuracy_type, Algorithm) %>% 
  summarise("Best_Accuracy" = max(as.numeric(All)))


table_results %>% knitr::kable(digits = 4)

view(table_results)






