##########################################################
# Reading data files previosly saved locally 
##########################################################
library(tidyverse)
library(recommenderlab)

df <- readRDS("BFtests.rds")
nObservsDevelopment <- 5000
set.seed(1, sample.kind="Rounding")
df <- df[sample(nrow(df), nObservsDevelopment), ]
df <- df %>% mutate(
  OPN2 = abs(6-OPN2), OPN4 = abs(6-OPN4), OPN6 = abs(6-OPN6), 
  CSN2 = abs(6-CSN2), CSN4 = abs(6-CSN4), CSN6 = abs(6-CSN6), CSN8 = abs(6-CSN8), 
  EXT2 = abs(6-EXT2), EXT4 = abs(6-EXT4), EXT6 = abs(6-EXT6), EXT8 = abs(6-EXT8), EXT10 = abs(6-EXT10), 
  AGR1 = abs(6-AGR1), AGR3 = abs(6-AGR3), AGR5 = abs(6-AGR5), AGR7 = abs(6-AGR7), 
  EST2 = abs(6-EST2), EST4 = abs(6-EST4))
BFdata <- as(data.matrix(df[,2:51]), "realRatingMatrix")
# Create Train and Test partitions: Validation (Test) set will be 20% of data
devReduction <- 0.2 # Percentage of original data we extract for development
set.seed(1, sample.kind="Rounding")
test_index <- sample(c(1:nrow(df)), (nrow(df) * devReduction), replace = FALSE)
BF_train <- df[-test_index,]
BF_test <- df[test_index,]



##########################################################
# PRUEBAS DE IBCF
##########################################################
nObservaciones <- 5000
nRegTest <- 1000
nFeatures <- 50
algoritmo <- 4

methods_choice = c(
  "RANDOM", 
  "POPULAR", 
  "UBCF", 
  "IBCF", 
  "SVD")
methods_params <- list(
  "random items" = list(NULL),# normalize = "Z-score"
  "popular items" = list(NULL),# normalize = "Z-score"
  "user-based CF" = list(nn=50),
  "item-based CF" = list(k=50),
  "SVD approximation" = list(k = 10)
)

# PTE:
#   -Arreglar # rows IBCF (parameters o “rellenar”)
# -HybridRecommender + ReRecommend (https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf)

nPredicciones <- (nFeatures - 5)
BF_ENTRENA <- BF_train[1:nObservaciones,2:51]
BF_VALIDA <- BF_test[1:nRegTest,2:51]
BF_ENTRENA <- BF_ENTRENA[1:nObservaciones,1:nFeatures]
BF_VALIDA <- BF_VALIDA[1:nRegTest,1:nFeatures]
BF_ENTRENA
BF_VALIDA
ratings <- matrix(NA, nrow = nRegTest, ncol = nFeatures)
ratings[,seq(1,50,11)] <- c(BF_VALIDA[,1],BF_VALIDA[,2],BF_VALIDA[,3],BF_VALIDA[,4],BF_VALIDA[,5])
colnames(ratings) <- names(BF_ENTRENA)
ratings


recom <- Recommender(
  as(data.matrix(BF_ENTRENA), "realRatingMatrix"),
  method = methods_choice[algoritmo],
  parameter=methods_params[[algoritmo]])

getModel(recom)

pred <- predict(recom, as(ratings, "realRatingMatrix"), n=nPredicciones)

matrixNamesPredicted <- matrix(unlist(getList(pred)), ncol = nPredicciones, byrow = TRUE)
matrixScoresPredicted <- matrix(unlist(getRatings(pred)), ncol = nPredicciones, byrow = TRUE)

# # Predicted answers from the model (still "rough")
# if (length(unlist(getList(pred)))/45 == totRowsValidation) {
#   # when all users got a prediction
#   matrixNamesPredicted <- matrix(unlist(getList(pred)), ncol = 45, byrow = TRUE)
#   matrixScoresPredicted <- matrix(unlist(getRatings(pred)), ncol = 45, byrow = TRUE)
# } else {
#   # populate if pending users to predict (IBCF method)
#   anyListNames <- getList(pred)[[1]]
#   matrixNamesPredicted <- matrix(NA, nrow=totRowsValidation, ncol=45)
#   matrixScoresPredicted <- matrix(NA, nrow=totRowsValidation, ncol=45)
#   for (i in c(1:totRowsValidation)) {
#     if (length(getList(pred)[[i]]) != 0) {
#       matrixNamesPredicted[i,] <- getList(pred)[[i]]
#       matrixScoresPredicted[i,] <- getRatings(pred)[[i]]
#     } else {
#       matrixNamesPredicted[i,] <- anyListNames
#       matrixScoresPredicted[i,] <- colMeans(BFdata[2:51])[anyListNames]
#     }
#   }
# }


length(getList(pred)) == nRegTest
nrow(matrixScoresPredicted) == nRegTest
nrow(matrixScoresPredicted)
matrixNamesPredicted
matrixScoresPredicted






tmpListPending[996,]
tmpRatingsPending[996,]

matrixNamesPredicted <- tmpListPending
matrixScoresPredicted <- tmpRatingsPending
matrixScoresPredicted[is.na(tmpRatingsPending[,1]),]






# getList(pred)[[1]][order(getList(pred)[[1]])]
# getRatings(pred)[[1]][order(getList(pred)[[1]])]
# !!
# names(getModel(recom))
# getModel(recom)$sim
