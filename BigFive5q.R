##########################################################
# BigFive.5q Project
# Author: Juan Eloy Suarez
##########################################################

##########################################################
# Initialize environment
##########################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(recommenderlab)) install.packages("recommenderlab", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(extraDistr)) install.packages("extraDistr", repos = "http://cran.us.r-project.org")
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
library(extraDistr)

library(recommenderlab)
library(psych)
library(gtools)
library(matrixStats)

options(digits=5)

Sys.setlocale("LC_TIME", "english")


##########################################################
# Create dataset
##########################################################

# Data sorce in Kaggle:
# https://www.kaggle.com/tunguz/big-five-personality-test/download
# API: kaggle datasets download -d tunguz/big-five-personality-test

# tab delimited, and  our int columns are character, perhaps because of the nulls!
df <- read.csv("./input/big-five-personality-test/data-final.csv", sep="\t", stringsAsFactors = FALSE, na.strings=c("NA","NaN", " ", "NULL"))
# We select relevant columns to use
df <- df %>% select(c(1:50), dateload, country) %>% rownames_to_column('userId') %>%
  mutate(Month = month(dateload), Year = year(dateload)) %>%
  select(-dateload)
# Remove rows containing any NA
df <- na.omit(df)
# Remove any value not between 1 and 5 in the answers columns
df <- df %>% filter_at(vars(2:51), all_vars((.) %in% c(1:5)))

nrow(df)

# Let's load up the questions from the data dictionary
dictionary <-
  read_table("./input/big-five-personality-test/codebook.txt", skip=5) %>%
  separate(1, sep="\t", extra="merge", into=c("ID", "Question")) %>%
  data.frame() %>% top_n(50)



##########################################################
# Save input data to a local file
##########################################################
# Save original data before added scores to expedite development
saveRDS(df, "BFtests.rds")
saveRDS(dictionary, "dictionary.rds")


##########################################################<<<<<<<<<<<<
##########################################################<<<<<<<<<<<<
##########################################################<<<<<<<<<<<<


##########################################################
# Reading data files previosly saved locally 
##########################################################
df <- readRDS("BFtests.rds")
dictionary <- readRDS("dictionary.rds")
head(df)
dictionary

## =======================================================
## =======================================================
## Exploratory Data Analysis
## =======================================================
## =======================================================

##########################################################
# Test reliability in each personality trait
##########################################################
# Preparation, questions structure. To prepare an analysis per "traits" (each one includes groups of questions), we create a list of 5 concepts (groups of questions per personality dimension)
extroversion = dictionary[c(1:10), "ID"]
natural_reactions = dictionary[c(11:20), "ID"]
agreeability = dictionary[c(21:30), "ID"]
conscienciousness = dictionary[c(31:40), "ID"]
openess = dictionary[c(41:50), "ID"]

buckets = list("extroversion" = extroversion, 
               "natural_reactions" = natural_reactions, 
               "agreeability" = agreeability, 
               "conscienciousness" = conscienciousness, 
               "openess"= openess)

# Reliability (internal consistency). Questions of the test are grouped in 5 categories (personality trait). We can expect a high correlation (positive or negative) among the 10 questions in each group. We also expect also reliability alfa vualues. The Cronbach’s alpha coefficient measures reliability, or internal consistency, to see if multiple-question Likert scale surveys are reliable. Cronbach’s alpha will tell us how closely related a set of test items are as a group.
# https://www.rdocumentation.org/packages/psych/versions/2.1.3/topics/alpha
questionsSigns <- numeric()
traitsAlphas <- data.frame()
# Groups analysis. For each concept, let's calculate alfa to check "internal consistency" of its questions
# The result also identify which variables should be inverted (negative correlation)
for(i in 1:length(buckets)){
  questiondf <- df[, colnames(df) %in% unlist(buckets[i])]
  cronbach <- psych::alpha(questiondf, check.keys=TRUE)
  questionsSigns <- c(questionsSigns, cronbach$keys)
  traitsAlphas <- rbind(traitsAlphas, cronbach$total[,1:3])
}

# Reverted questions. Signs indicating if questions correlate directly or "reverted"
questionsSigns

# Cronbach's alpha per trait's questions.
rownames(traitsAlphas) <- NULL
traitsAlphas <- cbind(Group = paste("Questions_", seq(1,5,1), "X", sep=""), traitsAlphas)# Assign question's group
# Visualize. Value 0.8 >= alfa > 0.9 is generally interpreted "Good Internal Consistency", and alfa >= 0.9 as "Excellent Internal consistency"
tmpAvg <- mean(traitsAlphas$raw_alpha)
traitsAlphas %>%
  ggplot(aes(Group, raw_alpha)) +
  geom_point(color="blue") +
  geom_hline(yintercept=tmpAvg, linetype="dashed", color = "orange", size=2) +
  geom_label_repel(aes(label = Group),
                   label.size = NA,
                   fill = "transparent",
                   box.padding   = 0.65,
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  ggtitle("Tests Reliability") +
  xlab(element_blank()) +
  ylab("Cronbach's alpha") +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color="blue", size=14, face="bold"))



##########################################################
# Correlation of the answers to questions
##########################################################
# Correlation calculation. Calculate global correlation matrix to see full landscape instead of just per trait
corAllQuestions <- cor(df[,2:51])
corAllQuestions
# Significance calculation. This function calculates de significance test associated to the corrrelation matrix
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(corAllQuestions)  # significance test
# Correlation within same group. Questions explitly associated to each "trait" strongly correlate, i.e. all of them are related because all of them explain the final value of the score for its specific group (trait). 
# Example for one of the traits (groups)
corrplot(corAllQuestions[41:50,41:50], 
         method = "circle", #circle, color, pie, number
         type = "upper", #full, upper, lower
         order = "alphabet", #"AOE", "FPC", "hclust", "alphabet"
         tl.col = "black", tl.srt = 90, 
         title =  "Trait: Openess", 
         p.mat = p.mat, sig.level = 0.01#, insig = "p-value"#blank, p-value, pch
         )

# Correlation with other groups. However, we see also some other significant correlations of some questions with questions that "belong" to different traits. Since our challenge is preciselly to use few (only five) questions to explain as mach as possible of the result for all traits, it will be useful to use these correlations to select what specific questions we show to get answer. 
# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
corrplot(corAllQuestions, 
         method = "circle", #circle, color, pie
         type = "upper", #full, upper, lower
         order = "alphabet", #"AOE", "FPC", "hclust", "alphabet"
         tl.col = "black", tl.srt = 90, 
         bg = "white", 
         p.mat = p.mat, sig.level = 0.01#, insig = "p-value"#blank, p-value, pch
         )



##########################################################
# Calculate scores for each observation based on its questions
# Version AS-IS: We score before change of "reverted questions"
##########################################################
# We create index variables for each respondent to take us on forward. Note that this is imputing median for missing data, which is definitely not always what you want, but for this case I'm going to go ahead just to avoid missing data issues.

# This list identifies each column to its correponding group and informs of the sign (to revert if necessary) based in our previous analysis of correlation
keys.list <- list(openess = c("OPN1","-OPN2","OPN3","-OPN4","OPN5","-OPN6","OPN7","OPN8","OPN9","OPN10"), 
                  conscienciousness = c("CSN1","-CSN2","CSN3","-CSN4","CSN5","-CSN6","CSN7","-CSN8","CSN9","CSN10"), 
                  extroversion = c("EXT1","-EXT2","EXT3","-EXT4","EXT5", "-EXT6", "EXT7", "-EXT8", "EXT9", "-EXT10"), 
                  agreeability = c("-AGR1", "AGR2", "-AGR3", "AGR4", "-AGR5", "AGR6", "-AGR7", "AGR8", "AGR9", "AGR10"), 
                  natural_reactions = c("EST1","-EST2","EST3","-EST4","EST5", "EST6", "EST7", "EST8", "EST9", "EST10"))

# We use psych::scoreItems to calculate scores (i.e. mean per group taking sign into account)
# See: https://www.rdocumentation.org/packages/psych/versions/2.1.3/topics/scoreItems
scoresfile <- scoreFast(keys.list, df) #scoresfile <- scoreVeryFast(keys.list, df)

df_scored_preReversion <- cbind(df, scoresfile)
# Once obtained the observation average per group, we need the p-value (0 to 100) per observation in group scope
P1 = ecdf(scoresfile[,1])    # P is a function giving the empirical CDF of X
P2 = ecdf(scoresfile[,2])
P3 = ecdf(scoresfile[,3])
P4 = ecdf(scoresfile[,4])
P5 = ecdf(scoresfile[,5])

df_scored_preReversion <- df_scored_preReversion %>% mutate(
  `openess-P` = round(P1(`openess-A`)*100), 
  `conscienciousness-P` = round(P2(`conscienciousness-A`)*100), 
  `extroversion-P` = round(P3(`extroversion-A`)*100), 
  `agreeability-P` = round(P4(`agreeability-A`)*100), 
  `natural_reactions-P` = round(P5(`natural_reactions-A`)*100) )


##########################################################
# Visualization
##########################################################
# First, we take a look at the different indices against each other, to see what sort of interrelationships they might have.

df_scored_preReversion %>% ggplot() +
  theme_bw() +
  geom_density(aes(x=`extroversion-A`, fill = "Extroversion"), alpha=.2)+
  geom_density(aes(x=`natural_reactions-A`, fill = "Natural Reactions"), alpha = .2)+
  geom_density(aes(x=`agreeability-A`, fill = "Agreeability"), alpha = .2)+
  geom_density(aes(x=`conscienciousness-A`, fill = "Concienciousness"), alpha = .2)+
  geom_density(aes(x=`openess-A`, fill = "Openess"), alpha = .2)+
  labs(title="Distributions of Indices", x="Response Index")+
  scale_fill_manual(name="Indices", values = c('Extroversion' = 'blue'
                                               , 'Natural Reactions' = 'red'
                                               , 'Agreeability' = 'green'
                                               , 'Concienciousness' = 'gray'
                                               , 'Openess' = 'purple'))

# Now let's poke around with the nationality field here. I bet there are differences by national origin on these measures.
# We have a zillion countries included, so let's first find out which ones are most represented.

countrysum <- df_scored_preReversion %>%
  group_by(country) %>%
  summarize(records = n()) %>%
  top_n(10)
countrysum

# That'll work fine, let's stick with those 10, not including "NONE".
ltd_df <- df_scored_preReversion[df_scored_preReversion$country %in% countrysum$country,] %>%
  filter(country != "NONE")

ltd_df %>% ggplot()+
  facet_wrap(country~.)+
  theme_bw()+
  geom_density(aes(x=`extroversion-A`, fill = "Extroversion"), alpha=.2)+
  labs(title="Distributions of Indices", x="Response Index")+
  scale_fill_manual(name="Indices", values = c('Extroversion' = 'blue'))


# We can see some definite differences. Philippines has a super pointy extroversion dist, meaning a lot of middle responses and not much outer edges.
ltd_df %>% ggplot()+
  facet_wrap(country~.)+
  theme_bw()+
  geom_density(aes(x=`natural_reactions-A`, fill = "Natural Reactions"), alpha = .2)+
  labs(title="Distributions of Indices", x="Response Index")+
  scale_fill_manual(name="Indices", values = c('Extroversion' = 'blue'
                                               , 'Natural Reactions' = 'red'
                                               , 'Agreeability' = 'green'
                                               , 'Concienciousness' = 'gray'
                                               , 'Openess' = 'purple'))

ltd_df %>% ggplot()+
  facet_wrap(country~.)+
  theme_bw()+
  geom_density(aes(x=`agreeability-A`, fill = "Agreeability"), alpha = .2)+
  labs(title="Distributions of Indices", x="Response Index")+
  scale_fill_manual(name="Indices", values = c('Extroversion' = 'blue'
                                               , 'Natural Reactions' = 'red'
                                               , 'Agreeability' = 'green'
                                               , 'Concienciousness' = 'gray'
                                               , 'Openess' = 'purple'))

ltd_df %>% ggplot()+
  facet_wrap(country~.)+
  theme_bw()+
  geom_density(aes(x=`conscienciousness-A`, fill = "Concienciousness"), alpha = .2)+
  labs(title="Distributions of Indices", x="Response Index")+
  scale_fill_manual(name="Indices", values = c('Extroversion' = 'blue'
                                               , 'Natural Reactions' = 'red'
                                               , 'Agreeability' = 'green'
                                               , 'Concienciousness' = 'gray'
                                               , 'Openess' = 'purple'))

ltd_df %>% ggplot()+
  facet_wrap(country~.)+
  theme_bw()+
  geom_density(aes(x=`openess-A`, fill = "Ideas"), alpha = .2)+
  labs(title="Distributions of Indices", x="Response Index")+
  scale_fill_manual(name="Indices", values = c('Extroversion' = 'blue'
                                               , 'Natural Reactions' = 'red'
                                               , 'Agreeability' = 'green'
                                               , 'Concienciousness' = 'gray'
                                               , 'Openess' = 'purple'))



# Clean environment before training
rm(questiondf, cronbach)
rm(scoresfile, ltd_df, countrysum, df_scored_preReversion, keys.list, buckets)





# 000000000000000000000000000000000000000000000000000000000000000000000000000000
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# 000000000000000000000000000000000000000000000000000000000000000000000000000000





## =======================================================
## =======================================================
## DATA PREPARATION FOR MODELLING
## =======================================================
## =======================================================



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
# As reference for accuracy improvement, we estimate using montecarlo a random selection
# This function adjusts random score taking into account that answers of one of every 10 question is known. For simplicity, we assume a linear improvement of accuracy for 1/10 and correct by chance of randomly hitting (1/5) 
adjustScore <- function(x) {
  x + (4/5)*((realScores - x) / 10)
}
# generate a real result for our simulation
set.seed(1, sample.kind="Rounding")
realScores <- sample(c(0:100), 5, replace = TRUE)
realQuartiles <- 1+floor(realScores/25)
realHalves <- 1+floor(realScores/50)
# iterate simulated results obtained randomly
B <- 10000
estimAccuracy <- NULL
for (b in c(1:B)) {
  predictedScores <- sample(c(0:100), 5, replace = TRUE)
  predictedScores <- round(adjustScore(predictedScores),0)
  predictedQuarters <- 1+floor(predictedScores/25)
  predictedHalves <- 1+floor(predictedScores/50)
  hitsQuartile <- (realQuartiles == predictedQuarters)
  hitsHiLo <- (realHalves == predictedHalves)
  mostHitHiLo <- (sum(hitsHiLo) >= 3)
  estimAccuracy <- rbind(estimAccuracy, c(hitsQuartile, hitsHiLo, mostHitHiLo))
}
colnames(estimAccuracy) <- c(
  c("hitsQuartile_O", "hitsQuartile_C", "hitsQuartile_E", "hitsQuartile_A", "hitsQuartile_N"), 
  c("hitsHiLo_O", "hitsHiLo_C", "hitsHiLo_E", "hitsHiLo_A", "hitsHiLo_N"), 
  "MostHitHilo")
estimAccuracy <- colMeans(estimAccuracy)
# We just obtained accuracies for each category
estimAccuracy
# Create a dataframe to store results of the analysis
analysis_results <- data_frame(
    Trait="All", Score = estimAccuracy["MostHitHilo"], Accuracy_type = "3+ hits HighLow", Algorithm = "Montecarlo") %>% rbind(
  data_frame(Trait="O_score", Score=estimAccuracy["hitsQuartile_O"], Accuracy_type = "Hits quartile", Algorithm = "Montecarlo"), 
  data_frame(Trait="C_score", Score=estimAccuracy["hitsQuartile_C"], Accuracy_type = "Hits quartile", Algorithm = "Montecarlo"), 
  data_frame(Trait="E_score", Score=estimAccuracy["hitsQuartile_E"], Accuracy_type = "Hits quartile", Algorithm = "Montecarlo"), 
  data_frame(Trait="A_score", Score=estimAccuracy["hitsQuartile_A"], Accuracy_type = "Hits quartile", Algorithm = "Montecarlo"), 
  data_frame(Trait="N_score", Score=estimAccuracy["hitsQuartile_N"], Accuracy_type = "Hits quartile", Algorithm = "Montecarlo"), 
  data_frame(Trait="O_score", Score=estimAccuracy["hitsHiLo_O"], Accuracy_type = "Hits HighLow", Algorithm = "Montecarlo"), 
  data_frame(Trait="C_score", Score=estimAccuracy["hitsHiLo_C"], Accuracy_type = "Hits HighLow", Algorithm = "Montecarlo"), 
  data_frame(Trait="E_score", Score=estimAccuracy["hitsHiLo_E"], Accuracy_type = "Hits HighLow", Algorithm = "Montecarlo"), 
  data_frame(Trait="A_score", Score=estimAccuracy["hitsHiLo_A"], Accuracy_type = "Hits HighLow", Algorithm = "Montecarlo"), 
  data_frame(Trait="N_score", Score=estimAccuracy["hitsHiLo_N"], Accuracy_type = "Hits HighLow", Algorithm = "Montecarlo"), 
  data_frame(Trait="All", Score=mean(estimAccuracy[1:5]), Accuracy_type = "Hits quartile", Algorithm = "Montecarlo"), 
  data_frame(Trait="All", Score=mean(estimAccuracy[6:10]), Accuracy_type = "Hits HighLow", Algorithm = "Montecarlo")
)
# Show results (theoretical random estimate with a montecarlo approach) as base reference for next improvements during modelling
analysis_results %>% knitr::kable(digits = 4)


##########################################################
# Loop available recommendation algorithms
##########################################################

# STARTS FOR-LOOP OF ALGORITHMS TO TEST
# >>>>>>>>>>>>
# >>>>>>>>>>>>

methods_choice <- list(
  list("ALS", "ALS" = list(NULL)),
  list("POPULAR", "popular items" = list(NULL)),
  list("UBCF", "user-based CF" = list(nn=50)),
  list("IBCF", "item-based CF" = list(k=5000)),
  list("SVD", "SVD approximation" = list(k = 10))
)

# methods_choice <- list(
#   list("POPULAR", "popular items" = list(NULL)),
#   list("UBCF", "user-based CF" = list(nn=50)),
#   list("IBCF", "item-based CF" = list(k=5000)),
#   list("SVD", "SVD approximation" = list(k = 10))
# )


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


  ##########################################################
  # Questions selection
  ##########################################################
  # We want to request answers for a few, very uncorrelated, questions, but we also want answers (looking forward to future developments) will cover all questions. So, we randomly select a "seed question" and then calculate the best set of questions we want to get answer to improve model results.  
  
  # Generate all possible combinations of questions (10^5)
  potentialQuestionsSets <- as(expand.grid(keys.list.allPositive), "matrix")
  head(potentialQuestionsSets) 
  # generate first "seed" question
  set.seed(1, sample.kind="Rounding")
  randomSeedQuestion <- colnames(BF_test[,2:51])[sample(1:50, 1)]
  randomSeedQuestion
  # Filter only sets of questions containing the initial ramdom seed question
  potentialQuestionsSets <- 
    potentialQuestionsSets[which(rowAnys(potentialQuestionsSets == randomSeedQuestion)),]
  head(potentialQuestionsSets) 
  # Once we know potential combinations of questions to get answer from, we need to calculate which is best for predicting. Our premise will be to get the set with minimum correlation among its pairs. The reason is that low correlation will inform us better of the "difficult" questions where model will be weaker
  correlPerSet <- NULL
  # Loop each potential set of questions
  for (qs in 1:nrow(potentialQuestionsSets)) {
    # generate all pairs of questions (10) within given set
    couplesThisCombination <- combinations(5,2,v=potentialQuestionsSets[qs,])
    pairsCorrelationsThisSet <- NULL
    # now we loop those pairs to get a summary number based on their correlation
    for (c in 1:nrow(couplesThisCombination)) {
      question1 <- couplesThisCombination[c,1]
      question2 <- couplesThisCombination[c,2]
      pairsCorrelationsThisSet <- 
        c(pairsCorrelationsThisSet, abs(corAllQuestions[question1,question2]))
      }
    correlPerSet <- c(correlPerSet, mean(pairsCorrelationsThisSet))
    # ...we could use other criteria for detecting "difficult" combined questions, but always based in the correlations as a set... mean(pairsCorrelationsThisSet), min(pairsCorrelationsThisSet), max(pairsCorrelationsThisSet), sd(pairsCorrelationsThisSet), median(pairsCorrelationsThisSet)
    }
  # Based on the minimum correlation among its questions, we select a set
  potentialQuestionsSets[which.min(correlPerSet),]
  # convert to column index for continuing modelling proccess
  chosen_questions <- 
    which(colnames(BF_test[,2:51]) %in% potentialQuestionsSets[which.min(correlPerSet),])
  chosen_questions


  # ==============================
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
  # Create data frame
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















