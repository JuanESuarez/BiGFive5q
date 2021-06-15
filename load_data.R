################################################################################
# BigFive.5q Project
# A model to predict 45 questions based in only 5 answered
# Author: Juan Eloy Suarez
################################################################################
#
## =============================================================================
## =============================================================================
## Initialize environment
## =============================================================================
## =============================================================================
#
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(lubridate)

## =============================================================================
## =============================================================================
## Load, clean and convert dataset
## =============================================================================
## =============================================================================
#
# Data are located in Kaggle site (https://www.kaggle.com/tunguz/big-five-personality-test/download)
# Data size in .CSV is 396 MB, so we will, as a previous step, load, select and convert them to .RDS, so a very significant size reduction is obtained to 21 MB full dataset.
# We store in directory /data_source this .RDS file available for our code to ru load them We copied kaggle dataset to ./data_source directory: 
# ===============================================
# Read Kaggle dataset copied to local directory
#
# Answers to test data
# tab delimited, and  our int columns are character, perhaps because of the nulls!
df <- read.csv("./data_source/data-final.csv", sep="\t", stringsAsFactors = FALSE, na.strings=c("NA","NaN", " ", "NULL"))
# We select relevant columns to use
df <- df %>% select(c(1:50), dateload, country) %>% rownames_to_column('userId') %>%
  mutate(Month = month(dateload), Year = year(dateload)) %>%
  select(-dateload)
# Remove rows containing any NA
df <- na.omit(df)
# Remove any value not between 1 and 5 in the answers columns
df <- df %>% filter_at(vars(2:51), all_vars((.) %in% c(1:5)))
#
# Let's also load up the questions from the data dictionary
dictionary <-
  read_table("./data_source/codebook.txt", skip=5) %>%
  separate(1, sep="\t", extra="merge", into=c("ID", "Question")) %>%
  data.frame() %>% top_n(50)

##########################################################
# Save .RDS input data to local files
##########################################################
saveRDS(df, "./data_source/BFtests.rds")
saveRDS(dictionary, "./data_source/dictionary.rds")

