# Shiny web application
#
if(!require(radarchart)) install.packages("radarchart", repos = "http://cran.us.r-project.org")

library(shiny)
library(recommenderlab)
library(psych)
library(radarchart)

# =============================================
# Data load and initialization
BFdata <- readRDS("BFdata.rds")
dictionary <- readRDS("dictionary.rds")

# Make sure requirements are met
req(BFdata)
req(dictionary)

# Let's prepare our list of questions and name items
questionsList <- dictionary$Question
names(questionsList) <- dictionary$ID

# We use all positive questions, since data received already reverted 'negative' questions
keys.list <- list(openess = c("OPN1","OPN2","OPN3","OPN4","OPN5","OPN6","OPN7","OPN8","OPN9","OPN10"), 
                  conscienciousness = c("CSN1","CSN2","CSN3","CSN4","CSN5","CSN6","CSN7","CSN8","CSN9","CSN10"), 
                  extroversion = c("EXT1","EXT2","EXT3","EXT4","EXT5", "EXT6", "EXT7", "EXT8", "EXT9", "EXT10"), 
                  agreeability = c("AGR1", "AGR2", "AGR3", "AGR4", "AGR5", "AGR6", "AGR7", "AGR8", "AGR9", "AGR10"), 
                  natural_reactions = c("EST1","EST2","EST3","EST4","EST5", "EST6", "EST7", "EST8", "EST9", "EST10"))

# some entered questions could be reverted. We identify which ones
revertedQuestions <- c(2,4,6,8,10,12,14,21,23,25,27,32,34,36,38,47,44,46)  
rand_questions <- c(21,11,31,41,1) + sample(c(0:9), 5)
rand_reverted_questions <- rand_questions %in% revertedQuestions

# Our dataset descriptive parameters per trait, which will be useful for percentile calculation assuming approx normality
traitsMeans <- c(2.9595, 3.0666, 3.7736, 3.3661, 3.8989)
traitsSds <- c(0.91036, 0.85857, 0.72926, 0.73734, 0.63024)
names(traitsMeans) <- c("extroversion", "natural_reactions", "agreeability", "conscienciousness", "openess")
names(traitsSds) <- c("extroversion", "natural_reactions", "agreeability", "conscienciousness", "openess")

### read ratings
traits <- c("Openess", "Conscientiousness", "Extroversion", "Agreeableness", "Natural Reactions")
### descriptive texts for each trait
traits_texts <- c(
  "Also often called Openness to Experience. People who score low tend to be traditional and conventional",
  "Individuals who score high on this factor are careful and diligent. Low scorers are impulsive and disorganized",
  "Surgency or Positive Emotionality. Individuals who score high on this one are outgoing and social. Individuals who score low tend to be shut ins",
  "High in this factor are friendly and optimistic. Low scorers are critical and aggressive",
  "Emotional Stability. Often referred as Neuroticism or Negative Emotionality (in these two cases interpretations are inverted, can be though of as the opposite of Emotional Stability")






# =============================================
# =============================================
# Define UI for application
# =============================================
# =============================================
ui <- fluidPage(
  
  titlePanel("BigFive.5q Personality Traits Test"),
  
  h4("This test is based in the prestigiuos Big Five Personality Traits method, but using artificial intelligence to predict answers based on only five questions actually scored."),
  
  sidebarLayout(position = "left",
                sidebarPanel(
                  h3("Please rate:"),
                  
                  ### create the sliders
                  lapply(1:5, function(i) sliderInput(paste0("slider", i),
                                                      label = textOutput(paste0("question", i)),
                                                      min = 1, max = 5, value = 3)),
                  # we fix algorithm to best performer vs accurate according to our modelling
                  # selectInput("select_algo", label = p("Select algorithm"),
                  #             choices = list("POPULAR", "UBCF", "SVD", "RANDOM"),
                  #             selected = "UBCF")
                  
                ),
                
                mainPanel(h3("Your score"),
                          chartJSRadarOutput("radar_results", width = "450", height = "300"), width = 7,
                          tableOutput("explain_results"),                           
                          h4("Detail of predictions per question - for academic purpose:"),
                          tableOutput("question_recom"),
                          p("Created by JuanESuarez || ",
                            a("databellum", href="http://databellum.es")
                          )
                )
  )
)



# =============================================
# =============================================
# Define server logic
# =============================================
# =============================================
server <- function(input, output) {
  
  ## pick random questions and display
  questions_to_rate <- reactive({
    # ignore <- input$new_questions  ### listen to button
    output[[paste0("question", 1)]] <- renderText(questionsList[rand_questions[1]])
    output[[paste0("question", 2)]] <- renderText(questionsList[rand_questions[2]])
    output[[paste0("question", 3)]] <- renderText(questionsList[rand_questions[3]])
    output[[paste0("question", 4)]] <- renderText(questionsList[rand_questions[4]])
    output[[paste0("question", 5)]] <- renderText(questionsList[rand_questions[5]])
    
    rand_questions
  })
  
  
  
  ### create and select recommender
  recom <- reactive({
    # we fix algorithm to best performer vs accurate according to our modelling  
    # Recommender(BFdata, method = input$select_algo)
    Recommender(BFdata, method = "UBCF")
  })
  
  
  
  ### make recommendations
  output$question_recom <- renderTable({
    ### read ratings
    ratings <- matrix(NA, nrow = 1, ncol = ncol(BFdata))
    for(i in 1:5) {
      ratings[1, questions_to_rate()[i]] <- input[[paste0("slider", i)]]
    }
    
    ### create recommendations
    pred <- predict(recom(), as(ratings, "realRatingMatrix"), n = 45)
    
    ### for academic purposes, we show predicted answers
    vector_hiddenQuestions <- questionsList[getList(pred)[[1]]]
    vector_hiddenAnswers <- sprintf("%1.1f", getRatings(pred)[[1]])
    cbind('Hidden question' = vector_hiddenQuestions,
          'Predicted Rating' = vector_hiddenAnswers)
  })
  
  
  
  ### show users results
  output$explain_results <- renderTable({
    ## Show meaning of each personality trait score. Constant data
    cbind('Personality trait' = paste(substr(traits,1,1), "=", traits), 
          'Interpretation' = traits_texts)
    
  })    
  
  output$radar_results <- renderChartJSRadar({
    ## Obtain predictions for each question so we can calculate 
    ### read ratings
    ratings <- matrix(NA, nrow = 1, ncol = ncol(BFdata))
    for(i in 1:5) {
      ratings[1, questions_to_rate()[i]] <- input[[paste0("slider", i)]]
      # if question entered is formulated as reverted, we transform it
      if (rand_reverted_questions[i]) {
        ratings[1, questions_to_rate()[i]] <- abs(6 - ratings[1, questions_to_rate()[i]])
      }
    }
    ### create recommendations
    pred <- predict(recom(), as(ratings, "realRatingMatrix"), n = 45)
    # Collect entered and predicted ratings to prepare results calculation
    enteredQuestions <- dictionary[questions_to_rate()[1:5],1]
    enteredRatings <- c(input[[paste0("slider", 1)]],
                        input[[paste0("slider", 2)]],
                        input[[paste0("slider", 3)]],
                        input[[paste0("slider", 4)]],
                        input[[paste0("slider", 5)]])
    predQuestions <- names(questionsList[getList(pred)[[1]]])
    predRatings <- getRatings(pred)[[1]]
    # Union of entered and predicted to have all needed answers ready
    allQuestions <- c(enteredQuestions, predQuestions)
    allRatings <- c(enteredRatings, predRatings)
    names(allRatings) <- allQuestions
    # Reorder obtained ratings to prepare rbind
    allRatings <- allRatings[colnames(BFdata)]
    #
    # We use psych::scoreItems to calculate scores (i.e. mean per group taking sign into account)
    # Add last row that contains predicted+entered answers and calculate results (OCEAN) for all lines
    scoresfile <- scoreFast(keys.list, rbind(as(BFdata, "matrix"), allRatings))
    # With all data + prediction scored together (n+1 rows matrix), calculate percentiles of the last row (results for the user) assuming normality and based in mean and std deviation of the whole initial dataset
    # we assume answers are approx normal within each trait so we use pnorm based on distribution of all population per trait
    result_O <- round(pnorm(scoresfile[nrow(BFdata)+1,]["openess-A"], traitsMeans["openess"], traitsSds["openess"])*100,0)
    result_C <- round(pnorm(scoresfile[nrow(BFdata)+1,]["conscienciousness-A"], traitsMeans["conscienciousness"], traitsSds["conscienciousness"])*100,0)
    result_E <- round(pnorm(scoresfile[nrow(BFdata)+1,]["extroversion-A"], traitsMeans["extroversion"], traitsSds["extroversion"])*100,0)
    result_A <- round(pnorm(scoresfile[nrow(BFdata)+1,]["agreeability-A"], traitsMeans["agreeability"], traitsSds["agreeability"])*100,0)
    result_N <- round(pnorm(scoresfile[nrow(BFdata)+1,]["natural_reactions-A"], traitsMeans["natural_reactions"], traitsSds["natural_reactions"])*100,0)
    
    traits_percentiles <- c(result_O, result_C, result_E, result_A, result_N)
    
    userValues <- data.frame("Trait"=substr(traits,1,1), 
                             "Your Score" = traits_percentiles)
    chartJSRadar(userValues[, c("Trait", "Your.Score")], 
                 maxScale = 100, scaleStepWidth = 25, 
                 labelSize = 30, 
                 responsive = TRUE, 
                 showLegend = FALSE, 
                 polyAlpha = 0.5, 
                 showToolTipLabel=TRUE)
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
