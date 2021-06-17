# Shiny web application
#

library(shiny)
library(recommenderlab)
library(psych)


# Data load and initialization -----------------------------
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

# ----------------------------------------------------------


# Define UI for application that draws a histogram
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
                  
                  selectInput("select_algo", label = p("Select algorithm"),
                              choices = list("POPULAR", "UBCF", "SVD", "RANDOM"),
                              selected = "UBCF")

                ),
                
                mainPanel(tableOutput("user_results"), 
                          tableOutput("question_recom"),
                          p("Created by ",
                            a("databellum", href="http://databellum.ai")
                          )
                )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## pick random questions and display
  questions_to_rate <- reactive({
    # ignore <- input$new_questions  ### listen to button
    
    rand_questions <- c(21,11,31,41,1) + sample(c(0:9), 5)
    rand_questions <- c(1,11,21,31,41)  #JES

    output[[paste0("question", 1)]] <- renderText(questionsList[rand_questions[1]])
    output[[paste0("question", 2)]] <- renderText(questionsList[rand_questions[2]])
    output[[paste0("question", 3)]] <- renderText(questionsList[rand_questions[3]])
    output[[paste0("question", 4)]] <- renderText(questionsList[rand_questions[4]])
    output[[paste0("question", 5)]] <- renderText(questionsList[rand_questions[5]])
    
    rand_questions
  })
  
  ### create and change recommender
  recom <- reactive({
    Recommender(BFdata, method = input$select_algo)
  })
  
  ### make recommendations
  output$question_recom <- renderTable({
    
    ### read ratings
    ratings <- matrix(NA, nrow = 1, ncol = ncol(BFdata))
    for(i in 1:5)
      ratings[1, questions_to_rate()[i]] <- input[[paste0("slider", i)]]
    
    ### create recommendations
    pred <- predict(recom(), as(ratings, "realRatingMatrix"), n = 45)
    
    vector_hiddenQuestions <- paste(names(questionsList[getList(pred)[[1]]]), questionsList[getList(pred)[[1]]])  #JES
    
    cbind('Hidden question' = vector_hiddenQuestions[order(vector_hiddenQuestions)],
          'Predicted Rating' = sprintf("%1.1f", getRatings(pred)[[1]]))  #JES
  })
  
  ### show usersresults
  output$user_results <- renderTable({
    
    ### read ratings
    traits <- c("Openess", "Conscientiousness", "Extroversion", "Agreeableness", "Natural Reactions")
    ### descriptive texts for each trait
    traits_texts <- c(
      "Also often called Openness to Experience. People who score low tend to be traditional and conventional",
      "Individuals who score high on this factor are careful and diligent. Low scorers are impulsive and disorganized",
      "Surgency or Positive Emotionality. Individuals who score high on this one are outgoing and social. Individuals who score low tend to be shut ins",
      "High in this factor are friendly and optimistic. Low scorers are critical and aggressive",
      "Emotional Stability. Often referred as Neuroticism or Negative Emotionality (in these two cases interpretations are inverted, can be though of as the opposite of Emotional Stability")
    
    ## Obtain predictions for each question so we can calculate 
    ### read ratings
    ratings <- matrix(NA, nrow = 1, ncol = ncol(BFdata))
    for(i in 1:5)
      ratings[1, questions_to_rate()[i]] <- input[[paste0("slider", i)]]
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
    # With all data + prediction scored together (n+1 rows matrix), calculate percentiles of the last row (results for the user) assuming normality and based in mean and stf deviation of the whole initial dataset
    traitsMeans <- c(2.9595, 3.0666, 3.7736, 3.3661, 3.8989)
    traitsSds <- c(0.91036, 0.85857, 0.72926, 0.73734, 0.63024)
    names(traitsMeans) <- c("extroversion_EXT", "natural_reactions_EST", "agreeability_AGR", "conscienciousness_CSN", "openess_OPN")
    names(traitsSds) <- c("extroversion_EXT", "natural_reactions_EST", "agreeability_AGR", "conscienciousness_CSN", "openess_OPN")
    
    # result_O <- round(pnorm(scoresfile[nrow(BFdata)+1,]["openess-A"], traitsMeans["openess_OPN"], traitsSds["openess_OPN"])*100,0)
    # result_C <- round(pnorm(scoresfile[nrow(BFdata)+1,]["conscienciousness-A"], traitsMeans["conscienciousness_CSN"], traitsSds["conscienciousness_CSN"])*100,0)
    # result_E <- round(pnorm(scoresfile[nrow(BFdata)+1,]["extroversion-A"], traitsMeans["extroversion_EXT"], traitsSds["extroversion_EXT"])*100,0)
    # result_A <- round(pnorm(scoresfile[nrow(BFdata)+1,]["agreeability-A"], traitsMeans["agreeability_AGR"], traitsSds["agreeability_AGR"])*100,0)
    # result_N <- round(pnorm(scoresfile[nrow(BFdata)+1,]["natural_reactions-A"], traitsMeans["natural_reactions_EST"], traitsSds["natural_reactions_EST"])*100,0)
    
    result_O <- paste(scoresfile[nrow(BFdata)+1,]["openess-A"],round(pnorm(scoresfile[nrow(BFdata)+1,]["openess-A"], traitsMeans["openess_OPN"], traitsSds["openess_OPN"])*100,0), sep=" // ")
    result_C <- paste(scoresfile[nrow(BFdata)+1,]["conscienciousness-A"],round(pnorm(scoresfile[nrow(BFdata)+1,]["conscienciousness-A"], traitsMeans["conscienciousness_CSN"], traitsSds["conscienciousness_CSN"])*100,0), sep=" // ")
    result_E <- paste(scoresfile[nrow(BFdata)+1,]["extroversion-A"],round(pnorm(scoresfile[nrow(BFdata)+1,]["extroversion-A"], traitsMeans["extroversion_EXT"], traitsSds["extroversion_EXT"])*100,0), sep=" // ")
    result_A <- paste(scoresfile[nrow(BFdata)+1,]["agreeability-A"],round(pnorm(scoresfile[nrow(BFdata)+1,]["agreeability-A"], traitsMeans["agreeability_AGR"], traitsSds["agreeability_AGR"])*100,0), sep=" // ")
    result_N <- paste(scoresfile[nrow(BFdata)+1,]["natural_reactions-A"],round(pnorm(scoresfile[nrow(BFdata)+1,]["natural_reactions-A"], traitsMeans["natural_reactions_EST"], traitsSds["natural_reactions_EST"])*100,0), sep=" // ")
    
    
    
    traits_percentiles <- c(result_O, result_C, result_E, result_A, result_N)
    cbind('Personality trait' = traits, 
          'Result' = traits_percentiles,
          'Interpretation' = traits_texts)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

