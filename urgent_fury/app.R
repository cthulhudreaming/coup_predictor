

library(shiny)
library(tidyverse)
library(shinythemes)
library(tidymodels)
library(rstanarm)
library(Rcpp)


load("coup_data_final.rds")

coup_country$level_infl <- as.numeric(coup_country$level_infl)

coup_country$gov <- as.numeric(coup_country$gov)

logistic_mod <- logistic_reg() %>% 
    set_engine("glm")

logistic_fit <- fit(logistic_mod,
                    realized_coup ~ mort_rate + birth_rate + death_rate + 
                        level_infl + gov,
                    data = coup_country)

forest_mod <- rand_forest(trees = 50) %>% 
    set_engine("randomForest") %>% 
    set_mode("classification")

forest_fit <- fit(forest_mod,
                  realized_coup ~ mort_rate + birth_rate + death_rate + 
                      level_infl + gov,
                  data = coup_country)


ui <- fluidPage(navbarPage("Urgent Fury",
                
    theme = shinytheme("yeti"),
    
    tabPanel("Model",
             h5("Create your own country with these indicators:"),
             sidebarLayout(
                 sidebarPanel(
                     sliderInput("infant", "Set infant mortality rate per 1000 births",
                                 max = 35, min = 0, value = 10),
                     sliderInput("births", "Set number of births per 1000 people",
                                 max = 60, min = 9, value = 20),
                     sliderInput("deaths", "Set number of deaths per 1000 people",
                                 max = 40, min = 2, value = 15),
                     numericInput("bases", "Select amount of foreign influence",
                                  max = 4, min = 1, value = 2),
                     h6("1 = a large amount of foreign influence"), 
                     h6("2 = a moderate amount of foreign influence"),
                     h6("3 = some foreign influence"),
                     h6("4 = very little foreign influence"),
                     numericInput("gov", "How centralized is the government?",
                                  max = 3, min = 1, value = 2),
                     h6("1 = Very centralized; e.g. absolute monarchies"),
                     h6("2 = Somewhat centralized; e.g. parliamentray republics"),
                     h6("3 = Not very centralized; e.g. federal presidential republics"),
                     actionButton("run_model_log", "Run Logistic Model"),
                     actionButton("run_model_forest", "Run RandomForest model")),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Logistic", verbatimTextOutput("logis")),
                         tabPanel("Random Forest", verbatimTextOutput("forest")))))),
    tabPanel("About",
             mainPanel(
                 h4("Background"),
                 p("The goal of this project was to determine what factors 
                 determine the success of a coup d'etat. In addition, I wanted 
                 to see if autocratic countries were more vulnerable to 
                 coups d'etat than democratic countries, or vice-versa."),
                 p("I got the idea for the first part of this project while 
                 reading Edward N. Luttwak's infamous book 
                 'Coup d'Etat: a practical handbook, which inspired various 
                 countries to 'coup proof' their governments after it was first 
                 released in 1968. In the second chapter, Luttwak argues that 
                 there are three criteria that determine the success of a coup 
                   in a country: econmic backwardness, a lack of foreign 
                   influence, and a highly centralized government"),
                 p("According to Luttwak, in order for a coup to succeed, a 
                 country must not be economically developed. In 
                 'economically backwards' countries, political participation is 
                 reserved for the elite, and the people are ignorant of politics.
                 He posited a few measures of 'economic backwardness' such as 
                 high birth and death rates."),
                 p("In addition, Luttwak stated that a country must be free of 
                 foreign influence in order for a coup to be successful. 
                 He argued that if a foreign power has a large military or 
                 political presence in the country, one must obtain the foreign 
                 power's permission before attempting a coup, or else it would fail."),
                 p("Lastly, Luttwak wrote that a country needs to have a highly \
                 centralized government in order to a coup to succeed, as there 
                 would only be one center of power to capture."),
                 p("I got the idea for the second part of this project from 
                 my fall freshman seminar, 'The Political Significance of 
                 Espionage and Subversion. A central question of the class was
                 whether or not 'autocracies' or 'democracies' are more 
                 vulnerable to political subversion. Since coups are arguably 
                 the ultimate form of political subversion, I wanted to see 
                 if coups were more common in 'autocracies' or 'democracies'."),
                 h4("About Me"),
                 p("My name is Nidal and I a member of Harvard College's class 
                   of 2023. This is my final project for GOV1005: Data. I hope
                   to concentrate in History with a Secondary in Government"))),
    tabPanel("The Data",
             mainPanel(
                 h4("Methods"),
                 p("For this project, I had to essentially quantify Luttwak's
                   criteria for what makes a successful coup. I found measures
                   of 'economic backwardess', such as high infant mortality, 
                   birth and overall death rates from the World Bank"),
                 p("Because such indicators vary by year, I decided to only look
                   at observations from the year 1991. The choice of year is
                   arbitrary. Futher, I cleaned this data by pivoting the 
                   datasets to a longer format and removing unnessary rows, such
                   as data for regions rather than countries"),
                 p("To measure the amount of 'foreign influence' in a country,
                   I used a dataset of US military bases abroad in 1989. Since I
                   need my data to be somewhat consistent, I decided to use data
                   from 1989, as I couldn't specifically find data from 1991. I 
                   then assigned a number from 1 to 4 to each country with a 
                   US military presence. If a country had 1 to 10 US bases or 
                   outposts, then it was assigned a 3. If a country had 10 to 50
                   US bases of outposts, then it was assigned a 2. If a country
                   had more than 50 US bases or outposts, then it was assigned 
                   a 1. A country with no US military presence was given a 4. 
                   Thus, a 1 represents the most amount of 'foreign influence' 
                   while a 4 represents the least. I wanted to include data on
                   Chinese, French, and Russian military presence abroad, but I
                   could not find a comprehensive dataset."),
                 p("To measure how 'centralized' a country's government is, I
                   scraped a table from the CIA World Factbook that lists each
                   country's system of government. I then assigned each country
                   a number from 1 to 3 where 1 is the most centralized and 3 is
                   the most decentralized. Examples of regimes that would be 
                   classified as a 1 would be absolute monarchies or Communist 
                   states. Countries with parliamentry systems are classified as 
                   a 2, as in a parliamentary system, the powers of the 
                   executive and legislative branches are fused, thus making
                   the government more centralized. Finally, countries with
                   presidential systems or federal countries are were given a 
                   3, as in a presidential system there is usually a separation
                   of powers, and in a federal state, the power of the central
                   government is typically weak. Although some authoritarian 
                   states, such as Syria, have 'presidential' systems, I 
                   chose to classify these states as 'less centralized'. Syria, 
                   in particular, implemented 'coup proofing' techniques that 
                   distributed power across the government, therefore reducing
                   its central power, and in the authoritarian states listed
                   there were other indicators of decentralization, such as
                   sectarianism or ethnic tensions."),
                 h4("Sources"),
                 p("I obtained the infant mortality data from the", a("World Bank",
                 href = "https://data.worldbank.org/indicator/SP.DYN.IMRT.IN)")),
                 p("I obtained the birth rate data (per 1000 people) from the", 
                   a("World Bank", href = "https://data.worldbank.org/indicator/SP.DYN.CBRT.IN")),
                p("I obtained the death rate (per 1000 peole ) from the", a("World Bank",
                href = "https://data.worldbank.org/indicator/SP.DYN.CBRT.IN")),
                p("I obtained the US military base data from David Vine, 
                'Lists of U.S. Military Bases Abroad, 1776-2019,
                  'American University Digital Research Archive, 2019, 
                  https://doi.org/10.17606/vfyb-nc07"),
                p("I obtained the regime type data from the", a("CIA World Factbook",
                 href = "https://www.cia.gov/library/publications/the-world-factbook/fields/299.html"))
             )
             )
    
)


    
)

    
server <- function(input, output, session) {
    set.seed(1234)
    
    observe({
        
        mort_rate <- as.numeric(input$infant)
        birth_rate <- as.numeric(input$births)
        death_rate <- as.numeric(input$deaths)
        level_infl <- as.integer(input$bases)
        gov <- as.integer(input$gov)
        
        temp <- cbind(mort_rate, birth_rate, death_rate, level_infl, gov)
        temp <- as_data_frame(temp)
        
        temp$realized_coup <- ""
        
        observeEvent(input$run_model_log, {
            
            log_model <- logistic_fit
            pred <- predict(log_model, new_data = temp)
            output$logis <- renderPrint(pred)
        }
                     
            
        )
        
        observeEvent(input$run_model_forest, {
            
            forest_model <- forest_fit
            pred_forest <- predict(forest_model, new_data = temp)
            output$forest <- renderPrint(pred_forest)
        }
            
        )
    }
        
    )

    
}


shinyApp(ui = ui, server = server)
