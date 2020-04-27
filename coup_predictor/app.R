
# Loading required packages.

library(shiny)
library(tidyverse)
library(shinythemes)
library(tidymodels)
library(rstanarm)
library(Rcpp)
library(gt)
library(scales)

# Loading in completed dataset.

load("coup_data_final.rds")

# Converting the 'level_infl' column to a numeric column.

coup_country$level_infl <- as.numeric(coup_country$level_infl)

# Converting the 'gov' column to a numeric column.

coup_country$gov <- as.numeric(coup_country$gov)

# Creating logistic model.

logistic_mod <- logistic_reg() %>% 
    set_engine("glm")

# Fitting the logistic model with the loaded data.

logistic_fit <- fit(logistic_mod,
                    realized_coup ~ mort_rate + birth_rate + death_rate + 
                    level_infl + gov,
                    data = coup_country)


# Start of Shiny App user interface.

ui <- fluidPage(
    
    # App title
    
    navbarPage("Urgent Fury: Will your coup succeed?",
    
    theme = shinytheme("yeti"),
    
    # My first tab is an instructions tab so users know how to use the model.
    
    tabPanel("Instructions",
             mainPanel(
               h3("How to use this model"),
               br(),
               h4("You've been tasked with planning a coup d'etat, and you want to 
                  determine the likelihood that this coup will succeed"),
               br(),
               h4("Step 1: Set level of economic underdevelopment"),
               br(),
               h5("The level of economic development is determined by infant mortality,
                  birth, and death rates. Higher infant mortality, birth, and 
                  death rates is associated with an overall lack of economic 
                  development"),
               br(),
               h4("Step 2: Set level of political independence"),
               br(),
               h5("The level of political independence is determined by the amount of
               foreign influence. A higher number of US military bases or outposts
              corresponds to more foreign influence, and thus a lower level of
              political independence."),
               br(),
               h4("Step 3: Determine level of government centralization"),
               br(),
               h5("The level of government centralization is determined by regime type. 
                  Absolute monarchies, Communist states, and one-party states are
                  highly centralized. Parliamentary republics, semi-presidential states,
                  and constitutional monarchies are somewhat centralized. Presidential 
                  republics and federal states are not very centralized."),
               br(),
               h6("Note: Authoritarian states known as 'presidential republics' 
                  are still classified as 'not very centralized' given how some
                  deliberately spread power across the government to prevent coup
                  attempts and how there are usually other factors that prevent 
                  the complete centralization of power such as the presence of 
                  influential state-owned enterprises and powerful paramilitary
                  groups."))),
    
    # This tab contains the actual model, where users select the value of 
    # various indicators, and then see the probability of a coup succeeding in
    # a country with those values.
    
    tabPanel("Coup Success Predictor",
             h5("Create your own country with these indicators:"),
             sidebarLayout(
                 sidebarPanel(
                     sliderInput("infant", "Set infant mortality rate per 1000 births",
                                 max = 35, min = 0, value = 10),
                     sliderInput("births", "Set number of births per 1000 people",
                                 max = 60, min = 9, value = 20),
                     sliderInput("deaths", "Set number of deaths per 1000 people",
                                 max = 40, min = 2, value = 15),
                     numericInput("bases", "Select level of political independence",
                                  max = 4, min = 1, value = 2),
                     h6("1 = Not very politically independent; under heavy foreign influence"), 
                     h6("2 = Moderately independent; under a moderate amount of foreign influence"),
                     h6("3 = Somewhat independent; under some foreign influence"),
                     h6("4 = Politically independent; foreign influence negligible"),
                     numericInput("gov", "How centralized is the government?",
                                  max = 3, min = 1, value = 2),
                     h6("1 = Not very centralized; e.g. federal presidential republicss"),
                     h6("2 = Somewhat centralized; e.g. parliamentray republics"),
                     h6("3 = Very centralized; e.g. absolute monarchies"),
                     actionButton("run_model_log", "Run Model")),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Predicted coup outcomes", tableOutput("logis")))))),
    
    # This tab contains information about my data cleaning process and a list
    # of my data sources.
    
    tabPanel("Data",
             mainPanel(
                 h4("Methods"),
                 p("For this project, I had to essentially quantify Luttwak's
                   criteria for what makes a successful coup. I found measures
                   of 'economic backwardess', such as high infant mortality, 
                   birth and overall death rates from the World Bank"),
                 p("Because such indicators vary by year, I decided to only look
                   at observations from the year 1991. I chose that year as that
                   was when more and more countries started to collect this data,
                   Futher, I cleaned this data by pivoting the datasets to a 
                   longer format and removing unnessary rows, such as data for 
                   regions rather than countries"),
                 p("To measure the amount of 'foreign influence' in a country,
                   I used a dataset of US military bases abroad in 1989. Since I
                   need my data to be somewhat consistent, I decided to use data
                   from 1989, as I couldn't specifically find data from 1991. I 
                   then assigned a number from 1 to 4 to each country with a 
                   US military presence. A country with no US military presence 
                   was given a 4. If a country had 1 to 10 US bases or outposts, 
                   then it was assigned a 3. If a country had 10 to 50 US bases 
                   or outposts, then it was assigned a 2. If a country had more 
                   than 50 US bases or outposts, then it was assigned a 1. 
                   Thus, a 1 represents the most amount of 'foreign influence' 
                   while a 4 represents the least. I wanted to include data on
                   Chinese, French, and Russian military presence abroad, but I
                   could not find a comprehensive dataset."),
                 p("To measure how 'centralized' a country's government is, I
                   scraped a table from the CIA World Factbook that lists each
                   country's system of government. I then assigned each country
                   a number from 1 to 3 where 1 is the most decentralized and 3 is
                   the most centralized. Examples of regimes that would be 
                   classified as a 3 would be absolute monarchies or Communist 
                   states. Countries with parliamentry systems are classified as 
                   a 2, as in a parliamentary system, the powers of the 
                   executive and legislative branches are fused, thus making
                   the government more centralized. Finally, countries with
                   presidential systems and/or federal countries are were given a 
                   1, as in a presidential system there is usually a separation
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
                p("I obtained the US military base data from", 
                  a("here", href = "https://doi.org/10.17606/vfyb-nc07")),
                p("I obtained the regime type data from the", 
                  a("CIA World Factbook",
                 href = "https://www.cia.gov/library/publications/the-world-factbook/fields/299.html")),
                p("Here is a link to my", a("Github repo", 
                    href = "https://github.com/cthulhudreaming/coup_predictor/")))),
    
    # This tab contains background information about my project and information
    # about me.
    
    tabPanel("About",
             mainPanel(
                 h4("Background"),
                 p("The goal of this project was to determine what factors 
                 determine the success of a coup d'etat. In addition, I wanted 
                 to see if autocratic countries were more vulnerable to 
                 coups d'etat than democratic countries, or vice-versa."),
                 p("I got the idea for the first part of this project while 
                 reading Edward N. Luttwak's infamous book 
                 'Coup d'Etat: A Practical Handbook', which inspired various 
                 countries to 'coup proof' their governments after it was first 
                 released in 1968. In the second chapter, Luttwak argues that 
                 there are three criteria that determine the success of a coup :
                 econmic backwardness, a lack of foreign influence, and a 
                 highly centralized government"),
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
                 br(),
                 h4("About Me"),
                 p("My name is Nidal and I a member of Harvard College's class 
                   of 2023. This is my final project for GOV1005: Data. I hope
                   to concentrate in History with a Secondary in Government. You
                 can contact me at nini00225 [at] protonmail.com")))

# End of Navbar and FluidPage.

)) 

server <- function(input, output, session) {
    set.seed(1234)
    
    # Creating an observer to react when the user pushes the "run model" 
    # button.
    
    observe({
    
        # Matching user input values to their corresponding labels as used 
        # in the model.
        
        mort_rate <- as.numeric(input$infant)
        birth_rate <- as.numeric(input$births)
        death_rate <- as.numeric(input$deaths)
        level_infl <- as.integer(input$bases)
        gov <- as.integer(input$gov)
        
        # Binding those input values into a data frame.
        
        temp <- cbind(mort_rate, birth_rate, death_rate, level_infl, gov)
        temp <- as_data_frame(temp)
        
        # Here I am returning a reactive expression in response to a suer
        # clicking the 'run model' button.
        
        observeEvent(input$run_model_log, {
            
            # Using the predict function to output the probability of a
            # successful/failed coup using the newly created dataframe.
            
            pred <- predict(logistic_fit, new_data = temp, type = "prob")
            
            # Reformatting the values of the probabilites to percentages.
            
            pred$.pred_0 <- percent(pred$.pred_0, suffix = "%")
            pred$.pred_1 <- percent(pred$.pred_1, suffix = "%")
            
            # Renaming the column names to more descriptive ones.
            
            pred_table <- pred %>% 
                rename("Chance of failure" = .pred_0,
                       "Chance of success" = .pred_1)
            
            # Rendering the above table as a reactive output, meaning it will
            # change depending on the input values.
            
            output$logis <- renderTable({pred_table})
            
            # End of observeEvent.
            
            }) 
        
            
    # End of observe.  
        
    }) 

# End of server.
    
} 


shinyApp(ui = ui, server = server)
