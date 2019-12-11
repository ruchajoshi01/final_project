#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(tidyverse)
library(tidytext)
library(googlesheets4)
library(RCurl)
library(SentimentAnalysis)
library(janitor)
library(reshape2)
library(psych)
library(ggcorrplot)
library(scales)
library(plotly)
library(shinythemes)
library(htmltools)
library(vembedr)

# Inputs: Cleaned data and word correlations data
clean_data.df <- read_rds("clean_data.rds")
fbc.df <- read_rds("frequency_by_character.rds")
imdb_data <- read_rds("imdb_data.rds")

# Define UI for application
ui <- fluidPage(
    theme = shinytheme("flatly"),
    
    # Navigation bar
    navbarPage(
        # Navigation bar title
        "The Office",
        
        # Page with information about amount of character appearance in the show per season
        tabPanel("Per Season",
                 
                 # Allow user to select information on the side
                 sidebarLayout(
                     sidebarPanel(
                         h4(
                             "Select a season and characteristic to see how prevalent a character was!"
                         ),
                         
                         # Select season of show
                         sliderInput(
                             "season",
                             "Season:",
                             min = 1,
                             max = 9,
                             value = 1
                         ),
                         
                         # Select lines, scenes, words of each character per season
                         selectInput(
                             "char",
                             "Characteristic:",
                             choices = c(
                                 "Number of Lines" = "speaker",
                                 "Number of Episodes" = "episode",
                                 "Number of Scenes" = "scene"
                             ),
                             selected = "speaker"
                         )
                         
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         plotOutput("seasonPlot"),
                         
                         plotOutput("imdbPlot"),
                         
                         h4("Some things I found interesting are: "),
                         
                         h5(
                             "1. It's very clear that Michael Scott is the main character.
                                  He consistently has the most number of lines compared to any other character
                                  during the seasons he is in the show. "
                         ),
                         
                         h5(
                             "2. Although some characters don't have many lines,
                                  they are in a lot of scenes which makes sense given that the show is filmed like
                                  a mockumentary, and the characters' reactions say more than words. "
                         ),
                         
                         h5(
                             "3. I would have expected the season finales to have the highest IMDB rating since
                             usually those have the most suspence to keep audience viewership, but only in four
                             out of the nine seasons was the last episode the most highly rated. "
                         ),
                         
                         plotOutput("totalimdbPlot")
                         
                     )
                     
                 )),
        
        
        # Page with information about character sentiments
        # This page shows the types of words the character used per season over time
        tabPanel(
            "Sentiment Analysis",
            sidebarPanel(
                h4(
                    "Select a season and character to see the emotions behind the character's words!"
                ),
                
                # Select a season
                sliderInput(
                    "season_senti",
                    "Season:",
                    min = 1,
                    max = 9,
                    value = 1
                ),
                
                # Select a main character
                selectInput(
                    "person",
                    "Character:",
                    choices =  c(
                        "Angela" = "angela",
                        "Darryl" = "darryl",
                        "Dwight" = "dwight",
                        "Jan" = "jan",
                        "Jim" = "jim",
                        "Kevin" = "kevin",
                        "Meredith" = "meredith",
                        "Michael" = "michael",
                        "Oscar" = "oscar",
                        "Pam" = "pam",
                        "Phyllis" = "phyllis",
                        "Roy" = "roy",
                        "Ryan" = "ryan",
                        "Stanley" = "stanley",
                        "Toby" = "toby"
                    ),
                    selected = "angela"
                ),
                
                h6(
                    "Sentiment analysis of character in the season done using the NRC sentiment dictionary."
                ),
                h6(
                    "Sentiment analysis of character over time done using the BING sentiment dictionary."
                )
            ),
            
            mainPanel(
                plotOutput("piePlot"),
                
                plotOutput("timePlot"),
                
                h4("Some things I found interesting are: "),
                
                h5(
                    "1. Almost all of the characters are more positive than they are negative,
                         including Dwight and Angela. I was suprised by this because they are such cynical
                         characters that I wasn't expecting them to be more positive. A notable exception
                         to this is Stanley in season 9 or Angela in season 4."
                ),
                
                h5("2. About 20% of the things most characters say is positive. "),
                
                h5(
                    "3. You can see character development with characters such as Toby and Jan.
                         Their sentiments become increasingly complex and change over time. "
                )
            )
        ),
        
        # Page with information about word correlations between characters
        tabPanel(
            "Relationships",
            
            sidebarPanel(
                h4("Select a couple to see how correlated their words are!"),
                
                selectInput(
                    "couple",
                    "Couple:",
                    choices =  c(
                        "Angela and Dwight" = "ad",
                        "Holly and Michael" = "hm",
                        "Jan and Michael" = "jm",
                        "Kelly and Ryan" = "kr",
                        "Pam and Jim" = "pj"
                    ),
                    selected = "pj"
                )
            ),
            
            mainPanel(
                plotOutput("compPlot"),
                
                plotlyOutput("rPlot")
            )
            
        ),
        
        # About page
        tabPanel(
            "About",
            
            # Header
            h1("An Analysis of The Office"),
            
            
            # Image of The Office characters
            imageOutput("image", width = "100%", height = "100%"),
            
            h2("About this project: "),
            
            h4("In this app, you will be able to explore The Office."),
            
            h4(
                "The first tab allows you to play around with the number of episodes, scenes, and
                lines for each character per season. You can also see the IMDB rating for each episode per season,
                as well as the total graph of the ratings over the course of the whole show."
            ),
            
            h4(
            
                "The second tab shows a sentiment analysis for each character depicting how positive or negative their characters were,
                as well as how their sentiments changed over the course of a season."
            ),
            
            h4(
               "The third tab shows correlations between the word frequencies between characters.
               The first plot shows the overall relationships between the characters, and the second lets you
               select to see the word frequencies between a couple in The Office."
            ),
            
            h3("For more information about this project, please click the link below!"),
            
            embed_url("https://youtu.be/L8ZCZSqmllU"),
            
            h2("About me: "),
            
            h4(
                "My name is Rucha Joshi, and I am a sophomore at Harvard College. I am concentrating in Statistics and
               planning on doing a secondary in Government. I am from Austin, TX and love the traveling, eating desserts, and snow!"
            ),
            
            h6(
                "Image Source: https://medium.com/@sparks_of_art/ux-research-methods-as-characters-from-the-office-fbcf6d70fd3e"
            )
            
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Display image of The Office characters
    output$image <- renderImage({
        # Return a list containing the filename and alt text
        list(
            src = 'the_office_image.png',
            height = 471,
            width = 628,
            style = "display: block; margin-left: auto; margin-right: auto;"
        )
    }, deleteFile = FALSE)
    
    # Show the correlations between word frequencies for each character
    output$compPlot <- renderPlot({
        # Return a list containing the filename and alt text
        cor_all <- corr.test(fbc.df[, -1], adjust = "none")
        cor_plot <- ggcorrplot(cor_all[["r"]], 
                               hc.order = TRUE, 
                               type = "lower",
                               method = "circle",
                               lab = TRUE,
                               lab_size = 2.5) +
            labs(title = "Correlation between Word Frequencies")
        
        cor_plot
    })
    
    # Output the imdb plot which shows the ratings for each episode per season
    output$imdbPlot <- renderPlot({
        imdb_season <- imdb_data %>%
            filter(season == input$season) %>%
            ggplot(aes(
                x = episode,
                y = imdb_rating,
                color = season,
                group = season
            )) +
            geom_line() +
            labs(
                x = "Episode Number",
                y = "IMDB Rating",
                title = paste0("Season ", input$season, ": IMDB Rating per Episode")
            ) +
            theme(legend.position = "none")
        
        imdb_season
    })
    
    # Output the imdb plot which shows the ratings for the entire show
    output$totalimdbPlot <- renderPlot({
        data <- tibble::rowid_to_column(imdb_data, "id") %>%
            ggplot(aes(
                x = id,
                y = imdb_rating,
                color = factor(season),
                group = season
            )) +
            geom_line() +
            labs(
                x = "Episode Number",
                y = "IMDB Rating",
                title = "Complete IMDB Ratings"
            ) +
            theme(legend.position = "none")
        
        data
    })
    
    # Data for first graph of information (episodes, scenes, lines) per character
    datareact1 <- reactive ({
        dr1 <- clean_data.df %>%
            # Filtering for season
            filter(season == input$season) %>%
            select(season, episode, speaker, scene) %>%
            filter(
                speaker %in% c(
                    "angela",
                    "darryl",
                    "dwight",
                    "jan",
                    "jim",
                    "kevin",
                    "meredith",
                    "michael",
                    "oscar",
                    "pam",
                    "phyllis",
                    "roy",
                    "ryan",
                    "stanley",
                    "toby"
                )
            )
    })
    
    # Determine which dataframe to create: lines, scenes, episodes
    plotreactive1 <- reactive({
        if (input$char == "speaker")
        {
            table_speaker <- datareact1()
        }
        else if (input$char == "scene") {
            table_scene <- datareact1() %>%
                #group_by(scene, speaker) %>%
                count(scene, speaker)
        }
        else{
            table_episode <- datareact1() %>%
                count(episode, speaker)
        }
    })
    
    # Output graph showing number of lines/scenes/episodes per character
    output$seasonPlot <- renderPlot({
        #DT::renderDataTable({
        
        plotreactive1() %>%
            ggplot(aes(x = speaker, color = speaker)) +
            geom_bar() +
            labs(
                title = ifelse(
                    input$char == "speaker",
                    paste0("Season ", input$season, ": Number of lines per character"),
                    paste0(
                        "Season ",
                        input$season,
                        ": Number of ",
                        input$char,
                        "s per character"
                    )
                ),
                x = "Character",
                y = ifelse(
                    input$char == "speaker",
                    "Number of Lines",
                    paste0("Number of ", input$char, "s")
                )
            ) +
            coord_flip() +
            theme(legend.position = "none")
    })
    
    # Classify each word used by characters using the NRC sentiment base
    datareact2 <- reactive ({
        nrc <- get_sentiments("nrc")
        
        char_seas_senti <- clean_data.df %>%
            filter(speaker == input$person) %>%
            filter(season == input$season_senti) %>%
            select(line = id,
                   line_text_mod,
                   everything(),
                   -line_text,
                   -actions,
                   -deleted) %>%
            unnest_tokens(word, line_text_mod, strip_numeric = TRUE) %>%
            mutate_at(vars(word), funs(str_replace_all(., "'s$", ""))) %>%
            inner_join(nrc)
    })
    
    # Count number of kinds of sentiments for the pie plot
    plotreactive2 <- reactive ({
        plot <- datareact2 () %>%
            count(sentiment, sort = TRUE)
    })
    
    # BING sentiments classify words into positive or negative
    # We use this data to make the line graph
    timereactive2 <- reactive ({
        bing <- get_sentiments("bing")
        
        time_plot <- clean_data.df %>%
            filter(speaker == input$person) %>%
            filter(season == input$season_senti) %>%
            select(line = id,
                   line_text_mod,
                   everything(),
                   -line_text,
                   -actions,
                   -deleted) %>%
            unnest_tokens(word, line_text_mod, strip_numeric = TRUE) %>%
            mutate_at(vars(word), funs(str_replace_all(., "'s$", ""))) %>%
            group_by(episode, speaker) %>%
            inner_join(bing) %>%
            count(sentiment, sort = TRUE)
        
    })
    
    # Make circle graph of the kinds of sentiments used by the character that season
    output$piePlot <- renderPlot({
        plotreactive2 () %>%
            ggplot(aes(
                x = "",
                y = n,
                fill = sentiment
            )) +
            geom_bar(width = 1, stat = "identity") +
            labs(
                x = "number",
                y = "",
                title = paste0(
                    "Season ",
                    input$season_senti,
                    ": Sentiment analysis of ",
                    str_to_title(input$person)
                )
            ) +
            coord_polar("y", start = 0)
    })
    
    # Make the line graph that shows the number of positive and negative words the character used
    # per episode in the given season
    output$timePlot <- renderPlot({
        timereactive2 () %>%
            ggplot(aes(
                x = episode,
                y = n,
                group = sentiment,
                color = sentiment
            )) +
            geom_line() +
            labs(
                x = "episode",
                y = "number",
                title = paste0(
                    "Number of positive and negative words: ",
                    str_to_title(input$person)
                )
            )
    })
    
    # Make the couple's word frequency correlation graph
    datareactive3 <- reactive ({
        if (input$couple == "pj") {
            relationships <- fbc.df %>%
                select(word, pam, jim) %>%
                ggplot(aes(
                    x = pam,
                    y = jim,
                    color = abs(pam - jim),
                    label = word
                )) +
                geom_abline(color = "gray40", lty = 2) +
                geom_jitter(
                    alpha = 0.1,
                    size = 2.5,
                    width = 0.3,
                    height = 0.3
                ) +
                scale_x_log10(labels = percent_format()) +
                scale_y_log10(labels = percent_format()) +
                labs(x = "Pam",
                     y = "Jim",
                     title = "Word Frequency Comparison: Pam and Jim") +
                theme(legend.position = "none")
        }
        
        else if (input$couple == "ad") {
            relationships <- fbc.df %>%
                select(word, angela, dwight) %>%
                ggplot(aes(
                    x = angela,
                    y = dwight,
                    color = abs(angela - dwight),
                    label = word
                )) +
                geom_abline(color = "gray40", lty = 2) +
                geom_jitter(
                    alpha = 0.1,
                    size = 2.5,
                    width = 0.3,
                    height = 0.3
                ) +
                scale_x_log10(labels = percent_format()) +
                scale_y_log10(labels = percent_format()) +
                labs(x = "Angela",
                     y = "Dwight",
                     title = "Word Frequency Comparison: Angela and Dwight") +
                theme(legend.position = "none")
            
        }
        
        else if (input$couple == "hm") {
            relationships <- fbc.df %>%
                select(word, holly, michael) %>%
                ggplot(aes(
                    x = holly,
                    y = michael,
                    color = abs(holly - michael),
                    label = word
                )) +
                geom_abline(color = "gray40", lty = 2) +
                geom_jitter(
                    alpha = 0.1,
                    size = 2.5,
                    width = 0.3,
                    height = 0.3
                ) +
                scale_x_log10(labels = percent_format()) +
                scale_y_log10(labels = percent_format()) +
                labs(x = "Holly",
                     y = "Michael",
                     title = "Word Frequency Comparison: Holly and Michael") +
                theme(legend.position = "none")
            
        }
        
        else if (input$couple == "jm") {
            relationships <- fbc.df %>%
                select(word, jan, michael) %>%
                ggplot(aes(
                    x = jan,
                    y = michael,
                    color = abs(jan - michael),
                    label = word
                )) +
                geom_abline(color = "gray40", lty = 2) +
                geom_jitter(
                    alpha = 0.1,
                    size = 2.5,
                    width = 0.3,
                    height = 0.3
                ) +
                scale_x_log10(labels = percent_format()) +
                scale_y_log10(labels = percent_format()) +
                labs(x = "Jan",
                     y = "Michael",
                     title = "Word Frequency Comparison: Jan and Michael") +
                theme(legend.position = "none")
        }
        
        else {
            relationships <- fbc.df %>%
                select(word, kelly, ryan) %>%
                ggplot(aes(
                    x = kelly,
                    y = ryan,
                    color = abs(kelly - ryan),
                    label = word
                )) +
                geom_abline(color = "gray40", lty = 2) +
                geom_jitter(
                    alpha = 0.1,
                    size = 2.5,
                    width = 0.3,
                    height = 0.3
                ) +
                scale_x_log10(labels = percent_format()) +
                scale_y_log10(labels = percent_format()) +
                labs(x = "Kelly",
                     y = "Ryan",
                     title = "Word Frequency Comparison: Kelly and Ryan") +
                theme(legend.position = "none")
        }
        
        relationships
        
    })
    
    # Plot the relationships correlation graph for couples
    output$rPlot <- renderPlotly ({
        datareactive3 ()
    })
    
    ########################################
    
}

# Run the application
shinyApp(ui = ui, server = server)
