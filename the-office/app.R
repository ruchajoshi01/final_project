#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Input cleaned data
clean_data.df <- read_rds("clean_data.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Navigation bar
    navbarPage(
        
        "The Office",
        
        # About page
        tabPanel(
            
            "About",
            
            # Header
            h1("An Analysis of The Office"),
            
            
            # Image of The Office characters
            imageOutput("image", width = "100%", height = "100%"),
            
            h4(
                "In this app, you will be able to explore The Office.
                
                The first tab allows you to play around with the number of episodes, scenes, and lines for each character per season.
                
                The second tab shows a sentiment analysis for each character depicting how positive or negative their characters were."
            ),
            
            h6(
                "Image Source: https://medium.com/@sparks_of_art/ux-research-methods-as-characters-from-the-office-fbcf6d70fd3e"
            )
            
        ),
        
        # Page with information about amount of character appearance in the show
        tabPanel("Per Character",
                 
                 # Allow user to select information on the side
                 sidebarLayout(
                     
                     sidebarPanel(
                         
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
                                 "Number of Scenes" = "scene"),
                                 selected = "speaker"
                             )
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(plotOutput("seasonPlot"))
                         
                     )
                     
                 ),
        # Page with information about character sentiments
        tabPanel(
            "Sentiment Analysis",
            sidebarPanel(
                
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
                
                h6("Sentiment analysis of character in the season done using the NRC sentiment dictionary."),
                h6("Sentiment analysis of character over time done using the BING sentiment dictionary.")
            ),
            
            mainPanel(plotOutput("piePlot"),
                      
                      plotOutput("timePlot"))
        )
        
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$image <- renderImage({
        # Return a list containing the filename and alt text
        list(
            src = 'the_office_image.png',
            height = 471,
            width = 628,
            style = "display: block; margin-left: auto; margin-right: auto;"
        )
    }, deleteFile = FALSE)
    
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
    
    # Determine which dataframe to create
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
    
    output$seasonPlot <- renderPlot({
        #DT::renderDataTable({
        
        plotreactive1() %>%
            ggplot(aes(x = speaker)) +
            geom_bar() +
            labs(title = ifelse(input$char == "speaker",
                                paste0("Season ", input$season, ": Number of lines per character"),
                                paste0("Season ", input$season, ": Number of ", input$char, "s per character")),
                 x = "Character",
                 y = ifelse(input$char == "speaker", 
                            "Number of Lines", 
                            paste0("Number of ",input$char, "s"))) +
            coord_flip()
    })
    
    datareact2 <- reactive ({
        nrc <- get_sentiments("nrc")
        
        char_seas_senti <- clean_data.df %>%
            filter(speaker == input$person) %>%
            filter(season == input$season_senti) %>%
            select(line = id, line_text_mod, everything(), -line_text, -actions, -deleted) %>% 
            unnest_tokens(word, line_text_mod, strip_numeric = TRUE) %>%
            mutate_at(vars(word), funs(str_replace_all(., "'s$", ""))) %>%
            inner_join(nrc)
    })
    
    plotreactive2 <- reactive ({
        plot <- datareact2 () %>%
            count(sentiment, sort = TRUE)
    })
    
    timereactive2 <- reactive ({
        
        bing <- get_sentiments("bing")
        
        time_plot <- clean_data.df %>%
            filter(speaker == "angela") %>%
            filter(season == 9) %>%
            select(line = id, line_text_mod, everything(), -line_text, -actions, -deleted) %>% 
            unnest_tokens(word, line_text_mod, strip_numeric = TRUE) %>%
            mutate_at(vars(word), funs(str_replace_all(., "'s$", ""))) %>%
            group_by(episode, speaker) %>%
            inner_join(bing) %>%
            count(sentiment, sort = TRUE)
        
    })
    
    output$piePlot <- renderPlot({
        
        plotreactive2 () %>%
            ggplot(aes(x = "", y = n, fill = sentiment)) + 
            geom_bar(width = 1, stat = "identity") + 
            labs(x = "number",
                 y = "sentiment",
                 title = paste0("Season ", input$season_senti, ": Sentiment analysis of ", str_to_title(input$person))) +
            coord_polar("y", start=0)
    })
    
    output$timePlot <- renderPlot({
        timereactive2 () %>%
            ggplot(aes(x = episode, y = n, group = sentiment, color = sentiment)) +
            geom_line() + 
            labs(x = "episode",
                 y = "number",
                 title = paste0("Number of positive and negative words: ", str_to_title(input$person)))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
