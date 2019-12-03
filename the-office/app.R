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

    # Application title
    #titlePanel("An Analysis of The Office"),

    # Navigation bar
    navbarPage("The Office",
               
               # About page
               tabPanel("About",
                        
                        # Header
                        h1("An Analysis of The Office"),
                        
                        
                        # Image of The Office characters
                        imageOutput("image", width = "100%", height = "100%"),
                        
                        h4("In this app, you will be able to explore The Office. 
                           The first tab allows you to play around with the number of episodes, scenes, and lines for each character per season.
                           The second tab shows a sentiment analysis for each character depicting how positive or negative their characters were."),
                        
                        h6("Image Source: https://medium.com/@sparks_of_art/ux-research-methods-as-characters-from-the-office-fbcf6d70fd3e")
               ),
    
               # Sidebar with a slider input for number of bins 
               tabPanel("Per Character",
               sidebarLayout(
       
                sidebarPanel(
            
                    # Select season of show
                    sliderInput("season",
                        "Season:",
                        min = 1,
                        max = 9,
                        value = 1),
                    
                    # Select lines, scenes, words of each character per season
                    selectInput("char", 
                                "Characteristic:", 
                                choices = c("speaker", "episode", "scene"), 
                                selected = "speaker")
                    ),

                # Show a plot of the generated distribution
                mainPanel(
           
                    plotOutput("seasonPlot")
        
                    )
    
                )

               ),
               
               
               
               
               tabPanel("Sentiment Analysis",
                        sidebarPanel(
                            
                            # Select a character
                            sliderInput("season_senti",
                                        "Season:",
                                        min = 1,
                                        max = 9,
                                        value = 1),
                            
                            selectInput("person", 
                                        "Character:", 
                                        choices =  c("Angela" = "angela",
                                                     "Darryl" = "darryl",
                                                     "Dwight" = "dwight",
                                                     "Jan" = "jan", 
                                                     "Jim" = "jim", 
                                                     "Holly" = "holly",
                                                     "Kevin" = "kevin",
                                                     "Meredith" = "meredith",
                                                     "Michael" = "michael",
                                                     "Oscar" = "oscar",
                                                     "Pam" = "pam", 
                                                     "Phyllis" = "phyllis",
                                                     "Roy" = "roy",
                                                     "Ryan" = "ryan",
                                                     "Stanley" = "stanley",
                                                     "Toby" = "toby"), 
                                        selected = "angela")
                        ),
                        mainPanel(
                            
                            plotOutput("piePlot"),
                            
                            dataTableOutput("dataTable")
                            
                        )
                        
               )

    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$image <- renderImage({
        # Return a list containing the filename and alt text
        list(src = 'the_office_image.png', 
             height = 471,
             width = 628, style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE
    )
    
    # Data for first graph of information (episodes, scenes, lines) per character
    datareact1 <- reactive ({
        dr1 <- clean_data.df %>%
            # Filtering for season
            filter(season == input$season) %>%
            select(season, episode, speaker, scene) %>%
            filter(speaker %in% c("toby", "stanley", "ryan", "roy", 
                                "phyllis", "pam", "oscar", "michael", 
                                "dwight", "meredith", "kevin", "jim", 
                                "jan", "angela", "darryl"))
    })
    
    # Determine which dataframe to create
    plotreactive1 <- reactive({
        if(input$char == "speaker") 
        {
            table_speaker <- datareact1()
        }
        else if(input$char == "scene") {
            table_scene <- datareact1() %>%
                #group_by(scene, speaker) %>% 
                count(scene, speaker)
        }
        else {
            table_episode <- datareact1() %>%
                count(episode, speaker) #%>% 
                #ungroup()
        }
    })

    output$seasonPlot <- renderPlot({ #DT::renderDataTable({
        
        plotreactive1() %>%
            ggplot(aes(x = speaker)) +
            geom_bar() +
            labs(title = "Number of -- spoken by each character in season 1", 
                 x = "Character", 
                 y = "Number of --") +
            coord_flip()
    })
    
    datareact2 <- reactive ({
        tidy_tokens <- clean_data %>%
            select(line = id, line_text_mod, everything(), -line_text, -actions, -deleted) %>% 
            unnest_tokens(word, line_text_mod, strip_numeric = TRUE) %>%
            mutate_at(vars(word), funs(str_replace_all(., "'s$", ""))) %>%
            anti_join(stop_words, by = "word")
    })
    
    plotreactive2 <- reactive({
        if(input$action == "freq_words") 
        {
            custom_stop_words <- bind_rows(data_frame(word = c("yeah", "hey", "uh", "um", "huh", "hmm", "ah", "umm", "uhh", "gonna", "na", "ha", "gotta"), 
                                                      lexicon = c("custom")), 
                                           stop_words)
            
            person_freq_words <- datareact2() %>%
                filter(speaker == input$person) %>%
                anti_join(custom_stop_words, by = "word") %>%
                #filter(speaker == input$person) %>%
                count(word) %>%
                arrange(desc(n)) %>%
                mutate(prop = round((n / sum(n))*100, 1)) %>%
                head(10)
        }
        else {
            person_unique_words_2 <- datareact2() %>%
                count(speaker, word, sort = TRUE) %>%
                ungroup() %>%
                filter(speaker == input$person) %>%
                bind_tf_idf(word, speaker, n) %>%
                arrange(desc(tf_idf)) %>%
                rename(prop = tf_idf) %>%
                head(10)
        }
    })
    
    output$charPlot <- renderPlot({
        
        plotreactive2 () %>% 
            ggplot(aes(x = reorder(word, prop), y = prop)) +
            geom_col() +
            labs(title = "Frequent Words: [Enter Character Name]", 
                 x = "", 
                 y = "Percentage") +
            coord_flip()
    })
    
    output$dataTable <- DT::renderDataTable({
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
