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
    titlePanel("An Analysis of The Office"),

    # Navigation bar
    navbarPage("Analysis",
               
               # About page
               tabPanel("About",
                        mainPanel(
                            h6("This page is an analysis of the popular TV show \"The Office\". 
                               We analyze the number of lines and scenes each character is in
                               per season.")
                        )
               ),
    
               # Sidebar with a slider input for number of bins 
               tabPanel("Per Season",
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
               tabPanel("Per Character",
                        sidebarPanel(
                            
                            # Select a character
                            selectInput("person", 
                                        "Character:", 
                                        choices =  c("Most Frequent Words" = "freq_words",
                                                     "Word Sentiments" = "senti_words"), 
                                        selected = "freq_workds")
                        ),
                        mainPanel(
                            
                            plotOutput("distPlot")
                            
                        )
                        
               )

    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Data for first graph of information (episodes, scenes, lines) per character
    datareact1 <- reactive ({
        clean_data.df %>%
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
    
    output$distPlot <- renderPlot({
        
        plotreactive1 () %>% 
            ggplot(aes(x = speaker)) +
            geom_bar() +
            labs(title = "Number of -- spoken by each character in season 1", 
                 x = "Character", 
                 y = "Number of --") +
            coord_flip()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
