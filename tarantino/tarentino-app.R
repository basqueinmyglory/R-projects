library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
curses <- read_csv("data/curses.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Tarantino Cursing"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("movie", "Movie:", 
                    choices=curses$movie,
                    selected="Pulp Fiction")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     Choice <- curses %>% filter( movie == input$movie)
      ggplot(data = Choice, aes(x=minutes_in, y=group)) + geom_point(aes(col=group), position = "jitter") + ggtitle("Distribution of Curse words") + xlab("Minutes into the Movie") + ylab("Type of Curse Word")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

