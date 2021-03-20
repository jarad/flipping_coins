#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library("shiny")
library("tidyverse")

d = readr::read_csv("../data/flips.csv") %>%
    dplyr::group_by(coin) %>% 
    dplyr::summarize(y = sum(result == "heads"),
                     n = n())

posterior = function(d) { 
    data.frame(probability = seq(0,1, length=101)) %>%
        dplyr::mutate(density = dbeta(probability, 
                                      shape1 = 1+d$y,
                                      shape2 = 1+d$n-d$y))
} 

p = d %>%
    dplyr::group_by(coin) %>%
    do(posterior(.))
    

# Color scale
coin_colors = c(gold = "#ffd700",
                silver = "#c0c0c0",
                bronze = "#cd7f32")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bayesian posterior distribution for coin flip probability"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("coin",
                        "Coin:",
                        choices = c("Gold","Silver","Bronze")),
            selectInput("result",
                         "Result",
                         choices = c("heads","tails")),
            actionButton("record",
                         "Record")
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
        ggplot(p, aes(x = probability,
                      y = density,
                      color = coin,
                      linetype = coin,
                      group = coin)) + 
            geom_line(size = 2) + 
            scale_color_manual(values = coin_colors) + 
            labs(x = "Probability of flipping heads",
                 y = "Probability density function",
                 title = "Flipping Bitcoin") +
            theme_bw() 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
