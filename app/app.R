
library(shiny)

state_choice <- kids %>% 
    distinct(state)

var_choice <- kids %>% 
    distinct(var_name)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Us Government Expenditure on Kids"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("state",
                      "Select States",
                      choices = kids %>% distinct(state) %>% pull,
                      selected = "Alabama"),
          checkboxGroupInput("variable",
                      "Select Variable",
                      choices = kids %>% distinct(var_name) %>% pull,
                      selected = "elementary and secondary education")
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("linePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$linePlot <- renderPlot({
        kids %>% 
            filter(var_name == input$variable,
                   state == input$state) %>%
            group_by(year) %>% 
            summarise(total = sum(inf_adj_perchild)) %>% 
            ggplot(aes(year, total)) +
            geom_line(size =1, color = "gray60") +
            labs(title = "Gov Spendings on Parks and Recreational Spaces",
                 x = "",
                 y = "Total Amount in $ per child")    
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
