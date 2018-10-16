#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

rm(list = ls())

library(shiny)

library(visNetwork)
library(Hmisc)

# Data load ---------------------------------------------------------

load(file = "data/viz.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("D-text Visualization"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "worker",
        label = "Select worker:",
        choices = users$name,
        selected = "Charles McGill",
        multiple = FALSE
      ),
      selectInput(inputId= "range",
                  label = "Range",
                  choices =c("Last 7 days, Among Week", "Last 7 days, Among Day",
                            "Historical, Among Week", "Historical, Among Day"),
                  selected="Historical, Among Week")
  ),

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput(outputId = "mailHist"),
      visNetworkOutput("network_proxy_nodes")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$mailHist <- renderPlot({

    filter_mail <- class_mails %>% filter(From_New == input$worker)

    if (input$range == "Last 7 days, Among Week") {
      max_Date <-  max(filter_mail$Date)
      filter_mail %<>% filter(Date > max_Date - (7*60*60*24) )
      x <- filter_mail$Weekday %>% ordered(levels = c("Monday", "Tuesday",
                                                    "Wednesday", "Thursday",
                                                    "Friday", "Saturday", "Sunday"))
    }
    if (input$range == "Last 7 days, Among Day") {
      max_Date <-  max(filter_mail$Date)
      filter_mail %<>% filter(Date > max_Date - (7*60*60*24))
      x <- paste(filter_mail$Hour, ":00", sep = "")
      x <- x %>% ordered(levels = paste(seq(max(24)), ":00", sep = ""))
    }

    if (input$range == "Historical, Among Week") {
      x <- filter_mail$Weekday %>% ordered(levels = c("Monday", "Tuesday",
                                                    "Wednesday", "Thursday",
                                              "Friday", "Saturday", "Sunday"))
      }
    if (input$range == "Historical, Among Day") {
      x <- paste(filter_mail$Hour, ":00", sep = "")
      x <- x %>% ordered(levels = paste(seq(max(24)), ":00", sep = ""))
     }

    barplot(height = table(x),
            col = "#75AADB",
            border = "white",
            xlab = input$range %>% str_sub( start = -10, end = -1))
  })

  output$network_proxy_nodes <- renderVisNetwork({

    #Filtering the social network by worker
    id_selected <-
      users %>% filter(name == input$worker) %>% select(id) %>% as.integer()
    edges <- connections %>%
      filter(sender == id_selected | receiver == id_selected)
    nodes <- data.frame(id = union(unique(edges$sender), unique(edges$receiver)))
    nodes %<>% left_join(users)
    second_level_nodes <- nodes %>% filter(id != id_selected)
    second_level_edges <-
      filter(connections, sender %in% second_level_nodes$id, suspicious == 1)
    edges %<>% rbind(second_level_edges)
    nodes <- data.frame(id = union(unique(edges$sender), unique(edges$receiver)))
    nodes %<>% left_join(users) %>% unique()

    #Ordering and changing color
    aux   <- nodes %>% filter(name == input$worker)
    auxC  <- nodes %>% filter(name != input$worker)
    nodes <- rbind(aux, auxC)
    nodes$color <- sapply("lightblue", function(x) rep(x, nrow(nodes) - 1)) %>%
      rbind("pink", .) %>% as.vector()

    colnames(nodes)[2] <- "label"
    colnames(edges)[c(1, 2)] <- c("from", "to")

    #Creating the plot
    visNetwork(nodes, edges, main = "Interaction Graph", width = "100%") %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1,
                                          labelOnly = FALSE, hover = TRUE))
  })

}

# Run the application
shinyApp(ui = ui, server = server)
