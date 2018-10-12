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
        choices = nodes$label,
        selected = "Greg Piper",
        multiple = FALSE
      ),
      selectInput(
        inputId = "range",
        label = "Range",
        choices = c("Within Year", "Within Month", "Within Week", "Within Day"),
        selected = "Within Year"
      )
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
      usermail <-
        nodes %>%
          filter(label == input$worker) %>%
          select(email)
      filter_mail <-
        class_mails %>%
          filter(From == usermail)

      filter_mail$Month_Name <- month.abb[filter_mail$Month]

      if (input$range == "Within Year") {
         x <-
          filter_mail$Month_Name %>%
            ordered(levels = c("Jan","Feb","Mar","Apr","May","Jun",
                               "Jul","Aug","Sep","Oct","Nov","Dec"))
      } else if (input$range == "Within Month") {
         x <- filter_mail$Day %>% ordered(levels = seq(max(31)))
      } else if (input$range == "Within Week") {
         x <-
          filter_mail$Weekday %>%
            ordered(levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                               "Friday", "Saturday", "Sunday"))
      } else if (input$range == "Within Day") {
         x <- filter_mail$Hour %>% ordered(levels = seq(max(24)))
      }

      barplot(
         height = table(x),
         col = "#75AADB",
         border = "white",
         xlab = input$range %>% str_sub(start = 7, end = -1))
   })

  output$network_proxy_nodes <- renderVisNetwork({
    #Filtering the social network by worker
    id_selected <-
      nodes %>%
        filter(label == input$worker) %>%
        select(id) %>%
        as.integer()
    edge <- data %>% filter(id_f == id_selected | id_t == id_selected)
    node <- tibble(id = union(unique(edge$id_f), unique(edge$id_t)))
    node <- left_join(node, nodes)
    second_level <- node %>% filter(id != id_selected)
    second_level_edges <- filter(data, id_f %in% second_level$id, is_s == 1)
    sec_edge <- rbind(second_level_edges, edge)
    node <- tibble(id = union(unique(sec_edge$id_f), unique(sec_edge$id_t)))
    node <- left_join(node, nodes)
    #Ordering and changing color
    aux <- node %>% filter(label == input$worker)
    auxC <- node %>% filter(label != input$worker)
    node <-
      rbind(aux, auxC) %>%
        tibble(
          color =
            sapply("lightBlue",
                   function(x) rep(x,nrow(node) - 1)) %>%
              rbind("pink", .) %>% as.vector())

    #Creating the plot
    visNetwork(node, sec_edge, main = "Social Network Bank", width = "100%") %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1,
                                          labelOnly = FALSE, hover = TRUE),
                 nodesIdSelection = T)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
