library(shiny)
library(stringr)
library(dplyr)

load(file = "Z:\\Camila Silva\\POC\\enron_training.Rdata")
class_mails$Date <- as.POSIXct(class_mails$Date)

#Email to User Name
class_mails$From_New <- sub("@.*", "", class_mails$From)
class_mails$From_New <- lapply(class_mails$From_New, gsub, pattern = ".", replacement = " ", fixed = TRUE)
class_mails$From_New <- str_to_title(class_mails$From_New)

#changing Names
class_mails$From_New %<>%  gsub('Phillip Allen', 'Charles McGill', .)
class_mails$From_New %<>% gsub('Patti99', 'Howard Hamlin', .)
class_mails$From_New %<>% gsub('Jeffrey Hodge', 'Ignacio Garcia', .)
class_mails$From_New %<>% gsub('Jacquestc', 'Miguel Hernandez', .)


ui <- fluidPage(
  titlePanel("Hist!"),
   sidebarLayout(
     sidebarPanel(
       selectInput(inputId= "User", 
                   label= "Select User", 
                   choices=class_mails$From_New %>% unique(),
                    selected="Charles McGill"), 
       
       selectInput(inputId= "range", 
                  label= "Range", 
                  choices=c("Last 7 days, Among Week", "Last 7 days, Among Day", 
                            "Historical, Among Week", "Historical, Among Day"),
                  selected="Historical, Among Week") ),
    mainPanel(
      plotOutput(outputId = "distPlott"),
      width = 8
    )  ) )
server <- function(input, output) {
  output$distPlott <- renderPlot({
    
    filter_mail <- class_mails %>% filter(From_New==input$User)
    
    if(input$range=="Last 7 days, Among Week"){    
      max_Date <-  max(filter_mail$Date)
      filter_mail <-filter_mail %>% filter(Date > max_Date-(7*60*60*24) )
      x <- filter_mail$Weekday %>% ordered(levels=c("Monday", "Tuesday", 
                                                    "Wednesday", "Thursday", 
                                                    "Friday", "Saturday", "Sunday"))
    }
    if(input$range=="Last 7 days, Among Day"){
      max_Date <-  max(filter_mail$Date)
      filter_mail <-filter_mail %>% filter(Date > max_Date-(7*60*60*24) )
      x <- paste(filter_mail$Hour, ":00", sep = "")
      x <- x %>% ordered(levels=paste(seq(max(24)), ":00", sep = ""))      
    }
    
    if(input$range=="Historical, Among Week"){
      x <- filter_mail$Weekday %>% ordered(levels=c("Monday", "Tuesday", 
                                                    "Wednesday", "Thursday", 
                                              "Friday", "Saturday", "Sunday"))
      }
    if(input$range=="Historical, Among Day"){
      x <- paste(filter_mail$Hour, ":00", sep = "")
      x <- x %>% ordered(levels=paste(seq(max(24)), ":00", sep = ""))      
     }
   
    barplot(height=table(x),
           col = "#75AADB",
          border = "white",
         xlab = input$range %>% str_sub( start = 7, end = -1))
  })}
shinyApp(ui, server)
