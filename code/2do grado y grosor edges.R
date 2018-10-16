library(shiny)
library(visNetwork)
library(tidyverse)
library(dplyr)
library(lubridate)
library(magrittr)
library(Hmisc)
library(stringr)

load(file = "W:\\Camila Saavedra\\Beta1 EMAILS\\enron_training.Rdata",verbose = T)
class_mails$Date %<>% ymd_hms
class_mails %<>% distinct()

# Split the receptor ------------------------------------------------------

class_mails$To<- ifelse(class_mails$To=="","Unknown",class_mails$To)

df <- class_mails$To %>% str_split(',')

nCol <- vapply(df, length, 0) %>% max()

df %<>% lapply(function(row) c(row, rep(NA, nCol - length(row))))
df <- matrix(unlist(df), nrow = length(df), ncol = nCol, byrow = TRUE)
df %<>% data.frame(stringsAsFactors = F) %>% as.tibble()

df %<>% cbind(ID = class_mails$ID)
df %<>% gather(key = var_, value = c(X1, X2, X3, X4, X5), -ID)

names(df)[3] <- "to_single"

df %<>% filter(nchar(to_single) > 2)
df[2] <- NULL

mails <- merge(x = class_mails, y = df, by = "ID")

nodes <- data.frame(nodos=unique(union(unique(mails$From),unique(mails$to_single))))

nodes["Emails"] <- nodes
# Cleaning data (names) ---------------------------------------------------

nodes$nodos <- sub("@.*", "", nodes$nodos)
nodes[1] <- lapply(nodes[1], gsub, pattern = ".", replacement = " ", fixed = TRUE)
nodes[,1] <- str_to_title(nodes[,1])
nodes$Emails %<>% trimws(which = "both")
nodes %<>% distinct(Emails,.keep_all = TRUE)

# Making nice ------------------------------------------------

nodes <- tibble(id= rownames(nodes),
                    nodos= nodes$nodos,
                    emails= nodes$Emails)

nodes$nodos <- gsub('Phillip Allen', 'Charles McGill', nodes$nodos)
nodes$nodos <- gsub('Patti99', 'Howard Hamlin', nodes$nodos)
nodes$nodos <- gsub('Jeffrey Hodge', 'Ignacio Garcia', nodes$nodos)
nodes$nodos <- gsub('Jacquestc', 'Miguel Hernandez', nodes$nodos)

# Subset mails to avoid garbage

data <- mails
data %<>% mutate(id_f=match(data$username_from,sapply(strsplit(as.character(nodes$emails), split='@', fixed=TRUE), function(x) (x[1]))))
data %<>% mutate(id_t=match(data$username_to,sapply(strsplit(as.character(nodes$emails), split='@', fixed=TRUE), function(x) (x[1]))))
data %<>% select(id_f,id_t,is_suspiscius)

data$id_f <- as.integer(gsub(296 , 424 , data$id_f ))
data$id_f <- as.integer(gsub(294 , 424 , data$id_f ))
data$id_f <- as.integer(gsub(275 , 370 , data$id_f ))
data$id_f <- as.integer(gsub(286 , 370 , data$id_f ))
data$id_f <- as.integer(gsub(169 , 346 , data$id_f ))
data$id_f <- as.integer(gsub(127 , 412 , data$id_f ))

data$color <- ifelse(data$is_suspiscius == 1,"red","lightblue")



# SHINY APP ---------------------------------------------------------------
ui <- fluidPage(
  sidebarLayout(
    # Input(s)
    sidebarPanel(
      # Explanatory text
      HTML(paste0("Network Filter tool")),
      # Break for visual separation
      br(), br(),
      selectInput(inputId = "worker",
                  label = "Select worker:",
                  choices = nodes$nodos,
                  selected = "Charles McGill",
                  multiple = FALSE)
    ),
    mainPanel(visNetworkOutput("network_proxy_nodes"))
  )
)

server <- function(input, output) {
  #The datas with the nodes and edges
  nododos <- data.frame(id = as.integer(nodes$id),
                        label = nodes$nodos)

  edgeges <- data.frame(from = data$id_f, to = data$id_t, is_s = data$is_suspiscius, color = data$color)
  edgeges <- edgeges[order(edgeges$is_s, decreasing = TRUE), ]

  edges0 <- edgeges %>% group_by(from, to) %>% summarise(value=n())

  edges1 <- distinct(edgeges, from, to, .keep_all = T)

  edges3 <- as.data.frame(merge(edges1,edges0))

  output$network_proxy_nodes <- renderVisNetwork({

    #Filtering the social network by worker
    id_selected <- nododos %>%  filter(input$worker == label) %>% select(id) %>%  as.integer()
    edge <- edges3 %>%  filter(id_selected == from | id_selected == to)
    node <- data.frame(id=union(unique(edge$from),unique(edge$to)))
    node <- left_join(node,nododos)
    second_level <- node[!(node$id == id_selected),]
    second_level_edges <- filter(edges3,from %in% second_level$id)
    second_level_edges <- second_level_edges[!(second_level_edges$is_s == 0),]
    sec_edge <- rbind(second_level_edges,edge)
    node <- data.frame(id=union(unique(sec_edge$from),unique(sec_edge$to)))
    node <- left_join(node,nododos) %>% unique()


    #Ordering and changing color
    aux <- node %>%  filter(input$worker == label)
    auxC <- node %>%  filter(input$worker != label)
    node <- rbind(aux,auxC) %>% data.frame(color=sapply("lightBlue", function (x) rep(x,nrow(node)-1)) %>% rbind("pink", .) %>% as.vector() )



    #Creating the plot
    visNetwork(node, sec_edge, main = "Social Network Bank", width = "100%") %>%
      visEdges(arrows = list(to = list(enabled = TRUE,  scaleFactor = 2, type = 'arrow'))) %>%
      visOptions( highlightNearest = list(enabled = TRUE, degree = 1, labelOnly = FALSE, hover = TRUE))
  })


}

shinyApp(ui=ui, server =server)
