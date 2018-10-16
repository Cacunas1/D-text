rm(list = ls())

library(tidyr)
library(dplyr)
library(lubridate)
library(magrittr)
library(Hmisc)
library(stringr)

load(file = "data/enron_training.Rdata")

class_mails$Date %<>% ymd_hms
class_mails %<>% distinct()

# Split the receptor ------------------------------------------------------

class_mails$To <- ifelse(class_mails$To=="","Unknown",class_mails$To)

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

nodes <- data.frame(nodos = union(unique(mails$From), unique(mails$to_single)))

nodes["Emails"] <- nodes
# Cleaning data (names) ---------------------------------------------------

nodes$nodos <- sub("@.*", "", nodes$nodos)
nodes[1] <- lapply(nodes[1], gsub, pattern = ".", replacement = " ", fixed = TRUE)
nodes[,1] <- str_to_title(nodes[,1])
nodes$Emails %<>% trimws(which = "both")
nodes %<>% distinct(Emails, .keep_all = TRUE)

# Making nice ------------------------------------------------

nodes <- data.frame(id = rownames(nodes),
                nodos = nodes$nodos,
                emails = nodes$Emails)

nodes$nodos <- gsub('Phillip Allen', 'Charles McGill', nodes$nodos)
nodes$nodos <- gsub('Patti99', 'Howard Hamlin', nodes$nodos)
nodes$nodos <- gsub('Jeffrey Hodge', 'Ignacio Garcia', nodes$nodos)
nodes$nodos <- gsub('Jacquestc', 'Miguel Hernandez', nodes$nodos)

# Subset mails to avoid garbage

data <- mails
data %<>% mutate(id_f = match(data$username_from,
                              sapply(strsplit(as.character(nodes$emails),
                                              split = '@', fixed = T),
                                     function(x) (x[1]))))
data %<>% mutate(id_t = match(data$username_to,
                              sapply(strsplit(as.character(nodes$emails),
                                              split = '@', fixed = T),
                                     function(x) (x[1]))))
data %<>% select(id_f,id_t,is_suspiscius)

data$id_f <- as.integer(gsub(296 , 424 , data$id_f ))
data$id_f <- as.integer(gsub(294 , 424 , data$id_f ))
data$id_f <- as.integer(gsub(275 , 370 , data$id_f ))
data$id_f <- as.integer(gsub(286 , 370 , data$id_f ))
data$id_f <- as.integer(gsub(169 , 346 , data$id_f ))
data$id_f <- as.integer(gsub(127 , 412 , data$id_f ))

data$color <- ifelse(data$is_suspiscius == 1,"red","lightblue")
