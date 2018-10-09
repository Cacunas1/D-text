# Set up ------------------------------------------------------------------

rm(list = ls())

library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(lubridate)

# Load data ---------------------------------------------------------------

file_path <- "data/enron_training.RData"

load(file = file_path)

class_mails$Date %<>% ymd_hms()

class_mails <- as.tibble(class_mails)

# Split the receptor ------------------------------------------------------

class_mails$To <- ifelse(class_mails$To == "", "Unknown", class_mails$To)

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
mails <- mails[1:1200,]

nodes <- data.frame(nodos = union(unique(mails$From), unique(mails$to_single)))

# Cleaning data (names) ---------------------------------------------------

nodes["email"] <- nodes
nodes$nodos <- sub("@.*", "", nodes$nodos)

# Making it nice ----------------------------------------------------------

nodes <- tibble(id = rownames(nodes) %>% as.integer(),
                label = nodes$nodos,
                email = nodes$email)

# Subset mails to avoid garbage
data <- mails[1:1200,]

data %<>% mutate(id_f = match(data$username_from, nodes$label))
data %<>% mutate(id_t = match(data$username_to, nodes$label))
data %<>% select(id_f, id_t, is_suspiscius) %>% unique()

nodes[-c(1, 3)] %<>%
  lapply(gsub, pattern = ".", replacement = " ", fixed = TRUE)
nodes$label %<>% str_to_title()
data$color <- ifelse(data$is_suspiscius == 1, "red", "lightblue")

colnames(data)[3] <- "is_s"

save(nodes, data, file = "data/viz.RData")
save(class_mails, file = "data/class_mails.RData")
