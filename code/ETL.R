# Set up ------------------------------------------------------------------

rm(list = ls())

library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(lubridate)

# Load data ---------------------------------------------------------------

load(file = "data/enron_training.RData")

# Split the receptor ------------------------------------------------------

class_mails$Date %<>% ymd_hms()

class_mails <- as.tibble(class_mails)

class_mails %<>% distinct()

class_mails$To %<>% ifelse(. == "", "Unknown", .)

class_mails$From %<>% trimws(which = "both")

df <- class_mails$To %>% str_split(',')
df %<>% lapply(trimws, which = "both")

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

users <- data.frame(name = union(unique(mails$From), unique(mails$to_single)))
users$name %<>% sort()

# Cleaning data (names) ---------------------------------------------------

users$email <- users$name %>% trimws(which = "both")
users$name %<>% sub("@.*", "", .)
users$name %<>% lapply(gsub, pattern = ".", replacement = " ", fixed = TRUE)
users$name %<>% str_to_title()
users %<>% distinct(email, .keep_all = T)

# Making it nice ----------------------------------------------------------

users$id <- rownames(users) %>% as.integer()

# Substitutions for hardcoded case
users$name %<>% gsub('Phillip Allen', 'Charles McGill', .)
users$name %<>% gsub('Patti99', 'Howard Hamlin', .)
users$name %<>% gsub('Jeffrey Hodge', 'Ignacio Garcia', .)
users$name %<>% gsub('Jacquestc', 'Miguel Hernandez', .)

# Subset mails to avoid garbage
connections <- mails

connections %<>%
  mutate(sender = match(connections$username_from,
                        sapply(strsplit(as.character(users$email),
                                        split = '@', fixed = T),
                               function(x) (x[1]))))
connections %<>%
  mutate(receiver = match(connections$username_to,
                          sapply(strsplit(as.character(users$email),
                                          split = '@', fixed = T),
                                 function(x) (x[1]))))
connections %<>% select(sender, receiver, is_suspiscius)# %>% unique()

connections$sender %<>% as.integer(gsub(296 , 424 , .))
connections$sender %<>% as.integer(gsub(294 , 424 , .))
connections$sender %<>% as.integer(gsub(275 , 370 , .))
connections$sender %<>% as.integer(gsub(286 , 370 , .))
connections$sender %<>% as.integer(gsub(169 , 346 , .))
connections$sender %<>% as.integer(gsub(127 , 412 , .))

connections$color <- ifelse(connections$is_suspiscius == 1, "red", "lightblue")

colnames(connections)[3] <- "suspicious"

connections %<>% arrange(desc(suspicious))

connections_summ <- connections %>%
  group_by(sender, receiver) %>%
  summarise(count = n())
connections_dist <- distinct(connections, sender, receiver, .keep_all = T)
connections <- as.data.frame(merge(connections_dist, connections_summ))

#Email to User Name
class_mails$From_New <- sub("@.*", "", class_mails$From)
class_mails$From_New <- lapply(class_mails$From_New, gsub, pattern = ".", replacement = " ", fixed = TRUE)
class_mails$From_New <- str_to_title(class_mails$From_New)

#changing Names
class_mails$From_New %<>% gsub('Phillip Allen', 'Charles McGill', .)
class_mails$From_New %<>% gsub('Patti99', 'Howard Hamlin', .)
class_mails$From_New %<>% gsub('Jeffrey Hodge', 'Ignacio Garcia', .)
class_mails$From_New %<>% gsub('Jacquestc', 'Miguel Hernandez', .)

class_mails %<>% filter(To != "Unknown")

save(users, connections, class_mails, file = "data/viz.RData")
