library(visNetwork)

load(file = "data/viz.RData")

nodes <- data.frame(id = users$id,
                    label = users$name)
edges <- data.frame(from = connections$sender,
                    to = connections$receiver)
visNetwork(nodes, edges, width = "100%")
