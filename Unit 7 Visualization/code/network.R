edges = read.csv("Desktop/Analytics Edge/Unit 7 Visualization/data/edges.csv")
users = read.csv("Desktop/Analytics Edge/Unit 7 Visualization/data/users.csv")

str(users)
str(edges)
table(users$locale)
table(users$school, users$gender)

library(igraph)
g = graph.data.frame(edges, directed = FALSE, vertices = users)
plot(g, vertex.size=5, vertex.label = NA)
table(degree(g))

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)
