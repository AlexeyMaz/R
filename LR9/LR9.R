setwd("C:/Users/AlmaZ/RStudioProjects/R/LR8")
N = 6
G_size = sample((N + 10) : (I((N / 10 + 5)^2) + 5 * N), 1)

# 1
library(igraph)

g <- make_ring(G_size)
plot(g, layout = layout_(g, in_circle()))

vcount(g); ecount(g); g[]



# 2
g1 <- graph.empty() + vertices(1 : G_size, color="yellow")
g1 <- g1 + edges(sample(V(g1), N * 8, replace = TRUE), color="red")
plot(g1, edge.arrow.size = .2, vertex.size = 10)
g1[]

g1 <- g1 + edges(sample(V(g1), N * 10, replace = TRUE), color="blue")
plot(g1, edge.arrow.size = .2, vertex.size = 10)
g1[]



# 3
if ((2*N + 23) %in% V(g1) && (2*N + 20) %in% V(g1))
  g1 <- g1 + edge((2*N + 23), (2*N + 20), color = 'black')

if ((2*N + 12) %in% V(g1) && (N + 15) %in% V(g1))
  g1 <- g1 + edge((2*N + 12), (N + 15), color = 'black')

if ((2*N - 1) %in% V(g1) && (N + 8) %in% V(g1))
  g1 <- g1 + edge((2*N - 1), (N + 8), color = 'black')

if ((2*N) %in% V(g1) && (2*N + 1) %in% V(g1))
  g1 <- g1 + edge((2*N), (2*N + 1), color = 'black')

if ((N + 7) %in% V(g1) && (N + 13) %in% V(g1))
  g1 <- g1 + edge((N + 7), (N + 13), color = 'black')
plot(g1, edge.arrow.size = .2, vertex.size = 10)

neighbors(g1, V(g1)[N], mode = 3)
incident(g1,V(g1)[N], mode=c("all", "out", "in", "total"))
are.connected(g1, V(g1)[N + 10], V(g1)[N + 12])
g1[]



# 4
deg <- degree(g1, mode="all")

most_con_v <- which.max(deg)
g1 <- g1 + vertices(G_size + 1, color="green")
g1 <- g1 + edge((G_size + 1), most_con_v, color = 'orange')

V(g1)[1 : 26]$name <- LETTERS
V(g1)[27 : G_size]$name <- letters[1 : 25]; V(g1)[G_size + 1]$name <- 'z'
g1[]

deg <- degree(g1, mode="all")
deg[deg > 2 & deg < 5]
plot(g1, edge.arrow.size = .2, vertex.size = 10)



# 5
plot(g1, edge.arrow.size = .2, vertex.size = 10, layout = layout.circle)
plot(g1, edge.arrow.size = .2, vertex.size = 10, layout = layout_as_tree)
plot(g1, edge.arrow.size = .2, vertex.size = 10, layout = layout.grid)



# 6
diameter(g1)
all_shortest_paths(g1, N, to = V(g1), mode = c("out", "all", "in"),weights = NULL)
plot(g1, vertex.size=deg*10)