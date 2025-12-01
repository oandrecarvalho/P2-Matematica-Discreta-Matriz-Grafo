data_csv <- read.csv(file.choose(), header = TRUE, sep = ",")

data_csv

inc_matrix <- xtabs(weight ~ from + to, data = data_csv)

class(inc_matrix)

inc_matrix <- unclass(inc_matrix)

class(inc_matrix)

print(inc_matrix)

s <-  inc_matrix %*% t(inc_matrix)

s

diag(s) <- 0

s

c <- t(inc_matrix) %*% inc_matrix

days <- c("SEGUNDA", "TERÇA", "QUARTA", "QUINTA", "SEXTA")

c <- c[days, days]

diag(c) <- 0
c


install.packages('igraph')
library('igraph')


inc_graph <- graph_from_biadjacency_matrix(inc_matrix, directed = T, mode = "out", weighted = T)
sim_graph <- graph_from_adjacency_matrix(s, weighted = T, mode = "undirected")
co_graph <- graph_from_adjacency_matrix(c, weighted = T, mode = "undirected", diag = FALSE)

plot(inc_graph, edge.width=E(inc_graph)$weight, edge.arrow.size=0.5)
plot(sim_graph, edge.width=E(sim_graph)$weight)
plot(co_graph, edge.width=E(co_graph)$weight)


# --- INC_GRAPH 
v_inc <- vcount(inc_graph)        # 28
m_inc <- ecount(inc_graph)        # 23
# grau médio 
grau_medio_total_inc <- 2*m_inc / v_inc
# densidade (direcionado sem laços)
densidade_dir_inc <- ecount(inc_graph) / (v_inc * (v_inc - 1))
# densidade bipartida 
n_pessoas <- 23
n_dias <- 5
densidade_bip <- m_inc / (n_pessoas * n_dias)
# força média 
forca_media_inc <- mean(E(inc_graph)$weight)

# --- SIM_GRAPH 
v_sim <- vcount(sim_graph)
m_sim <- ecount(sim_graph)
grau_medio_sim <- 2*m_sim / v_sim
densidade_sim <- edge_density(sim_graph, loops = FALSE) 
forca_media_sim <- mean(E(sim_graph)$weight) 
# --- CO_GRAPH 
v_co <- vcount(co_graph)          # 5
# arestas entre dias diferentes:
# se você criou co_graph a partir da matriz com só diagonal, egraph pode ter laços.
# para ignorar laços:
m_co_inter <- sum(E(co_graph)[!is.loop(co_graph)])  
grau_medio_co <- 2 * m_co_inter / v_co
densidade_co <- edge_density(co_graph, loops = FALSE)
# força média das arestas 
if(m_co_inter > 0) {
  forca_media_co <- mean(E(co_graph)[!is.loop(co_graph)]$weight)
} else {
  forca_media_co <- NA
}
mean(E(co_graph)[is.loop(co_graph)]$weight)

