
unifDAGslow <- function (n) 
{
  stopifnot(n > 1)
  if (n > 100) 
    stop("Use unifDAG only for n <= 100; for larger n use unifDAG.approx")
  r <- unifDAG:::sampleZ2(unifDAG:::.unifDagPreComp$a[n])
  K <- unifDAG:::findK.exact(n, r)
  Q <- unifDAG:::sampleQ(n, K)
  perm <- sample.int(n)
  adj <- Q[perm, perm]
  
  g <- igraph::graph_from_adjacency_matrix(adj)
  top.ord <- igraph::topological.sort(g)
  
  adj[top.ord, top.ord]
}

unifDAGfast <- function (n, n.exact = 20) 
{
  stopifnot(n > 1)
  if (n < n.exact) {
    # message("going for slow version\n")
    return(unifDAGslow(n))
  }
  K <- unifDAG:::findK.approx(n, n.exact)
  Q <- unifDAG:::sampleQ(n, K)
  perm <- sample.int(n)
  adj <- Q[perm, perm]
  
  g <- igraph::graph_from_adjacency_matrix(adj)
  top.ord <- igraph::topological.sort(g)
  
  adj[top.ord, top.ord]
}

gen_Gnp <- function(n, p) {
  A <- matrix(rbinom(n^2, size = 1, prob = p), ncol = n)
  upper.tri(A) * A
}

gen_Gnpq <- function(n, p, q) {
  
  A <- matrix(rbinom(n^2, size = 1, prob = p), ncol = n)
  A <- upper.tri(A) * A
  
  B <- matrix(rbinom(n^2, size = 1, prob = q), ncol = n)
  B <- upper.tri(B) * B
  B <- B + t(B)
  
  list(adj = A, cfd = B)
}

gen_scale_free <- function(n, m, directed = TRUE) {
  
  # generating the starting graph
  g0 <- graph_from_adjacency_matrix(adjmatrix = gen_Gnp(2 * m + 1, 1))
  
  g <- igraph::sample_pa(n, m = m, start.graph = g0)
  
  if (directed) {
    top.ord <- igraph::topological.sort(g)
    adj_mat <- as.matrix(igraph::get.adjacency(g))[top.ord, top.ord]
  } else {
    adj_mat <- as.matrix(igraph::get.adjacency(g))
  }
  
  adj_mat
}

gen_graph <- function(adj = c("Gnp", "unif", "scale_free"), 
                      cfd = c("Gnq", "prunning", "scale_free"), params) {
  
  adj <- match.arg(adj, c("Gnp", "unif", "scale_free"))
  cfd <- match.arg(cfd, c("Gnq", "prunning", "scale_free"))
  
  adj.mat <- switch(
    adj,
    Gnp = gen_Gnp(params$n, params$p),
    unif = unifDAGfast(params$n),
    scale_free = gen_scale_free(params$n, params$m)
  )
  
  if (cfd == "prunning") {
    
    cfd.mat <- adj.mat
    cfd.mat[,] <- 0
    g <- list(adj = adj.mat, cfd = cfd.mat)
    uc_idx <- rbinom(nrow(adj.mat), 1, prob = params$q)
    return(proj(g, uc_idx))
  } else if (cfd == "Gnq") {
    
    cfd.mat <- gen_Gnp(params$n, params$q)
    cfd.mat <- upper.tri(cfd.mat) * cfd.mat
    cfd.mat <- cfd.mat + t(cfd.mat)
  } else if (cfd == "scale_free") {
    
    cfd.mat <- gen_scale_free(params$n, params$pow_cfd, 
                              1, # params$zero_app_cfd, 
                              FALSE)
  }
  
  list(adj = adj.mat, cfd = cfd.mat)
}
