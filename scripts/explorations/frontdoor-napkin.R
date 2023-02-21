
# front-door example
A <- B <- array(0, dim = c(3, 3))
A[1, 2] <- A[2, 3] <- 1
B[1, 3] <- B[3, 1] <- 1

g <- list(adj = A, cfd = B)

x <- 1
y <- 3

is_id(g, x, y, show_plot = TRUE)

check_bd(g, x, y, show_plot = TRUE)

# Napkin graph
A <- B <- array(0, dim = c(4, 4))
A[1, 2] <- A[2, 3] <- A[3, 4] <- 1
B[1, 3] <- B[3, 1] <- B[1, 4] <- B[4, 1] <- 1

g <- list(adj = A, cfd = B)

x <- 3
y <- 4
is_id(g, x, y, show_plot = TRUE)

check_bd(g, 3, 4, show_plot = TRUE)

# check marginalizer
A <- B <- array(0, dim = c(3, 3))
A[2, 3] <- 1
B[1, 3] <- B[3, 1] <- B[2, 1] <- B[1, 2] <- 1

g <- list(adj = A, cfd = B)
plot(graphModel(g$adj, g$cfd))

check_bd(g, 2, 3, TRUE)

g <- proj_single(g, 1)
plot(graphModel(g$adj, g$cfd))
