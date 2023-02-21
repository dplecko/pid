
n <- 50
p <- 0.75
for (i in seq_len(10^3)) {
  
  set.seed(i)
  g <- gen_Gnp(n, p)
  v <- sample.int(n, 1)
  
  if (!setequal(getDescendants(v, g), getDescendants_ig(v, g))) {
    cat("Error in try", i, "\n")
    break
  }
}

microbenchmark::microbenchmark(
  {
    g <- gen_Gnp(n, p)
    v <- sample.int(n, 1)
    getDescendants(v, g)
  }
)

microbenchmark::microbenchmark(
  {
    g <- gen_Gnp(n, p)
    v <- sample.int(n, 1)
    getDescendants_ig(v, g)
  }
)

n <- 10
p <- 0.9
q <- 0.25
for (i in seq_len(10^3)) {
  
  set.seed(i)
  g <- gen_Gnpq(n, p, q)
  xy <- sort(sample.int(n, 2, replace = FALSE))
  x <- xy[1]
  y <- xy[2]
  
  if (is_id(g, x, y) != is_id_fast(g, x, y)) {
    cat("Error in try", i, "\n")
    break
  }
}

microbenchmark::microbenchmark(
  {
    set.seed(i)
    g <- gen_Gnpq(n, p, q)
    xy <- sort(sample.int(n, 2, replace = FALSE))
    x <- xy[1]
    y <- xy[2]
    is_id(g, x, y)
  }
)

microbenchmark::microbenchmark(
  {
    set.seed(i)
    g <- gen_Gnpq(n, p, q)
    xy <- sort(sample.int(n, 2, replace = FALSE))
    x <- xy[1]
    y <- xy[2]
    is_id_fast(g, x, y)
  }
)

