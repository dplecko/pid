
root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

puc_grid <- seq(0.1, 0.9, 0.1)
n <- 10
p <- 0.5
nrep <- 100
n_cores <- get_cores()
df <- NULL

for (p_uc in puc_grid) {
  
  res <- mclapply(seq_len(nrep), 
                  function(i) pid_single(n = n, p = p, p_uc = p_uc),
                  mc.cores = n_cores)
  df <- rbind(df, c(p_uc, colMeans(do.call(rbind, res))))
  cat("\r", p_uc)
}

df <- rbind(df, c(0, 1, 1))
df <- rbind(df, c(1, 0, 0))
df <- data.frame(df)
names(df) <- c("puc", "gen", "back")
df <- reshape2::melt(df, id.vars = "puc")