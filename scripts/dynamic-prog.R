
root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

n_cores <- get_cores()

# dynamic prog
n <- 20

p <- 0.9
q <- 0.1
dp <- array(0, dim = c(n+2, n+2, n+2, n+2))

for (i in 1+seq_len(n)) {
  
  for (a in seq_len(n+1)) {
    
    for (x in seq_len(n + 1)) {
      
      for (y in seq_len(n + 1)) {
        
        dp[i, a, x, y] <- (1-p)^a * dp[i-1, a, x, y] +
          (1 - (1 - p)^a) * (
            (1 - (1-q)^x) * (1-q)^y * dp[i-1, a+1, x+1, y] +
            (1 - q)^x * (1 - (1-q)^y) * dp[i-1, a+1, x, y+1] +
            (1 - (1-q)^x) * (1 - (1-q)^y) +
            (1-q)^x * (1-q)^y * dp[i-1, a+1, x, y] 
          )
      }
    }
  }
}

((1-q) * dp[n-1, 2, 1, 1] + q) * p
# dp[n+1, 2, 1, 1]
# dp[n, 2, 1, 1] * (1-q) + q
# dp[n-1, 2, 1, 1]

# simulation result

mns <- rep(0, 20)
nrep <- 10000
for (n_size in 1:16) {
  
  if (n_size < 2) next
  res <- mclapply(
    seq_len(nrep), 
    function(i) {
      g <- gen_Gnpq(n_size, p, q)
      check_bd(g, n_size-1, n_size)
    },
    mc.cores = n_cores
  )
  mns[n_size] <- mean(unlist(res))
  cat("\r", n_size)
}

idx <- which(mns > 0)

df <- data.frame(
  dyn = ((1-q) * dp[idx-1, 2, 1, 1] + q) * p,
  sim = 1 - mns[idx],
  n = idx
)

ggplot(melt(df, id.vars = "n"), aes(x = n, y = value, color = variable)) +
  geom_point() + geom_line() + theme_bw() +
  ggtitle("Dynamic Programming vs. Simulation") + ylab("P(not identifiable)") + 
  xlab("n nodes") +
  scale_color_discrete(name = "Approach", labels = c("Dynamic Prog.", "Simulation"))

ggsave(file.path(root, "results", "dynamic-vs-simulation.png"),
       height = 5, width = 8)

###

#' * check the correct answer for n = 3 *

nrep <- 10000
nexp <- 20
graph_size <- 12

res <- lapply(
  seq_len(nexp),
  function(i) {
    set.seed(i)
    xp <- vapply(
      seq_len(nrep), 
      function(x) {
        g <- gen_Gnpq(graph_size, p, q)
        !check_bd(g, graph_size - 1, graph_size)
      }, logical(1L)
    )
    c(mean(xp) - 1.96 * sd(xp) / sqrt(nrep), mean(xp) + 1.96 * sd(xp) / sqrt(nrep))
  }
) 

df <- data.frame(do.call(rbind, res), rep = seq_len(nexp))

ggplot(df, aes(x = rep, y = (X1 + X2)/2)) +
  geom_point() + geom_errorbar(aes(ymin = X1, ymax = X2)) + theme_bw() +
  geom_hline(yintercept = ((1-q) * dp[graph_size - 1, 2, 1, 1] + q) * p,
             color = "red", linetype = "dashed")

# p3 <- mean(three$X2)
# p3b <- 1.96 * p3 * (1-p3) / sqrt(10^5)
# c(1-p3 - p3b, 1-p3 + p3b)
# 
# three <- three[!duplicated(three),]

