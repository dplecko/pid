
root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

p <- 0.9
q <- 0.1
n <- 20

# size of the c-component, i.e. P( x, y connected in G(n, q) )
compute_rn <- function(n, p) {
  
  pn <- rn <- rep(0, n)
  
  pn[1] <- 1
  
  for (i in seq.int(2, n)) {
    
    smp <- smr <- 0
    for (k in seq.int(1, i-1)) {
      smp <- smp + choose(i-1, k-1) * pn[k] * (1-p)^(k * (i - k))
    }
    
    pn[i] <- 1 - smp
    
    for (k in seq.int(1, i-1)) {
      smr <- smr + choose(i-2, k-1) * pn[k] * (1-p)^(k * (i - k))
    }
    
    rn[i] <- 1 - smr
    
  }
  
  rn
}


dp <- array(0, dim = c(n, n))
dp[1, 1] <- 1 # Y \in \an(Y)
dp[2, 2] <- p # X \in \an(Y) with prob. p
dp[2, 1] <- 0 # X \notin \an(Y) w.p. 1-p, but avoid this case because always id

rn <- compute_rn(n, q)

# distribution of An(Y)
for (j in seq.int(3, n)) {
  
  for (i in seq.int(1, n)) {
    
    if (i == 1) {
      
      dp[j, i] <- dp[j-1,i] * (1 - p)^i
    } else {
      
      dp[j, i] <- dp[j-1,i] * (1 - p)^i + dp[j-1,i-1] * (1-(1-p)^(i-1))
    }
  }
}

dyn <- rep(0, n)
for (i in 2:n) {
  
  dyn[i] <- sum(dp[i, seq_len(i)] * rn[seq_len(i)])
}

mns <- rep(0, n)
nrep <- 1000
for (n_size in seq_len(n)) {
  
  if (n_size < 2) next
  res <- mclapply(
    seq_len(nrep), 
    function(i) {
      g <- gen_Gnpq(n_size, p, q)
      is_id(g, n_size-1, n_size)
    },
    mc.cores = get_cores()
  )
  mns[n_size] <- mean(unlist(res))
  cat("\r", n_size)
}

idx <- which(mns > 0)

df <- data.frame(
  dyn = dyn[idx],
  sim = 1-mns[idx],
  n = idx
)

ggplot(melt(df, id.vars = "n"), aes(x = n, y = value, color = variable)) +
  geom_point() + geom_line() + theme_bw() +
  ggtitle("Dynamic Programming vs. Simulation") + ylab("P(not identifiable)") + 
  xlab("n nodes") +
  scale_color_discrete(name = "Approach", labels = c("Dynamic Prog.", "Simulation"))

#####
ancestral_dp <- function(n, p) {
  
  dp <- array(0, dim = c(n, n))
  dp[1, 1] <- 1
  
  rn <- compute_rn(n, q)
  
  for (j in seq.int(3, n)) {
    
    for (i in seq.int(1, n)) {
      
      if (i == 1) {
        dp[j, i] <- dp[j-1,i] * (1 - p)^i
      } else {
        dp[j, i] <- dp[j-1,i] * (1 - p)^i + dp[j-1,i-1] * (1-(1-p)^(i-1))
      }
      
      
    }
  }
  
  dp[n, 1:n]
  
}

backdoor_sim <- function(m, n, p, q, nrep = 10^4) {
  
  res <- mclapply(
    seq_len(nrep), 
    function(i) {
      set.seed(i)
      g <- gen_Gnpq(n, p, q)
      if (i == 4818L) browser()
      !check_bd(g, m, n)
    },
    mc.cores = get_cores()
  )
  mean(unlist(res))
}

backdoor_prob <- function(m, n, p, q) {
  
  pxdiry <- 1 - sum(ancestral_dp(n-m-1, p) * (1-p)^(seq_len(n-m-1)))
  
  pxbidiry <- 1 - sum(ancestral_dp(n-m-1, p) * (1-q)^(seq_len(n-m-1)))
  
  dp <- array(0, dim = c(m+1, m+1))
  dp[1, 1] <- 1
  dp[2, 2] <- pxdiry
  dp[2, 1] <- 0
  
  rn <- compute_rn(m+1, q)
  
  for (j in seq.int(3, m+1)) {
    
    for (i in seq.int(1, m+1)) {
      
      if (i == 1) {
        dp[j, i] <- dp[j-1,i] * (1 - p)^i
      } else {
        dp[j, i] <- dp[j-1,i] * (1 - p)^i + dp[j-1,i-1] * (1-(1-p)^(i-1))
      }
      
      
    }
  }
  
  rn <- compute_rn(m+1, q)
  rn_alt <- (rn - q) / (1-q) * pxbidiry
  
  sum(dp[m+1, seq_len(m+1)] * rn_alt[seq_len(m+1)])
}

dp <- sim <- rep(0, 20)
idx <- 13:20
for (n in idx) {
  
  dp[n] <- backdoor_prob(n-5, n, p, q)
  sim[n] <- backdoor_sim(n-5, n, p, q)
  cat("\r", n)
}

df <- data.frame(dp = dp[idx], sim = sim[idx], graph_size = idx)

ggplot(melt(df, id.vars = "graph_size"), aes(x = graph_size, y = value, 
                                             color = variable)) +
  geom_point() + geom_line() + theme_bw() +
  ggtitle("Dynamic Programming vs. Simulation") + ylab("P(not identifiable)") + 
  xlab("n nodes") +
  scale_color_discrete(name = "Approach", labels = c("Dynamic Prog.", "Simulation"))





