
# testing the joint P(dexany, any)
n <- 8
p <- 0.5
joint <- jnt_prc(n, p)
nsamp <- 10^5
smp <- lapply(
  seq_len(nsamp),
  function(i) {
    
    adj <- gen_Gnp(n, p)
    desx <- c(1, getDescendants(1, adj))
    ancy <- c(n, getAncestors(n, adj))
    
    c(length(unique(intersect(desx, ancy))), length(unique(ancy)))
  }
)

smp_arr <- array(0, dim = c(n+1, n+1))
smp_arr <- index_from_0(smp_arr)
for (i in seq_len(nsamp)) {
  
  smp_arr[smp[[i]][1], smp[[i]][2]] <- smp_arr[smp[[i]][1], smp[[i]][2]] + 1
}
smp_arr <- smp_arr / nsamp

max(abs(smp_arr - joint))


## check difference
plot(as.vector(joint), pch = 19)
lines(as.vector(joint))

points(as.vector(smp_arr), pch = 19, col = "red")
lines(as.vector(smp_arr), col = "red")


# check if the difference is significant by repetitions
reps <- 20
itry <- rep(0, reps)
for (i in seq_len(reps)) {
  
  smp <- lapply(
    seq_len(nsamp),
    function(i) {
      
      adj <- gen_Gnp(n, p)
      desx <- c(1, getDescendants(1, adj))
      ancy <- c(n, getAncestors(n, adj))
      
      c(length(unique(intersect(desx, ancy))), length(unique(ancy)))
    }
  )
  
  itry[i] <- mean(vapply(smp, function(x) x[1] == 6 & x[2] == 6, logical(1L)))
  cat("\r", i)
}

plot(density(itry))
abline(v = joint[6, 6])

# testing P_id(x, y | p, q)
sim_pid_backdoor_xy <- function(x, y, p, q, nsamp = 10^4) {
  
  sim <- mclapply(
    seq_len(nsamp),
    function(i) {
      
      g <- gen_Gnpq(y, p, q)
      check_bd(g, x, y)
    }, mc.cores = get_cores()
  )
  
  mean(unlist(sim))
}

q_seq <- seq(0.1, 0.9, 0.1)
ngraph <- 5
par(mfrow = c(2, 2))
for (x in seq_len(ngraph - 1)) {
  
  pidxy <- pidxy_sim <- rep(0, length(q_seq))
  
  for (i in seq_along(q_seq)) {
    
    pidxy[i] <- pid_backdoor_xy(x, 5, 0.9, q_seq[i])
    pidxy_sim[i] <- sim_pid_backdoor_xy(x, 5, 0.9, q_seq[i])
  }
  
  plot(pidxy, pch = 19, ylim = c(0, 1))
  lines(pidxy)
  
  points(pidxy_sim, pch = 19, col = "red")
  lines(pidxy_sim, col = "red")
}


# testing chxany_prc
n <- 7
x <- 5
p <- 0.5
gtruth <- chxany_prc(n, x, p)[n, 0:n, 0:n]

nsamp <- 10^5
smp <- lapply(
  seq_len(nsamp),
  function(i) {
    
    adj <- gen_Gnp(n, p)
    
    chx <- which(adj[n-x+1, ] > 0)
    ancy <- c(n, getAncestors(n, adj))
    
    c(length(unique(intersect(chx, ancy))), length(unique(ancy)))
  }
)

smp_arr <- array(0, dim = c(n+1, n+1))
smp_arr <- index_from_0(smp_arr)
for (i in seq_len(nsamp)) {
  
  smp_arr[smp[[i]][1], smp[[i]][2]] <- smp_arr[smp[[i]][1], smp[[i]][2]] + 1
}
smp_arr <- smp_arr / nsamp

plot(as.vector(gtruth), pch = 19)
lines(as.vector(gtruth))

points(as.vector(t(smp_arr)), pch = 19, col = "red")
lines(as.vector(t(smp_arr)), col = "red")

# testing pcg_powj
nsamp <- 10000
r <- 5
j <- 3
q <- 0.25

smp <- lapply(
  seq_len(nsamp),
  function(i) {
    
    cfd <- gen_Gnp(r+j, q)
    diag(cfd) <- 0
    cfd <- 1 / 2 * (cfd + t(cfd))
    
    ccs <- lapply(seq_len(j), function(i) confoundedComponent(i, cfd))
    ccs <- do.call(c, ccs)
    length(unique(ccs)) == (r + j) # i.e., connected
  }
)

mean(unlist(smp))
pcg_powj(r, j, q)[r]

# testing pid_xy
nsamp <- 10^4
x <- 3
n <- 7
p <- 0.8
q <- 0.25

smp <- mclapply(
  seq_len(nsamp),
  function(i) {
    
    g <- gen_Gnpq(n, p, q)
    is_id(g, n-x+1, n)
  }, mc.cores = get_cores()
)

mean(unlist(smp))
pid_xy(x, n, p, q)

# debug the gap
sim_pid_backdoor_xy <- function(x, y, p, q, nsamp = 10^4) {
  
  sim <- mclapply(
    seq_len(nsamp),
    function(i) {
      
      g <- gen_Gnpq(y, p, q)
      check_bd(g, x, y)
    }, mc.cores = get_cores()
  )
  
  mean(unlist(sim))
}

p <- 0.5
q <- 0.2
n <- 8
pidxy <- pidxy_sim <- rep(0, n-1)
for (x in seq_len(n-1)) {
  
  pidxy[x] <- pid_backdoor_xy(x, n, p, q)
  pidxy_sim[x] <- sim_pid_backdoor_xy(x, n, p, q)
}

plot(pidxy, pch = 19, ylim = c(0, 1))
lines(pidxy)

points(pidxy_sim, pch = 19, col = "red")
lines(pidxy_sim, col = "red")


p <- 0.5
q <- 0.2
n <- 3

reps <- 20
old <- new <- rep(0, reps)
for (i in seq_len(reps)) {
  
  old[i] <- mean(vapply(seq_len(n-1), function(x) sim_pid_backdoor_xy(x, n, p, q), 
                        numeric(1L)))
  new[i] <- mean(prob_id_grid("Gnp", "Gnq", list(p = 0.5, q = 0.2, n = 3))$b_id)
  cat("\r", i)
}

plot(density(new))
lines(density(old))
abline(v = pid_backdoor(n, p, q))

# is pid biased?
p <- 0.5
q <- 0.2
n <- 8
x <- 1

reps <- 20
old <- new <- rep(0, reps)
for (i in seq_len(reps)) {
  
  new[i] <- sim_pid_backdoor_xy(x, n, p, q)
  cat("\r", i)
}

plot(density(new))
abline(v = pid_backdoor_xy(x, n, p, q), col = "red")

