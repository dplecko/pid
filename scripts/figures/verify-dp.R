
root <- rprojroot::find_root(".git/config")
r_dir <- file.path(root, "r")
cpp_dir <- file.path(root, "cpp")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))
invisible(lapply(list.files(cpp_dir, full.names = TRUE), sourceCpp))

# define the verification grid
grid <- list(p = c(0.5, 0.7, 0.8, 0.9), q = c(0.05, 0.1, 0.2), n = 3:15)

# run the simulation
nsamp <- 10^4
dt_sim <- prob_id_grid("Gnp", "Gnq", grid, nsamp)
dt_sim <- setnames(dt_sim, c("params1", "params2"), c("p", "q"))

# run the dp
dt_dp <- prob_id_dp(grid)

# go into plotting
dt_dp[, p := paste("p =", p)]
dt_dp[, q := paste("q =", q)]
dt_sim[, p := paste("p =", p)]
dt_sim[, q := paste("q =", q)]
ggplot(
  melt(dt_sim[, list(b_id = mean(b_id), g_id = mean(g_id)), by = c("p", "q", "n")], 
       id.vars = c("p", "q", "n")),
  aes(x = n, y = value, color = variable, fill = variable)
) +
  # geom_line() + # geom_point(size = 1) +
  geom_ribbon(aes(ymin = value - 2.58 * value * (1-value) / sqrt(nsamp),
                  ymax = value + 2.58 * value * (1-value) / sqrt(nsamp)),
              alpha = 0.4, linewidth = 0) +
  theme_bw() +
  facet_grid(q ~ p, scales = "free_y") +
  # facet_wrap(vars(p, q), scales = "free", nrow = 3) +
  geom_point(shape = 23, data = melt(dt_dp, id.vars = c("p", "q", "n")),
             aes(x = n, y = value, color = variable), size = 1) +
  xlab("Graph size") + ylab("P(identification)") +
  scale_color_discrete(labels = c("Back-door", "ID Algorithm"),
                       name = "ID Approach") +
  scale_fill_discrete(labels = c("Back-door", "ID Algorithm"),
                       name = "ID Approach") +
  theme(
    legend.position = "bottom"
  )

ggsave(file.path("paper", "figures", "verify-dp.png"),
       width = 12, height = 8)