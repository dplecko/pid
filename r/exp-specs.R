
gspec <- list(
  list(
    adj = "Gnp",
    cfd = "Gnq",
    grid = list(p = seq(0.1, 0.9, 0.1), q = seq(0.1, 0.9, 0.1))
  ),
  list(
    adj = "Gnp",
    cfd = "prunning",
    grid = list(p = seq(0.1, 0.9, 0.1), p_prune = seq(0.1, 0.9, 0.1))
  ),
  list(
    adj = "unif",
    cfd = "Gnq",
    grid = list(q = seq(0.1, 0.9, 0.1))
  ),
  list(
    adj = "unif",
    cfd = "prunning",
    grid = list(p_prune = seq(0.1, 1, 0.1))
  ),
  list(
    adj = "scale_free",
    cfd = "Gnq",
    grid = list(pow = seq(0.25, 2.75, 0.25), q = seq(0.1, 0.9, 0.1))
  ),
  list(
    adj = "scale_free",
    cfd = "prunning",
    grid = list(pow = seq(0.25, 2.75, 0.25), p_prune = seq(0.1, 1, 0.1))
  )
)

fspec <- list(
  list(
    adj = "Gnp",
    cfd = "Gnq",
    grid = list(p = c(0, 1), q = c(0, 1))
  ),
  list(
    adj = "Gnp",
    cfd = "prunning",
    grid = list(p = c(0, 1), p_prune = c(0, 1))
  ),
  list(
    adj = "unif",
    cfd = "Gnq",
    grid = list(q = c(0, 1))
  ),
  list(
    adj = "unif",
    cfd = "prunning",
    grid = list(p_prune = c(0, 1))
  ),
  list(
    adj = "scale_free",
    cfd = "Gnq",
    grid = list(pow = c(0.5, 3), q = c(0, 1))
  ),
  list(
    adj = "scale_free",
    cfd = "prunning",
    grid = list(pow = c(0.5, 3), p_prune = c(0, 1))
  )
)

attr(fspec, "name") <- "full"

fspec_dps <- list(
  list(
    adj = "Gnp",
    cfd = "Gnq",
    grid = list(p = c(0.7, 1), q = c(0, 0.2))
  ),
  list(
    adj = "Gnp",
    cfd = "prunning",
    grid = list(p = c(0.7, 1), p_prune = c(0, 0.2))
  ),
  list(
    adj = "unif",
    cfd = "Gnq",
    grid = list(q = c(0, 0.2))
  ),
  list(
    adj = "unif",
    cfd = "prunning",
    grid = list(p_prune = c(0, 0.2))
  ),
  list(
    adj = "scale_free",
    cfd = "Gnq",
    grid = list(pow = c(0.5, 3), q = c(0, 0.2))
  ),
  list(
    adj = "scale_free",
    cfd = "prunning",
    grid = list(pow = c(0.5, 3), p_prune = c(0, 0.2))
  )
)

attr(fspec_dps, "name") <- "dps"