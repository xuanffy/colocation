# pair_dist <- read.csv(file = "CA_sites_pair.csv", header = TRUE)
# pair_dist <- pair_dist[,-1]
load("CA_sites_pair.RData")
# pair_dist <- pair_dist + t(upper.tri(pair_dist))
n1 <- 410 # (1) - (n1) : prison
n2 <- 491 # (n1+1) - (n1+n2) : airport
n3 <- 155 # (n1+n2+1) - (n1+n2+n3) : military base
n4 <- 867 # (n1+n2+n3+1) - (n1+n2+n3+n4) : superfund sites
n5 <- 1212 # (n1+n2+n3+n4+1) - n : TRI sites
n <- n1 + n2 + n3 + n4 + n5

sites_info <- data.frame(row.names = c("pb", "ap", "mil", "sfs", "tri"),
                         count = c(n1, n2, n3, n4, n5),
                         id_start = c(1, n1+1, n1+n2+1, n1+n2+n3+1, n1+n2+n3+n4+1),
                         id_end = c(n1, n1+n2, n1+n2+n3, n1+n2+n3+n4, n))

######################################################

# Local CLQ
gaussian_kernel <- function(x) {
  return (exp(-x * 0.5) )
}

Local_CLQ_a_b <- function(a, b, n, loc, kernel = gaussian_kernel, nhb = 5) {
  cnt_a <- sites_info[a, 1]
  cnt_b <- sites_info[b, 1]
  a_start <- sites_info[a, 2]
  b_start <- sites_info[b, 2]
  a_end <- sites_info[a, 3]
  b_end <- sites_info[b, 3]
  # search for b sites that are within nhb centered at a
  r1 <- 0
  r2 <- 0
  for (i in seq(a_start, a_end)) {
    new_i <- which(loc == i)
    # the nearest site is always itself
    nearest <- order(pair_dist[new_i, ])
    d_ib <- pair_dist[new_i, nearest[nhb+1]] # bandwidth threshold defined by nhb-th nearest
    d_ij <- pair_dist[new_i, ]
    type_b <- (b_start <= nearest[2:(nhb+1)]) * (nearest[2:(nhb+1)] <= b_end)
    
    # Use kernel
    r2 <- r2 + sum(kernel(d_ij^2 / d_ib^2)) - 1 # remove the site itself from the calculation
    r1 <- r1 + sum(kernel(pair_dist[new_i, nearest[2:(nhb+1)]]^2 / d_ib^2) * type_b)
  }
  if (a == b) {cnt_b <- cnt_a - 1}
  return( (r1 / r2) / (cnt_b / n) )
}

# local test algorithm
sites <- c("pb", "ap", "mil", "sfs", "tri")
observed_LCLQ <- matrix(-1, nrow = 5, ncol = 5)
start_time <- Sys.time()
for (i in seq(1, 5)) {
  for (j in seq(1, 5)) {
    observed_LCLQ[i,j] <- Local_CLQ_a_b(sites[i], sites[j], n, seq(1, n))
  }
}
end_time <- Sys.time()
print(as.numeric(end_time - start_time, units = "secs"))
save(observed_LCLQ, file = "observed_LCLQ.RData")


n_perm <- 2500
ncores <- 50
registerDoParallel(ncores)

r <- foreach(i = 1 : ncores, .combine=rbind) %dopar% {
  block <- n_perm / ncores
  sites <- c("pb", "ap", "mil", "sfs", "tri")
  simulated_local_CLQ <- matrix(-1, nrow = block, ncol = 5 * 5) # last col for the global CLQ test
  for (k in seq(1, block)) {
    loc_perm <- sample(seq(1, n), replace = FALSE)
    for (i in seq(1,5)) {
      for (j in seq(1,5)) {
        simulated_local_CLQ[k, (i-1)*5+j] <- Local_CLQ_a_b(sites[i], sites[j], n, loc_perm)
      }
    }
  }
  simulated_local_CLQ
}

save(r, file = "Local_CLQ_simulated.RData")
