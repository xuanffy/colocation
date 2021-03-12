library(doParallel)
library(foreach)

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

CLQ_a_b <- function(a, b, n, loc, nhb = 5) {
  cnt_a <- sites_info[a, 1]
  cnt_b <- sites_info[b, 1]
  a_start <- sites_info[a, 2]
  b_start <- sites_info[b, 2]
  a_end <- sites_info[a, 3]
  b_end <- sites_info[b, 3]
  # search for b sites that are within nhb centered at a
  C_a_b <- 0
  for (i in seq(a_start, a_end)) {
    # the nearest site is always itself
    nearest <- order(pair_dist[which(loc == i), ])[2:(nhb+1)]
    C_a_b <- C_a_b + sum( (b_start <= nearest) *(nearest <= b_end) )
  }
  if (a == b) {cnt_b <- cnt_a - 1}
  return( (C_a_b / (cnt_a * nhb) ) / (cnt_b / (n-1) ) )
}

# testing
# start_time <- Sys.time()
# CLQ_a_b("tri", "tri", n, seq(1,n))
# end_time <- Sys.time()
# print(as.numeric(end_time - start_time, units = "secs"))


CLQ_global <- function(n, loc, nhb = 5) {
  
  sites <- c("pb", "ap", "mil", "sfs", "tri")
  r1 <- 0 # numerator
  r2 <- 0 # denominator

  for (s in sites) {
    cnt <- sites_info[s, 1]
    r1 <- r1 + CLQ_a_b(s, s, n, loc, nhb) * (cnt-1) * (cnt * nhb) / (n-1)
    r2 <- r2 + sites_info[s, 1] * (sites_info[s, 1] - 1) / (n-1)
  }  
  
  return (r1/r2)
}

n_perm <- 2500
ncores <- 50
registerDoParallel(ncores)

r <- foreach(i = 1 : ncores, .combine=rbind) %dopar% {
  nhb <- 1
  block <- n_perm / ncores
  global_ <- (sites_info[,1]-1) * (sites_info[,1]) * nhb / (n-1)
  sites <- c("pb", "ap", "mil", "sfs", "tri")
  simulated_CLQ <- matrix(-1, nrow = block, ncol = 5 * 5 + 1) # last col for the global CLQ test
  for (k in seq(1, block)) {
    loc_perm <- sample(seq(1, n), replace = FALSE)
    for (i in seq(1,5)) {
      for (j in seq(1,5)) {
        simulated_CLQ[k, (i-1)*5+j] <- CLQ_a_b(sites[i], sites[j], n, loc_perm, nhb)
      }
    }
    simulated_CLQ[k, 26] <- (simulated_CLQ[k,1] * global_[1] + 
                             simulated_CLQ[k,7] * global_[2]+ 
                             simulated_CLQ[k,13]* global_[3] + 
                             simulated_CLQ[k,19]* global_[4] + 
                             simulated_CLQ[k,25]* global_[5]) / sum(global_)
  }
  simulated_CLQ
}

save(r, file = "CLQ_simulated.RData")

# global test algorithm
# sites <- c("pb", "ap", "mil", "sfs", "tri")
# observed_CLQ <- matrix(-1, nrow = 5, ncol = 5)
# start_time <- Sys.time()
# for (i in seq(1, 5)) {
#   for (j in seq(1, 5)) {
#     observed_CLQ[i,j] <- CLQ_a_b(sites[i], sites[j], n, seq(1, n))
#   }
# }
# end_time <- Sys.time()
# print(as.numeric(end_time - start_time, units = "secs"))


# library(hash)
# perm_n <- 10
# 
# MC <- hash()
# 
# for (s in sites) {
#   for (t in sites) {
#     MC[[paste(s, "-", t)]] <- vector('numeric', perm_n)
#   }
# }
# global_test_MC <- vector('numeric', perm_n)
# 
# for (k in seq(1, perm_n)) {
#   # each permutation of sites
#   loc_perm <- sample(seq(1, n), replace = FALSE)
#   sites <- c("pb", "ap", "mil", "sfs", "tri")
#   for (s in sites) {
#     for (t in sites) {
#       MC[[paste(s, "-", t)]][k] <- CLQ_a_b(s, t, n, loc_perm)
#     }
#   }
#   global_test_MC[k] <- CLQ_global(n, loc_perm)
# }

