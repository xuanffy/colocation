# copied from CLQ_test.R
###########################################################
library(doParallel)
library(foreach)

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
###########################################################

load("CLQ_simulated_nhb=1.RData")
simulated_CLQ <- r

sites <- c("pb", "ap", "mil", "sfs", "tri")
if (TRUE) {
  load("CA_sites_pair.RData")
  observed_CLQ <- data.frame( row.names = sites,
                              "pb" = rep(-1, 5),
                              "ap" = rep(-1, 5),
                              "mil" = rep(-1, 5),
                              "sfs" = rep(-1, 5),
                              "tri" = rep(-1, 5))
  for (i in seq(1,5)) {
    for (j in seq(1,5)) {
      observed_CLQ[i,j] <- CLQ_a_b(sites[i], sites[j], n, seq(1, n), nhb = 1)
    }
  }
  save(observed_CLQ, file = "observed_CLQ.RData")
}

load("observed_CLQ.RData")
global_observed_CLQ <- sum(diag(as.matrix(observed_CLQ))) / sum( (sites_info[,1]-1)*(sites_info[,1]) / (n-1) )


for (i in seq(1,5)) {
  for (j in seq(1,5)) {
    if (i != j) {
      q <- quantile(simulated_CLQ[,(i-1)*5+j], probs = c(0.025, 0.0975))
      print( paste( "results:", observed_CLQ[i,j] >= q[2], ",",
                    sites[i],"->",sites[j],"=", observed_CLQ[i,j], 
                    ", 2.5% =", q[1], ", 97.5% =", q[2]) )
    }
  }
}

q <- quantile(simulated_CLQ[,26], probs = c(0.025, 0.0975))
print( paste( "results:", global_observed_CLQ >= q[2], ",",
              sites[i],"->",sites[j],"=", global_observed_CLQ , 
              ", 2.5% =", q[1], ", 97.5% =", q[2]) )


