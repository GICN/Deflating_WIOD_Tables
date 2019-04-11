# Algorithm of the GRAS method, as in Temurshoev et al. 2013

# REFERENCES: 
# 1) Junius T. and J. Oosterhaven (2003), The solution of
# updating or regionalizing a matrix with both positive and negative
# entries, Economic Systems Research, 15, pp. 87-96.
# 2) Lenzen M., R. Wood and B. Gallego (2007), Some comments on the GRAS
# method, Economic Systems Research, 19, pp. 461-465.
# 3) Temurshoev, U., R.E. Miller and M.C. Bouwmeester (2013), A note on the
# GRAS method, Economic Systems Research, 25, pp. 361-367.

# Written by: Quentin Perrier (QuentinPerrier@Github.com)

# This work is a retranscription in R of the code written in Matlab by Temurshoev et al. 2013 in the third paper above.
# I thank them for providing the source code at https://fr.mathworks.com/matlabcentral/fileexchange/43231-generalized-ras--matrix-balancing-updating--biproportional-method?focused=3796073&tab=function


library(tidyverse)

GRAS <- function(Z0, u, v, tolerance = 0.01) {

  # Z0 base year matrix, not necessarily squared
  # u represent the exact row sums 
  # v represent the exact column sums 
  # r substitution effects (row multipliers)
  # s fabrication effects (column multipliers)
  
  #Check types of inputs
  if(!is.matrix(Z0)) {
    stop("Z0 is not a matrix")
  }
  
  if(!is.vector(u)) {
    stop("u is not a vector")
  }
  
  if(!is.vector(v)) {
    stop("v is not a vector")
  }
  
  #Define elements
  m <- nrow(Z0) #Dimensions
  n <- ncol(Z0)
  
  print("Initializing...")
  
  P <- Z0; P[Z0<0] <- 0
  N <- matrix(0, nrow = m, ncol = n); N[Z0<0] <- -Z0[Z0<0]
  
  #Keep names if they exist
  if (!is.null(row.names(Z0))) {rnames <- row.names(Z0)}
  if (!is.null(row.names(Z0))) {cnames <- colnames(Z0)}
  
  # function to be used later
  invd <- function(x) {
    # x is a vector
    vector <- c(x)
    out <- ifelse(vector == 0, 1, 1/vector)
    diag(out)
  }
  
  #Step 1. Start from a given initial matrix rˆ(0).
  #They recommend r(0) = i
  r <- list()
  r[[1]] <- rep(1,m)
  
  #Step 2. Calculate the matrix sˆ(1).
  s <- list()
  pr <- t(P) %*% r[[1]] 
  nr <- t(N) %*% invd(r[[1]]) %*% rep(1,m)
  s[[1]] <- invd(2*pr) %*% (v + sqrt(v^2 + 4*pr*nr))
  ss <-  - invd(v) %*% nr
  s[[1]][pr==0] <- ss[pr==0]
  
  Z <- list()
  Z[[1]] <- diag(r[[1]]) %*% P %*% diag(c(s[[1]])) - invd(r[[1]]) %*% N %*% invd(s[[1]])
  
  
  #Loop
  
  for (i in 2:10) {
    
    print(paste0("Starting loop ", i - 1))
    
    ps <- P %*% s[[i-1]]
    ns <- N %*% invd(s[[i-1]]) %*% rep(1,n)
    r[[i]] <- invd(2*ps) %*% (u + sqrt(u^2 + 4*ps*ns))
    rr <- -invd(u) %*% ns
    r[[i]][ps==0] <- rr[ps==0]
    
    pr <- t(P) %*% r[[i]]
    nr <- t(N) %*% invd(r[[i]]) %*% rep(1,m)
    s[[i]] <- invd(2*pr) %*% (v + sqrt(v^2 + 4*pr*nr))
    ss <-  -invd(v) %*% nr
    s[[i]][pr==0] <- ss[pr==0]
    
    error <- max(abs(s[[i]] - s[[i-1]]))
    if (error < tolerance) { 
      print(paste0("The GRAS algorithm converged with ", length(s) - 1, " iterations.")) # minus 1 because the first one is not an iteration
      break 
    } else if (i == 10) {
      print(paste0("Max error is now ", round(error, 3), ". Exiting GRAS"))
    } else {
      print(paste0("Max error is now ", round(error, 3)))
    }
  }
  
  iter_max <- length(s)
  ps <- P %*% s[[iter_max]]
  ns <- N %*% invd(s[[iter_max]]) %*% rep(1,n)
  r[[iter_max+1]] <- invd(2*ps) %*% (u + sqrt(u^2 + 4*ps*ns))
  rr <- -invd(u) %*% ns
  r[[iter_max+1]][ps==0] <- rr[ps==0]
  Z[[i]] <- diag(c(r[[iter_max+1]])) %*% P %*% diag(c(s[[iter_max]])) - invd(r[[iter_max+1]]) %*% N %*% invd(s[[iter_max]])
  
  #Output
  Zf <- Z[[length(Z)]] 
  
  colnames(Zf) <- cnames
  row.names(Zf) <- rnames
  
  Zf
  
} 


# Example   ---------------------------------------------------------------

# 
# Z0 <- tibble(Goods = c(7,2,-2), 
#              Services = c(3,9,0),
#              Consumption = c(5,8,2),
#              Exports = c(-3,1,1)) %>% 
#   as.matrix()
# #And we suppose that the new information is
# u <- c(15, 26, -1) # #Rowsum; u0 = Ai
# v <- c(9, 16, 17, -2) #Colsum; v0 = iA
# 
# tolerance <- 0.01 #tolerance level
# 
# GRAS(Z0, u, v, tolerance = 0.01)
