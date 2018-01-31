#' From + To 2 DV
#'
#' \code{fromTo2DV} creates a list Descision variables
#'
#' @param from a vector of names of the "From" variables
#' @param to a vector of names of the To" variables
fromTo2DV = function(from, to) {
  
  l1 = length(from)
  l2 = length(to)
  vars = vector(mode = "character", length = (l1*l2))
  
  p = 1
  for(i in 1:l1) {
    for(j in 1:l2) {
      vars[p] = sprintf("%s%s", from[i], to[j])
      p = p + 1
    }
  }

  return(vars)
}


CapacityRequirments = function(from, to) {
  
  res = matrix(0, nrow=from, ncol = (from*to))
  
  for(i in 1:from) {
    for(j in 1:to) {
      res[i,(i-1)*to+j] = 1
    }
  }
  
  return(res)
}

DemandRequirments = function(from, to) {
  
  res = matrix(0, nrow=from, ncol = (from*to))
  
  for(i in 1:from) {
    v = rep_len(0,from)
    v[i] = 1
    res[i,] = rep_len(v, from*to)
  }
  
  return(res)
}
# c(1,0,0,1,0,0,1,0,0) 1,4,7
# c(0,1,0,0,1,0,0,1,0) 2,5,8
# c(0,0,1,0,0,1,0,0,1) 3,6,9

LinkRequirments = function(from, to, M) {
  
}
# * f1: c(-M,0,1,0,1,0,1,0)
# * f2: c(0,-M,0,1,0,1,0,1)
# 
# 
# * The form of the demand requirment for 2 "from"s and 3 "to"s is
# 
# * The form of the link requirment for 2 "from"s and 3 "to"s is
