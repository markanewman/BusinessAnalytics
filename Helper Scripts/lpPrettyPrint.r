if (!require('stringr')) install.packages('stringr', quiet=TRUE)

#' Linear Programing Pretty Print
#'
#' \code{lpPrettyPrint} creates a list of prettly formated answers of an lp
#'
#' @param model a model object as returned by \code{\link{lpSolve::lp}}
lpPrettyPrint = function(model) {
  
  t1 = t(model$constraints)
  
  # extract the origional values
  d = dim(t1)
  lhs = t1[,1:(d[2]-2)]
  dir = dirnum2sym(t1[,(d[2]-1)])
  rhs = t1[,d[2]]
  dv = names(model$objective)
  if(is.null(dv)) { dv = sprintf("x%s",1:(d[2]-2)) }
  
  # objective function
  objfun = if(model$direction == 0) {"Minimize:"} else { "Maximize:"}
  objfun = paste(objfun, rbarpm(model$objective, dv, c("*","+")))
  
  # solution
  sol = paste(rbarpm(dv, model$solution, c("=",",")),"yealds", model$objval)
  
  # double check math
  lhs.res = vector(mode="numeric", length=d[1])
  for(i in 1:d[1]) {
    lhs.res[i] = rbarpmps(lhs[i,], dv)
  }
  dcm = cbind(lhs.res,dir,rhs)
  colnames(dcm) = c("lhs", "dir", "rhs")
  
  # double check result
  lhs.res = vector(mode="numeric", length=d[1])
  for(i in 1:d[1]) {
    lhs.res[i] = sum(lhs[i,]*model$solution)
  }
  dcr = cbind(lhs.res,dir,rhs)
  colnames(dcr) = c("result", "dir", "rhs")
  

  
  return(list(Objective=objfun, Solution=sol, DoubleCheckMath = dcm, DoubleCheckResult = dcr))
}

#' Linear Programing Pretty Print
#' 
#' \code{lppp} alias for \code{\link{lpPrettyPrint}}
lppp = function(model){return(lpPrettyPrint(model))}

#' rbar paste macro for (a[i],op[1],b[i],op[2])
rbarpm = function(a, b, op) {
  
  l = length(a)
  res = vector(mode = "character", length = l)
  for(i in 1:l) {
    res[i] = paste(a[i], op[1], b[i], op[2], sep="")
  }
  res = paste(res, collapse = '')
  res = substr(res, 0, stringr::str_length(res)-1)
  
  return(res)
}
#' rbar paste macro for (a[i],"*",b[i],"+") that elimates terms when available
rbarpmps = function(a, b) {
  
  l = length(a)
  res = vector(mode = "character", length = l)
  for(i in 1:l) {
    if(a[i] == 0) {
      # blank out term
    }
    else if(a[i] == 1) {
      res[i] = paste(b[i], "+", sep="")
    }
    else {
      res[i] = paste(a[i], b[i], "+", sep="")
    }
  }
  res = paste(res, collapse = '')
  res = substr(res, 0, stringr::str_length(res)-1)
  
  return(res)
}
dirnum2sym = function(num) {
  
  sym = c("<=", ">=", "=")
  
  l = length(num)
  res = vector(mode="character", length=l)
  for(i in 1:l) {
    res[i] = sym[num[i]]
  }
  
  return(res)
}
