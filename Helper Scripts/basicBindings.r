nonzeroRequirment = function(varnames) {
  
  len = length(varnames)
  vars = vector(mode = "integer", length = len*len)
  mat = matrix(vars, nrow = len, ncol = len, byrow=TRUE,
               dimnames = list(
                 n=sprintf("req.gt0%s",varnames)))
  
  for(i in 1:len) {
    mat[i,i] = -1
  }
  return(mat)
}

nonzeroConstraint = function(varnames) {
  
  len = length(varnames)
  vars = vector(mode = "integer", length = len)
  mat = matrix(vars, nrow = len, ncol = 1, byrow=TRUE,
               dimnames = list(
                 n=sprintf("con.gt0%s",varnames)))

  return(mat)
}
