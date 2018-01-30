rbindPattern = function(pattern) {
  
  varnames = ls(pattern = pattern, pos = 1)
  len = length(varnames)
  vars = vector(mode = "list", length = len)
  
  for(i in 1:len) {
    vars[[i]] = get(varnames[i])
  }
  
  mat = do.call(rbind, vars)
  rownames(mat) = varnames
  
  rm(varnames, len, i, vars)
  return(mat)
}