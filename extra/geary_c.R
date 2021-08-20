
N <- sum(!is.na(resid.lme)) - 1
keep <- !is.na(resid.lme)  
weights <- nb2listw(xy.rook, style = "B") # weights are all one

num <- lapply(1:nrow(Nin), function(x) {
  xi <- resid.lme[x]
  xj <- resid.lme[weights$neighbours[x][[1]]]
  if(weights$style == "B") squares = (xi - xj)^2
  else squares = weights$weights[x][[1]] * (xi - xj)^2
  return(squares)
})

num_sum <- sum(unlist(num[keep]), na.rm = T)

den_sum <- sum((resid.lme[keep] - mean(resid.lme, na.rm = T))^2)

twoW <- 2 * sum(unlist(weights$weights)[keep])

C = (N* num_sum)/(twoW * den_sum)