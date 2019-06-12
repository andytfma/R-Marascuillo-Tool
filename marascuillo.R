## Enter proportions, sample size, and alpha.
p = c(.35, .16, .27, .05)
samplesize = 496
alpha = .05

N = length(p)
value = critvalue = comparison = c()
categories <- c(seq(1:N))
categories <- paste("cat", seq(1:N), sep="")
categories

## Compute critical values.
for (i in 1:(N-1)){ 
  for (j in (i+1):N){
    
    value <- c(value,(abs(p[i]-p[j])))
    critvalue = c(critvalue,
                       sqrt(qchisq(1-alpha, N-1))*sqrt(p[i]*(1-p[i])/samplesize + p[j]*(1-p[j])/samplesize))
    comparison = c(comparison, paste(categories[i], categories[j], sep = "-"))
  }
}
df <- as.data.frame(cbind(comparison, value, critvalue), stringsAsFactors = F)
df$value <- round(as.numeric(df$value),3) 
df$critvalue <- round(as.numeric(df$critvalue),3)
df$significant <- ifelse(df$value > df$critvalue, "SIG", "n.s.")

print(df)


