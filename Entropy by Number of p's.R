#1.)
#Create 100 random values for p:
ps <- vector(mode="double", length=100)
ps <- runif(100, 0.0,1.0)
ps <- sort(ps, decreasing=FALSE)

#Create Entropy function
entropy <- function(p){
  #accepts as input the probability that a variable takes on a certain value
  inlog <- 1/p
  return(p*log2(inlog))
}

#Create entropies vector
es <- vector(mode="double", length=100)
for(i in 1:length(es)){
  es[i] <- entropy(ps[i])
}
plot(ps,es, main='Entropy at 100 p\'s', xlab='P(x=X)', ylab='Resulting Entropy')

#Repeat at 1000 p's
ps <- vector(mode="double", length=1000)
ps <- runif(1000, 0.0,1.0)
ps <- sort(ps, decreasing=FALSE)
es <- vector(mode="double", length=1000)
for(i in 1:length(es)){
  es[i] <- entropy(ps[i])
}
plot(ps,es, main='Entropy at 1000 p\'s', xlab='P(x=X)', ylab='Resulting Entropy')

#2.)
print('From this, we can conclude that Bernoulli variables with the greatest variability (being closer to 50%) will have the greatest entropy.')
print('Bernoulli variables with p\'s closer to either tail (1 or 0) have the least entropy.')