#Here we go.
###Noah Yasarturk, Assignment 2, Pimental-Alarcon Intro to Machine Learning

#In this assignment, we are looking for the minimizer of our function (the vector x that minimizes f(x))

#Instantiate our matrix A and vector b
A = matrix(c(1,1,1,1,1, 1,2,3,4,5, 1,4,9,16,25, 1,8,27,64,125, 1,16, 81,256,625), nrow=5, ncol=5)
b = c(5,31,121,341,781)

#  We are looking for a vector x that best minimizes the square of the L2 Norm.

#  What will x look like? 
## x wil be a 5 x 1 matrix (vector) to satisfy the function Ax-b.

#  What would y look like?
## y = (Ax-b).

#  What would the L2-norm look like?
## The L2 norm is the absolute value of each element of the vector squared and then summed together, giving us a sum that will be placed under a radicand.
## We would be taking the L2-norm of a [5x5] * [5x1] - [5x1] = [5x1] result matrix y, giving us a single number, and then squaring it.

#  What does the L2-norm squared look like?
## L2-norm squared would be the square of the L2-norm of y. 

#  Select an initial point x0. We want x to be made s.t. the result of it times A minus b produces the least value possible.
x0 = matrix(c(1,1,1,1,1))

#  t=0
yp = (A%*%x0)
y = (yp-b)
## This gives us [0,0,0,0,0], meaning the squared L2 norm would be the lowest with this x0. Now, we have to show how we'd arrive at this with gradient descent.

#  Set x to a larger set of values.
x0 = matrix(c(1000,1000,1000,1000,1000))

#  Create a function that takes as input our y and outputs the gradient of the squared-L2 norm of y.
grad <- function(y){
  result = 0
  #y will be a 5 x 1 matrix
  for(i in 1:5){
    result = result + 2*abs(y[i])
  }
  return(result)
}

#  Make a function that finds y when fed x
findY <- function(x,A,b){
  #A and b should remain constant
  #we are finding y=Ax-b
  result = c(0,0,0,0,0)
  left = (A%*%x)
  result = (left - b)
  return(result)
}

# Make a function that finds f(x)
findF <- function(y){
  result = 0
  for(i in 1:5){
    result = (y[i])^2 + result
  }
  return(result)
}

#  Create a function that takes step size n and and xt-1 as inputs and outputs xt.
descent <- function(xtb4, A, b, n){
  xt = c(0,0,0,0,0)
  left = xtb4
  gradMultiplier = grad(findY(xtb4,A,b))
  right = (n * gradMultiplier)
  xt = (left - right)
  return(xt)
}

#  Choose some step sizes.
n = c(0.00001, 0.0001, 0.001, 0.01, 0.1)

#  How will we know when our algorithm converges?
## We'll know when another "descent" using our step size will produce a squared L2-norm greater than our previous result.

#  Make a function that implements gradient descent as a function of our chosen x0 and step size.
gradDescent <- function(x0, A, b, n){
  # gradDescent should repeatively:
  # commence descent, giving us an xt to test. Store the prior xt-1 and the new xt, input each into f and test to see which one gives the smaller value.
  # If f(xt-1)<f(xt), we say (xt-1) is the superior x. If f(xt-1)>f(xt), we need to rerun the test with xt now being xt-1. 
  xtb4 = x0  #Our xt-1. Now, generate xt:
  xt = descent(x0, A, b, n)
  result = c(0,0,0,0,0)
  while( (findF(findY(xtb4,A,b))) > (findF(findY(xt,A,b))) ){ #While our newly calculated value (xt) is lesser, loop.
      if( (findF(findY(xtb4,A,b))) > (findF(findY(xt,A,b))) ){#If f(xt-1)>f(xt), our new value is a better argmin. 
        result = xt
        xtb4 = xt                                             #xt = xt-1
        xt = descent(xtb4,A,b,n)
      }
      if( (findF(findY(xtb4,A,b))) < (findF(findY(xt,A,b))) ){        #If f(xt-1)<f(xt), the old value is a better argmin.
        result = xtb4                                       
      }
  }
  return(result)
}

#  Now, we will test our gradient descent function.
## Construct a test matrix with 5 vectors to represent our intial point x0. Make another vector to deal with varying step sizes
testXs = matrix(c(10,10,10,10,10, 100,100,100,100,100, 1000,1000,1000,1000,1000, 10000,10000,10000,10000,10000, 5,12,45,780,57),nrow=5,ncol=5)
testXs = t(testXs)
testNs = c(0.1, 0.001, 0.001, 0.0001, 0.00001)
## Test the each x0 at each n
for(jj in 1:5){    
  for(kk in 1:5){
    print(gradDescent(testXs[jj,],A,b,testNs[kk]))
  }
}

#  We see that for all instances investigated above, a step size of 0.0001 or below is required for the algorithm to converge properly. Additionally, our varied x0
#  vector caused the descent to give us a very unique (a few negative) set of values. 
## We'll repeat this operation for step sizes below 0.0001.
test2Ns = c(0.0001, 0.000075, 0.00005, 0.000025, 0.00001)
for(jj in 1:5){    
  for(kk in 1:5){
    print(gradDescent(testXs[jj,],A,b,test2Ns[kk]))
  }
}
#  This saw near all values converge to 1 (meaning x=[1,1,1,1,1] is likely our most viable option for the argmin). Our algorithm had trouble with the varied vector
#  because it may be mathematically impossible to come up with a precise value for it.