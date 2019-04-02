#Noah Yasarturk, Mini-Project 1

#Create matrix of size 4 x 100 and populate it with 0's
X = matrix(
  0L, nrow = 4, ncol = 100
)
X
#Populate the first row with i.i.d. random variables N (165, 25) (simulating height, assumedly in centimeters)
c <- rnorm(100,165,25)
X[1,] = c
#Populate the second row with i.i.d. random variables N (137, 100) (simulating weight, assumedly in pounds)
c2 <- rnorm(100,137,100)
X[2,] = c2
X
#Does the data make sense? Are all values reasonable?
print("While the height data makes sense (assuming it is height in cm), the weight data does not make sense, as it includes negative values.")


##Here I spent hours attempting to clean the data with consideration to impossible BMIs:
##print("We also need to control for BMIs that are actually impossible (for instance, being 3 lbs at 6 foot 2).")
###How would you clean/preprocess the data?
##print("We will replace any negative values with a new random value following the mean and standard deviation provided.")
##print("We will then calculate the BMIs for all individuals and replace any impossible BMIs by replacing the weight variable until a possible value is reached.")
##print("The smallest BMI is from Kate Chilver at 12.4; the largest BMI is from Jon Brower Minnoch at 52.0.")
##BMI <- function(ht,wt){
##  ht <- ht * 100 #convert centimeters to meters
##  wt <- wt * 0.45359237 #convert pounds to kilograms
##  return(wt/(ht*ht)) #BMI formula
##}
##allGood <- FALSE
##while(allGood == FALSE){
##  #create list of BMIs for respective individuals
##  bmiL <- vector("list",100)
##  for(j in 1:100){
##    bmiL[j] <- BMI(X[1,j],X[2,j])
##  }
##  #check if the weight is negative or if the BMI is impossible; if so, replace the weight with a new random value
##  for(i in 1:100){
##    if(X[2,i]<0||bmiL[i]<12.4||bmiL[i]>52){
##      X[2,i] <- rnorm(1,137,100)
##    }
##  }
##  #recalculate BMIs
##  for(k in 1:100){
##    bmiL[k] <- BMI(X[1,k],X[2,k])
##  }
##  #recheck to see if new random variables satisfy our conditions
##  for(ii in 1:100){
##    if(X[2,ii]<0||bmiL[ii]<12.4||bmiL[ii]>52){
##      allGood <- FALSE
##    }else{
##      allGood <- TRUE
##    }
##  }
##}
##X

#How would you clean/preprocess this data?
print("For simplicity's sake, we will simply multiply all negative values by -1 to make them appropriate values.")
for(i in 1:100){
  if(X[2,i]<0){
    X[2,i] <- X[2,i] * -1
  }
}
#Visualizing this data:
plot(X[1,],X[2,], main = "Height Compared to Weight",xlab="Height in Centimeters",ylab = "Weight in Pounds")
#Histogram of Weight/Height Ratio
whRatio <- numeric(100)
for(j in 1:100){
  whRatio[j]<- X[2,j]/X[1,j]
}
hist(whRatio, main = "Ratio of Weight/Height",xlab="Weight(in lbs)/Height(in cm)")
#Modelling glucose level
stdDev <- sd(whRatio)
c3 <- rnorm(100,0,(stdDev * stdDev))
glucLvl <- numeric(100)
for(k in 1:100){
  glucLvl[k] = (c3[k]+whRatio[k])
}
X[3,] <- glucLvl
X
#Fill out row 4 of the matrix with Tau as any arbritrary number
t <- 1
for(ii in 1:100){
  if(X[3,ii]<t){
    #the person is healthy
    X[4,ii] <- 0
  }else{
    #the person is diabetic
    X[4,ii]<- 1
  }
}
X
#Visualize the clustered data (in two dimensions) for different values of tau and sigma
#We will plot the weight/height ratio on the x-axis and glucose level on the y-axis, with a line being used to indicate whether someone is diabetic or not
#tau=1,sigma= 0.5295489
plot(whRatio,glucLvl,main = "Diabetes Diagnosis as Dependent on Weight/Height Ratio",sub ="tau = 1, sigma = 0.5295489",xlab = "Weight(in lbs)/Height(in cm) Ratio",ylab="Glucose Level")
abline(h=1,col=c("blue"))
legend("topleft",legend=c("Above the blue line = DIABETIC"))
#tau=1.5,sigma= 0.5295489
plot(whRatio,glucLvl,main = "Diabetes Diagnosis as Dependent on Weight/Height Ratio",sub ="tau = 1.5, sigma = 0.5295489",xlab = "Weight(in lbs)/Height(in cm) Ratio",ylab="Glucose Level")
abline(h=1.5,col=c("blue"))
legend("topleft",legend=c("Above the blue line = DIABETIC"))
#tau=0.5,sigma= 0.5295489
plot(whRatio,glucLvl,main = "Diabetes Diagnosis as Dependent on Weight/Height Ratio",sub ="tau = 0.5, sigma = 0.5295489",xlab = "Weight(in lbs)/Height(in cm) Ratio",ylab="Glucose Level")
abline(h=0.5,col=c("blue"))
legend("topleft",legend=c("Above the blue line = DIABETIC"))
#tau=1,sigma=0.75
c3 <- rnorm(100,0,(0.75*0.75))
glucLvl<-numeric(100)
for(jj in 1:100){
  glucLvl[jj]=(c3[jj]+whRatio[jj])
}
X[3,]<-glucLvl
plot(whRatio,glucLvl,main = "Diabetes Diagnosis as Dependent on Weight/Height Ratio",sub ="tau = 1, sigma = 0.75",xlab = "Weight(in lbs)/Height(in cm) Ratio",ylab="Glucose Level")
abline(h=1,col=c("blue"))
legend("topleft",legend=c("Above the blue line = DIABETIC"))
#tau=1,sigma=0.25
c3 <- rnorm(100,0,(0.25*0.25))
glucLvl<-numeric(100)
for(kk in 1:100){
  glucLvl[kk]=(c3[kk]+whRatio[kk])
}
X[3,]<-glucLvl
plot(whRatio,glucLvl,main = "Diabetes Diagnosis as Dependent on Weight/Height Ratio",sub ="tau = 1, sigma = 0.25",xlab = "Weight(in lbs)/Height(in cm) Ratio",ylab="Glucose Level")
abline(h=1,col=c("blue"))
legend("topleft",legend=c("Above the blue line = DIABETIC"))
