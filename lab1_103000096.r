library(mvtnorm)

#Problem 1

#(a)i
#generate the four data sets with given mean and covariance
sigma = matrix(c(0.2,0,0,0.2), 2, 2)
x1 = rmvnorm(n=100, mean=c(-10,-10), sigma=sigma)
x2 = rmvnorm(n=100, mean=c(-10,10), sigma=sigma)
x3 = rmvnorm(n=100, mean=c(10,-10), sigma=sigma)
x4 = rmvnorm(n=100, mean=c(10,10), sigma=sigma)

#(a)ii
#Input variable data, for the functions below, is a list, each element of the list is a vector
#sw is the function to compute Within-class scatter matrix
sw = function(data){
  N = 0
  d = dim(data[[1]])[2]
  for(i in data){
    N = N + dim(i)[1]
  }
  wc_matrix = matrix(0, d, d)
  for(i in data){
    wc_matrix = wc_matrix + (dim(i)[1] / N) * cov(i)
  }
  return(wc_matrix)
}

#sb is the function to compute Between-class scatter matrix
sb = function(data){
  d = dim(data[[1]])[2]
  N = 0
  data_all = matrix(,0,d)
  for(i in data){
    data_all = rbind(data_all, i)
    N = N + dim(i)[1]
  }

  #mean_all is the mean of all data
  mean_all = c()
  for(i in seq(1,d)){
    mean_all = c(mean_all, mean(data_all[,i]))
  }
  #transform mean_all into matrix type
  mean_all = matrix(mean_all, d, 1)
  
  bc_matrix = matrix(0, d, d)
  for(i in data){
    #mean_cla is the mean of each data set
    mean_cla = c()
    for(j in seq(1,d)){
      mean_cla = c(mean_cla, mean(i[,j]))
    }
    diff_matrix = matrix(mean_cla, d, 1) - mean_all
    bc_matrix = bc_matrix + (dim(i)[1] / N) * (diff_matrix %*% t(diff_matrix))
  }
  return(bc_matrix)
}

#J3 is the function to compute J3 critrion,
#Mixture scatter matrix in J3 function is computed from sample covariance matrix
J3 = function(data){
  d = dim(data[[1]])[2]
  X = matrix(,0,d)
  for(i in data){
    X = rbind(X,i)
  }
  return(sum(diag((solve(sw(data)) %*% cov(X)))))
}

#computer sw, sb, sm and J3
X = list(x1,x2,x3,x4)
#Sw scatter matrix is:
print(format(round(sw(X), 2)))
#Sb scatter matrix is:
print(format(round(sb(X), 2)))
sm_cov = cov(rbind(x1,x2,x3,x4))
sm_sum = sw(X) + sb(X)
#Sm scatter matrix is(cov form):
print(format(round(sm_cov, 2)))
#Sm scatter matrix is(sum form):
print(format(round(sm_sum, 2)))
#There are two ways to compute Sm scatter matrix. 
#One is sample covriance, the other is sum of Sw and sb.
#There is small numerical error between them.

#plot the data
par(mfrow = c(2,2))
plot(rbind(x1,x2,x3,x4))
title(paste("J3 value of dataset A:", format(round(J3(X), 2), nsmall = 2)))

#a(iii)
J3(X)

#(b)
#generate the data
sigma = matrix(c(0.2,0,0,0.2), 2, 2)
x1 = rmvnorm(n=100, mean=c(-1,-1), sigma=sigma)
x2 = rmvnorm(n=100, mean=c(-1,1), sigma=sigma)
x3 = rmvnorm(n=100, mean=c(1,-1), sigma=sigma)
x4 = rmvnorm(n=100, mean=c(1,1), sigma=sigma)

#computer sw, sb, sm and J3
X = list(x1,x2,x3,x4)

#Sw scatter matrix is:
print(format(round(sw(X), 2)))
#Sb scatter matrix is:
print(format(round(sb(X), 2)))
sm_cov = cov(rbind(x1,x2,x3,x4))
sm_sum = sw(X) + sb(X)
#Sm scatter matrix is(cov form):
print(format(round(sm_cov, 2)))
#Sm scatter matrix is(sum form):
print(format(round(sm_sum, 2)))

#plot the data
plot(rbind(x1,x2,x3,x4))
title(paste("J3 value of dataset B:", format(round(J3(X), 2), nsmall = 2)))

#(c)
#generate the data
sigma = matrix(c(3,0,0,3), 2, 2)
x1 = rmvnorm(n=100, mean=c(-10,-10), sigma=sigma)
x2 = rmvnorm(n=100, mean=c(-10,10), sigma=sigma)
x3 = rmvnorm(n=100, mean=c(10,-10), sigma=sigma)
x4 = rmvnorm(n=100, mean=c(10,10), sigma=sigma)

#computer sw, sb, sm and J3
X = list(x1,x2,x3,x4)

#Sw scatter matrix is:
print(format(round(sw(X), 2)))
#Sb scatter matrix is:
print(format(round(sb(X), 2)))
sm_cov = cov(rbind(x1,x2,x3,x4))
sm_sum = sw(X) + sb(X)
#Sm scatter matrix is(cov form):
print(format(round(sm_cov, 2)))
#Sm scatter matrix is(sum form):
print(format(round(sm_sum, 2)))

#plot the data
plot(rbind(x1,x2,x3,x4))
title(paste("J3 value of dataset C:", format(round(J3(X), 2), nsmall = 2)))



#Problem 2
#generate the data randomly
sigma = diag(5)
sigma[1,1] = 0.5
sigma[2,2] = 0.5
sigma[5,5] = 1.5
a <- rmvnorm(n=100, mean=rep(0,5), sigma=sigma)
b <- rmvnorm(n=100, mean=c(0,2,2,3,3), sigma=sigma)

#(a)
best_i = 0
best_j3 = 0
for(i in seq(1,5)){
  X = list(matrix(a[,i],dim(a)[1],1), matrix(b[,i],dim(b)[1],1))
  j3c = J3(X)
  if(j3c > best_j3){
    best_j3 = j3c
    best_i = i
  }
  print(paste("i:", i, "   J3 value:", format(round(J3(X), 4), nsmall = 4), sep = ""))
}
print(paste("The best combination of one feature is:", best_i))
print(paste("With a J3 value equals to", format(round(best_j3, 4), nsmall = 4)))

#(b)
best_i = 0
best_j = 0
best_j3 = 0
for(i in seq(1,4)){
  for(j in seq(i+1,5)){
    X = list(a[,c(i,j)], b[,c(i,j)])
    j3c = J3(X)
    if(j3c > best_j3){
      best_j3 = j3c
      best_i = i
      best_j = j
    }
    print(paste("i:", i," j:",j, "   J3 value:", format(round(J3(X), 4), nsmall = 4), sep = ""))
  }
}
print(paste("The best combination of one feature is:", best_i, best_j))
print(paste("With a J3 value equals to", format(round(best_j3, 4), nsmall = 4)))


#(c)
best_i = 0
best_j = 0
best_k = 0
best_j3 = 0
for(i in seq(1,3)){
  for(j in seq(i+1,4)){
    for(k in seq(j+1, 5)){
      X = list(a[,c(i,j,k)], b[,c(i,j,k)])
      j3c = J3(X)
      if(j3c > best_j3){
        best_j3 = j3c
        best_i = i
        best_j = j
        best_k = k
      }
      print(paste("i:", i," j:",j, " k:", k,"   J3 value:", format(round(J3(X), 4), nsmall = 4), sep = ""))
    }
  }
}
print(paste("The best combination of one feature is:", best_i, best_j, best_k))
print(paste("With a J3 value equals to", format(round(best_j3, 4), nsmall = 4)))

#(d)
#discussion of the result is in the report


#Problem 3
#import the libraries needed for plotting
library(ggplot2)
library(Hmisc)
library(dplyr)
library(reshape)
library(lme4)
library(nlme)

#(a)i
#generate the data
sigma = diag(2)
a <- rmvnorm(n=100, mean=c(2,4), sigma=sigma)
b <- rmvnorm(n=100, mean=c(2.5,10), sigma=sigma)

#(a)ii
#funtion for computing FDR value
FDR = function(x,y){
  return( (mean(x) - mean(y)) ^ 2 / (var(x) + var(y)) )
}
FDR1 = FDR(a[,1], b[,1])
FDR1
FDR2 = FDR(a[,2], b[,2])
FDR2

#(b)
sigma = 0.25*diag(2)
c = rmvnorm(n=100, mean=c(2,4), sigma=sigma)
d = rmvnorm(n=100, mean=c(2.5,10), sigma=sigma)
FDR3 = FDR(c[,1], d[,1])
FDR3
FDR4 = FDR(c[,2], d[,2])
FDR4

#visualize the two datasets
#color represents which dataset the point belongs to
type = c(rep("Set A",200), rep("Set B",200))
df = data.frame(rbind(a, b, c,d), type)
ggplot(data = df, aes(x=X1, y=X2)) +
  geom_point(aes(color=type))

#visualize dataset B
df2 = data.frame(rbind(c,d))
ggplot(data = df2, aes(x=X1, y=X2)) +geom_point()

#(c)
#discussion of the result is in the report