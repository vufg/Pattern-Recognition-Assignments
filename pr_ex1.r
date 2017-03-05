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
#data is a list, each element of the list is a vector
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

J3 = function(data){
  d = dim(data[[1]])[2]
  X = matrix(,0,d)
  for(i in data){
    X = rbind(X,i)
  }
  return(sum(diag((solve(sw(data)) %*% cov(X)))))
}

X = list(x1,x2,x3,x4)
sw(X)
sb(X)
sm_cov = cov(rbind(x1,x2,x3,x4))
sm_cov
sm_sum = sw(X) + sb(X)
sm_sum

#a(iii)
J3(X)

#(b)
#generate the data
sigma = matrix(c(0.2,0,0,0.2), 2, 2)
x1 = rmvnorm(n=100, mean=c(-1,-1), sigma=sigma)
x2 = rmvnorm(n=100, mean=c(-1,1), sigma=sigma)
x3 = rmvnorm(n=100, mean=c(1,-1), sigma=sigma)
x4 = rmvnorm(n=100, mean=c(1,1), sigma=sigma)
X = list(x1,x2,x3,x4)
sw(X)
sb(X)
sm_cov = cov(rbind(x1,x2,x3,x4))
sm_cov
sm_sum = sw(X) + sb(X)
sm_sum
J3(X)

#(c)
#generate the data
sigma = matrix(c(3,0,0,3), 2, 2)
x1 = rmvnorm(n=100, mean=c(-10,-10), sigma=sigma)
x2 = rmvnorm(n=100, mean=c(-10,10), sigma=sigma)
x3 = rmvnorm(n=100, mean=c(10,-10), sigma=sigma)
x4 = rmvnorm(n=100, mean=c(10,10), sigma=sigma)
X = list(x1,x2,x3,x4)
sw(X)
sb(X)
sm_cov = cov(rbind(x1,x2,x3,x4))
sm_cov
sm_sum = sw(X) + sb(X)
sm_sum
J3(X)

#plotttttttttttttttttttttttttttttttttttt


#Problem 2
#generate the data randomly
sigma = diag(5)
sigma[1,1] = 0.5
sigma[2,2] = 0.5
sigma[5,5] = 1.5
a <- rmvnorm(n=100000, mean=rep(0,5), sigma=sigma)
b <- rmvnorm(n=100000, mean=c(0,2,2,3,3), sigma=sigma)

#(a)
for(i in seq(1,5)){
  X = list(matrix(a[,i],dim(a)[1],1), matrix(b[,i],dim(b)[1],1))
  print(J3(X))
}

#(b)
for(i in seq(1,4)){
  for(j in seq(i+1,5)){
    X = list(a[,c(i,j)], b[,c(i,j)])
    print(J3(X))
  }
}

#(c)
for(i in seq(1,3)){
  for(j in seq(i+1,4)){
    for(k in seq(j+1, 5)){
      X = list(a[,c(i,j,k)], b[,c(i,j,k)])
      print(J3(X))
    }
  }
}

#(d)

#Problem 3
#(a)i
sigma = diag(2)
a <- rmvnorm(n=100, mean=c(2,4), sigma=sigma)
b <- rmvnorm(n=100, mean=c(2.5,10), sigma=sigma)
data = rbind(a,b)

#(a)ii
FDR = function(x,y){
  return((mean(x)-mean(y))^2 / (var(x) + var(y)))
}

FDR1 = FDR(a[,1], b[,1])
FDR1
FDR2 = FDR(a[,2], b[,2])
FDR2

#(b)
sigma = 0.25*diag(2)
a <- rmvnorm(n=100, mean=c(2,4), sigma=sigma)
b <- rmvnorm(n=100, mean=c(2.5,10), sigma=sigma)
data = rbind(a,b)
FDR1 = FDR(a[,1], b[,1])
FDR1
FDR2 = FDR(a[,2], b[,2])
FDR2

#(c)
