#<---------------KNN from Scratch-------------->

setwd("D:\\downloads")  #set the working directory
#Taking sample data to perform knn algorithm. 
iris = read_csv("iris _2.csv")
X = iris[c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')]
y = iris['Species']
#build_scales to scale
library(caret)
set.seed(100)
test_inds = createDataPartition(y = 1:length(y[[1]]), p = 0.2, list = F)

# Split data into test/train using indices
X_test = X[test_inds,]
y_test = y[test_inds,] 
X_train = X[-test_inds,]
y_train = y[-test_inds,]



#Function to calculate the distance
distance_fn <- function(x,y){
  d = dist(rbind(x, y))
  return(d)
}

#Function to get the nearest data points
nn_fn <- function(X_train, y_train, x_star,K){
  temp_data = data.frame()
  k<-c()
  min_dist <- 2/0
  distance = vector()
  for (i in 1:dim(X_train)[1]) {
    distance <- append(distance_fn(x_star,X_train[i,]),distance)
  }
  temp_data = cbind(X_train,distance)
  newdata <- temp_data[order(distance),]
  index=data.frame(rownames(newdata[1:K,]))
  index = as.numeric(unlist(index))
  for (i in index) {
    k<-append(y_train[i,],k)
  }
  if(sapply(y_train, class) != "numeric")
  {
    uniqv <- unique(k)
    prediction=uniqv[which.max(tabulate(match(k, uniqv)))]
    return(prediction)
  }
  else{
    prediction= mean(k)
    return(prediction)
  }
}

#Function to get the predicted output
KNN_fn <- function(X_train,y_train,X_test){
  K <- readline(prompt="Enter the value of K: ")
  K <- as.integer(K)
  pred_val <- vector()
  for (i in 1:dim(X_test)[1]) {
    y_hat_star = nn_fn(X_train, y_train, X_test[i,],K)
    pred_val<-append(y_hat_star,pred_val)
    }
  h<-data.frame(matrix(unlist(pred_val), nrow=length(pred_val), byrow=T))
  colnames(h)<-'prediciction'
  return(h)
}
pred = KNN_fn(X_train,y_train,X_test)





