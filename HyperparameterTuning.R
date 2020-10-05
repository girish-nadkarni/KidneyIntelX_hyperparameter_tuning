#This is for Hyper-parameter tuning

#Take 10 fold data and save train and test AUC
#Tune parameters

depth <- c(2,3,5,6,8,10) #nodedepth
n_var <- c(2,3,4,5,10) #mtry
n_nodesize <- c(2,3,6,10,15,20) #nodesize

parameterResult <- data.frame(matrix(NA, nrow =6*5*6 , ncol = 5))
names(parameterResult) <- c("depth", "noVars","nodeSize", "TrainAUC", "TestAUC")

count_n <- 0

for (i in 1:length(depth))
{
  for (j in 1:length(n_var))
  {
    for (k in 1:length(n_nodesize))
    {
      count_n <- count_n + 1
      
      
      
smp_size <- floor(0.7 * nrow(data2))

TenFoldResult <- data.frame(matrix(NA, nrow =10 , ncol = 2))
names(TenFoldResult) <- c("TrainAUC", "TestAUC")

for (ii in 1:10)
{
  #Train vs Test
  set.seed(ii*3)
  train_ind <- sample(seq_len(nrow(data2)), size = smp_size)
  train <- data2[train_ind, ]
  test <- data2[-train_ind, ]
  
  #Model
  fitrsf <- rfsrc(fitform_selected, data = train, forest = TRUE,  nodedepth=depth[i],
                  mtry=n_var[j], nodesize=n_nodesize[k], ntree=100)
  
  #Prediction
  pred.train <- predict(object = fitrsf, newdata = train)
  pred.test <- predict(object = fitrsf,newdata = test)
  
  #AUC
  accuracyTrain <- stdROC(as.numeric(1-pred.train$predicted), train$requiredTarget, "ROC (Training Data)")
  accuracyTest <- stdROC(as.numeric(1-pred.test$predicted), test$requiredTarget, "ROC (Testing Data)")

  #Save Results
  TenFoldResult$TrainAUC[ii] <- as.numeric(accuracyTrain[1])
  TenFoldResult$TestAUC[ii] <- as.numeric(accuracyTest[1])

  
  #print(i)
}

parameterResult$depth[count_n] <- depth[i]
parameterResult$noVars[count_n] <- n_var[j] 
parameterResult$nodeSize[count_n] <- n_nodesize[k]

parameterResult$TrainAUC[count_n] <-  mean(TenFoldResult$TrainAUC)
parameterResult$TestAUC[count_n] <- mean(TenFoldResult$TestAUC)

}}}
