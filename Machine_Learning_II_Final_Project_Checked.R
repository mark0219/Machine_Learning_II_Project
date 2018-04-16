rm(list=ls())
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}

needed <- c("caret", "snow", "doParallel", "stringr")
installIfAbsentAndLoad(needed)

################
#Data Cleansing#
################

#column period means that the period the animal has been staying in the shelter
#outcome_age.days are rounded

cat_data = read.csv('aac_shelter_cat_outcome_eng.csv', sep = ',', na.strings = c("", "NA"))
str(cat_data)
sum(is.na(cat_data))    #checking total number of empty cells
sapply(cat_data, function(y) sum(length(which(is.na(y)))))  #checking which columns have NAs

cat_data = cat_data[!is.na(cat_data$outcome_type),]  #deleting rows where outcome_type = NA

#converting all the column names to lower case
colnames(cat_data) = tolower(colnames(cat_data))

#NOTE I included 'datetime' to exclude and 'color' because it had 155 levels
#I also included "animal_ID" because that shouldn't have been in there
columns_to_exclude = c('age_upon_outcome',
                       'animal_id',
                       'animal_type',
                       'date_of_birth',
                       'datetime',
                       'monthyear',
                       'outcome_subtype',
                       'sex_upon_outcome',
                       'cat.kitten..outcome.',
                       'count',
                       'periods',
                       'period.range',
                       'outcome_age_.years.',
                       'sex_age_outcome',
                       'dob_year',
                       'dob_month',
                       'dob_monthyear',
                       'outcome_month',
                       'outcome_year',
                       'outcome_weekday',
                       'outcome_hour',
                       'breed1',
                       'breed2',
                       'color1',
                       'color2')

#excluding columns that are either empty, repeatitive, or no predictive value
cat_data = cat_data[, -which(colnames(cat_data) %in% columns_to_exclude)]

#converting all factor columns to character columns for feature re-engineering
for (j in 1:ncol(cat_data)) {
  if (class(cat_data[,j]) == "factor") {
    cat_data[,j] = as.character(cat_data[,j])
  }
}

#re-labeling name column into binary: if it has a name 1, else 0
cat_data$name[cat_data$name != "NA"] = 1
cat_data$name[is.na(cat_data$name)] = 0

#relabeling y-variable Adoption, Return to Owner, and Rto-Adopt as 'Adopt'
#relabeling Transfer, Died, Euthanasia, Missing, and Disposal as 'Other'
unique(cat_data$outcome_type)
cat_data$outcome_type[cat_data$outcome_type == "Adoption"] <- 'Adopt'
cat_data$outcome_type[cat_data$outcome_type == "Return to Owner"] <- 'Adopt'
cat_data$outcome_type[cat_data$outcome_type == "Rto-Adopt"] <- 'Adopt'
cat_data$outcome_type[cat_data$outcome_type != 'Adopt'] <- 'Other'

#assigning outcome_age_days into different intervals
cat_data$period_of_stay = rep("Short", rep(nrow(cat_data)))
cat_data$period_of_stay[cat_data$outcome_age_.days. > 90] = 'Medium.Long'
cat_data$outcome_age_.days. = NULL

#assigning NotKnown to cell where it's "NA"
for (j in 1:ncol(cat_data)) {
  if (any(is.na(cat_data[,j]))) {
    cat_data[,j][is.na(cat_data[,j])] = "NotKnown"
  }
}

#Warning from SVM: These variables have zero variances: brindle, tricolor, agouti
cat_data$coat_pattern[cat_data$coat_pattern == 'brindle'] = 'rare_coat_pattern'
cat_data$coat_pattern[cat_data$coat_pattern == 'tricolor'] = 'rare_coat_pattern'
cat_data$coat_pattern[cat_data$coat_pattern == 'agouti'] = 'rare_coat_pattern'

#Clearing all the non-characters from each class in color column
cat_data$color[cat_data$color == '/'] = 'NotKnown'
cat_data$color = str_replace_all(str_replace_all(cat_data$color, '[[:punct:]]', ''), ' ', '')
cat_data$color = factor(cat_data$color, levels = unique(cat_data$color), ordered = F)

#converting all character columns back to factor columns
for (j in 1:ncol(cat_data)) {
  cat_data[,j] = as.factor(cat_data[,j])
}

#merging all the breed class < 50 observations into 'rare' class
cat_data$breed_merged = as.character(cat_data$breed)
names_to_be_merged = names(summary(cat_data$breed)[summary(cat_data$breed) < 50])
for (name in names_to_be_merged) {
  cat_data$breed_merged[cat_data$breed_merged == name] = 'rare_breed'
}

#deleting original breed column and converting new breed_merged column to factor
cat_data$breed = NULL
cat_data$breed_merged = as.factor(cat_data$breed_merged)

#merging all the coat class < 10 observations into 'rare_coat' class
cat_data$coat_merged = as.character(cat_data$coat)
coat_names_to_be_merged = names(summary(cat_data$coat)[summary(cat_data$coat) < 10])
for (name in coat_names_to_be_merged) {
  cat_data$coat_merged[cat_data$coat_merged == name] = 'rare_coat'
}

#deleting original coat column and converting new coat_merged column to factor
cat_data$coat = NULL
cat_data$coat_merged = as.factor(cat_data$coat_merged)

#merging all the color class < 15 observations into 'rare_color' class
number_of_levels = length(unique(cat_data$color))
cat_data$color_merged = as.character(cat_data$color)
color_names_to_be_merged = names(summary(cat_data$color, maxsum = number_of_levels)[summary(cat_data$color, maxsum = number_of_levels) < 18])
for (name in color_names_to_be_merged) {
  cat_data$color_merged[cat_data$color_merged == name] = 'rare_color'
}

#deleting original color column and converting new color_merged column to factor
cat_data$color = NULL
cat_data$color_merged = factor(cat_data$color_merged, levels = unique(cat_data$color_merged))

#specifying level of response variable
cat_data$period_of_stay = factor(cat_data$period_of_stay, levels = c('Short', 'Medium.Long'))
cat_data$outcome_type = factor(cat_data$outcome_type, levels = c('Adopt', 'Other'))

######################
#Customized Functions#
######################

getResults <- function(mytable){
  
  if (dim(mytable)[1] == 2) {
    FN <- mytable[2,1] / sum(mytable[2,])
    FP <- mytable[1,2] / sum(mytable[1,])
    TP <- mytable[2,2] / sum(mytable[2,])
    TN <- mytable[1,1] / sum(mytable[1,])
    OverallError <- (mytable[1,2] + mytable[2,1]) / sum(mytable)
    
    results <- list("FN" = FN,
                    "FP" = FP,
                    "TP" = TP,
                    "TN" = TN,
                    "OverallError" = OverallError)
    
    return(results)
    
  } else if (dim(mytable)[1] == 3) {
    
    OverallError <- 1 - (mytable[1,1] + mytable[2,2] + mytable[3,3]) / sum(mytable)
    
    return(OverallError)
  }
}

saveResults <- function(result_list, matrix) {
  
    if (result_list[[1]] %in% matrix[,1] &
        result_list[[2]] %in% matrix[,2] &
        result_list[[3]] %in% matrix[,3] &
        result_list[[4]] %in% matrix[,4] &
        result_list[[5]] %in% matrix[,5]) {
      
      warning('The results you are trying to save already exist in the matrix.')
      
    } else {
      
      row_temp <- c()
      
      for (i in 1:length(result_list)) {
        row_temp[i] <- result_list[[i]]
      }
      
      matrix <- rbind(matrix, row_temp)
    
    }

  return(matrix)
}

#Cost function for Adopt/Other
pop.yes <- sum(cat_data$outcome_type == 'Other')/nrow(cat_data)
pop.not_adopted <- sum(cat_data$outcome_type != 'Other')/nrow(cat_data)
#Period_of_stay "Medium-Long" is 1/"yes" and there are 14,091 of them
computeCost <- function (tp, tn, fp, fn) {
  p.yes_in_pop <- pop.yes
  p.not_adopted_in_pop <- pop.not_adopted
  adopt.cost <- 20 # adoption events cost
  other.cost <- 50 # finding foster care
  fp_cost <- 50 #Model says 'Other', so we find foster care but it's somehow adopted and we didn't show it at events
  fn_cost <- 70 #Model says 'Adopt', so we took it to the adoption events but it didn't get adopted, so we find later foster care
  
  exp.cost = tp*p.yes_in_pop*other.cost + 
    tn*(1-p.yes_in_pop)*adopt.cost +
    fn*p.yes_in_pop*fn_cost +
    fp*(1-p.yes_in_pop)*fp_cost 
  return(exp.cost)
}

#Cost function for Short/Medium-Long
pop.yes <- sum(cat_data$period_of_stay == 'Medium.Long')/nrow(cat_data)
pop.no <- sum(cat_data$period_of_stay != 'Medium.Long')/nrow(cat_data)
#Period_of_stay "Medium-Long" is 1/"yes" and there are 14,091 of them
computeCost.period_of_stay <- function (tp, tn, fp, fn) {
  p.yes_in_pop <- pop.yes
  p.not_adopted_in_pop <- pop.no
  short.cost <- 20 # adoption events cost
  medium.long.cost <- 50 # finding foster care
  fp_cost <- 50 #+ p.not_adopted_in_pop * 100# finding foster care but it's somehow adopted but we didn't show it at events
  fn_cost <- 70 #+ p.not_adopted_in_pop * 100 # adoption events + late foster care
  
  exp.cost = tp*p.yes_in_pop*medium.long.cost + 
    tn*(1-p.yes_in_pop)*short.cost +
    fn*p.yes_in_pop*fn_cost +
    fp*(1-p.yes_in_pop)*fp_cost 
  return(exp.cost)
}

#Cross validation specification
fitControl <- trainControl(method = "cv", 
                           number = 3,
                           allowParallel = T)

registerDoParallel(cores=detectCores())

###########################################################
# Predictive Modeling (outcome_type) as Response Variable #
###########################################################

###Taking a subset of data to train and compare model###
set.seed(0871)
train_idx = sample(1:nrow(cat_data), 8000)
cat_data_subset = cat_data[train_idx, -which(names(cat_data) %in% c('period_of_stay'))]
error_rates_outcome_type <- c()

################
#  SVM Linear  #
################

#Takes approximately 2302.11 sec
grid <- expand.grid(C = seq(0.01, 10, 0.5))

processing_time <- system.time({
  SVM.linear.cv <- train(outcome_type ~ .,
                          data = cat_data_subset,
                          method = 'svmLinear',
                          preProcess = c('center', 'scale'),
                          tuneGrid = grid,
                          trControl = fitControl)
})
processing_time

plot(SVM.linear.cv)
error_rates <- getResults(t(confusionMatrix(SVM.linear.cv, 'average')$table))
error_rates_outcome_type <- saveResults(error_rates, error_rates_outcome_type)

confusionMatrix(SVM.linear.cv, 'average')

################
#  SVM Radial  #
################

#Takes approximately 265.89 sec
grid <- expand.grid(C = seq(0.01, 10, 0.5), sigma = c(0.001, 0.01, 0.1))

processing_time <- system.time({
  SVM.radial.cv <- train(outcome_type ~ .,
                          data = cat_data_subset,
                          method = 'svmRadial',
                          preProcess = c('center', 'scale'),
                          tuneGrid = grid,
                          trControl = fitControl)
})
processing_time

plot(SVM.radial.cv)
error_rates <- getResults(t(confusionMatrix(SVM.radial.cv, 'average')$table))
error_rates_outcome_type <- saveResults(error_rates, error_rates_outcome_type)

confusionMatrix(SVM.radial.cv, 'average')

################
#  RPart Tree  #
################

#Takes approximately 3.71 sec
processing_time <- system.time({
  Tree.cv <- train(outcome_type ~ .,
                    data = cat_data_subset,
                    method = 'rpart',
                    trControl = fitControl,
                    tuneLength = 15)
})
processing_time

plot(Tree.cv)
error_rates <- getResults(t(confusionMatrix(Tree.cv, 'average')$table))
error_rates_outcome_type <- saveResults(error_rates, error_rates_outcome_type)

confusionMatrix(Tree.cv, 'average')

################
# RandomForest #
################

#Takes approximately 382 sec
processing_time <- system.time({
  RF.cv <- train(outcome_type ~ .,
                  data = cat_data_subset,
                  method = 'rf',
                  trControl = fitControl,
                  tuneLength = 15)
})
processing_time

varImp(RF.cv$finalModel)

plot(RF.cv)
error_rates <- getResults(t(confusionMatrix(RF.cv, 'average')$table))
error_rates_outcome_type <- saveResults(error_rates, error_rates_outcome_type)

confusionMatrix(RF.cv, 'average')

################
#   Boosting   #
################

#Takes approximately 1520 sec
gbm.grid <- expand.grid(interaction.depth = c(1, 5, 10, 15), 
                        n.trees = seq(1, 20, 2)*100, 
                        shrinkage = c(0.1, 0.2, 0.3),
                        n.minobsinnode = c(15, 20, 25))

processing_time <- system.time({
  Boosting.cv <- train(outcome_type ~ .,
                        data = cat_data_subset,
                        method = 'gbm',
                        trControl = fitControl,
                        tuneGrid = gbm.grid)
})
processing_time

plot(Boosting.cv)
error_rates <- getResults(t(confusionMatrix(Boosting.cv, 'average')$table))
error_rates_outcome_type <- saveResults(error_rates, error_rates_outcome_type)

confusionMatrix(Boosting.cv, 'average')

colnames(error_rates_outcome_type) <- c('FN', 'FP', 'TP', 'TN', 'OverallErr')
rownames(error_rates_outcome_type) <- c('svmLinear', 'svmRadial', 'Tree', 'RF','Boosting')

##################
#Model Comparison# 
##################

model_comparison <- resamples(list(SVMLinear = SVM.linear.cv,
                                    SVMRadial = SVM.radial.cv,
                                    rpart = Tree.cv,
                                    RF = RF.cv,
                                    Boosting = Boosting.cv))
summary(model_comparison)
dotplot(model_comparison, metric = 'Accuracy')
bwplot(model_comparison, metric = 'Accuracy')

best.model.name <- rownames(summary(model_comparison)$statistics$Accuracy)[which.max(summary(model_comparison)$statistics$Accuracy[,'Mean'])]

print(paste('Based on Accuracy measurement, the optimal model is:', best.model.name))

##################
#Model Validation#
##################

#Runs quickly

#Run the best model on full dataset and then compare the accuracy to the accuracy from the subset on which the best model was trained
best.model.validation <- train(outcome_type ~ ., 
                               data = cat_data[-train_idx,-which(names(cat_data) %in% 'period_of_stay')],
                               method = 'gbm',
                               trControl = trainControl(method = 'cv',
                                                        number = 8),
                               tuneGrid = data.frame(n.trees = Boosting.cv$bestTune[,1],
                                                     interaction.depth = Boosting.cv$bestTune[,2],
                                                     shrinkage = Boosting.cv$bestTune[,3],
                                                     n.minobsinnode = Boosting.cv$bestTune[,4]))

confusionMatrix(best.model.validation, 'average')
confusionMatrix(Boosting.cv, 'average')
##################################
#Compute Cost with the Best Model#
##################################

#Runs quickly
cat_data_shuffled <- cat_data[sample(1:nrow(cat_data), nrow(cat_data)), -which(names(cat_data) %in% 'period_of_stay')]

numfolds <- 8
fold.indices <- cut(1:nrow(cat_data_shuffled), breaks = numfolds, labels=FALSE)
cutoff <- seq(0.01, 0.99, 0.01)
stacked_cost_table <- c()
fitControl.best <- trainControl(method = "none",
                                classProbs = TRUE,
                                summaryFunction = twoClassSummary)

processing_time <- system.time({
  
  stacked_cost_table <- foreach (i = 1:numfolds, .packages = 'caret', .combine = rbind) %dopar% {
    
    cost_table_ith <- c()
    
    test.indices <- which(fold.indices == i)
    test.data <- cat_data_shuffled[test.indices, ]
    train.data <- cat_data_shuffled[-test.indices, ]
    best.model <- train(outcome_type ~ .,
                        data = train.data,
                        method = 'gbm',
                        trControl = fitControl.best,
                        tuneGrid = data.frame(n.trees = Boosting.cv$bestTune[,1],
                                              interaction.depth = Boosting.cv$bestTune[,2],
                                              shrinkage = Boosting.cv$bestTune[,3],
                                              n.minobsinnode = Boosting.cv$bestTune[,4]))
    best.model.pred <- predict(best.model, newdata = test.data, type = 'prob')
    
    for (j in 1:length(cutoff)) {
      
      pred.output <- rep('Adopt', nrow(test.data))
      pred.output[best.model.pred[,2] >= cutoff[j]] <- 'Other'
      cf_mtx.temp = table(factor(test.data$outcome_type, levels=c('Adopt', 'Other')),
                          factor(pred.output, levels=c('Adopt', 'Other')))
      fn_j <- getResults(cf_mtx.temp)[[1]]
      fp_j <- getResults(cf_mtx.temp)[[2]]
      tp_j <- getResults(cf_mtx.temp)[[3]]
      tn_j <- getResults(cf_mtx.temp)[[4]]
      ov_j <- getResults(cf_mtx.temp)[[5]]
      
      exp.cost <- computeCost(tp = tp_j,
                              tn = tn_j,
                              fp = fp_j,
                              fn = fn_j)
      
      row_temp <- c(cutoff[j], tp_j, tn_j, fp_j, fn_j, ov_j, exp.cost)
      cost_table_ith <- rbind(cost_table_ith, row_temp)
    }
    
    cost_table_ith
  }
})
processing_time 

final_cost_table <- aggregate(stacked_cost_table, by = list(stacked_cost_table[,1]), FUN = mean)
final_cost_table <- final_cost_table[,-1]
colnames(final_cost_table) <- c('cutoff', 'TP', 'TN', 'FP', 'FN', 'OVER', 'Exp.Cost')
best.cost.index <- which.min(final_cost_table[,7])

#Visualizing expected cost with FP, FN, and Accuracy as reference
#We couldn't have the proper secondary axis worked out, so we put an Excel version of 
#this graph in our presentation
par(mar = c(5, 4, 4, 4) + 0.3)
plot(seq(0.01, 0.99, 0.01), final_cost_table[,7], 
     xlab='Cutoff Probability',
     ylab='Expected Cost Per Cat',
     main='Expected Cost, TP, FP, TN, and FN V.S. Cutoff', type = 'l', lwd = 2)
par(new = TRUE)
plot(seq(0.01, 0.99, 0.01), final_cost_table[,4], type = 'l', 
     axes = FALSE, bty = "n", xlab = "", ylab = "", col = 2, lty = 2, lwd = 2)
par(new = TRUE)
plot(seq(0.01, 0.99, 0.01), final_cost_table[,5], type = 'l', 
     axes = FALSE, bty = "n", xlab = "", ylab = "", col = 3, lty = 2, lwd = 2)
axis(side=4)
par(new = TRUE)
plot(seq(0.01, 0.99, 0.01), 1-final_cost_table[,6], type = 'l', 
     axes = FALSE, bty = "n", xlab = "", ylab = "", col = 4, lty = 2, lwd = 2)
legend('topright', legend = c("Exp.Cost","FP", "FN", "Accuracy"), cex = 0.6,
       lty=c(1,1), lwd=c(2.5,2.5), col=c('black', 'red', 'green', 'blue'))
abline(v = cutoff[which.min(final_cost_table[,7])], lty = 3)

#Reporting optimal values
print(paste('Optimal', colnames(final_cost_table)[1],
            'is:', final_cost_table[,1][best.cost.index]))

print(paste('Optimal', colnames(final_cost_table)[2],
            'is:', final_cost_table[,2][best.cost.index]))

print(paste('Optimal', colnames(final_cost_table)[3],
            'is:', final_cost_table[,3][best.cost.index]))

print(paste('Optimal', colnames(final_cost_table)[4],
            'is:', final_cost_table[,4][best.cost.index]))

print(paste('Optimal', colnames(final_cost_table)[5],
            'is:', final_cost_table[,5][best.cost.index]))

print(paste('Optimal', colnames(final_cost_table)[6],
            'is:', final_cost_table[,6][best.cost.index]))

print(paste('Optimal', colnames(final_cost_table)[7],
            'is:', final_cost_table[,7][best.cost.index]))

#write.csv(final_cost_table, 'table_1.csv')

#############################################################
# Predictive Modeling (period_of_stay) as Response Variable #
#############################################################

###Taking a subset of data to train and compare model###
set.seed(0871)
cat_data_subset2 = cat_data[train_idx, -which(names(cat_data) %in% c('outcome_type'))]
error_rates_period_of_stay <- c()

################
#  SVM Linear  #
################

#Takes approximately 1612.48 sec
grid <- expand.grid(C = seq(0.01, 10, 0.5))

processing_time <- system.time({
  SVM.linear.cv2 <- train(period_of_stay ~ .,
                         data = cat_data_subset2,
                         method = 'svmLinear',
                         preProcess = c('center', 'scale'),
                         tuneGrid = grid,
                         trControl = fitControl)
})
processing_time

plot(SVM.linear.cv2)
error_rates <- getResults(confusionMatrix(SVM.linear.cv2, 'average')$table)
error_rates_period_of_stay <- saveResults(error_rates, error_rates_period_of_stay)

confusionMatrix(SVM.linear.cv2, 'average')

################
#  SVM Radial  #
################

#Takes approximately 355.28 sec
grid <- expand.grid(C = seq(0.01, 10, 0.5), sigma = c(0.001, 0.01, 0.1))

processing_time <- system.time({
  SVM.radial.cv2 <- train(period_of_stay ~ .,
                         data = cat_data_subset2,
                         method = 'svmRadial',
                         preProcess = c('center', 'scale'),
                         tuneGrid = grid,
                         trControl = fitControl)
})
processing_time

plot(SVM.radial.cv2)
error_rates <- getResults(confusionMatrix(SVM.radial.cv2, 'average')$table)
error_rates_period_of_stay <- saveResults(error_rates, error_rates_period_of_stay)

confusionMatrix(SVM.radial.cv2, 'average')

################
#  RPart Tree  #
################

#Takes approximately 3.31 sec
processing_time <- system.time({
  Tree.cv2 <- train(period_of_stay ~ .,
                   data = cat_data_subset2,
                   method = 'rpart',
                   trControl = fitControl,
                   tuneLength = 15)
})
processing_time

plot(Tree.cv2)
error_rates <- getResults(confusionMatrix(Tree.cv2, 'average')$table)
error_rates_period_of_stay <- saveResults(error_rates, error_rates_period_of_stay)

confusionMatrix(Tree.cv2, 'average')

################
# RandomForest #
################

#Takes approximately 467.37 sec
processing_time <- system.time({
  RF.cv2 <- train(period_of_stay ~ .,
                 data = cat_data_subset2,
                 method = 'rf',
                 trControl = fitControl,
                 tuneLength = 15)
})
processing_time

varImp(RF.cv2$finalModel)

plot(RF.cv2)
error_rates <- getResults(confusionMatrix(RF.cv2, 'average')$table)
error_rates_period_of_stay <- saveResults(error_rates, error_rates_period_of_stay)

confusionMatrix(RF.cv2, 'average')

################
#   Boosting   #
################

#Takes approximately 1512.89 sec
gbm.grid <- expand.grid(interaction.depth = c(1, 5, 10, 15), 
                        n.trees = seq(1, 20, 2)*100, 
                        shrinkage = c(0.1, 0.2, 0.3),
                        n.minobsinnode = c(15, 20, 25))

processing_time <- system.time({
  Boosting.cv2 <- train(period_of_stay ~ .,
                       data = cat_data_subset2,
                       method = 'gbm',
                       trControl = fitControl,
                       tuneGrid = gbm.grid)
})
processing_time

plot(Boosting.cv2)
error_rates <- getResults(confusionMatrix(Boosting.cv2, 'average')$table)
error_rates_period_of_stay <- saveResults(error_rates, error_rates_period_of_stay)

confusionMatrix(Boosting.cv2, 'average')

colnames(error_rates_period_of_stay) <- c('FN', 'FP', 'TP', 'TN', 'OverallErr')
rownames(error_rates_period_of_stay) <- c('svmLinear', 'svmRadial', 'Tree', 'RF','Boosting')

##################
#Model Comparison#
##################

model_comparison2 <- resamples(list(SVMLinear = SVM.linear.cv2,
                                    SVMRadial = SVM.radial.cv2,
                                    rpart = Tree.cv2,
                                    rf = RF.cv2,
                                    Boosting = Boosting.cv2))
summary(model_comparison2)

dotplot(model_comparison2, metric = 'Accuracy')
bwplot(model_comparison2, metric = 'Accuracy')

best.model.name2 <- rownames(summary(model_comparison2)$statistics$Accuracy)[which.max(summary(model_comparison2)$statistics$Accuracy[,'Mean'])]

print(paste('Based on Accuracy measurement, the optimal model is:',best.model.name2))

##################
#Model Validation#
##################

#Runs quickly
#Run the best model on full dataset and then compare the accuracy to the accuracy from the subset on which the best model was trained
best.model.validation2 <- train(period_of_stay ~ ., 
                               data = cat_data[-train_idx,-which(names(cat_data) %in% 'outcome_type')],
                               method = 'gbm',
                               trControl = trainControl(method = 'cv',
                                                        number = 8),
                               tuneGrid = data.frame(n.trees = Boosting.cv2$bestTune[,1],
                                                     interaction.depth = Boosting.cv2$bestTune[,2],
                                                     shrinkage = Boosting.cv2$bestTune[,3],
                                                     n.minobsinnode = Boosting.cv2$bestTune[,4]))

confusionMatrix(best.model.validation2, 'average')
confusionMatrix(Boosting.cv2, 'average')
##################################
#Compute Cost with the Best Model#
##################################

#Runs quickly
cat_data_shuffled2 <- cat_data[sample(1:nrow(cat_data), nrow(cat_data)), -which(names(cat_data) %in% 'outcome_type')]

numfolds <- 8
fold.indices <- cut(1:nrow(cat_data_shuffled2), breaks = numfolds, labels=FALSE)
cutoff <- seq(0.01, 0.99, 0.01)
stacked_cost_table2 <- c()
fitControl.best <- trainControl(method = "none",
                                classProbs = TRUE,
                                summaryFunction = twoClassSummary)

print(paste('Parameters of the best model:',Boosting.cv2$bestTune))

processing_time <- system.time({
  
  stacked_cost_table2 <- foreach (i = 1:numfolds, .packages = 'caret', .combine = rbind) %dopar% {
    
    cost_table_ith <- c()
    
    test.indices <- which(fold.indices == i)
    test.data <- cat_data_shuffled2[test.indices, ]
    train.data <- cat_data_shuffled2[-test.indices, ]
    best.model <- train(period_of_stay ~ .,
                        data = train.data,
                        method = 'gbm',
                        trControl = fitControl.best,
                        tuneGrid = data.frame(n.trees = Boosting.cv2$bestTune[,1],
                                              interaction.depth = Boosting.cv2$bestTune[,2],
                                              shrinkage = Boosting.cv2$bestTune[,3],
                                              n.minobsinnode = Boosting.cv2$bestTune[,4]))
    best.model.pred <- predict(best.model, newdata = test.data, type = 'prob')
    
    for (j in 1:length(cutoff)) {
      
      pred.output <- rep('Short', nrow(test.data))
      pred.output[best.model.pred[,2] >= cutoff[j]] <- 'Medium.Long'
      cf_mtx.temp = table(factor(test.data$period_of_stay, levels=c('Short', 'Medium.Long')),
                          factor(pred.output, levels=c('Short', 'Medium.Long')))
      fn_j <- getResults(cf_mtx.temp)[[1]]
      fp_j <- getResults(cf_mtx.temp)[[2]]
      tp_j <- getResults(cf_mtx.temp)[[3]]
      tn_j <- getResults(cf_mtx.temp)[[4]]
      ov_j <- getResults(cf_mtx.temp)[[5]]
      
      exp.cost <- computeCost.period_of_stay(tp = tp_j,
                              tn = tn_j,
                              fp = fp_j,
                              fn = fn_j)
      
      row_temp <- c(cutoff[j], tp_j, tn_j, fp_j, fn_j, ov_j, exp.cost)
      cost_table_ith <- rbind(cost_table_ith, row_temp)
    }
    
    cost_table_ith
  }
})
processing_time 

final_cost_table2 <- aggregate(stacked_cost_table2, by = list(stacked_cost_table2[,1]), FUN = mean)
final_cost_table2 <- final_cost_table2[,-1]
colnames(final_cost_table2) <- c('cutoff', 'TP', 'TN', 'FP', 'FN', 'OVER', 'Exp.Cost')
best.cost.index2 <- which.min(final_cost_table2[,7])

#Visualizing expected cost with FP, FN, and Accuracy as reference
#We couldn't have the proper secondary axis worked out, so we put an Excel version of 
#this graph in our presentation
par(mar = c(5, 4, 4, 4) + 0.3)
plot(seq(0.01, 0.99, 0.01), final_cost_table2[,7], 
     xlab='Cutoff Probability',
     ylab='Expected Cost Per Cat',
     main='Expected Cost, TP, FP, TN, and FN V.S. Cutoff', type = 'l', lwd = 2)
par(new = TRUE)
plot(seq(0.01, 0.99, 0.01), final_cost_table2[,4], type = 'l', 
     axes = FALSE, bty = "n", xlab = "", ylab = "", col = 2, lty = 2, lwd = 2)
par(new = TRUE)
plot(seq(0.01, 0.99, 0.01), final_cost_table2[,5], type = 'l', 
     axes = FALSE, bty = "n", xlab = "", ylab = "", col = 3, lty = 2, lwd = 2)
axis(side=4, at = pretty(c(0.01, 0.99)))
par(new = TRUE)
plot(seq(0.01, 0.99, 0.01), 1-final_cost_table2[,6], type = 'l', 
     axes = FALSE, bty = "n", xlab = "", ylab = "", col = 4, lty = 2, lwd = 2)
legend('topright', legend = c("Exp.Cost","FP", "FN", "Accuracy"), cex = 0.6,
       lty=c(1,1), lwd=c(2.5,2.5), col=c('black', 'red', 'green', 'blue'))
abline(v = cutoff[which.min(final_cost_table2[,7])], lty = 3)

#Reporting optimal values
print(paste('Optimal', colnames(final_cost_table2)[1],
            'is:', final_cost_table2[,1][best.cost.index2]))

print(paste('Optimal', colnames(final_cost_table2)[2],
            'is:', final_cost_table2[,2][best.cost.index2]))

print(paste('Optimal', colnames(final_cost_table2)[3],
            'is:', final_cost_table2[,3][best.cost.index2]))

print(paste('Optimal', colnames(final_cost_table2)[4],
            'is:', final_cost_table2[,4][best.cost.index2]))

print(paste('Optimal', colnames(final_cost_table2)[5],
            'is:', final_cost_table2[,5][best.cost.index2]))

print(paste('Optimal', colnames(final_cost_table2)[6],
            'is:', final_cost_table2[,6][best.cost.index2]))

print(paste('Optimal', colnames(final_cost_table2)[7],
            'is:', final_cost_table2[,7][best.cost.index2]))

#write.csv(final_cost_table2, 'table_2.csv')

registerDoSEQ()
