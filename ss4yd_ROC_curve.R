## Creating ROC curves

source('decision_tree_trials.R')
# set.seed(111)
df <- iris

df <- df[df$Species=='virginica' | df$Species=='versicolor',]

accuracy_list <- numeric()
result_list <- list()
for (t in 1:4){
        # split the train and test datasets
        train = sample (1: nrow(df ), 2*nrow(df)/3)
        
        df.train <- df[train,]
        df.test <- df[-train,]
        
        # fit the tree on the training data
        tree = fit.decision.tree(df.train)
        # initialize an empty dataframe to store the actual and predicted values and probabilities
        results <- data.frame(actual = character(nrow(df.test)))
        actual <- character()
        predicted <- character()
        versicolor.prob <- numeric()
        
        # predict on each row of the test data
        for (i in 1:nrow(df.test)){
                # remove label
                row <- df.test[i, -length(df)]
                actual <- c(actual, as.character(df.test[i, length(df)]))
                predicted <- c(predicted, predict.tree(tree,row))
                versicolor.prob <- c(versicolor.prob, predict.versicolor.prob(tree,row))
        }
        
        results$actual <- actual
        results$predicted <- predicted
        results$versicolor.prob <- versicolor.prob
        
        accuracy <- sum(actual==predicted)/length(actual)
        accuracy_list[t] <- accuracy
        
        # order the results the versicolor probs in descending order
        results <- results[order(-versicolor.prob),]
        # total ground postives
        total_gpos <- nrow(results[results$actual == 'versicolor',])
        # total ground negativess
        total_gneg <- nrow(results[results$actual == 'virginica',])
        
        results$tpr <- numeric(nrow(results))
        results$fpr <- numeric(nrow(results))
        
        # calculate the tpr and fpr
        for (i in 1:nrow(results)){
                cum.vers.count <- sum(results[0:i, 1] == 'versicolor')
                cum.virg.count <- sum(results[0:i, 1] == 'virginica')
                results$tpr[i] <- cum.vers.count/total_gpos
                results$fpr[i] <- cum.virg.count/total_gneg
                
        }
        
        result_list[[t]] <- results
        
        
        
}


# plot the data
par(mfrow= c(2,2))
plot(result_list[[1]]$fpr, result_list[[1]]$tpr, type = 'l', xlab = 'FPR', ylab = 'TPR', main = 'ROC Curve')
plot(result_list[[2]]$fpr, result_list[[2]]$tpr, type = 'l', xlab = 'FPR', ylab = 'TPR', main = 'ROC Curve')
plot(result_list[[3]]$fpr, result_list[[3]]$tpr, type = 'l', xlab = 'FPR', ylab = 'TPR', main = 'ROC Curve')
plot(result_list[[4]]$fpr, result_list[[4]]$tpr, type = 'l', xlab = 'FPR', ylab = 'TPR', main = 'ROC Curve')

# # order the results the versicolor probs in descending order
# results <- results[order(-versicolor.prob),]
# # total ground postives
# total_gpos <- nrow(results[results$actual == 'versicolor',])
# # total ground negativess
# total_gneg <- nrow(results[results$actual == 'virginica',])
# 
# results$tpr <- numeric(nrow(results))
# results$fpr <- numeric(nrow(results))
# 
# # calculate the tpr and fpr
# for (i in 1:nrow(results)){
#         cum.vers.count <- sum(results[0:i, 1] == 'versicolor')
#         cum.virg.count <- sum(results[0:i, 1] == 'virginica')
#         results$tpr[i] <- cum.vers.count/total_gpos
#         results$fpr[i] <- cum.virg.count/total_gneg
#         
# }
# 
# # plot the data
# orig <- par('mar')
# 
# par(mar=orig)
# plot(results$fpr, results$tpr, type = 'l', xlab = 'FPR', ylab = 'TPR', main = 'ROC Curves')
# 
