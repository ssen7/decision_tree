source('decision_tree_trials.R')

df <- iris

df <- df[df$Species=='virginica' | df$Species=='versicolor',]

accuracy_list <- numeric()
for (t in 1:1){
        
        train = sample (1: nrow(df ), 2*nrow(df)/3)
        
        df.train <- df[train,]
        df.test <- df[-train,]
        
        tree = fit.decision.tree(df.train)
        
        results <- data.frame(actual = character(nrow(df.test)))
        actual <- character()
        predicted <- character()
        versicolor.prob <- numeric()
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
        
}

results <- results[order(-versicolor.prob),]
total_gpos <- nrow(results[results$actual == 'versicolor',])
total_gneg <- nrow(results[results$actual == 'virginica',])

results$tpr <- numeric(nrow(results))
results$fpr <- numeric(nrow(results))

for (i in 1:nrow(results)){
        cum.vers.count <- sum(results[0:i, 1] == 'versicolor')
        cum.virg.count <- sum(results[0:i, 1] == 'virginica')
        results$tpr[i] <- cum.vers.count/total_gpos
        results$fpr[i] <- cum.virg.count/total_gneg
        
}
orig <- par('mar')

par(mar=orig)
plot(results$fpr, results$tpr, type = 'l', xlab = 'FPR', ylab = 'TPR', main = 'ROC Curves')

