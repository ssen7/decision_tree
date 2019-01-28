# Functions for generating tree and probabilities and making predictions

# Function to get versicolor probabilities
predict.versicolor.prob <- function(tree, row){
        
        # get the split
        split1 <- tree$split
        
        # check if its a leaf node
        if(!is.null(tree$data)){
                # get label
                label <- tree$label
                # if versicolor return probability
                if(label == 'versicolor'){
                        return(tree$prop)
                }
                # else 1 - P(versicolor)
                else{
                        return(1-tree$prop)                        
                }
        }
        
        # recursively get the label
        if(row[,split1[1]] < split1[2]){
                tree <- tree$left.subtree
                label <- predict.versicolor.prob(tree,row)
                
        }
        else {
                tree <- tree$right.subtree
                label <- predict.versicolor.prob(tree,row)
        }
        
}


# Function to make label predictionss
predict.tree <- function(tree, row){
        
        # get the split
        split1 <- tree$split
        
        # check if its a leaf node
        if(!is.null(tree$data)){
                label <- tree$label
                return(label)
        }
        
        # recursively get the label
        if(row[,split1[1]] < split1[2]){
                tree <- tree$left.subtree
                label <- predict.tree(tree,row)
        
        }
        else {
                tree <- tree$right.subtree
                label <- predict.tree(tree,row)
        }
        
}

# construct the tree
fit.decision.tree <- function(iris.d) {
        # initialize empty tree
        tree <- list()
        
        l_levels <- length(unique(iris.d$Species))
        
        # get the split position
        js <- get_j_s(iris.d)
        
        # check if input data frame has 2 levels
        if(l_levels == 1){
                return(tree)
        }
        
        # specify the labels in iris data frame
        level1 <- 'virginica'
        level2 <- 'versicolor'
        
        # initial split of the data
        left <- iris.d[iris.d[,js[1]] <= js[2],]
        right <- iris.d[iris.d[,js[1]] > js[2],]
        
        # get the number of obs in each split
        l_len <- nrow(left)
        r_len <- nrow(right)
        
        # calculate the classification error rate at each split
        l_error = calc_error(left$Species, level1, level2)
        r_error = calc_error(right$Species, level1, level2)
        
        # if somehow zero error, stop splitting and populate the tree or the number of obs < 4
        if(l_len <=4 | r_len <=4 |(l_error == 0  & r_error == 0)){
                js <- get_j_s(iris.d)
                tree$split <- js
                tree$left.subtree <- list(data=left, prop = calc_prop(left$Species),
                                          label=calc_region_classification(left,level1, level2))
                tree$right.subtree <- list(data=right, prop = calc_prop(right$Species),
                                           label=calc_region_classification(right,level1, level2))
                return(tree)
        }
        else {
                # assign the split region
                tree$split <- js
                # check if there is optimized split region for the splitted data
                js_left <- get_j_s(left)
                js_right <- get_j_s(right)
                
                # if not, then stop splitting and populate the tree
                if(js_left[1]==0){
                        tree$left.subtree = list(data=left, prop = calc_prop(left$Species),
                                                 label=calc_region_classification(left,level1, level2))
                }
                # else, keep on splitting
                if(js_left[1]!=0){
                        tree$left.subtree <- fit.decision.tree(left)
                } 
                # same for the righ split
                if(js_right[1]==0){
                        tree$right.subtree = list(data=right, prop = calc_prop(right$Species), 
                                                  label=calc_region_classification(right,level1, level2))
                }
                if (js_right[1] !=0) {
                        tree$right.subtree <- fit.decision.tree(right)
                }
        }
        return(tree)
        
        
}

# function to get the split region
get_j_s <- function(df){
        
        # get the number of rows
        n <- nrow(df)
        
        # separate the predictors and the outcomes
        predictors <- df[,-length(df)]
        outcome <- df[, length(df)]
        
        # get the outcome levels
        outcome_levels <- as.character(unique(outcome))
        level1 <- outcome_levels[1]
        level2 <- outcome_levels[2]
        
        # calculate the original classification error rate
        error_orig <- calc_error(outcome, level1, level2)
        error_reducts <- predictors
        
        # initialize the j and s for the split
        predictor_num = 0
        s = 0
        
        # loop over each predictor column
        for (i in 1:length(predictors)){
                # get the column of the predictor
                col <- predictors[,i]
                error_list <- numeric()
                
                # initialize dummy value for the change in classification error rate due to splitting
                # reference: http://users.umiacs.umd.edu/~joseph/classes/enee752/Fall09/solutions3.pdf
                delta.e <- -1
                # iterate over each value of the predictor
                for (j in 1:length(col)){
                        val <- col[j]
                        
                        # split the data
                        left.tree <- outcome[predictors[,i] <= val]
                        right.tree <- outcome[predictors[,i] > val]
                        
                        if(!is.na(length(left.tree)) & !is.na(length(right.tree)) & length(left.tree)!=0 & 
                           length(right.tree)!=0){
                                # get the classifcation label based on proportion
                                L <- calc_region_classification(left.tree, level1, level2)
                                R <- calc_region_classification(right.tree, level1, level2)
                                
                                # get the error rate of each region
                                e.left <- calc_error(left.tree, level1, level2)
                                e.right <- calc_error(right.tree, level1, level2)
                                
                                # calculate the decrease in error rate
                                # the maximum decrease is better
                                # this check is done to calculate the first value of delta decrease in error rate
                                if(!is.na(delta.e) & delta.e == -1){
                                        delta.e <- error_orig - (length(left.tree)/n)*e.left - (length(right.tree)/n)*e.right
                                }
                                
                                # subsequent calculations of decrease in error rate
                                delta.e2 <- error_orig - (length(left.tree)/n)*e.left - (length(right.tree)/n)*e.right                  
                                # print('inside js')
                                
                                # if delta is greater than previous, store the j and s values
                                if(!is.na(delta.e) & !is.na(delta.e2) & delta.e2 >= delta.e & delta.e2 > 0 ){
                                        delta.e <- delta.e2
                                        predictor_num = i
                                        s = val
                                }
                                
                        }
                        
                }
                
        }
        
        # return the predictor and the value of the predictor at which to split the data
        return(c(predictor_num,s))
        
        
        
        
}

### Helper functions ###
calc_error <- function(split, l1, l2){
        
        if(length(split)==0){
                return(0)
        }
        
        p1 <- length(split[split==l1])/length(split)
        p2 <- length(split[split==l2])/length(split)
        
        if(is.na(p1)){
                e <- 1 - p2
                return(e)
        }
        if(is.na(p2)){
                e <- 1 - p1
                return(e)
        }
        e <- 1 - max(p1,p2)
        
        return(e)
}


calc_prop <- function(split){
        
        levels <- length(unique(split))
        if (levels == 1){
                return(1)
        }
        else {
                l1 <- unique(split)[1]
                l2 <- unique(split)[2]
                p1 <- length(split[split==l1])/length(split)
                p2 <- length(split[split==l2])/length(split)
                
                return(max(p1,p2))
        }
}

calc_region_classification <- function(region, level1, level2){
        
        if(is.na(level2)){
                return(level1)
        }
        
        if(is.na(level1)){
                return(level2)
        }
        
        reg_lev1 <- region[region==level1]
        reg_lev2 <- region[region==level2]
        
        if(length(reg_lev1) > length(reg_lev2)){
                return(level1)
        }
        else {
                return(level2)
        }
}


cal_gini_index <- function(split1, split2){
        col <- c(split1,split2)
        levs <- unique(col)
        n <- length(col)
        pm1 <- length(col[col == levs[1]])/n
        pm2 <- length(col[col == levs[2]])/n
        
        gini <- pm1*(1-pm1) + pm2*(1-pm2)
        
        gini
}

