# Functions for generating tree and probabilities and making predictions

predict.versicolor.prob <- function(tree, row){
        
        split1 <- tree$split
        if(!is.null(tree$data)){
                label <- tree$label
                if(label == 'versicolor'){
                        return(tree$prop)
                }
                else{
                        return(1-tree$prop)                        
                }
        }
        
        if(row[,split1[1]] < split1[2]){
                tree <- tree$left.subtree
                label <- predict.versicolor.prob(tree,row)
                
        }
        else {
                tree <- tree$right.subtree
                label <- predict.versicolor.prob(tree,row)
        }
        
}



predict.tree <- function(tree, row){
        
        split1 <- tree$split
        if(!is.null(tree$data)){
                label <- tree$label
                return(label)
        }
        
        if(row[,split1[1]] < split1[2]){
                tree <- tree$left.subtree
                label <- predict.tree(tree,row)
        
        }
        else {
                tree <- tree$right.subtree
                label <- predict.tree(tree,row)
        }
        
}

fit.decision.tree <- function(iris.d) {
        tree <- list()
        l_levels <- length(unique(iris.d$Species))
        js <- get_j_s(iris.d)
        if(l_levels == 1){
                return(tree)
        }
        
        level1 <- 'virginica'
        level2 <- 'versicolor'
        
        left <- iris.d[iris.d[,js[1]] <= js[2],]
        right <- iris.d[iris.d[,js[1]] > js[2],]
        l_len <- nrow(left)
        r_len <- nrow(right)
        l_error = calc_error(left$Species, level1, level2)
        r_error = calc_error(right$Species, level1, level2)
        
        if( (l_error == 0  & r_error == 0)){
                js <- get_j_s(iris.d)
                tree$split <- js
                tree$left.subtree <- list(data=left, prop = calc_prop(left$Species),
                                          label=calc_region_classification(left,level1, level2))
                tree$right.subtree <- list(data=right, prop = calc_prop(right$Species),
                                           label=calc_region_classification(right,level1, level2))
                return(tree)
        }
        else {
                tree$split <- js
                js_left <- get_j_s(left)
                js_right <- get_j_s(right)
                if(js_left[1]==0){
                        tree$left.subtree = list(data=left, prop = calc_prop(left$Species),
                                                 label=calc_region_classification(left,level1, level2))
                }
                if(js_left[1]!=0){
                        tree$left.subtree <- fit.decision.tree(left)
                } 
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


get_j_s <- function(df){
        
        n <- nrow(df)
        
        predictors <- df[,-length(df)]
        outcome <- df[, length(df)]
        
        outcome_levels <- as.character(unique(outcome))
        level1 <- outcome_levels[1]
        level2 <- outcome_levels[2]
        
        error_orig <- calc_error(outcome, level1, level2)
        
        error_reducts <- predictors
        predictor_num = 0
        s = 0
        for (i in 1:length(predictors)){
                col <- predictors[,i]
                error_list <- numeric()
                delta.e <- -1
                for (j in 1:length(col)){
                        val <- col[j]
                        left.tree <- outcome[predictors[,i] <= val]
                        right.tree <- outcome[predictors[,i] > val]
                        
                        if(!is.na(length(left.tree)) & !is.na(length(right.tree)) & length(left.tree)!=0 & 
                           length(right.tree)!=0){
                                L <- calc_region_classification(left.tree, level1, level2)
                                R <- calc_region_classification(right.tree, level1, level2)
                                
                                e.left <- calc_error(left.tree, level1, level2)
                                e.right <- calc_error(right.tree, level1, level2)
                                
                                # calculate the decrease in error rate
                                # the maximum decrease is better
                                if(!is.na(delta.e) & delta.e == -1){
                                        delta.e <- error_orig - (length(left.tree)/n)*e.left - (length(right.tree)/n)*e.right
                                }
                                
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
                # print(c(error_orig,delta.e))
                
        }
        
        return(c(predictor_num,s))
        
        
        
        
}

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

