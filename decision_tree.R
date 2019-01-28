df <- iris

df <- df[df$Species=='virginica' | df$Species=='versicolor',]
df

iris.d <- df
fit.decision.tree <- function(df, node_size = 5){
        gini_indices <- df[,-length(df)]
        
        gini_indices <- gini_indices[-c(1:nrow(df)), ]
        
        node_size <- nrow(df)
        
        while(node_size != 5){
                # gini_indices <- data.frame()
                for(i in 1:(length(df)-1)){
                        col <- df[,i]
                        col_name <- as.name(names(df)[i])
                        for ( j in col){
                                split1 <- df$Species[df[,i] <= j]
                                split2 <- df$Species[df[,i] > j]
                                gini_indices[,j] <- cal_gini_index(split1,split2)
                                node_size <- min(c(nrow(split1), nrow(split2)))
                        }
                        
                }
        }
        gini_indices
        
        
}

cal_gini_index <- function(split1, split2){
        col <- c(split1,split2)
        levs <- unique(col)
        n = length(col)
        pm1 <- length(col[col == levs[1]])/n
        pm2 <- length(col[col == levs[2]])/n
        
        gini <- pm1*(1-pm1) + pm2*(1-pm2)
        
        gini
}

gini_indices <- df[,-length(df)]

gini_indices <- gini_indices[-c(1:nrow(df)), ]

node_size <- nrow(df)

while(node_size != 5){
        # gini_indices <- data.frame()
        for(i in 1:(length(df)-1)){
                col <- df[,i]
                for ( j in col){
                        split1 <- df$Species[col <= j]
                        split2 <- df$Species[col > j]
                        gini_indices[j,i] <- cal_gini_index(split1,split2)
                        node_size <- min(length(split1), length(split2))
                }
                
        }
        print(node_size)
}
col <- c(split1,split2)
levs <- unique(col)
n = length(col)
pm1 <- length(col[col == levs[1]])/n
pm2 <- length(col[col == levs[2]])/n

gini <- pm1*(1-pm1) + pm2*(1-pm2)

gini