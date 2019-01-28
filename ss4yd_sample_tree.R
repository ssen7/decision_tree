## Sample Run of the decision tree

source('decision_tree_trials.R')
# set.seed(111)
df <- iris

df <- df[df$Species=='virginica' | df$Species=='versicolor',]

train = sample (1: nrow(df ), 2*nrow(df)/3)

df.train <- df[train,]
df.test <- df[-train,]

tree = fit.decision.tree(df.train)

# predict on each row of the test data
for (i in 1:nrow(df.test)){
        # remove label
        row <- df.test[i, -length(df)]
        actual <- c(actual, as.character(df.test[i, length(df)]))
        predicted <- c(predicted, predict.tree(tree,row))
        versicolor.prob <- c(versicolor.prob, predict.versicolor.prob(tree,row))
}

# confusion matrix
table(actual, predicted)
#              predicted
# actual       versicolor virginica
# versicolor         48         2
# virginica           0        52

accuracy <- sum(actual==predicted)*100/length(actual)
accuracy
# [1] 98.03922

tree
# $`split`
# [1] 4.0 1.7
# 
# $left.subtree
# $left.subtree$`split`
# [1] 4.0 1.1
# 
# $left.subtree$left.subtree
# $left.subtree$left.subtree$`data`
# Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
# 80          5.7         2.6          3.5         1.0 versicolor
# 61          5.0         2.0          3.5         1.0 versicolor
# 81          5.5         2.4          3.8         1.1 versicolor
# 70          5.6         2.5          3.9         1.1 versicolor
# 63          6.0         2.2          4.0         1.0 versicolor
# 68          5.8         2.7          4.1         1.0 versicolor
# 
# $left.subtree$left.subtree$prop
# [1] 1
# 
# $left.subtree$left.subtree$label
# [1] "versicolor"
# 
# 
# $left.subtree$right.subtree
# $left.subtree$right.subtree$`split`
# [1] 3.0 5.1
# 
# $left.subtree$right.subtree$left.subtree
# $left.subtree$right.subtree$left.subtree$`split`
# [1] 4.0 1.6
# 
# $left.subtree$right.subtree$left.subtree$left.subtree
# $left.subtree$right.subtree$left.subtree$left.subtree$`split`
# [1] 3.0 4.9
# 
# $left.subtree$right.subtree$left.subtree$left.subtree$left.subtree
# $left.subtree$right.subtree$left.subtree$left.subtree$left.subtree$`data`
# Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
# 88           6.3         2.3          4.4         1.3 versicolor
# 59           6.6         2.9          4.6         1.3 versicolor
# 51           7.0         3.2          4.7         1.4 versicolor
# 54           5.5         2.3          4.0         1.3 versicolor
# 85           5.4         3.0          4.5         1.5 versicolor
# 60           5.2         2.7          3.9         1.4 versicolor
# 86           6.0         3.4          4.5         1.6 versicolor
# 76           6.6         3.0          4.4         1.4 versicolor
# 65           5.6         2.9          3.6         1.3 versicolor
# 53           6.9         3.1          4.9         1.5 versicolor
# 52           6.4         3.2          4.5         1.5 versicolor
# 92           6.1         3.0          4.6         1.4 versicolor
# 100          5.7         2.8          4.1         1.3 versicolor
# 56           5.7         2.8          4.5         1.3 versicolor
# 93           5.8         2.6          4.0         1.2 versicolor
# 62           5.9         3.0          4.2         1.5 versicolor
# 67           5.6         3.0          4.5         1.5 versicolor
# 64           6.1         2.9          4.7         1.4 versicolor
# 97           5.7         2.9          4.2         1.3 versicolor
# 77           6.8         2.8          4.8         1.4 versicolor
# 87           6.7         3.1          4.7         1.5 versicolor
# 96           5.7         3.0          4.2         1.2 versicolor
# 95           5.6         2.7          4.2         1.3 versicolor
# 
# $left.subtree$right.subtree$left.subtree$left.subtree$left.subtree$prop
# [1] 1
# 
# $left.subtree$right.subtree$left.subtree$left.subtree$left.subtree$label
# [1] "versicolor"
# 
# 
# $left.subtree$right.subtree$left.subtree$left.subtree$right.subtree
# $left.subtree$right.subtree$left.subtree$left.subtree$right.subtree$`split`
# [1] 4.0 1.5
# 
# $left.subtree$right.subtree$left.subtree$left.subtree$right.subtree$left.subtree
# $left.subtree$right.subtree$left.subtree$left.subtree$right.subtree$left.subtree$`data`
# Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
# 134          6.3         2.8          5.1         1.5 virginica
# 120          6.0         2.2          5.0         1.5 virginica
# 
# $left.subtree$right.subtree$left.subtree$left.subtree$right.subtree$left.subtree$prop
# [1] 1
# 
# $left.subtree$right.subtree$left.subtree$left.subtree$right.subtree$left.subtree$label
# [1] "virginica"
# 
# 
# $left.subtree$right.subtree$left.subtree$left.subtree$right.subtree$right.subtree
# $left.subtree$right.subtree$left.subtree$left.subtree$right.subtree$right.subtree$`data`
# Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
# 84            6         2.7          5.1         1.6 versicolor
# 
# $left.subtree$right.subtree$left.subtree$left.subtree$right.subtree$right.subtree$prop
# [1] 1
# 
# $left.subtree$right.subtree$left.subtree$left.subtree$right.subtree$right.subtree$label
# [1] "versicolor"
# 
# 
# 
# 
# $left.subtree$right.subtree$left.subtree$right.subtree
# $left.subtree$right.subtree$left.subtree$right.subtree$`data`
# Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
# 78          6.7           3            5         1.7 versicolor
# 
# $left.subtree$right.subtree$left.subtree$right.subtree$prop
# [1] 1
# 
# $left.subtree$right.subtree$left.subtree$right.subtree$label
# [1] "versicolor"
# 
# 
# 
# $left.subtree$right.subtree$right.subtree
# $left.subtree$right.subtree$right.subtree$`data`
# Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
# 130          7.2           3          5.8         1.6 virginica
# 
# $left.subtree$right.subtree$right.subtree$prop
# [1] 1
# 
# $left.subtree$right.subtree$right.subtree$label
# [1] "virginica"
# 
# 
# 
# 
# $right.subtree
# $right.subtree$`split`
# [1] 4.0 2.2
# 
# $right.subtree$left.subtree
# $right.subtree$left.subtree$`data`
# Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
# 140          6.9         3.1          5.4         2.1  virginica
# 113          6.8         3.0          5.5         2.1  virginica
# 124          6.3         2.7          4.9         1.8  virginica
# 104          6.3         2.9          5.6         1.8  virginica
# 71           5.9         3.2          4.8         1.8 versicolor
# 126          7.2         3.2          6.0         1.8  virginica
# 105          6.5         3.0          5.8         2.2  virginica
# 131          7.4         2.8          6.1         1.9  virginica
# 112          6.4         2.7          5.3         1.9  virginica
# 103          7.1         3.0          5.9         2.1  virginica
# 132          7.9         3.8          6.4         2.0  virginica
# 143          5.8         2.7          5.1         1.9  virginica
# 150          5.9         3.0          5.1         1.8  virginica
# 102          5.8         2.7          5.1         1.9  virginica
# 128          6.1         3.0          4.9         1.8  virginica
# 109          6.7         2.5          5.8         1.8  virginica
# 138          6.4         3.1          5.5         1.8  virginica
# 125          6.7         3.3          5.7         2.1  virginica
# 127          6.2         2.8          4.8         1.8  virginica
# 122          5.6         2.8          4.9         2.0  virginica
# 118          7.7         3.8          6.7         2.2  virginica
# 111          6.5         3.2          5.1         2.0  virginica
# 148          6.5         3.0          5.2         2.0  virginica
# 147          6.3         2.5          5.0         1.9  virginica
# 
# $right.subtree$left.subtree$prop
# [1] 0.9583333
# 
# $right.subtree$left.subtree$label
# [1] "virginica"
# 
# 
# $right.subtree$right.subtree
# $right.subtree$right.subtree$`data`
# Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
# 145          6.7         3.3          5.7         2.5 virginica
# 141          6.7         3.1          5.6         2.4 virginica
# 119          7.7         2.6          6.9         2.3 virginica
# 144          6.8         3.2          5.9         2.3 virginica
# 146          6.7         3.0          5.2         2.3 virginica
# 136          7.7         3.0          6.1         2.3 virginica
# 115          5.8         2.8          5.1         2.4 virginica
# 110          7.2         3.6          6.1         2.5 virginica
# 
# $right.subtree$right.subtree$prop
# [1] 1
# 
# $right.subtree$right.subtree$label
# [1] "virginica"