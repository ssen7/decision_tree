# Consider the following decision tree, where the parenthesized values
# are splits (j,s) of a predictor j and threshold s, and the sets of
# values {...} are observation indices:
#
#                  (1,2.4)
#                   /    \
#                 /        \
#               /            \
#            (2,1.2)      (3,4.6)
#             /  \          /  \
#           /      \      /      \
#        {1,2}   {3,4} {5,6}    {7,8}
#         R1       R2   R3        R4
#
# The above tree has 3 split points fit to 8 observations, resulting
# in 4 prediction regions R1, R2, R3, and R4. This tree could be 
# represented in R with the following structure:

sample.tree = list(split = c(1,2.4), 
                   left.subtree = list(split = c(2,1.2),
                                       left.subtree = list(data = c(1,2)),
                                       right.subtree = list(data = c(3,4))),
                   right.subtree = list(split = c(3,4.6),
                                        left.subtree = list(data = c(5,6)),
                                        right.subtree = list(data = c(7,8)))
)

# Given the above data structure, we can do things like the following:

sample.tree$split                                          # root split condition

sample.tree$left.subtree$left.subtree$data                 # observations in R1
sample.tree$right.subtree$left.subtree$data                # observations in R3

"split" %in% names(sample.tree$right.subtree)              # is the root's right-subtree a split node?
"data" %in% names(sample.tree$right.subtree)               # is the root's right-subtree a region?

"split" %in% names(sample.tree$right.subtree$left.subtree) # is this subtree a split node?
"data" %in% names(sample.tree$right.subtree$left.subtree)  # is this subtree a region?
