# library(grf) p = 10 n = 100

# X = matrix(2 * runif(n * p) - 1, n, p) Y = (X[,1] > 0) + 2 * rnorm(n)

# forest = regression_forest(X, Y)

# function that takes in a list of samples and calculates the r_loss
# (assuming regression tree)
r_loss <- function(samples) {
  size <- length(samples)
  if (size == 1) {
    return(samples[1]^2)
  }
  unscaled_spread <- sum((samples - mean(samples))^2)
  output <- unscaled_spread * (size^2)/((size - 1)^2)
  return(output)
}

# function that takes as input a tree, the index of a node, the extra
# cost for not pruning, and a list that records whether each node
# should be pruned and a list of samples in that node and outputs the
# r_loss of that node, as well as the modified list
get_r_loss <- function(tree, index, cost, prune_info) {
  node <- tree$nodes[[index]]
  if (node$is_leaf) {
    prune_info[[index]]$is_pruned_leaf <- TRUE
    prune_info[[index]]$samples <- node$samples
    # here we assume that the samples contain the honest sample, needs to
    # be fixed perhaps should pass on all the S, H, O samples from one node
    # to the next
    node_r_loss <- r_loss(node$samples)
    return(list(node_r_loss = node_r_loss, prune_info = prune_info))
  } else {
    left_leaf <- get_r_loss(tree, node$left_child, cost, prune_info)
    new_prune_info <- left_leaf$prune_info
    left_r_loss <- left_leaf$node_r_loss
    right_leaf <- get_r_loss(tree, node$right_child, cost, new_prune_info)
    new_prune_info <- right_leaf$prune_info
    right_r_loss <- right_leaf$node_r_loss
    node_samples <- c(new_prune_info[[node$left_child]]$samples, new_prune_info[[node$right_child]]$samples)
    new_prune_info[[index]]$samples <- node_samples
    node_r_loss <- r_loss(node_samples)
    if (node_r_loss < (left_r_loss + right_r_loss + cost)) {
      new_prune_info[[index]]$is_pruned_leaf <- TRUE
      return(list(node_r_loss = node_r_loss, prune_info = new_prune_info))
    } else {
      new_prune_info[[index]]$is_pruned_leaf <- FALSE
      return(list(node_r_loss = left_r_loss + right_r_loss + cost,
                  prune_info = new_prune_info))
    }
  }
}

# function that takes as input a forest and the cost for not pruning
# returns an index of the best tree, the r_loss, and a list
# corresponding to the best tree, where each element of the list
# corresponds to a node. The attribute of the nodes in the list include
# is_pruned_leaf, which indicates whether the node is a leaf in the
# pruned tree (or rather it indicates whether the r_loss of the node
# comes directly from the samples in that node or from its children
# nodes), and samples, which is a vector of the samples in that node
find_best_tree <- function(forest, cost = 0) {
  best_r_loss <- Inf
  best_tree <- 0
  best_prune_info <- list()
  for (t in 1:forest$num.trees) {
    t_tree <- get_tree(forest, t)
    prune_info <- rep(list(list(is_pruned_leaf = NA, samples = NA)),
                      length(t_tree$nodes))
    t_tree <- get_r_loss(t_tree, 1, cost, prune_info)
    if (t_tree$node_r_loss < best_r_loss) {
      best_r_loss <- t_tree$node_r_loss
      best_tree <- t
      best_prune_info <- t_tree$prune_info
    }
  }
  return(list(best_tree = best_tree, best_r_loss = best_r_loss, best_prune_info = best_prune_info))
}
