
#' Function that takes in a list of samples, with the corresponding Y values, and calculates the r_loss, assuming a regression tree.
#' If a leaf only has one sample, then we return the Y value squared. We choose this value because
#' we don't want to return 0, which will encourage the tree to keep leaves with 1 sample, but we also
#' don't want to return a huge value, which will force the tree to never have leaves with 1 sample, even if
#' the sample is an outlier. But of course we could choose to return something else when there is only
#' one sample in the leaf, if it makes more sense.
#'
#' @param Y The Y values
#' @param samples The samples on which to calculate the r_loss.
#'
#' @return The r_loss of the samples.
#'
#' @export
r_loss <- function(Y, samples) {
  size <- length(samples)
  if (size == 1) {
    return(Y[samples[1]]^2)
  }
  unscaled_spread <- sum((Y[samples] - mean(Y[samples]))^2)
  output <- unscaled_spread * (size^2)/((size - 1)^2)
  return(output)
}



#' Function that takes as input a tree, and calculates the r_loss of a specific node, specified by the index.
#'
#' @param Y The Y values of the data
#' @param tree The tree on which to calculate r_loss
#' @param index The index of the specific node whose r_loss we want
#' @param cost This is the cost that we pay if we do not prune the tree.
#' @param prune_info This is list that corresponds to nodes of the tree. Each element keeps track of the samples in the node and whether the node is a leaf in the pruned tree.
#'
#' @return The r_loss for the specified node, and the updated prune_info which keeps track of what nodes are now leaves.
#'
#' @examples \dontrun{
#' # Train a regression forest
#' n <- 50; p <- 10
#' X <- matrix(rnorm(n*p), n, p)
#' Y <- X[,1] * rnorm(n)
#' forest <- grf::regression_forest(X,Y)
#' tree <- grf::get_forest(forest, 1)
#'
#' # Calculate the r_loss of the tree
#' prune_info <- rep(list(list(is_pruned_leaf = FALSE, samples = c())), length(tree$nodes))
#' results <- get_r_loss(Y, tree, 1, prune_info)
#'
#' # Caluculate the r_loss of the 3rd node
#' prune_info <- rep(list(list(is_pruned_leaf = FALSE, samples = c())), length(tree$nodes))
#' results <- get_r_loss(Y, tree, 3, prune_info)
#' }
#'
#' @export
get_r_loss <- function(Y, tree, index, cost = 0, prune_info) {
  node <- tree$nodes[[index]]
  if (node$is_leaf) {
    # If the node is a leaf, then we jsut calculate the r_loss and return
    prune_info[[index]]$is_pruned_leaf <- TRUE
    prune_info[[index]]$samples <- node$samples
    node_r_loss <- r_loss(Y, node$samples)
    return(list(node_r_loss = node_r_loss, prune_info = prune_info))
  } else {
    # If the node is not a leaf, first we get the samples and r_loss of the left child
    left_leaf <- get_r_loss(Y, tree, node$left_child, cost, prune_info)
    new_prune_info <- left_leaf$prune_info
    left_r_loss <- left_leaf$node_r_loss
    # Then we get samples and r_loss from the right child
    right_leaf <- get_r_loss(Y, tree, node$right_child, cost, new_prune_info)
    new_prune_info <- right_leaf$prune_info
    right_r_loss <- right_leaf$node_r_loss
    # Then we aggregate the samples and calculace the aggregated r_loss
    node_samples <- c(new_prune_info[[node$left_child]]$samples, new_prune_info[[node$right_child]]$samples)
    new_prune_info[[index]]$samples <- node_samples
    node_r_loss <- r_loss(Y, node_samples)
    # if(length(left_r_loss) == 0){
    #   browser()
    # }
    # if (length(right_r_loss)==0){
    #   browser()
    # }
    # Compare the r_losses, and decide whether to prune, then return
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

#' Function that takes as input a forest and the cost for not pruning.
#' Returns an index of the best tree, the r_loss, and a list
#' corresponding to the best tree, where each element of the list
#' corresponds to a node. The attribute of the nodes in the list include
#' is_pruned_leaf, which indicates whether the node is a leaf in the
#' pruned tree (or rather it indicates whether the r_loss of the node
#' comes directly from the samples in that node or from its children
#' nodes), and samples, which is a vector of the samples in that node
#'
#' @param Y The Y values of the tree.
#' @param forest The forest from which we want the best tree.
#' @param cost The cost for not pruning
#'
#' @return A list with the index for the best tree, the corresponding r_loss, the and prune_info
#'
#' @examples \dontrun{
#' # Train a regression forest
#' n <- 50; p <- 10
#' X <- matrix(rnorm(n*p), n, p)
#' Y <- X[,1] * rnorm(n)
#' forest <- grf::regression_forest(X,Y)
#'
#' # Find the best tree
#' best_tree_info <- find_best_tree(Y, forest)
#' }
#'
#' @export
find_best_tree <- function(Y, forest, cost = 0) {
  best_r_loss <- Inf
  best_tree <- 0
  best_prune_info <- list()
  for (t in 1:forest$num.trees) {
    t_tree <- grf::get_tree(forest, t)
    prune_info <- rep(list(list(is_pruned_leaf = FALSE, samples = NA)),
                      length(t_tree$nodes))
    t_tree <- get_r_loss(Y, t_tree, 1, cost, prune_info)
    if (t_tree$node_r_loss < best_r_loss) {
      best_r_loss <- t_tree$node_r_loss
      best_tree <- t
      best_prune_info <- t_tree$prune_info
    }
  }
  return(list(best_tree = best_tree, best_r_loss = best_r_loss, best_prune_info = best_prune_info))
}

#' Takes the x (covariates) of a data point, and finds the leaf in the pruned tree that it belongs to
#'
#' @param x Covariates of a data point
#' @param tree The tree that we want to put data points into
#' @param prune_info Info about which are leaves in the pruned tree
#'
#' @return The leaf that the data point belongs to.
#' @export
find_leaf <- function(x, tree, prune_info) {
  nodes <- tree$nodes

  # Begin at root
  n <- nodes[[1]]
  idx <- 1

  # Propagate down until hit leaf
  while(!prune_info[[n]]$is_pruned_leaf) {
    if (x[n$split_variable] <= n$split_value) {
      idx <- n$left_child
    } else {
      idx <- n$right_child
    }
    n <- nodes[[idx]]
  }
  return(idx)
}

#' Function that calculates the means and variances of leaves of a pruned tree, using out-of-bag samples.
#'
#' @param X Covariates
#' @param Y
#' @param tree The tree that we want to calculate with.
#' @param prune_info Info about where the tree is pruned.
#'
#' @return A list, tree_with_oob, where each element represents a node in the tree, with oob samples, oob means, oob standard deviations
#'
#' @examples \dontrun{
#' # Train a regression forest
#' n <- 50; p <- 10
#' X <- matrix(rnorm(n*p), n, p)
#' Y <- X[,1] * rnorm(n)
#' forest <- grf::regression_forest(X,Y)
#'
#' # Find the best tree
#' best_tree_info <- find_best_tree(Y, forest)
#'
#' # Calculate parameters for the best pruned tree
#' best_tree_params <- estimate_params(X, Y, best_tree_info$best_tree, best_tree_info$best_prune_info)
#' }
#'
#' @export
estimate_params <- function(X, Y, tree, prune_info){
  tree_with_oob <- rep(list(list(samples = NA, sample_mean = NA, sample_sd = NA)),
                       length(tree$nodes))
  oob_indices <- base::setdiff(seq(2*length(tree$drawn_samples)), tree$drawn_samples)
  for (idx in oob_indices) {
    correct_leaf <- find_leaf(X[idx,], tree, prune_info)
    tree_with_oob[[correct_leaf]]$samples <- c(tree_with_oob[[correct_leaf]]$samples, idx)
  }
  for (idx in 1:length(tree$nodes)) {
    if (prune_info[[idx]]$is_pruned_leaf){
      tree_with_oob[[idx]]$sample_mean <- mean(Y[tree_with_oob[[idx]]$samples])
      tree_with_oob[[idx]]$sample_sd <- sd(Y[tree_with_oob[[idx]]$samples])
    }
  }
  return(tree_with_oob)
}

