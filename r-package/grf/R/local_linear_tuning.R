#' Local linear forest tuning
#' 
#' Finds the optimal ridge penalty for local linear prediction.
#'
#' @param forest The forest used for prediction.
#' @param linear.correction.variables Variables to use for local linear prediction. If left null,
#'          all variables are used.
#' @param use.unweighted.penalty Optional parameter to adjust ridge regression standardization.
#' @param num.threads Number of threads used in training. If set to NULL, the software
#'                    automatically selects an appropriate amount.
#' @param lambda.path Optional list of lambdas to use for cross-validation.
#' @return A list of lambdas tried, corresponding errors, and optimal ridge penalty lambda.
#'
#' @examples \dontrun{
#' # Find the optimal tuning parameters.
#' n = 500; p = 10
#' X = matrix(rnorm(n*p), n, p)
#' Y = X[,1] * rnorm(n)
#' forest = regression_forest(X,Y)
#' tuned.lambda = tune_local_linear_forest(forest)
#'
#' # Use this parameter to predict from a local linear forest.
#' predictions = predict(forest, linear.correction.variables = 1:p, lambda = tuned.lambda)
#' }
#'
#' @export
tune_local_linear_forest <- function(forest,
                                     linear.correction.variables = NULL,
                                     use.unweighted.penalty = TRUE,
                                     num.threads = NULL,
                                     lambda.path = NULL) {
  Y = forest[["Y.orig"]]
  X = forest[["X.orig"]]
  data = create_data_matrices(X)
  forest.short = forest[-which(names(forest) == "X.orig")]

  # Validate variables
  num.threads = validate_num_threads(num.threads)
  linear.correction.variables = validate_ll_vars(linear.correction.variables, ncol(X))
  lambda.path = validate_ll_path(lambda.path)

  # Subtract 1 to account for C++ indexing
  linear.correction.variables = linear.correction.variables - 1

  prediction.object = local_linear_predict_oob(forest.short, data$default, data$sparse, lambda.path, use.unweighted.penalty,
                                 linear.correction.variables, num.threads)

  prediction.object = prediction.object$predictions
  errors = apply(prediction.object, MARGIN = 2, FUN = function(row){
    mean( (row - Y)**2 )
  })

  return(list(lambdas = lambda.path, errors = errors, oob.predictions = prediction.object,
           lambda.min = lambda.path[which.min(errors)]))
}
