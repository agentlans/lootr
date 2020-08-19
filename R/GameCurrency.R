#' Calculates the values of virtual goods in terms of a currency
#'
#' @param transactions A numeric matrix representing the transactions available in the game.
#'     Each row represents a transaction.
#'     The first column represents the cost of a transaction in some currency.
#'     If the currency is used up in the transaction, then the value should be negative.
#'
#'     The remaining columns represent the quantity of virtual goods gained after the transaction,
#'     each column representing a specific virtual good.
#'     There should be at least as many transactions as there are virtual goods.
#' @return Values of each virtual good in terms of the currency given in the matrix.
#'     If the matrix of transactions is over- or under-determined, then the values of goods
#'     are returned which minimizes the squared error between the actual and calculated total values
#'     of the transaction.
#' @export
valueGoods <- function(transactions) {
  d <- transactions
  m <- t(d[,-1]) # Without the first column which is base currency
  ginvm <- MASS::ginv(m)
  td <- t(d)

  # For each virtual good...
  cost <- sapply(1:nrow(m), function(ind) {
    y <- rep(0, ncol(d) - 1)
    y[ind] <- 1
    # Find linear combination of transactions that produces exactly 1 unit of that good
    x <- ginvm %*% y
    # Find the currency needed to fund that combo of transactions.
    # The returned value is negative because that currency is consumed.
    (td %*% x)[1]
  })
  # The value of each currency in terms of base currency
  names(cost) <- colnames(d)[-1]
  if (any(cost > 0)) {
    warning("Some items were found to have negative value. Please check your transaction matrix.")
  }
  -cost
}

