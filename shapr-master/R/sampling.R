#' Sample conditional variables using the Gaussian copula approach
#'
#' @param index_given Integer vector. The indices of the features to condition upon. Note that
#' \code{min(index_given) >= 1} and \code{max(index_given) <= m}.
#' @param m Positive integer. The total number of features.
#' @param x_test_gaussian Numeric matrix. Contains the observation whose predictions ought to be explained (test data),
#' after quantile-transforming them to standard Gaussian variables.
#' @param x_test Numeric matrix. Contains the features of the observation whose
#' predictions ought to be explained (test data).
#'
#' @return data.table
#'
#' @keywords internal
#'
#' @examples
#' m <- 10
#' n <- 40
#' n_samples <- 50
#' mu <- rep(1, m)
#' cov_mat <- cov(matrix(rnorm(n * m), n, m))
#' x_train <- MASS::mvrnorm(n, mu, cov_mat)
#' x_test <- MASS::mvrnorm(1, mu, cov_mat)
#' x_test_gaussian <- MASS::mvrnorm(1, mu, cov_mat)
#' index_given <- 3:6
#' ret <- shapr:::sample_copula(index_given, n_samples, mu, cov_mat, m = m,
#'                              x_test_gaussian, x_train, x_test)
#'
#' @author Martin Jullum

sample_copula <- function(index_given, n_samples, mu, cov_mat, m, x_test_gaussian, x_train, x_test) {
  # Handles the unconditional and full conditional separtely when predicting
  if (length(index_given) %in% c(0, m)) {
    ret <- matrix(x_test, ncol = m, nrow = 1)
  } else {
    dependent_ind <- (1:length(mu))[-index_given]

    tmp <- condMVNorm::condMVN(
      mean = mu,
      sigma = cov_mat,
      dependent.ind = dependent_ind,
      given.ind = index_given,
      X.given = x_test_gaussian[index_given]
    )

    ret0_z <- mvnfast::rmvn(n = n_samples, mu = tmp$condMean, sigma = tmp$condVar)

    ret0_x <- apply(
      X = rbind(ret0_z, x_train[, dependent_ind, drop = F]),
      MARGIN = 2,
      FUN = inv_gaussian_transform,
      n_z = n_samples
    )

    ret <- matrix(NA, ncol = m, nrow = n_samples)
    ret[, index_given] <- rep(x_test[index_given], each = n_samples)
    ret[, dependent_ind] <- ret0_x
  }
  colnames(ret) <- colnames(x_test)
  return(as.data.table(ret))
}

#' Sample conditional Gaussian variables following a causal chain graph with do-calculus
#'
#' @inheritParams sample_copula
#'
#' @return data.table
#'
#' @keywords internal
#'
#' @examples
#' m <- 10
#' n_samples <- 50
#' mu <- rep(1, m)
#' cov_mat <- cov(matrix(rnorm(n_samples * m), n_samples, m))
#' x_test <- matrix(MASS::mvrnorm(1, mu, cov_mat), nrow = 1)
#' cnms <- paste0("x", seq(m))
#' colnames(x_test) <- cnms
#' index_given <- c(4, 7)
#' ordering <- list(c(1,2,3),c(4,5,6),c(7,8,9,10))
#' confounding <- c(TRUE, FALSE, TRUE)
#' r <- sample_causal(index_given, n_samples, mu, cov_mat, m, x_test, ordering, confounding)
#'
#' @author Tom Heskes, Ioan Gabriel Bucur

sample_causal <- function(index_given, n_samples, mu, cov_mat, m, x_test, 
                          ordering = list(c(seq(m))), confounding = rep(FALSE, length(ordering))) {

  # Check input
  stopifnot(is.matrix(x_test))
  if (length(confounding) != length(ordering)) {
    stop("For each component in the partial order, it must be specified if there is confounding or not.")
  }
  if (!base::setequal(unlist(ordering), seq(m))) {
    stop(paste("Incomplete or incorrect partial ordering specified for", m, "variables"))
  }

  # not sure if this is needed/makes sense
  #  if (length(index_given) %in% c(0, m)) return(data.table::as.data.table(x_test))
  # if (is.null(ordering)) {
  #   ordering <- list(c(seq(m)))
  # }
  
  dependent_ind <- setdiff(1:length(mu),index_given)
  xall <- matrix(NA, ncol = m, nrow = n_samples)
  xall[, index_given] <- rep(x_test[index_given], each = n_samples)

  for(i in seq(length(ordering))) {
    # check overlap between dependent_ind and component
    to_be_sampled <- intersect(ordering[[i]],dependent_ind)
    if (length(to_be_sampled) > 0) {
      # condition upon all variables in ancestor components
      to_be_conditioned <- unlist(ordering[0:(i-1)])
      
      # back to conditioning if confounding is FALSE or no conditioning if confounding is TRUE
      if (!confounding[i]) {
        # add intervened variables in the same component
        to_be_conditioned <- union(intersect(ordering[[i]], index_given), to_be_conditioned)
      }
      if (length(to_be_conditioned) == 0) {
        # draw new samples from marginal distribution
        newsamples <- mvnfast::rmvn(n_samples, mu=mu[to_be_sampled], sigma=as.matrix(cov_mat[to_be_sampled,to_be_sampled]))
      } else {
        # compute conditional Gaussian
        C <- cov_mat[to_be_sampled,to_be_conditioned, drop=FALSE]
        D <- cov_mat[to_be_conditioned, to_be_conditioned]
        CDinv <- C %*% solve(D)
        cVar <- cov_mat[to_be_sampled,to_be_sampled] - CDinv %*% t(C)
        if (!isSymmetric(cVar)) {
          cVar <- Matrix::symmpart(cVar)
        }
        # draw new samples from conditional distribution
        mu_sample <- matrix(rep(mu[to_be_sampled],each=n_samples),nrow=n_samples)
        mu_cond <- matrix(rep(mu[to_be_conditioned],each=n_samples),nrow=n_samples)
        cMU <- mu_sample + t(CDinv %*% t(xall[,to_be_conditioned] - mu_cond))
        newsamples <- mvnfast::rmvn(n_samples, mu=matrix(0,1,length(to_be_sampled)), sigma=as.matrix(cVar))
        newsamples <- newsamples + cMU

#        DEPRECATED: MUCH TOO SLOW!!!
#        newsamples <- matrix(NA, ncol = length(to_be_sampled), nrow = n_samples)
#        for(j in seq(n_samples)) {
#          cMu <- c(mu[to_be_sampled] + CDinv %*% (xall[j,to_be_conditioned] - mu[to_be_conditioned]))
#          newsamples[j,] <- mvnfast::rmvn(1, mu=cMu, sigma=as.matrix(cVar))
#        }
      }
      xall[,to_be_sampled] <- newsamples
    }
  }

  colnames(xall) <- colnames(x_test)
  return(as.data.table(xall))

}

#' Sample conditional Gaussian variables
#'
#' @inheritParams sample_copula
#'
#' @return data.table
#'
#' @keywords internal
#'
#' @examples
#' m <- 10
#' n_samples <- 50
#' mu <- rep(1, m)
#' cov_mat <- cov(matrix(rnorm(n_samples * m), n_samples, m))
#' x_test <- matrix(MASS::mvrnorm(1, mu, cov_mat), nrow = 1)
#' cnms <- paste0("x", seq(m))
#' colnames(x_test) <- cnms
#' index_given <- c(4, 7)
#' r <- shapr:::sample_gaussian(index_given, n_samples, mu, cov_mat, m, x_test)
#'
#' @author Martin Jullum

sample_gaussian <- function(index_given, n_samples, mu, cov_mat, m, x_test) {

  # Check input
  stopifnot(is.matrix(x_test))

  # Handles the unconditional and full conditional separately when predicting
  cnms <- colnames(x_test)
  if (length(index_given) %in% c(0, m)) return(data.table::as.data.table(x_test))

  dependent_ind <- (1:length(mu))[-index_given]
  x_test_gaussian <- x_test[index_given]
  tmp <- condMVNorm::condMVN(
    mean = mu,
    sigma = cov_mat,
    dependent.ind = dependent_ind,
    given.ind = index_given,
    X.given = x_test_gaussian
  )

  # Makes the conditional covariance matrix symmetric in the rare case where numerical instability made it unsymmetric
  if (!isSymmetric(tmp[["condVar"]])) {
    tmp[["condVar"]] <- Matrix::symmpart(tmp$condVar)
  }

  ret0 <- mvnfast::rmvn(n = n_samples, mu = tmp$condMean, sigma = tmp$condVar)

  ret <- matrix(NA, ncol = m, nrow = n_samples)
  ret[, index_given] <- rep(x_test_gaussian, each = n_samples)
  ret[, dependent_ind] <- ret0

  colnames(ret) <- cnms
  return(as.data.table(ret))
}

#' Helper function to sample a combination of training and testing rows, which does not risk
#' getting the same observation twice. Need to improve this help file.
#'
#' @param ntrain Positive integer. Number of training observations to sample from.
#'
#' @param ntest Positive integer. Number of test observations to sample from.
#'
#' @param nsamples Positive integer. Number of samples.
#'
#' @param joint_sampling Logical. Indicates whether train- and test data should be sampled
#' separately or in a joint sampling space. If they are sampled separately (which typically
#' would be used when optimizing more than one distribution at once) we sample with replacement
#' if \code{nsamples > ntrain}. Note that this solution is not optimal. Be careful if you're
#' doing optimization over every test observation when \code{nsamples > ntrain}.
#'
#' @return data.frame
#'
#' @keywords internal
#'
#' @examples
#' ntrain <- 10
#' ntest <- 10
#' nsamples <- 7
#' joint_sampling <- FALSE
#' cnms <- c("samp_train", "samp_test")
#' x <- shapr:::sample_combinations(ntrain, ntest, nsamples, joint_sampling)
#'
#' @author Martin Jullum
sample_combinations <- function(ntrain, ntest, nsamples, joint_sampling = TRUE) {

  if (!joint_sampling) {

    # Sample training data
    samp_train <- sample(
      x = ntrain,
      size = nsamples,
      replace = ifelse(nsamples < ntrain, FALSE, TRUE)
    )

    # Sample test data
    samp_test <- sample(
      x = ntest,
      size = nsamples,
      replace = ifelse(nsamples < ntrain, nsamples > ntest, TRUE)
    )
  } else {

    n <- ntrain * ntest
    if (nsamples < n) {
      input_samp <- sample(
        x = n,
        size = nsamples,
        replace = FALSE
      )
    } else {
      input_samp <- seq(n)
    }

    samp_train <- (input_samp - 1) %% ntrain + 1
    samp_test <- (input_samp - 1) %/% ntrain + 1
  }
  ret <- data.frame(samp_train = samp_train, samp_test = samp_test)

  return(ret)
}
