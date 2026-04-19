#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

#include <cmath>

// Function to round a number to a given number of decimal places
double roundTo(double value, int decimals) {
  double factor = std::pow(10.0, decimals);
  return std::round(value * factor) / factor;
}

// [[Rcpp::export]]
arma::vec candidate_cor_cpp(const arma::mat &candidate, const arma::vec &outcome) {
  // Combine candidate predictors and outcome column
  arma::mat full_data = arma::join_horiz(candidate, outcome);

  // Compute the correlation matrix
  arma::mat C = arma::cor(full_data);

  // Use number of columns since the matrix is square.
  int p = C.n_cols;
  int num_elements = (p * (p - 1)) / 2;
  arma::vec cor_vec(num_elements);
  int idx = 0;

  // Extract upper-triangular elements in column-major order:
  for (int j = 1; j < p; j++) {
    for (int i = 0; i < j; i++) {
      cor_vec(idx++) = C(i, j);
    }
  }
  return cor_vec;
}

// [[Rcpp::export]]
arma::vec candidate_reg_cpp(const arma::mat &candidate,
                            const arma::vec &y,
                            const arma::uvec &positions) {
  // candidate: matrix of predictors (without intercept)
  // y: outcome vector
  // positions: a vector (1-indexed, as produced by R's match()) indicating
  // which columns of the full design matrix to use for the regression.

  int n = candidate.n_rows;
  int p = candidate.n_cols;
  int num_interactions = (p * (p - 1)) / 2;
  int total_cols = 1 + p + num_interactions;  // intercept + main effects + interactions

  // Build full design matrix X.
  arma::mat X(n, total_cols, arma::fill::ones);
  // Main effects: columns 1 to p (column 0 is intercept).
  X.cols(1, p) = candidate;

  // Fill in interaction terms in the same order as in your R function:
  int col_index = p + 1;
  for (int i = 0; i < p; i++) {
    for (int j = i + 1; j < p; j++) {
      X.col(col_index) = candidate.col(i) % candidate.col(j);
      col_index++;
    }
  }

  // Now, subset the full design matrix using the positions vector.
  // Note: positions are 1-indexed in R; convert to 0-indexed in C++.
  arma::mat X_sub(n, positions.n_elem);
  for (arma::uword i = 0; i < positions.n_elem; i++) {
    // Subtract 1 to convert from R's 1-indexing to C++'s 0-indexing.
    unsigned int pos = positions(i) - 1;
    X_sub.col(i) = X.col(pos);
  }

  // Solve the OLS problem using only the columns specified by X_sub.
  arma::vec beta = arma::solve(X_sub, y);
  return beta;
}

// [[Rcpp::export]]
arma::mat candidate_reg_cpp_se(const arma::mat &candidate,
                            const arma::vec &y,
                            const arma::uvec &positions) {
  // candidate: matrix of predictors (without intercept)
  // y: outcome vector
  // positions: a vector (1-indexed, as produced by R's match()) indicating
  // which columns of the full design matrix to use for the regression.

  int n = candidate.n_rows;
  int p = candidate.n_cols;
  int num_interactions = (p * (p - 1)) / 2;
  int total_cols = 1 + p + num_interactions;  // intercept + main effects + interactions

  // Build full design matrix X.
  arma::mat X(n, total_cols, arma::fill::ones);

  // Main effects: columns 1 to p (column 0 is intercept).
  X.cols(1, p) = candidate;

  // Fill in interaction terms in the same order as in your R function:
  int col_index = p + 1;
  for (int i = 0; i < p; i++) {
    for (int j = i + 1; j < p; j++) {
      X.col(col_index) = candidate.col(i) % candidate.col(j);
      col_index++;
    }
  }

  // Subset the design matrix using the positions vector.
  // Note: positions are 1-indexed in R; convert to 0-indexed in C++.
  arma::mat X_sub(n, positions.n_elem);
  for (arma::uword i = 0; i < positions.n_elem; i++) {
    unsigned int pos = positions(i) - 1;
    if (pos >= X.n_cols) {
      Rcpp::stop("Error: a position is out of range.");
    }
    X_sub.col(i) = X.col(pos);
  }

  // Solve the OLS problem using only the columns specified by X_sub.
  arma::vec beta = arma::solve(X_sub, y);

  // Calculate residuals.
  arma::vec residuals = y - X_sub * beta;

  // Degrees of freedom: n - k, where k is number of parameters estimated.
  int k = X_sub.n_cols;
  double sigma2_hat = arma::dot(residuals, residuals) / (n - k);

  // Variance-covariance matrix of beta estimates.
  arma::mat XtX_inv = arma::inv_sympd(X_sub.t() * X_sub);
  arma::mat var_beta = sigma2_hat * XtX_inv;

  // Standard errors: square roots of the diagonal elements.
  arma::vec se = arma::sqrt(var_beta.diag());

  // Combine beta and se into a matrix: first column beta, second column se.
  arma::mat out(beta.n_elem, 2);
  out.col(0) = beta;
  out.col(1) = se;

  return out;
}


// Helper: Objective function for the vector optimization
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
double objective_cpp(NumericVector x,
                     double target_sd) {
  int n = x.size();
  if(n < 2) return NA_REAL; // Avoid division by zero in sd calculation

  // Compute mean of x
  double sum = 0.0;
  for (int i = 0; i < n; i++) {
    sum += x[i];
  }
  double mean_x = sum / n;

  // Compute sample standard deviation (denom = n - 1)
  double ssd = 0.0;
  for (int i = 0; i < n; i++) {
    double diff = x[i] - mean_x;
    ssd += diff * diff;
  }
  double sd_x = std::sqrt(ssd / (n - 1));

  // Compute squared errors
  double diff_sd = sd_x - target_sd;

  // Compute error.
  double total_error = std::sqrt((diff_sd) * (diff_sd));

  return total_error;
}





#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List error_function_cpp(const arma::mat &candidate,
                              const arma::vec &outcome,
                              const arma::vec &target_cor,
                              const arma::vec &target_reg,
                              const arma::vec &weight,
                              const arma::uvec &positions) {

  // Compute candidate's correlation vector (no rounding).
  arma::vec cor_vec = candidate_cor_cpp(candidate, outcome);

  // RMSE for correlations (over non-missing targets).
  arma::uvec idx = arma::find_finite(target_cor);
  double cor_error = std::sqrt(
    arma::accu(arma::square(cor_vec.elem(idx) - target_cor.elem(idx))) / idx.n_elem
  );

  // Compute candidate's regression coefficients (no rounding).
  arma::vec reg_vec = candidate_reg_cpp(candidate, outcome, positions);

  // RMSE for regression coefficients (over non-missing targets).
  arma::uvec idxx = arma::find_finite(target_reg);
  double reg_error = std::sqrt(
    arma::accu(arma::square(reg_vec.elem(idxx) - target_reg.elem(idxx))) / idxx.n_elem
  );

  // Weighted average of the two RMSE components.
  double total_error = (cor_error * weight(0) + reg_error * weight(1)) / 2.0;

  // Error ratio (with protection against division by zero).
  double error_ratio = (reg_error == 0.0) ? R_PosInf : cor_error / reg_error;

  return Rcpp::List::create(
    Rcpp::Named("total_error") = total_error,
    Rcpp::Named("error_ratio") = error_ratio
  );
}



// [[Rcpp::export]]
Rcpp::List error_function_cpp_se(const arma::mat &candidate,
                                 const arma::vec &outcome,
                                 const arma::vec &target_cor,
                                 const arma::mat &target_reg_se,
                                 const arma::vec &weight,
                                 const arma::uvec &positions) {

  // Compute candidate's correlation vector (no rounding).
  arma::vec cor_vec = candidate_cor_cpp(candidate, outcome);

  // RMSE for correlations (over non-missing targets).
  arma::uvec idx = arma::find_finite(target_cor);
  double cor_error = std::sqrt(
    arma::accu(arma::square(cor_vec.elem(idx) - target_cor.elem(idx))) / idx.n_elem
  );

  // Compute candidate's regression coefficients and standard errors (no rounding).
  arma::mat reg_se = candidate_reg_cpp_se(candidate, outcome, positions);

  // RMSE for regression coefficients and SEs (over non-missing targets).
  arma::uvec idxx = arma::find_finite(target_reg_se);
  double reg_error = std::sqrt(
    arma::accu(arma::square(reg_se.elem(idxx) - target_reg_se.elem(idxx))) / idxx.n_elem
  );

  // Weighted average of the two RMSE components.
  double total_error = (cor_error * weight(0) + reg_error * weight(1)) / 2.0;

  // Error ratio (with protection against division by zero).
  double error_ratio = (reg_error == 0.0) ? R_PosInf : cor_error / reg_error;

  return Rcpp::List::create(
    Rcpp::Named("total_error") = total_error,
    Rcpp::Named("error_ratio") = error_ratio
  );
}



// Helper: OLS soultion given design matrix and outcome
// [[Rcpp::export]]
arma::vec ols_from_design(const arma::mat &X, const arma::vec &y) {
  // X: design matrix from R's model.matrix() (including intercept and any interactions)
  // y: outcome vector

  // Solve the OLS problem: find beta such that X * beta approximates y.
  arma::vec beta = arma::solve(X, y);

  // Return the estimated coefficients as a plain vector.
  return beta;
}
