
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

#include <iostream>
#include <chrono>
#include <ctime>

using namespace Rcpp;
using namespace arma;

// did2s -----------------------------------------------------------------------

// x2'x1 (x10'x10)^-1
// [[Rcpp::export]]
arma::mat make_V(arma::sp_mat x1, arma::sp_mat x10, arma::sp_mat x2) {
	arma::mat A, B, V;
	A = arma::mat(x10.t() * x10);
	B = arma::mat(x2.t() * x1);
	B = B.t();

	V = arma::solve(A, B);
	V = V.t();

	return V;
}

// (X_2'X_2)^-1 (sum W_g W_g') (X_2'X_2)^-1
// [[Rcpp::export]]
arma::mat make_sandwich(arma::sp_mat x2, arma::mat meat_sum) {
	arma::mat A = arma::mat(x2.t() * x2);
	return A.i() * meat_sum * A.i();
}


// [[Rcpp::export]]
arma::mat make_meat(
		arma::sp_mat& x2g, arma::sp_mat& x10g,
		arma::vec first_ug, arma::vec second_ug,
		arma::mat& V
) {
	arma::mat A, B, W;
	A = x2g.t() * second_ug;
	B = V * x10g.t() * first_ug;
	W = A-B;
	return W * W.t();
}

// [[Rcpp::export]]
arma::mat make_meat_nonsparse(
		arma::mat& x2g, arma::mat& x10g,
		arma::vec first_ug, arma::vec second_ug,
		arma::mat& V
) {
	arma::mat A, B, W;
	A = x2g.t() * second_ug;
	B = V * x10g.t() * first_ug;
	W = A-B;
	return W * W.t();
}
