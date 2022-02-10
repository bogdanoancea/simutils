// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// doCellFromXY
NumericVector doCellFromXY(int ncols, int nrows, double xmin, double xmax, double ymin, double ymax, NumericVector x, NumericVector y);
RcppExport SEXP _simutils_doCellFromXY(SEXP ncolsSEXP, SEXP nrowsSEXP, SEXP xminSEXP, SEXP xmaxSEXP, SEXP yminSEXP, SEXP ymaxSEXP, SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type ncols(ncolsSEXP);
    Rcpp::traits::input_parameter< int >::type nrows(nrowsSEXP);
    Rcpp::traits::input_parameter< double >::type xmin(xminSEXP);
    Rcpp::traits::input_parameter< double >::type xmax(xmaxSEXP);
    Rcpp::traits::input_parameter< double >::type ymin(yminSEXP);
    Rcpp::traits::input_parameter< double >::type ymax(ymaxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(doCellFromXY(ncols, nrows, xmin, xmax, ymin, ymax, x, y));
    return rcpp_result_gen;
END_RCPP
}
// doXYFromCell
NumericMatrix doXYFromCell(unsigned ncols, unsigned nrows, double xmin, double xmax, double ymin, double ymax, NumericVector cell);
RcppExport SEXP _simutils_doXYFromCell(SEXP ncolsSEXP, SEXP nrowsSEXP, SEXP xminSEXP, SEXP xmaxSEXP, SEXP yminSEXP, SEXP ymaxSEXP, SEXP cellSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< unsigned >::type ncols(ncolsSEXP);
    Rcpp::traits::input_parameter< unsigned >::type nrows(nrowsSEXP);
    Rcpp::traits::input_parameter< double >::type xmin(xminSEXP);
    Rcpp::traits::input_parameter< double >::type xmax(xmaxSEXP);
    Rcpp::traits::input_parameter< double >::type ymin(yminSEXP);
    Rcpp::traits::input_parameter< double >::type ymax(ymaxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type cell(cellSEXP);
    rcpp_result_gen = Rcpp::wrap(doXYFromCell(ncols, nrows, xmin, xmax, ymin, ymax, cell));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_simutils_doCellFromXY", (DL_FUNC) &_simutils_doCellFromXY, 8},
    {"_simutils_doXYFromCell", (DL_FUNC) &_simutils_doXYFromCell, 7},
    {NULL, NULL, 0}
};

RcppExport void R_init_simutils(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}