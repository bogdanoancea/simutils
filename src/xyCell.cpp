#include <Rcpp.h>
using namespace Rcpp;

//' Get cell number from x and y coordinates
//' 
//' NB: This source code is taken from \code{raster} package with a slight 
//' change of name.
//' 
//' @param ncols Number of colums of the raster grid.
//' 
//' @param nrows Number of rows of the raster grid.
//' 
//' @param xmin Minimal value of x coordinates.
//' 
//' @param xmax Maximal value of x coordinates.
//' 
//' @param ymin Minimal value of y coordinates.
//' 
//' @param ymax Maximal value of y coordinates.
//' 
//' @param x Vector of x coordinates.
//' 
//' @param y Vector of y coordinates.
//' 
//' @export
// [[Rcpp::export(name = "doCellFromXY")]]
  NumericVector doCellFromXY(
    int ncols, int nrows, double xmin, double xmax, double ymin, double ymax,
    NumericVector x, NumericVector y) {
    
    size_t len = x.size();
    
    double yres_inv = nrows / (ymax - ymin);
    double xres_inv = ncols / (xmax - xmin);
    
    //IntegerVector result(len);
    NumericVector result(len);
    
    for (size_t i = 0; i < len; i++) {
      // cannot use trunc here because trunc(-0.1) == 0
      double row = floor((ymax - y[i]) * yres_inv);
      // points in between rows go to the row below
      // except for the last row, when they must go up
      if (y[i] == ymin) {  
        row = nrows-1 ;
      }
      
      double col = floor((x[i] - xmin) * xres_inv);
      // as for rows above. Go right, except for last column
      if (x[i] == xmax) {
        col = ncols-1 ;
      }
      
      if (row < 0 || row >= nrows || col < 0 || col >= ncols) {
        result[i] = NA_REAL;
      } else {
       // result[i] = static_cast<int>(row) * ncols + static_cast<int>(col) + 1;
        result[i] = row * ncols + col + 1 ;
      }
    }
    
    return result;
  }
  
//' Get x and y coordinates from cell ID number.
//' 
//' NB: This source code is taken from \code{raster} package with a slight 
//' change of name.
//' 
//' @param ncols Number of colums of the raster grid.
//' 
//' @param nrows Number of rows of the raster grid.
//' 
//' @param xmin Minimal value of x coordinates.
//' 
//' @param xmax Maximal value of x coordinates.
//' 
//' @param ymin Minimal value of y coordinates.
//' 
//' @param ymax Maximal value of y coordinates.
//' 
//' @param cell Vector of cell ID numbers.
//' 
//' @export
// [[Rcpp::export(name = "doXYFromCell")]]
  NumericMatrix doXYFromCell(
    unsigned ncols, unsigned nrows, double xmin, double xmax, double ymin, double ymax,
    NumericVector cell	//    IntegerVector cell
  ) {
    size_t len = cell.size();
    
    double yres = (ymax - ymin) / nrows;
    double xres = (xmax - xmin) / ncols;
    
    NumericMatrix result(len, 2);
    
    for (size_t i = 0; i < len; i++) {
      // double in stead of int
      double c = cell[i] - 1;
      double row = floor(c / ncols);
      double col = c - row * ncols;
      result(i,0) = (col + 0.5) * xres + xmin;
      result(i,1) = ymax - (row + 0.5) * yres;
    }
    
    return result;
  }
  