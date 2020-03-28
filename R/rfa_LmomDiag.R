#' @export
LmomDiag <- function(distr = c('glo','gev','gno','pe3','gpa'), 
                     x = seq(-1,1, len = 50), plot = TRUE){
  
  ## Coefficient of fitted polynomials that approximate the relation between
  ## L-skewness and L-kurtosis
  bmat <- read.table(text = "
               glo          gev           gno           pe3           gpa
  0   1.666667e-01  0.107158699  1.225038e-01  1.226259e-01 -4.316343e-11
  1   5.500064e-16  0.112615852  2.100695e-17  4.849455e-16  2.000000e-01
  2   8.333333e-01  0.845179976  7.923906e-01  2.887205e-01  9.600000e-01
  3  -1.085827e-14 -0.073172867 -7.453141e-15 -4.074033e-15 -1.919999e-01
  4   2.658698e-14  0.005277763 -2.114303e-02  1.064608e+00  3.839996e-02
  5   4.643463e-14 -0.066938343  4.843957e-14  1.743865e-14 -7.680604e-03
  6  -9.309061e-14  0.100281501  2.770630e-01 -8.906645e-01  1.536177e-03
  7  -7.209454e-14  0.087677507 -9.447091e-14 -3.055534e-14 -3.057862e-04
  8   1.274086e-13 -0.132783946 -3.775294e-01  5.728072e-01  6.109048e-05
  9   3.641720e-14 -0.060103832  5.399747e-14  1.810627e-14 -1.370841e-05
  10 -5.915841e-14  0.074848480  2.066031e-01 -1.581181e-01  2.769001e-06")

  bmat <- as.matrix(bmat[, distr])
  
  ## Evaluate the function at multiple points
  xmat <- model.matrix(~poly(x, 10, raw = TRUE))
  pt <- xmat %*% bmat
  
  if(plot)
    for(ii in 1:ncol(bmat)) lines(x, pt[,ii], col = ii)  
  
  return(invisible(pt)) 
}
