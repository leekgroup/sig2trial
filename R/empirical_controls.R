#'
#' Empirical controls calculation function
#'
#' Generate candidate top-scoring pairs (TSPs) by finding "empirical
#' control" genes within quantiles of the data.
#'
#' @param exprs An p x m input matrix of gene expression values with p genes and m samples. This matrix MUST have rownames.
#' @param n Number of pairs to create within each quantile bin (default 40)
#' @param q Number of quantiles desired (default 4)
#'
#' @export
#'
#' @details We intend to use TSPs as the features in our gene signature. Examining all features
#' would require generating $p$ choose 2 comparisons, most of which would be uninformative. We prioritize
#' the creation of features where one gene is expressed at a constant level across classes and the other
#' gene is expressed high and low across classes. To unearth pairs that behave like this, we rank all
#' genes by their average expression and break them into quantile groups. We then rank all genes within
#' each group by their variance and compute comparisons between the top $n$ most variable and least variable genes.
#' The result of this function provides a matrix of size $(n*q)$ x $m$, where each row is a binary vector indicating
#' the value of I($g_1 > g_2$) for two selected.
#'
#' @return A (n*q) x m matrix with TSPs in rows and samples in columns

empirical_controls = function(exprs, n=40, q=4){
  if(is.null(rownames(exprs))){
	stop("Input data matrix needs rownames.")
  }

  gm <- cbind(rowMeans(exprs), apply(exprs,1,sd))
  qt <- quantile(gm[,1], na.rm=T, probs=seq(0,1,length.out=q+1)) # Get cut points
  all_pairs <- all_min <- all_max <- c()
  
  for(i in 1:(length(qt)-1)){
    # Get the ith quantile subset and operate within it
    sub <- gm[which(gm[,1] >= qt[i] & gm[,1] < qt[i+1]),]
    rn <- rownames(sub)
    sd_ord <- order(sub[,2], sub[,1])
    rn_min <- rn[head(sd_ord, n)]
    rn_max <- rn[tail(sd_ord, n)]
    
    all_pairs <- rbind(all_pairs, make_pairs(rn_min, rn_max, exprs, n))
  }	
  
  all_pairs
}