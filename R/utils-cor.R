# Define a distance function https://davetang.org/muse/2010/11/26/hierarchical-clustering-with-p-values-using-spearman/
abs.spearman.dist <- function(x, ...) {
  x <- as.matrix(x)
  res <- as.dist(1 - abs(cor(x, method = "spearman", use = "everything")))
  res <- as.dist(res)
  attr(res, "method") <- "abs. spearman"
  return(res)
}

# Make non-significance symbols for heatmaps
make.sig.mat <- function(pmat, x.90 = TRUE){
  pval <- pmat$P
  sig <- ifelse(pval<=0.05, "", "O")
  sig2 <- ifelse(pval<=0.01, "", "o")
  sig3 <- ifelse(pval<=0.001, "", ".")
  sig123 <- ifelse(sig=="", ifelse(sig2=="",sig3, sig2), sig)
  
  if(x.90){
    sig123 <- ifelse(pmat$r>=.9,"X",sig123)
  }
  
  sig123
}

# heatmap function
speardist.heatmap <- function(varmat){
  spearcor <- Hmisc::rcorr(as.matrix(varmat), type = "spearman")
  
  spearcor.pval <- make.sig.mat(spearcor) # add significance
  
  gplots::heatmap.2(as.matrix(abs.spearman.dist(varmat)), trace = "none",
                    distfun = as.dist,
                    hclustfun = function(d) hclust(d, method = "complete"),
                    col=cm.colors(64), margin=c(5, 10),
                    offsetCol = 0, offsetRow = 0,
                    srtCol=45,  adjCol = c(0.9,0),
                    cellnote = spearcor.pval,
                    notecol = "black"
  )
}

