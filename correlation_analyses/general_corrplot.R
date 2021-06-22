
##### Global correlation plots.

#Following the [plotting recipe](http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram) on sthda.com. 

corelation_plot <- function(meta, target_group){
  
  suppressPackageStartupMessages(require(vegan))
  suppressPackageStartupMessages(require(corrplot))
  packageVersion("corrplot")

#REad in the data. In this case there are two columns for Zm. Run one examples. Get feedback from Eric, the run the final on all samples.


sfc16s.cor<-cor(meta[complete.cases(meta),], method = "pearson")
colnames(sfc16s.cor) <- colnames(meta)
rownames(sfc16s.cor) <- colnames(meta)


#check the plot before doing the cor.test


#corrplot(sfc16s.cor, method="square", type="upper", order="hclust", tl.col="black", tl.srt=45)



#Now add the code for corr.test

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

#with corrected p-value
cor.mtest_corrected <- function(mat, ...) {
  library(psych)
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- corr.test(mat[, i], mat[, j], method = "pearson", adjust="holm")
      p.mat[i, j] <- p.mat[j, i] <- tmp$p
      #bonferroni correction?
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}


#cor.mtest function: produces p-value and confidence intervals for each pair of input feature. 


#calculate P values



sfc16s.pmat<-cor.mtest(meta[complete.cases(meta),])
sfc16s.pmat.corrected<-cor.mtest_corrected(meta[complete.cases(meta),])

#Add p values to correlelogram




#print(corrplot(sfc16s.cor, method= "circle", type="lower", tl.col="black", tl.srt= 45,  p.mat = sfc16s.pmat, sig.level = .05, diag=FALSE, tl.cex = 0.8, pch.cex = 0.8, number.cex=0.1))
print(corrplot::corrplot(sfc16s.cor, title = target_group, method= "circle", type="lower", tl.col="black", tl.srt= 45,  p.mat = sfc16s.pmat.corrected, sig.level = .05, diag=FALSE, tl.cex = 0.8, pch.cex = 0.8, number.cex=0.1,  mar=c(0,0,1,0)))
#corrplot(sfc16s.cor, method= "circle", type="lower", tl.col="black", tl.srt= 45,  p.mat = sfc16s.pmat, #sig.level = .05, diag=FALSE, tl.cex = 0.8, pch.cex = 0.8, number.cex=0.1, addCoef.col = "white")

}

