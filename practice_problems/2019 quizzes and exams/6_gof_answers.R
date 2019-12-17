#reads down first column, then down second, then specificies numbers of rows and columns
x <- as.table(matrix(c(46,8,4,8,52,18,38,42,2,74,58,50),3,4, byrow = T))
colnames(x) <- c("a", "b", "c", "d")
rownames(x) <- c("decrease", "no change", "increase")
chisq.test(x)
chisq.test(x)$expected


library(rcompanion)
bonf_correct_x <- pairwiseNominalIndependence(x, compare = "column", method = "holm")
bonf_correct_x

