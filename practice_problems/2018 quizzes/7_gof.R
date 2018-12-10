#reads down first column, then down second, then specificies numbers of rows and columns
x <- as.table(matrix(c(5,15,12,45,35,38),2,3, byrow = T))
colnames(x) <- c("a", "b", "c")
rownames(x) <- c("quit", "not")
chisq.test(x)
chisq.test(x, correct=F)
chisq.test(x, correct=F)$expected

require(rcompanion)
bonf_correct_x <- pairwiseNominalIndependence(x, compare = "column", method = "bonf")
bonf_correct_x

#should be same as

y <- as.table(matrix(c(5,15,12,45,35,38),3,2, byrow = F))
rownames(y) <- c("a", "b", "c")
colnames(y) <- c("quit", "not")
chisq.test(y)
chisq.test(y, correct=F)
bonf_correct_y <- pairwiseNominalIndependence(y, compare = "row", method = "bonf")
bonf_correct_y