#reads down first column, then down second, then specificies numbers of rows and columns
x = matrix(c(131,117,205, 329),2,2, byrow = T)
chisq.test(x)
chisq.test(x, correct=F)

