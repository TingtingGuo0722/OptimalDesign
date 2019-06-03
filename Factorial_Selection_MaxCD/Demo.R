source("E:/Code_diag.R")
mat.1=matrix(1:121,11,11)
T.1=Diag(mat.1)$Diagonal


mat.2=mat.1[,11:1]
T.2=Diag(mat.2)$Diagonal

final.list=c(T.1[seq(1,11,2)],T.2[seq(2,11,2)]);

##Note: It's alwasy good to keep your matrix demension odd numbers.