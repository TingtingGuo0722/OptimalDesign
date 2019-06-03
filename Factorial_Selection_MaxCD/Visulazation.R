source("E:/Manuscript/Writing/All The Figures and Tables MoP_1/Fig.3/Factorial/Code_diag.txt")

#n.c>n.r;

n.r=50;
n.c=51;

mat.1=matrix(1:(n.r*n.c),n.r,n.c)
if(max(dim(mat.1))%% 2==0){mat.1=cbind(mat.1,NA)};

n.r=nrow(mat.1)
n.c=ncol(mat.1)

T.1=Diag(mat.1)$Diagonal

mat.2=mat.1[,ncol(mat.1):1]
T.2=Diag(mat.2)$Diagonal

final.list=c(T.1[seq(1,n.c,2)],T.2[seq(2,n.c,2)]);

plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '',xlim=c(0,n.c),ylim=c(0,n.r))
rect(0,0,n.c,n.r)

for(i in 1:n.c){for(j in 1:n.r){rect(xl=i-1,yb=j-1,xr=i,yt=j,col="white",border="black")}}


for(v in 1:length(final.list))
{id=which(mat.1 == final.list[v], arr.ind = TRUE);rect(xl=id[2]-1,yb=id[1]-1,xr=id[2],yt=id[1],col="red",border="black")}


