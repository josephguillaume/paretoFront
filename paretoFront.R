library(Rcpp)
cppFunction('
LogicalVector paretofront(NumericMatrix M){
  unsigned int t,s,i,j,j1,j2;
  bool coldominatedflag;
  unsigned int row=M.nrow(),col=M.ncol();
  
  LogicalVector checklist(row);
  LogicalVector front(row);
  for(t = 0; t<row; t++) checklist[t] = true;
      for(s = 0; s<row; s++) {
        t=s;
        if (!checklist[t]) continue;
        checklist[t]=false;
        coldominatedflag=true;
        for(i=t+1;i<row;i++) {
            if (!checklist[i]) continue;
            checklist[i]=false;
            for (j=0,j1=i,j2=t;j<col;j++,j1+=row,j2+=row) {
                if (M[j1] < M[j2]) {
                    checklist[i]=true;
                    break;
                }
            }
            if (!checklist[i]) continue;
            coldominatedflag=false;
            for (j=0,j1=i,j2=t;j<col;j++,j1+=row,j2+=row) {
                if (M[j1] > M[j2]) {
                    coldominatedflag=true;
                    break;
                }
            }
            if (!coldominatedflag) {     //swap active index continue checking
                front[t]=false;
                checklist[i]=false;
                coldominatedflag=true;
                t=i;
            }
        }
        front[t]=coldominatedflag;
        if (t>s) {
            for (i=s+1; i<t; i++) {
                if (!checklist[i]) continue;
                checklist[i]=false;
                for (j=0,j1=i,j2=t;j<col;j++,j1+=row,j2+=row) {
                    if (M[j1] < M[j2]) {
                        checklist[i]=true;
                        break;
                    }
                }
            }
        }
    }

  return(front);
}')


paretoGroup <- function(X){
  m=nrow(X)
  n=ncol(X)
  groupcut=floor(2^13/n)
  gRoup=max(1,ceiling(m/groupcut))
  front=rep(FALSE,m)
  for (k in 1:gRoup){
    z0=(k-1)*groupcut;
    z=(z0+1):min(z0+groupcut,m)
    front[z]=paretofront(X[z,])
  }
  print(sum(front))
  ##browser()
  ##if(sum(front)>5e4) stop("Front is still very large")
  if (gRoup>1){
    front[front]=paretofront(X[front,])
  }
  return(front)
}


## M=matrix(runif(1e5),ncol=2)
## pp <- paretofront(M)
## plot(M[pp,])
