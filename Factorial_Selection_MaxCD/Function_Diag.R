########################Function_Diag#################################
Diag <- function(A)
{
  
  edge <- 1
  row <- nrow(A)
  col <- ncol(A)
  eps <- 10^(-10)
  
  #====Diagonal Line Slope====
  slope <- -row/col
  
  #====Extract Diagonal Elements====
  #Delta: Judging matrix determing whether the element is diagonal
  #Delta = 0: Off-diagonal elements
  #Delta = 1: Diagonal elements
  #Diagonal: Values of diagonal elements
  #Distance: Coverage extent of each elements;
  #          Inf means the corresponding cell does not intersect with the diagonal line
  #Row: Row number of diagonal elements
  #Col: Column number of diagonal elements
  #num: Number of diagonal elements
  Delta <- matrix(0,row,col)
  Distance <- matrix(Inf,row,col)
  Diagonal <- list()
  Row <- list()
  Col <- list()
  num <- 0
  for (r in 1:row)
  {
    for (c in 1:col)
    {
      delta <- 0
      x <- c-1+edge/2
      y <- -(r-1+edge/2)
      delta1 <- y-edge/2-slope*(x-edge/2)
      delta2 <- y-edge/2-slope*(x+edge/2)
      delta3 <- y+edge/2-slope*(x-edge/2)
      delta4 <- y+edge/2-slope*(x+edge/2)
      delta_vector <- c(delta1,delta2,delta3,delta4)
      distance_vector <- abs(delta_vector)/sqrt(1+slope^2)
      
      num_up <- sum(delta_vector>0)
      num_on <- sum(delta_vector==0)
      num_below <- sum(delta_vector<0)
      
      distance <- Inf
      if (num_up==4|num_below==4)
      {
        delta <- 0
        distance <- Inf
      }
      if ((num_on==1&num_up==3)|(num_on==1&num_below==3))
      {
        delta <- 0
        distance <- Inf
      }
      if ((num_up==3&num_below==1)|(num_up==1&num_below==3))
      {
        delta <- 1
        if (num_up==1)
        {
          distance <- distance_vector[which(delta_vector>0)]
        }else{
          distance <- distance_vector[which(delta_vector<0)]
        }
      }
      if ((num_on==1&num_up==1)|(num_on==1&num_below==1))
      {
        delta <- 1
        if (num_up==1)
        {
          distance <- distance_vector[which(delta_vector>0)]
        }else{
          distance <- distance_vector[which(delta_vector<0)]
        }
      }
      if (num_up==2&num_below==2)
      {
        delta <- 1
        distance <- min(c(max(distance_vector[which(delta_vector>0)]),max(distance_vector[delta_vector<0])))
      }
      if (num_on==2)
      {
        delta <- 1
        distance <- 0
      }
      
      Delta[r,c] <- delta
      Distance[r,c] <- distance
      
    }
  }
  
  if (r>c)
  {
    for (r in 1:row)
    {
      sel_num <- which(abs(Distance[r,]-max(Distance[r,which(Distance[r,]!=Inf)]))>eps)
      Delta[r,sel_num] <- 0
    }
  }
  ######################################  
  if (r>c)
  {
    for (r in 1:row)
    {
      for (c in 2:col)
      {
        if((Delta[r,c]==1) & (Delta[r,(c-1)]==1))
        {
          Delta[r,c]=0;
        } else {Delta[r,c]=Delta[r,c]}
      }
    }
  }
  ######################################  
  if (r<c)
  {
    for (c in 1:col)
    {
      sel_num <- which(abs(Distance[,c]-max(Distance[which(Distance[,c]!=Inf),c]))>eps)
      Delta[sel_num,c] <- 0
    }
  }
  ######################################  
  if (r<c)
  {
    for (c in 1:col)
    {
      for (r in 2:row)
      {
        if((Delta[r,c]==1) & (Delta[(r-1),c]==1))
        {
          Delta[r,c]=0;
        } else {Delta[r,c]=Delta[r,c]}
      }
    }
  }
  ######################################
  
  
  
  
  for (r in 1:row)
  {
    for (c in 1:col)
    {
      if (Delta[r,c]==1)
      {
        num <- num+1
        Diagonal[[num]] <- A[r,c]
        Row[[num]] <- r
        Col[[num]] <- c
      }
    }
  }
  
  Diagonal <- unlist(Diagonal)
  Row <- unlist(Row)
  Col <- unlist(Col)
  Results <- list(Row = Row, Col = Col, Diagonal = Diagonal, Distance = Distance, Delta = Delta)
  return(Results) 
}
