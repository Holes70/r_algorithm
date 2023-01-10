#Vyrobte práve 3 riešenia pre šachovnicu 9x9, zvyšné negenerujte.

N=9 
limit=3
counter=0

board =matrix(rep(0,N*N),nrow=N, ncol=N,byrow = TRUE)

isSafe<-function(board, row, col) { 
  for(i in 1:col) 
    if (board[row,i]==1) return(FALSE)
  
  i = row
  j = col
  while(i >= 1 && j >= 1) {
    if (board[i,j]) return(FALSE)
    i=i-1
    j=j-1
  }

  i = row
  j = col
  while(j >= 1 && i <= N) {
    if (board[i,j]) return(FALSE)
    i=i+1
    j=j-1
  }

  return(TRUE)
} 

solveNQUtil<-function(board, col) { 
  if (col > N) {
    counter <<- counter + 1

    plot(1:(N+1), 1:(N+1), type = "n", asp=1,axes = FALSE,xlab="",ylab="",main=NULL)
    for (i in 1:N) {
      col <- if (i %% 2) rep(c("white", "black"),length.out=N)
             else rep(c("black", "white"),length.out=N)
      col[which(board[i,]==1)]="red"
      rect(i, 1:(N+1), i+1, N+1, col = col, border = "white")
    }

    return()
  }
 
  if (counter == limit) return (TRUE)

  for (i in 1:N) { 
    if(isSafe(board, i, col)) { 
      board[i,col] = 1 
            
      solveNQUtil(board, col + 1)       
      board[i,col] = 0
    } 
  } 
} 

op <- par(mar = rep(0, 4))
par(op)
par(mfrow=c(3,4),oma = rep(0,4) + 0.3,
  mar = rep(0,4) + 0.3)
solveNQUtil(board, 1)
    