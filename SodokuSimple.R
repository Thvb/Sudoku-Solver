###Sudoku Solve Simple###
#########################
Sudoku.SimpleSolve <- function(mat,memo,number){
  select1 <- match(number,colnames(memo))
  		cls <- mat.or.vec(0,0)
		rws <- mat.or.vec(0,0)
  numlist <- ncol(mat)
      rowcheck <- table(memo[,select1],memo$row)

      if(any(rowcheck[1,]!=max(numlist))){
        if(any(rowcheck[2,]==1)){
          surerownr <- as.numeric(colnames(rowcheck)[rowcheck[2,]==1])
          select2 <- row.names(memo)%in%grep(c(paste0("^",surerownr,"$",collapse=("|"))),memo$row)
          rwmat <- memo$row[select2]
          clmat <- memo$column[select2]
          stopifnot(length(clmat)==length(memo[select2,select1]))
          rwmat <- rwmat[memo[select2,select1]]
          clmat <- clmat[memo[select2,select1]]
          stopifnot(length(clmat)==length(surerownr))
          for(k in 1:length(clmat)){
            mat[rwmat[k],clmat[k]] <- number
          }
        memo <- updatememo(mat,memo,number)
		rws <-c(rws,rwmat)
		cls <-c(cls,clmat)
        }
      }
		
      colcheck <- table(memo[,select1],memo$column)

      if(any(colcheck[1,]!=max(numlist))){
          if(any(colcheck[2,]==1)){
            surecolnr <- as.numeric(colnames(colcheck)[colcheck[2,]==1])
            select2 <- row.names(memo)%in%grep(c(paste0("^",surecolnr,"$",collapse=("|"))),memo$column)
            clmat <- memo$column[select2]
            rwmat <- memo$row[select2]
            stopifnot(length(rwmat)==length(memo[select2,select1]))
            clmat <- clmat[memo[select2,select1]]
            rwmat <- rwmat[memo[select2,select1]]
            stopifnot(length(rwmat)==length(surecolnr))
            for(k in 1:length(rwmat)){
              mat[rwmat[k],clmat[k]] <- number
            }
            memo <- updatememo(mat,memo,number)
			rws <-c(rws,rwmat)
			cls <-c(cls,clmat)
        }
        }
		

		
      boxcheck <- table(memo[,select1],memo$boxID)
     
      if(any(boxcheck[1,]!=max(numlist))){
        
            if(any(boxcheck[2,]==1)){
              surenumbox <- as.numeric(colnames(boxcheck)[boxcheck[2,]==1])
              select2 <- row.names(memo)%in%grep(c(paste0("^",surenumbox,"$",collapse=("|"))),memo$boxID)
              rwmat <- memo$row[select2]
              stopifnot(length(rwmat)==length(memo[select2,select1]))
              rwmat <- rwmat[memo[select2,select1]]
              clmat <- memo$column[select2]
              clmat <- clmat[memo[select2,select1]]
              stopifnot(length(rwmat)==length(clmat))
              for(k in 1:length(rwmat)){
                mat[rwmat[k],clmat[k]] <- number
              }
              memo <- updatememo(mat,memo,number)
			  rws <-c(rws,rwmat)
			  cls <-c(cls,clmat)
            }
          }
ans <- list()
ans$memo <- memo
ans$mat <- mat
ans$rw	<- rws
ans$cl <- cls
return(ans)
}