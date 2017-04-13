###Sudoku Solve complex###
#########################
Sudoku.ComplexSolve <- function(mat,memo,number,storage){
  numlist <- ncol(mat)
  select1 <- match(number,colnames(memo))
  colcheck <- table(memo[,select1],memo$column)
  
  if(all(colcheck[1,]==numlist)){
    colcheck <- data.frame(rbind(colcheck,mat.or.vec(1,ncol(colcheck))))
  }
  rowcheck <- table(memo[,select1],memo$row)
 
  if(all(rowcheck[1,]==numlist)){
    rowcheck <- data.frame(rbind(rowcheck,mat.or.vec(1,ncol(rowcheck))))
  }
  boxcheck <- table(memo[,select1],memo$boxID) 
  
  if(all(boxcheck[1,]==numlist)){
    boxcheck <- data.frame(rbind(boxcheck,mat.or.vec(1,ncol(boxcheck))))
  }
  p <- 1
  
  while(p <= 3 &(all(boxcheck[2,]!=1)&all(rowcheck[2,]!=1)&all(colcheck[2,]!=1))){
    
    #nboxes <- (ncol(mat)/3)*(nrow(mat)/3)
    #boxind <- vector("list",nboxes)
    #boxfill <- mat.or.vec(nboxes,1)
    #begrw <- rep(seq(1,ncol(mat),3),3)
    #begcl <- rep(begrw,each=3)
    #for(i in 1:length(boxind)){
      #boxind[[i]] <- matrix(c(begrw[i]:(begrw[i]+2),begcl[i]:(begcl[i]+2)),3,2) 
      #rw <- boxind[[i]][,1];cl <- boxind[[i]][,2]
     # boxfill[i] <- sum(!is.na(mat[rw,cl])&mat[rw,cl]>0&mat[rw,cl]<10&is.numeric(mat[rw,cl]))
    #}
   # highestfillbox <- sort(boxfill,decreasing=T)
    check <- list()
    check$rowcheck <- rowcheck
    check$colcheck <- colcheck
    pick <- sample(1:2,1)
      check <- check[[pick]]

  if(any(check[1,]!=numlist)&any(check[,2]!=0)){
      q <- 2
      cond <- 0
      while(cond==0&q<=numlist){
         if(all(check[2,]!=q)){
           q <-q+1
         }else{
           if(pick==1){
            rowz <- as.numeric(colnames(check)[check[2,]==q])
          if(length(row)>1){
            rowz <- sample(rowz,1)
            rowz <- rowz[!is.na(rowz)]
          }      
          }else{
              colz <- as.numeric(colnames(check)[check[2,]==q])
              if(length(col)>1){
                colz <- sample(colz,1)
                colz <- colz[!is.na(colz)]
              }           
         }
          cond<-1
        }
      }
    
      if(pick==1){
        select2 <- row.names(memo)%in%grep(c(paste0("^",rowz,"$",collapse=("|"))),memo$row)
        rwmat <- memo$row[select2]
        stopifnot(length(rwmat)==length(memo[select2,select1]))
        rwmat <- rwmat[memo[select2,select1]][1]
        clmat <- memo$column[select2]
        clmat <- clmat[memo[select2,select1]][1]
        stopifnot(length(rwmat)==length(clmat))
      }else{
        select2 <- row.names(memo)%in%grep(c(paste0("^",colz,"$",collapse=("|"))),memo$column)
        rwmat <- memo$row[select2]
        stopifnot(length(rwmat)==length(memo[select2,select1]))
        rwmat <- rwmat[memo[select2,select1]][1]
        clmat <- memo$column[select2]
        clmat <- clmat[memo[select2,select1]][1]
        stopifnot(length(rwmat)==length(clmat))
      }

      storage$mats <- c(storage$mats,list(mat))
      storage$memos <- c(storage$memos,list(memo))
        
        mat[rwmat,clmat] <- number
        active <- T
        storage$hypotheses <- rbind(storage$hypotheses,c(rwmat,clmat,number,active))

      memo <- updatememo(mat,memo,number)
      }
  colcheck <- table(memo[,select1],memo$column)
  if(all(colcheck[1,]==numlist)){
    te <- mat.or.vec(1,ncol(colcheck))
    colnames(te) <- colnames(colcheck)
    colcheck <- rbind(colcheck,te)
  }
  rowcheck <- table(memo[,select1],memo$row)
  
  if(all(rowcheck[1,]==numlist)){
    te <- mat.or.vec(1,ncol(rowcheck))
    colnames(te) <- colnames(rowcheck)
    rowcheck <- rbind(rowcheck,te)
  }
  boxcheck <- table(memo[,select1],memo$boxID) 
  
  if(all(boxcheck[1,]==numlist)){
    te <- mat.or.vec(1,ncol(boxcheck))
    colnames(te) <- colnames(boxcheck)
    boxcheck <- rbind(boxcheck,te)
  }

  p <- p+1
  }
    ans <- list()
    ans$memo <- memo
    ans$mat <- mat
    ans$storage <- storage
    return(ans)
}