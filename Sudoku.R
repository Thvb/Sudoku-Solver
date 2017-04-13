Sudoku.Solve <- function(unsolved.matrix,maxit,acc=ncol(unsolved.matrix),visualize=F){
  
  mat <- unsolved.matrix
  
stopifnot((is.matrix(mat)||is.data.frame(mat)),ncol(mat)==nrow(mat),sqrt(ncol(mat))%%1==0)

#Markers
if(is.data.frame(mat)){
    markers <- apply(mat,1,function(x){tm <- grep("[a-z]",x);if(length(tm)==0){tm <- NA};return(tm)})
    markers <- markers[!is.na(markers)]
    markercols <- as.numeric(unlist(markers))
    markerrows <- as.numeric(rep(names(markers),lapply(markers,length)))
    stopifnot(length(markerrows)==length(markercols))
    if(length(markers)!=0){
    for(i in 1:length(markerrows)){
      names(markerrows)[i] <- mat[markerrows[i],markercols[i]]
      names(markercols)[i] <- mat[markerrows[i],markercols[i]]
      mat[markerrows[i],markercols[i]] <- 0
    }
    mat <- as.matrix(apply(mat,2,as.numeric))
    }
}

stopifnot(all(mat<=ncol(mat)),all(mat>(-1)),is.numeric(mat))

lengthboxes <- sqrt(ncol(mat))
nboxes <- ncol(mat)

#check all numbers
numlist <- 1:ncol(mat)

boxbegincol <- rep(seq(1,max(numlist),by=lengthboxes),lengthboxes)
boxbeginrow <- rep(seq(1,max(numlist),by=lengthboxes),each=lengthboxes)
boxids <- 1:max(numlist)

memo <- data.frame(row=rep(1:nrow(mat),times=ncol(mat)),column=rep(1:ncol(mat),each=nrow(mat)),boxID=0,matrix(TRUE,nrow(mat)*ncol(mat),max(numlist)))
for(i in 1:length(boxbeginrow)){
  subst <- memo$row %in% c(seq(boxbeginrow[i],boxbeginrow[i]+(lengthboxes-1))) & memo$column %in% c(seq(boxbegincol[i],boxbegincol[i]+(lengthboxes-1)))
  memo$boxID[subst] <- boxids[i]
}

colnames(memo)[4:(max(numlist)+3)] <- numlist


#Set basic fill for memo
recurs <- rep(numlist,maxit)
i <-1
while(i<=max(numlist)){
  number <- recurs[i]
  memo <- updatememo(mat,memo,number)
  i <- i+1
}
if(visualize==T){
  graphics.off()
branch <- 1
	Visualize.Sudoku(mat,numlist)
	mtext(paste("Branch",branch),1,line=1)
	readline("Type any Enter to continue") 
}
i <-1
hyp <- 0
storage <- list()
storage$hypotheses <- mat.or.vec(0,4)
colnames(storage$hypotheses) <- c("row","col","number","active")
storage$memos <- list()
storage$mats <- list()

#Solve

while(c(any(mat==0)&i<=maxit)|c(any(memo[,4:ncol(memo)]==T)&i<=maxit)){
  if(all(apply(mat,2,function(x){recurs[i]%in%x}))){
  }else{
  tans <- Sudoku.SimpleSolve(mat,memo,recurs[i])
  if(visualize==T){
	if((length(tans$rw)==length(tans$cl))&length(tans$rw)>0){
	Visualize.Sudoku(mat,numlist,tans$rw,tans$cl,recurs[i],action="add")
	wait(0.5)
	}
  }
  if(all(tans$mat==mat)&all(tans$memo==memo)){
    hyp <- hyp+1
    if(hyp>acc){   
        tans <- Sudoku.ComplexSolve(tans$mat,tans$memo,recurs[i],storage)     
        storage <- tans$storage  
		selectactive <- max(row(storage$hypotheses)[storage$hypotheses[,"active"] == T,])	
		if(visualize==T){
		Visualize.Sudoku(mat,numlist,storage$hypotheses[storage$hypotheses[,"active"] == T,"row"],storage$hypotheses[storage$hypotheses[,"active"] == T,"col"],storage$hypotheses[storage$hypotheses[,"active"] == T,"number"],action="Hyp")
		wait(0.5)
		}
    hyp <- 1
        }
  }else{
    hyp <- 1
  }
 
  if(c(any(mat==0)&all(memo[,4:ncol(memo)]==F))|c(all(mat!=0)&any(memo[,4:ncol(memo)]==T))){
    message("Hypothesis Refuted")
	selectactive <- max(row(storage$hypotheses)[storage$hypotheses[,"active"] == T,])
    storage$hypotheses[selectactive,"active"] <- F
    
    reverse <- storage$hypotheses[selectactive,c("row","col","number")]
    mat <- storage$mats[[selectactive]]
    memo <- storage$memos[[selectactive]]
    
    selectcl <- match(reverse["number"],colnames(memo))  
    
    memo[memo$row==reverse["row"]&memo$column==reverse["col"],selectcl] <- F
	if(visualize==T){
	  graphics.off()
	branch <- branch+1
		Visualize.Sudoku(mat,numlist,storage$hypotheses[selectactive,"row"],storage$hypotheses[selectactive,"col"],storage$hypotheses[selectactive,"number"],action="substract")
		Visualize.Sudoku(mat,numlist,storage$hypotheses[storage$hypotheses[,"active"] == T,"row"],storage$hypotheses[storage$hypotheses[,"active"] == T,"col"],storage$hypotheses[storage$hypotheses[,"active"] == T,"number"],action="Hyp")
		wait(0.5)
	}
  }else{
  
  hypothesistest1 <- apply(tans$mat,2,function(x){any(table(x)[names(table(x))!=0]>1)})
  hypothesistest2 <- apply(tans$mat,1,function(x){any(table(x)[names(table(x))!=0]>1)})
  hypothesistest3 <- sapply(1:nboxes,function(z){
   rwcl <- memo[memo$boxID==z,1:2]
   q <- table(apply(rwcl,1,function(x){tans$mat[x[1],x[2]]}))
   q <- any(q[names(q)!=0]>1)
   return(q)
  })
  
  if(any(hypothesistest1==T)|any(hypothesistest2==T)|any(hypothesistest3==T)){
    message("Hypothesis Refuted")
    selectactive <- max(row(storage$hypotheses)[storage$hypotheses[,"active"] == T,])
    storage$hypotheses[selectactive,"active"] <- F
    
    reverse <- storage$hypotheses[selectactive,c("row","col","number")]
    mat <- storage$mats[[selectactive]]
    memo <- storage$memos[[selectactive]]

    selectcl <- match(reverse["number"],colnames(memo))  

    memo[memo$row==reverse["row"]&memo$column==reverse["col"],selectcl] <- F
	if(visualize==T){
	  graphics.off()
	branch <- branch+1
		rmv1 <- mat[table(mat[,hypothesistest1])>1,hypothesistest1]
		rmv2 <- mat[hypothesistest2,][table(mat[hypothesistest2,])>1]
		rwstr <- storage$hypotheses[selectactive,"row"]
		clstr <- storage$hypotheses[selectactive,"col"]
		nmbrstr <- storage$hypotheses[selectactive,"number"]
		if(!is.null(rmv1)){
			rwstr <- c(rwstr,match(rmv1,mat[,hypothesistest1]))
			clstr <- c(clstr,match(TRUE,hypothesistest1))
		}
		if(!is.null(rmv2)){
			rwstr <- c(rwstr,match(TRUE,hypothesistest2))
			clstr <- c(clstr,match(rmv2,mat[hypothesistest2,]))
		}
		print(rwstr,clstr)
		Visualize.Sudoku(mat,numlist,row=rwstr,col=clstr,number=nmbrstr,action="substract")
		Visualize.Sudoku(mat,numlist,storage$hypotheses[storage$hypotheses[,"active"] == T,"row"],storage$hypotheses[storage$hypotheses[,"active"] == T,"col"],storage$hypotheses[storage$hypotheses[,"active"] == T,"number"],action="Hyp")

		mtext(paste("Branch",branch),1,line=1)
		wait(1.5)
	}
  }else{
    mat <- tans$mat
    memo <- tans$memo
  }
  
  }
  }
  
  i <- i+1
}

ans <- list()
ans$PreSolution <- unsolved.matrix
ans$Solution <- mat
ans$Iterations <- i
ans$Hypotheses <- storage$hypotheses
if(length(markers)!=0){
  markans <- mat.or.vec(1,length(markerrows))
  for(i in 1:length(markerrows)){
    markans[i] <- mat[markerrows[i],markercols[i]]
  }
  markans <- as.data.frame(markans)
  names(markans) <- names(markercols)
  row.names(markans) <- "Solution"
  ans$Markers <- markans
}
#ans$memo <- memo
graphics.off()
Visualize.Sudoku(mat,numlist)
mtext(paste("Final Solution"),1,line=1)
return(ans)
}