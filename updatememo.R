updatememo <- function(mat,memo,number){
  if(number%in%mat){
    cl <- col(mat)[grep(paste0("^",number,"$"),mat)]
    rw <- row(mat)[grep(paste0("^",number,"$"),mat)]  
    select1 <- match(number,colnames(memo))
    
    stopifnot(length(cl)==length(rw))
    for(j in 1:length(rw)){
      memo[memo$row==rw[j]&memo$column==cl[j],4:ncol(memo)] <- F
      memo[memo$row==rw[j]|memo$column==cl[j],select1] <- F
      ID <- memo$boxID[memo$row==rw[j]&memo$column==cl[j]]
      stopifnot(length(ID)==1)
      memo[memo$boxID==ID,select1] <- F
    }
  }
    return(memo)
}