#Visualize Sudoku Function

Visualize.Sudoku <- function(mat,numlist,row=NULL,col=NULL,number=NULL,action=c("add","substract","Hyp")){
action <- match.arg(action,c("add","substract","Hyp"))
max <- 2*ncol(mat)
lengthboxes <- sqrt(ncol(mat))

if(is.null(getTitle())){
	
	x11(title="Sudoku")
	plot.new()
	plot.window(xlim=c(0,max),ylim=c(0,max))
	
	alllines <- seq(0,max,by=2)
	abline(h=c(alllines),v=c(alllines),col="gray")
	boxbegincol <- seq(lengthboxes,max(numlist),by=lengthboxes)*2
	boxbeginrow <- seq(lengthboxes,max(numlist),by=lengthboxes)*2
	abline(h=c(0,boxbeginrow,max),v=c(0,boxbegincol,max))
	for(i in 1:length(numlist)){
		rw <- row(mat)[grep(paste0("^",numlist[i],"$"),mat)] 
		cl <- col(mat)[grep(paste0("^",numlist[i],"$"),mat)]
		text(x=c(cl*2-1),y=c(max-c(rw*2-1)),labels=numlist[i])
	}
}
if(action=="Hyp"){
	rect(col*2-2, max-c(row*2-2), col*2, max-c(row*2),col="orange",border="gray")
	text(x=c((col*2)-1),y=c(max-c((row*2)-1)),labels=number,col="white")

	alllines <- seq(0,max,by=2)
	abline(h=c(alllines),v=c(alllines),col="gray")
	boxbegincol <- seq(lengthboxes,max(numlist),by=lengthboxes)*2
	boxbeginrow <- seq(lengthboxes,max(numlist),by=lengthboxes)*2
	abline(h=c(0,boxbeginrow,max),v=c(0,boxbegincol,max))
	wait(0.5)
}
if(action=="substract"){
	mtext(paste("Hypothesis Refuted"),3,line=1,col="red")
	rect(col*2-2, max-c(row*2-2), col*2, max-c(row*2),col="red",border="gray")
	text(x=c((col*2)-1),y=c(max-c((row*2)-1)),labels=number,col="white")

	alllines <- seq(0,max,by=2)
	abline(h=c(alllines),v=c(alllines),col="gray")
	boxbegincol <- seq(lengthboxes,max(numlist),by=lengthboxes)*2
	boxbeginrow <- seq(lengthboxes,max(numlist),by=lengthboxes)*2
	abline(h=c(0,boxbeginrow,max),v=c(0,boxbegincol,max))
	wait(0.5)
}
if(getTitle()!="Sudoku (ACTIVE)"|action=="substract"){
	
	x11(title="Sudoku")
	plot.new()
	plot.window(xlim=c(0,max),ylim=c(0,max))
	
	alllines <- seq(0,max,by=2)
	abline(h=c(alllines),v=c(alllines),col="gray")
	boxbegincol <- seq(lengthboxes,max(numlist),by=lengthboxes)*2
	boxbeginrow <- seq(lengthboxes,max(numlist),by=lengthboxes)*2
	abline(h=c(0,boxbeginrow,max),v=c(0,boxbegincol,max))
	for(i in 1:length(numlist)){
		rw <- row(mat)[grep(paste0("^",numlist[i],"$"),mat)] 
		cl <- col(mat)[grep(paste0("^",numlist[i],"$"),mat)]
		text(x=c((cl*2)-1),y=c(max-c((rw*2)-1)),labels=numlist[i])
	}
	
}


if(!is.null(row)&!is.null(col)&!is.null(number)){	
	if(action=="add"){
		rect(col*2-2, max-c(row*2-2), col*2, max-c(row*2),col="green",border="gray")
		text(x=c(col*2-1),y=c(max-c(row*2-1)),labels=number,col="white")

	alllines <- seq(0,max,by=2)
	abline(h=c(alllines),v=c(alllines),col="gray")
	boxbegincol <- seq(lengthboxes,max(numlist),by=lengthboxes)*2
	boxbeginrow <- seq(lengthboxes,max(numlist),by=lengthboxes)*2
	abline(h=c(0,boxbeginrow,max),v=c(0,boxbegincol,max))
	}
	}
}
