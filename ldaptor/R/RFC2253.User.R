

readPasswd <- function(file='/etc/passwd',
colClasses = c('character','NULL','integer','integer','gecos','character','character')
){
args <- match.call()[-1]
args$sep <- ':'
args$header <- F
do.call(read.table,args)
}

UserClasses<-c('character','integer','integer','gecos','character','character')

#'@method valid User.class
valid.User.class <- function(x,class1='User.class',uidRange=c(1000,65533){
	if(typeof(x)!='list')
	return(F)
	if(length(x)!=6)
	return(F)
	if(any(sapply(x,length)!=1))
	return(F)
	if(class(x[[2]])=='numeric'&&x[[2]]>=0&&x[[2]]%%1==0)
	x[[2]] <- as.integer(x[[2]])
	if(class(x[[3]])=='numeric'&&x[[3]]>=0&&x[[3]]%%1==0)
	x[[3]] <- as.integer(x[[3]])
	if(!all(sapply(x,class)==UserClasses))
	return(F)
	if(x[[2]]<0)
	return(F)
	if(x[[3]]<0)
	return(F)
	return(T)
}


valid.gecos <- function