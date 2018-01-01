

UserClasses<-c('character','character','integer','integer','gecos','character','character')

#'@method valid User.class
valid.User.class <- function(x,class1='User.class'){
	if(typeof(x)!='list')
	return(F)
	if(length(x)!=7)
	return(F)
	if(any(sapply(x,length)!=1))
	return(F)
	if(any(sapply(x,class)!=UserClasses))
	return(F)
	if(x[[1]]=='')
	return(F)
	if(x[[3]]<0)
	return(F)
	if(x[[4]]<0)
	return(F)
	if(x[[6]]=='')
	return(F)
	if(x[[7]]=='')
	return(F)
	return(T)
}

#'User.class
#'@param x an object to form a 'User.class' from
#'@export
User.class<-function(x,...){
	UseMethod('User.class',x)
}

#'@method User.class default
User.class.default <- function(x,...){
if(length(x)==7)x<-lapply(seq(7),function(i)if(class(x[[i]])==UserClasses[i]) x[[i]] else as(x[[i]],UserClasses[i]))
class(x)<-'User.class'
	if(!valid(x))stop()
names(x)<-c('Username','Password','uid','gid','gecos','home','shell')
x
}

#'@method User.class character
User.class.character <- function(x,...){
	x<-as.list(strsplit(x,split=':')[[1]])
	NextMethod()
}

#'@method is User.class
is.User.class <- function(object,...) inherits('User.class',object)

#'@method as User.class
as.User.class <- function(object,...) User.class(object)

#'@method valid gecos
valid.gecos <- function(x,class1='gecos'){
	if(typeof(x)!='character')
	return(F)
if(length(x)!=5)
return(F)
return(T)	
}

#'gecos
#'@param x an object to build a gecos object with
#'@export
gecos <- function(x,...){
	UseMethod('gecos',x)
}

#'@method gecos default
gecos.default <- function(x,...){
class(x)<-'gecos'
if(!valid(gecos))stop()
names(x)<-c('Full Name','Location','Office #','Home #','Other')
x
}

#'@method gecos character
gecos.character <- function(x,...){
	if(length(x)==1) {
	x<-strsplit(x,split=',')[[1]]
	x<-c(x,rep('',length(x)-5))
	}
	NextMethod()
}

#'@method is gecos
is.gecos <- function(object,...) inherits('gecos',object)

#'@method as gecos
as.gecos <- function(object,...) gecos(object)
