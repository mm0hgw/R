# ultraCombo
Conversion, storage and data manipulation functions for 
combination objects.

devtools::install_github("mm0hgw/ultraCombo",subdir="ultraCombo")


------OUTPUT

> showCombo(5,3)
showCombo n: 5 k: 3
> combn( 5 , 3 )
     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
[1,]    1    1    1    1    1    1    2    2    2     3
[2,]    2    2    2    3    3    4    3    3    4     4
[3,]    3    4    5    4    5    5    4    5    5     5
> choose( 4 , 2 )
[1] 6
> rep(1,choose( 4 , 2 ))
[1] 1 1 1 1 1 1
> combn( 4 , 2 )+1
     [,1] [,2] [,3] [,4] [,5] [,6]
[1,]    2    2    2    3    3    4
[2,]    3    4    5    4    5    5
> rbind(rep(1,choose( 4 , 2 )),combn( 4 , 2 )+1)
     [,1] [,2] [,3] [,4] [,5] [,6]
[1,]    1    1    1    1    1    1
[2,]    2    2    2    3    3    4
[3,]    3    4    5    4    5    5
> combn( 4 , 3 )+1
     [,1] [,2] [,3] [,4]
[1,]    2    2    2    3
[2,]    3    3    4    4
[3,]    4    5    5    5
> cbind(rbind(rep(1,choose( 4 , 2 )),combn( 4 , 2 )+1),combn( 4 , 3 )+1)
     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
[1,]    1    1    1    1    1    1    2    2    2     3
[2,]    2    2    2    3    3    4    3    3    4     4
[3,]    3    4    5    4    5    5    4    5    5     5

-------RAMBLINGS

The combination index number distinguishes the combinations.
The combn() function from R has this as the [,x] index.

Combinations are very low entropy, they also get very big, very 
quickly. You can get the error codes for 'nice try, but not 
enough free memory' and 'nice try, but your CPU registers aren't 
long enough to directly reference an object that big.' by 
calling this function with pretty modest integers.

It is possible to distinguish, whether a user-supplied index 
number contains the first element of the combination. We must 
know the index number, and the number of combinations containing 
the first element.

It is also possible to define the remainder of the combination 
as a discrete combination with this information.

This means there's a recursive solution, to 'what combination 
corresponds to a given index?' which doesn't involve generating 
any other part of the table, which works as long as you can 
calculate choose(n-1,k-1) with integer precision, and have a 
datatype with sufficient precision to distinguish the index 
integers.

This means there's a faster solution using loops, implemented
in the package, in C since R doesn't have the '++' command.

In the processor cache, the '[' method's most memory efficient 
way to define a subset of k=7 is as 7 32-bit ints. For a n=127 
set, only 6-bits of these ints are used. The 26 most significant
bits are all 0.

The '[' method doesn't require this method of storage outside 
the processor cache.

Where n=100, choose(100,7) > .Machine$integer.max and thus the 
indicies are stored as 64-bit doubles. Which reduces load on the 
North Bridge by 150-bits per combination when combinations are 
read or written as indices.

The transformation to index also trivialises the identification
of duplicate combinations.

--------CODE

showCombo <- function(n,k){
	cat(paste('showCombo n:',n,'k:',k,'\n'))
	cat('> combn(',n,',',k,')\n')
	print(combn(n,k))
	cat('> choose(',n-1,',',k-1,')\n')
	print(choose(n-1,k-1))
	cat('> rep(1,choose(',n-1,',',k-1,'))\n')
	print(rep(1,choose(n-1,k-1)))
	cat('> combn(',n-1,',',k-1,')+1\n')
	print(combn(n-1,k-1)+1)
	cat('> rbind(rep(1,choose(',n-1,',',k-1,')),combn(',n-1,',',k-1,')+1)\n')
	print(rbind(rep(1,choose(n-1,k-1)),combn(n-1,k-1)+1))
	cat('> combn(',n-1,',',k,')+1\n')
	print(combn(n-1,k)+1)
	cat('> cbind(rbind(rep(1,choose(',n-1,',',k-1,')),combn(',n-1,',',k-1,')+1),combn(',n-1,',',k,')+1)\n')
	print(cbind(rbind(rep(1,choose(n-1,k-1)),combn(n-1,k-1)+1),combn(n-1,k)+1))
	
}
