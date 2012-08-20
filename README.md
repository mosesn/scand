#scand
scala _and_ parser

#motivation
Scala parsers are very nice, but I ran into a problem with them.  
Basically, I was trying to handle parsing several things where they  
all needed to be there, but they could be in any order.  Basically,  
I needed an and operator, but there wasn't one.  So I wrote it!

#what have we got here?
tests and the object to do it.  it's really just a proof of concept  
though, this doesn't make sense as a library.  There is an "and" with  
repetition, where anything that has been parsed can be repeated, and an  
and without repetition, where everything that has been parsed but not be  
parsed again, but can still be in any order.