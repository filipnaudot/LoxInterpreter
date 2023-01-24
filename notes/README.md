# Notes
<br/>
<br/>

# Lists

### prepend and append
> : Add to the start of a list

takes a number and a list of numbers or a character and a list of characters
```
'A':" SMALL CAT" --> "A SMALL CAT"
```

<br/>

> ++ Add to the end of a list

takes two lists
```
[1,2,3,4] ++ [9,10,11,12] --> [1,2,3,4,9,10,11,12]  
``` 

<br/>
<br/>

### Index
> !! Index in a list
```
list !! 6
````
 gives the 6th elemtn in the list

Example:
```
"Steve Buscemi" !! 6  --> 'B'  
```

<br/>

> head takes a list and returns its head.
```
ghci> head [5,4,3,2,1]  
5  
```
> tail takes a list and returns its tail.
```
tail [5,4,3,2,1]  
[4,3,2,1]   
```
> last takes a list and returns its last element.
```
ghci> last [5,4,3,2,1]  
1    
```
> init takes a list and returns everything except its last element.
```
ghci> init [5,4,3,2,1]  
[5,4,3,2]   
```

<br/>

> length takes a list and returns its length.
```
ghci> length [5,4,3,2,1]  
5  
```

-------------------------------------------------