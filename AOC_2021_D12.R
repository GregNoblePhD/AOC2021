options(scipen = 9999)  #disable scientific notation

library(data.table)
library(stringr)

Input = data.table(readLines('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D12.txt'))
Input = strsplit(Input$V1, split = '-')
Input = data.table(cbind('S1' = sapply(Input, "[[", 1), 
                         'S2' = sapply(Input, "[[", 2)))

#Which values are capitalized
Cap_Key = data.table('Spot' = unique(c(Input$S1, Input$S2)), 
                     'Cap' = str_detect(unique(c(Input$S1, Input$S2)), "^[:upper:]+$"))

#Part 1
#Recursive function for determining if a valid path was found
Paths = function(Cur, Visited_Path){
  if(Cur == 'end'){return (1)} #Valid Path
  if(!(Cap_Key$Cap[Cap_Key$Spot == Cur]) & Cur %in% Visited_Path){return (0)} #Invalid Path
  
  #Track path, used for validation on future iterations
  Visited_Path = c(Visited_Path, Cur)
  
  #Call recursive function, return the list of paths indicating 1 for valid, or 0 for invalid
  Made_to_end = 0
  for (iMove in c(Input$S1[Input$S2 == Cur], Input$S2[Input$S1 == Cur])){
    Made_to_end = c(Made_to_end, Paths(iMove, Visited_Path))
  }
  return (Made_to_end)
}
sum(Paths('start', NULL)) #Result


#Part 2
Paths = function(Cur, Visited_Path, Low_Visit){
  if(Cur == 'end'){return (1)} #Valid Path
  if(Cur == 'start' & Cur %in% Visited_Path){return (0)} #Invalid Path
  
  #Check for already visited any lower case spot, if so invalid, if not then track the spot
  if(!(Cap_Key$Cap[Cap_Key$Spot == Cur]) & Cur %in% Visited_Path){
    if(is.null(Low_Visit)){
      Low_Visit = Cur
    } else {
      return (0)
    }
  }
  
  #Track path, used for validation on future iterations
  Visited_Path = c(Visited_Path, Cur)

  #Call recursive function, return the list of paths indicating 1 for valid, or 0 for invalid  
  Made_to_end = 0
  for (iMove in c(Input$S1[Input$S2 == Cur], Input$S2[Input$S1 == Cur])){
    Made_to_end = c(Made_to_end, Paths(iMove, Visited_Path, Low_Visit))
  }
  return (Made_to_end)
}
sum(Paths('start', NULL, NULL)) #Result


