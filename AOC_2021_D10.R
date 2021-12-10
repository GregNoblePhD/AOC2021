options(scipen = 9999)  #disable scientific notation

library(data.table)

Input = fread('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D10.txt', header = F, keepLeadingZeros = T, fill=TRUE)

Open_Chars = c('(', '[', '{', '<')
Close_Chars = c(')', ']', '}', '>')
Char_Key = data.table('Open' = Open_Chars, 'Close' = Close_Chars)

#Part 1
Result = NULL
Incorrect_List = NULL
for(iCom in 1:nrow(Input)){
  temp_Com = strsplit(Input$V1[iCom], '')[[1]]
  Open_List = which(temp_Com %in% Open_Chars)
  Close_List = which(temp_Com %in% Close_Chars)
  Match_Com = NULL
  Taken_Open = NULL
  for (iClose in Close_List){
    Match_Com = rbindlist(list(Match_Com, data.table('Close' = temp_Com[iClose], 
                                                      'Open' = temp_Com[max(Open_List[Open_List < iClose & !(Open_List %in% Taken_Open)])])
                          ))
    Taken_Open = c(Taken_Open, max(Open_List[Open_List < iClose & !(Open_List %in% Taken_Open)]))
    #Open_List = Open_List[-(max(Open_List[Open_List < iClose]))]
  }
  Match_Com$Order = 1:nrow(Match_Com)
  
  Match_Com = merge(Match_Com, Char_Key, by = 'Close', all.x = T)
  setnames(Match_Com, c('Close', 'Open', 'Order', 'Key_Open'))
  Match_Com = Match_Com[order(Order),]
  if(nrow(Match_Com[Open != Key_Open]) > 0){
    Result = rbindlist(list(Result, Match_Com[min(which(Match_Com$Open != Match_Com$Key_Open)),]))
    Incorrect_List = c(Incorrect_List, iCom)
  }
}

sum(nrow(Result[Close == ')'])*3 + 
    nrow(Result[Close == ']'])*57 + 
    nrow(Result[Close == '}'])*1197 + 
    nrow(Result[Close == '>'])*25137)


#Part 2
Input = Input[!Incorrect_List,]

Score_Key = data.table('Close' = c(')', ']', '}', '>'), 'Val' = c(1,2,3,4))

Result = NULL
for(iCom in 1:nrow(Input)){
  temp_Com = strsplit(Input$V1[iCom], '')[[1]]
  Open_List = which(temp_Com %in% Open_Chars)
  Close_List = which(temp_Com %in% Close_Chars)
  Match_Com = NULL
  Taken_Open = NULL
  for (iClose in Close_List){
    Match_Com = rbindlist(list(Match_Com, data.table('Close' = temp_Com[iClose], 
                                                     'Open' = temp_Com[max(Open_List[Open_List < iClose & !(Open_List %in% Taken_Open)])])
    ))
    Taken_Open = c(Taken_Open, max(Open_List[Open_List < iClose & !(Open_List %in% Taken_Open)]))
    #Open_List = Open_List[-(max(Open_List[Open_List < iClose]))]
  }
  
  Incomplete = data.table('Open' = temp_Com[Open_List[!(Open_List %in% Taken_Open)]])
  
  Incomplete$Order = 1:nrow(Incomplete)
  
  Incomplete = merge(Incomplete, Char_Key, by = 'Open', all.x = T)
  Incomplete = Incomplete[order(Order, decreasing = T),]

  
  temp_score = 0
  for (iScore in Incomplete$Close){
    temp_score = temp_score * 5 + Score_Key$Val[Score_Key$Close == iScore]
  }
  Result = c(Result, temp_score)
}

median(Result)



