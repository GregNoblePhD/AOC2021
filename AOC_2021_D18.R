options(scipen = 9999)  #disable scientific notation

#libraries
library(data.table)

#Read in the data
Input = data.table(readLines('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D18.txt'))
setnames(Input, 'Val')


Explode_and_Split = function(L_split){
  Action_Found = 1
  while(Action_Found == 1){
    #Make Bracket matches
    B_open = which(L_split == "[")
    B_close = which(L_split == "]")
    B_all = c(B_open, B_close)
    Group_Match = NULL
    for(B_group in B_open[length(B_open):1]){
      temp_close = min(B_close[B_close > B_group])
      Group_Match = rbindlist(list(Group_Match, data.table('Open' = B_group, 'Close' = temp_close)))
      B_close = B_close[B_close != temp_close]
    }
    
    #Calculate nested count
    Group_Match$Nest_Count = 0
    for(iGroup in 1:nrow(Group_Match)){
      Temp_Open = Group_Match$Open[iGroup]
      Temp_Close = Group_Match$Close[iGroup]
      Group_Match$Nest_Count[iGroup] = sum(Group_Match$Open < Temp_Open & Group_Match$Close > Temp_Close)
    }
    
    Group_Match$Left_Val_Pos = Group_Match$Open + 1
    Group_Match$Right_Val_Pos = Group_Match$Close - 1
    Group_Match$Left_Val = L_split[Group_Match$Left_Val_Pos]
    Group_Match$Right_Val = L_split[Group_Match$Right_Val_Pos]
    
    Num_Index = (1:length(L_split))[!is.na(as.numeric(L_split))]
    
    if(any(Group_Match$Nest_Count >= 4)){
      Temp_Expl = Group_Match[which(Group_Match$Nest_Count >= 4)]
      Temp_Expl = Temp_Expl[order(Close),]
      Next_Left_Vals = Num_Index[Num_Index < Temp_Expl$Left_Val_Pos[1]]
      Next_Right_Vals = Num_Index[Num_Index > Temp_Expl$Right_Val_Pos[1]]
      if(any(Next_Left_Vals)){
        Change_Val = max(Next_Left_Vals)
        L_split[Change_Val] = as.character(as.numeric(L_split[Change_Val]) + as.numeric(Temp_Expl$Left_Val[1]))
      }
      if(any(Next_Right_Vals)){
        Change_Val = min(Next_Right_Vals)
        L_split[Change_Val] = as.character(as.numeric(L_split[Change_Val]) + as.numeric(Temp_Expl$Right_Val[1]))
      }
      L_split = L_split[-c(Temp_Expl$Open[1], 
                           Temp_Expl$Close[1], 
                           Temp_Expl$Left_Val_Pos[1], 
                           Temp_Expl$Right_Val_Pos[1])]
      L_split = c(L_split[1:(Temp_Expl$Open[1] - 1)], '0', L_split[Temp_Expl$Open[1]:length(L_split)])
    
    } else if(any(as.numeric(L_split[Num_Index]) >= 10)){
      Split_Index = min(Num_Index[which(as.numeric(L_split[Num_Index]) >= 10)])
      Split_Val = as.numeric(L_split[Split_Index])
      L_split = c(L_split[1:(Split_Index - 1)],
                  '[',
                  as.character(floor(Split_Val/2)),
                  as.character(ceiling(Split_Val/2)),
                  ']',
                  L_split[(Split_Index + 1):length(L_split)])
                  
    } else {
      Action_Found = 0
    }
  }
  return(L_split)
}

Calc_Mag = function(L_split){
  #Make Bracket matches
  B_open = which(L_split == "[")
  B_close = which(L_split == "]")
  B_all = c(B_open, B_close)
  Group_Match = NULL
  for(B_group in B_open[length(B_open):1]){
    temp_close = min(B_close[B_close > B_group])
    Group_Match = rbindlist(list(Group_Match, data.table('Open' = B_group, 'Close' = temp_close)))
    B_close = B_close[B_close != temp_close]
  }
  
  #Calculate nested count
  Group_Match$Nest_Count = 0
  for(iGroup in 1:nrow(Group_Match)){
    Temp_Open = Group_Match$Open[iGroup]
    Temp_Close = Group_Match$Close[iGroup]
    Group_Match$Nest_Count[iGroup] = sum(Group_Match$Open < Temp_Open & Group_Match$Close > Temp_Close)
  }
  
  Group_Match$Left_Val_Pos = Group_Match$Open + 1
  Group_Match$Right_Val_Pos = Group_Match$Close - 1
  Group_Match$Left_Val = L_split[Group_Match$Left_Val_Pos]
  Group_Match$Right_Val = L_split[Group_Match$Right_Val_Pos]
  
  Num_Index = (1:length(L_split))[!is.na(as.numeric(L_split))]
  
  Mag_Vals = Group_Match[!is.na(as.numeric(Group_Match$Left_Val)),]
  
  while(any(Group_Match$Close - Group_Match$Open == 3)){
    Temp_Mag = Group_Match[which(Group_Match$Close - Group_Match$Open == 3)]
    for(iMag in 1:nrow(Temp_Mag)){
      L_split = c(L_split[1:(Temp_Mag$Open[iMag] - 1)],
                  as.character(as.numeric(Temp_Mag$Left_Val[iMag]) * 3 + as.numeric(Temp_Mag$Right_Val[iMag]) * 2),
                  L_split[(Temp_Mag$Close[iMag] + 1):length(L_split)])
    }
    
    B_open = which(L_split == "[")
    B_close = which(L_split == "]")
    B_all = c(B_open, B_close)
    Group_Match = NULL
    for(B_group in B_open[length(B_open):1]){
      temp_close = min(B_close[B_close > B_group])
      Group_Match = rbindlist(list(Group_Match, data.table('Open' = B_group, 'Close' = temp_close)))
      B_close = B_close[B_close != temp_close]
    }
    
    #Calculate nested count
    Group_Match$Nest_Count = 0
    for(iGroup in 1:nrow(Group_Match)){
      Temp_Open = Group_Match$Open[iGroup]
      Temp_Close = Group_Match$Close[iGroup]
      Group_Match$Nest_Count[iGroup] = sum(Group_Match$Open < Temp_Open & Group_Match$Close > Temp_Close)
    }
    
    Group_Match$Left_Val_Pos = Group_Match$Open + 1
    Group_Match$Right_Val_Pos = Group_Match$Close - 1
    Group_Match$Left_Val = L_split[Group_Match$Left_Val_Pos]
    Group_Match$Right_Val = L_split[Group_Match$Right_Val_Pos]
    
    Num_Index = (1:length(L_split))[!is.na(as.numeric(L_split))]
    if(length(Num_Index) == 1){break}
    
  }

  return(as.numeric(L_split[Num_Index]))
}



#Part 1
L_split = NULL
for(iStep in 1:nrow(Input)){
  
  temp_split = strsplit(Input$Val[iStep], split = '')[[1]]
  temp_split = temp_split[temp_split != ',']
  
  if(iStep == 1){
    L_split = c(L_split, temp_split)
  } else {
    L_split = c('[', L_split, temp_split, ']')
  }
  
  L_split = Explode_and_Split(L_split)
}
Calc_Mag(L_split)



#Part 2
Combo_List = expand.grid('Num1' = 1:nrow(Input), 'Num2' = 1:nrow(Input))
Combo_List = Combo_List[Combo_List$Num1 != Combo_List$Num2,]

Results = NULL
for(iCombo in 1:nrow(Combo_List)){
  
  temp_split1 = strsplit(Input$Val[Combo_List$Num1[iCombo]], split = '')[[1]]
  temp_split1 = temp_split1[temp_split1 != ',']
  
  temp_split2 = strsplit(Input$Val[Combo_List$Num2[iCombo]], split = '')[[1]]
  temp_split2 = temp_split2[temp_split2 != ',']
  
  L_split = c('[', temp_split1, temp_split2, ']')
  L_split = Explode_and_Split(L_split)
  Results = c(Results, Calc_Mag(L_split))
}
max(Results)

