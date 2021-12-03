library(data.table)

Input = fread('C:/Users/noble/Desktop/AOC 2021/AOC_2021_D3.txt', header = F, keepLeadingZeros = T)

#Part 1
Val_Length = nchar(Input$V1[1])
Char_seq = NULL
for(iChar in 1:Val_Length){
  char_list = substr(Input$V1, iChar, iChar)
  if(length(which(char_list == 1)) > length(which(char_list == 0))){
    Char_seq = c(Char_seq, '1')
  } else{
    Char_seq = c(Char_seq, '0')
  }
}

Char_Seq_2 = ifelse(Char_seq == "1", "0", "1")

strtoi(paste(Char_seq, collapse = ""), base = 2) * strtoi(paste(Char_Seq_2, collapse = ""), base = 2)

#Part 2
OX = Input
for(iChar in 1:(Val_Length)){
  char_list = substr(OX$V1, iChar, iChar)
  if(length(which(char_list == 1)) >= length(which(char_list == 0))){
    OX = OX[substr(OX$V1, iChar, iChar) == '1']
  } else{
    OX = OX[substr(OX$V1, iChar, iChar) == '0']
  }
}

CO = Input
for(iChar in 1:(Val_Length)){
  char_list = substr(CO$V1, iChar, iChar)
  if(nrow(CO) > 1){
    if(length(which(char_list == 1)) < length(which(char_list == 0))){
      CO = CO[substr(CO$V1, iChar, iChar) == '1']
    } else{
      CO = CO[substr(CO$V1, iChar, iChar) == '0']
    }
  }
}

strtoi(paste(OX$V1[1], collapse = ""), base = 2) * strtoi(paste(CO$V1[1], collapse = ""), base = 2)

