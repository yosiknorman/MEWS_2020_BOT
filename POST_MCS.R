load("data/SYSTEM/CS_202001011800.bin")

MID = list()
MID_ar = c()
for(i in 1:length(CONVEVTIVE_SYSTEM)){
  MID[[i]] = CONVEVTIVE_SYSTEM[[i]][, 2]
  MID_ar[i] = paste(CONVEVTIVE_SYSTEM[[i]][, 2], collapse = "&")
}

TABLE_MID = table(unlist(MID))
NTM = names(TABLE_MID)[TABLE_MID > 1] 
man = list()
for(i in 1:length(NTM)){
  man[[i]] = grep(MID_ar,pattern =  NTM[i])
}

noman = CONVEVTIVE_SYSTEM[-man]
CS_MID = CONVEVTIVE_SYSTEM[man]


