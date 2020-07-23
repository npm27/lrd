##Write a script to combine all these files and only keep the recall data

###Start with Batch 1

##Get the csv files
setwd("C:/Users/nickm.000/Documents/GitHub/lrd/free_recall_SHINY/Test Data RGN/Batch 1/Version A Data - Guess First/RGN Data")

files = list.files(pattern = "*.csv")

##Now combine all the csvs into one file
#Get the files names
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#Fix column names
names(dat) = substring(names(dat[1:11]), 3)

colnames(dat)[18] = "Trial_Type"
colnames(dat)[21] = "List_Type" 
colnames(dat)[23] = "Response" 

#get the number of participants
length(unique(dat$Username))

##Add version number and batch number
dat$Version = rep("A")
dat$Batch = rep("1")

##Drop unused columns
A = dat[ , -c(2:17, 19:20, 22, 24:32)]

#Drop everything but the six recall recall lists
A_Final = data.frame()

for (i in unique(A$Username)){
  
  print(i)
  
  temp = subset(A,
                A$Username == i)
  temp = temp[-c(1:322, 460:nrow(temp)), ]
  
  A_Final = rbind(A_Final, temp)
  
}

##Get the list types
subA = subset(A_Final,
              A_Final$Trial_Type == "Study")

list_names = unique(subA$List_Type)

##Get the participant responses
A_Final = subset(A_Final,
               A_Final$Trial_Type == "freerecall")

##Now add in the correct list type
A_Final$List_Type = list_names

####Now do version B####
setwd("C:/Users/nickm.000/Documents/GitHub/lrd/free_recall_SHINY/Test Data RGN/Batch 1/Version B Data - Guess First/RGN Data")

files = list.files(pattern = "*.csv")

##Now combine all the csvs into one file
#Get the files names
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#Fix column names
names(dat) = substring(names(dat[1:11]), 3)

colnames(dat)[18] = "Trial_Type"
colnames(dat)[21] = "List_Type" 
colnames(dat)[23] = "Response" 

#get the number of participants
length(unique(dat$Username))

##Add version number and batch number
dat$Version = rep("B")
dat$Batch = rep("1")

##Drop unused columns
B = dat[ , -c(2:17, 19:20, 22, 24:32)]

#Drop everything but the six recall recall lists
B_Final = data.frame()

for (i in unique(B$Username)){
  
  print(i)
  
  temp = subset(B,
                B$Username == i)
  temp = temp[-c(1:322, 460:nrow(temp)), ]
  
  B_Final = rbind(B_Final, temp)
  
}

##Get the list types
subB = subset(B_Final,
              B_Final$Trial_Type == "Study")

list_names = unique(subB$List_Type)

##Get the participant responses
B_Final = subset(B_Final,
                 B_Final$Trial_Type == "freerecall")

##Now add in the correct list type
B_Final$List_Type = list_names

####Now for version C####
setwd("C:/Users/nickm.000/Documents/GitHub/lrd/free_recall_SHINY/Test Data RGN/Batch 1/Version C Data - Recall First/RGN Data")

files = list.files(pattern = "*.csv")

##Now combine all the csvs into one file
#Get the files names
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#Fix column names
names(dat) = substring(names(dat[1:11]), 3)

colnames(dat)[18] = "Trial_Type"
colnames(dat)[21] = "List_Type" 
colnames(dat)[23] = "Response" 

#get the number of participants
length(unique(dat$Username))

##Add version number and batch number
dat$Version = rep("C")
dat$Batch = rep("1")

##Drop unused columns
C = dat[ , -c(2:17, 19:20, 22, 24:32)]

#Drop everything but the six recall recall lists
C_Final = data.frame()

for (i in unique(C$Username)){
  
  print(i)
  
  temp = subset(C,
                C$Username == i)
  temp = temp[c(1:138), ]
  
  C_Final = rbind(C_Final, temp)
  
}

##Get the list types
subC = subset(C_Final,
              C_Final$Trial_Type == "Study")

list_names = unique(subC$List_Type)

##Get the participant responses
C_Final = subset(C_Final,
                 C_Final$Trial_Type == "freerecall")

##Now add in the correct list type
C_Final$List_Type = list_names

####Now do Version D####
setwd("C:/Users/nickm.000/Documents/GitHub/lrd/free_recall_SHINY/Test Data RGN/Batch 1/Version D Data - Recall First/RGN Data")

files = list.files(pattern = "*.csv")

##Now combine all the csvs into one file
#Get the files names
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#Fix column names
names(dat) = substring(names(dat[1:11]), 3)

colnames(dat)[18] = "Trial_Type"
colnames(dat)[21] = "List_Type" 
colnames(dat)[23] = "Response" 

#get the number of participants
length(unique(dat$Username))

##Add version number and batch number
dat$Version = rep("D")
dat$Batch = rep("1")

##Drop unused columns
D = dat[ , -c(2:17, 19:20, 22, 24:32)]

#Drop everything but the six recall recall lists
D_Final = data.frame()

for (i in unique(D$Username)){
  
  print(i)
  
  temp = subset(D,
                D$Username == i)
  temp = temp[c(1:138), ]
  
  D_Final = rbind(D_Final, temp)
  
}

##Get the list types
subD = subset(D_Final,
              D_Final$Trial_Type == "Study")

list_names = unique(subD$List_Type)

##Get the participant responses
D_Final = subset(D_Final,
                 D_Final$Trial_Type == "freerecall")

##Now add in the correct list type
D_Final$List_Type = list_names

####Now do Version E####
setwd("C:/Users/nickm.000/Documents/GitHub/lrd/free_recall_SHINY/Test Data RGN/Batch 1/Version E Data - Restudy First/RGN Data")

files = list.files(pattern = "*.csv")

##Now combine all the csvs into one file
#Get the files names
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
dat = dat[ , -33]

#Fix column names
names(dat) = substring(names(dat[1:11]), 3)

colnames(dat)[19] = "Trial_Type"
colnames(dat)[22] = "List_Type" 
colnames(dat)[24] = "Response" 

#get the number of participants
length(unique(dat$Username))

##Add version number and batch number
dat$Version = rep("E")
dat$Batch = rep("1")

##Drop unused columns
E = dat[ , -c(2:18, 20:21, 23, 25:32)]

#Drop everything but the six recall recall lists
E_Final = data.frame()

for (i in unique(E$Username)){
  
  print(i)
  
  temp = subset(E,
                E$Username == i)
  temp = temp[c(759:894), ]
  
  E_Final = rbind(E_Final, temp)
  
}

##Get the list types
subE = subset(E_Final,
              E_Final$Trial_Type == "Study")

list_names = unique(subE$List_Type)

##Get the participant responses
E_Final = subset(E_Final,
                 E_Final$Trial_Type == "freerecall")

##Now add in the correct list type
E_Final$List_Type = list_names

####Now do version F####
setwd("C:/Users/nickm.000/Documents/GitHub/lrd/free_recall_SHINY/Test Data RGN/Batch 1/Version F Data - Restudy First/RGN Data")

files = list.files(pattern = "*.csv")

##Now combine all the csvs into one file
#Get the files names
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#Fix column names
names(dat) = substring(names(dat[1:11]), 3)

colnames(dat)[18] = "Trial_Type"
colnames(dat)[21] = "List_Type" 
colnames(dat)[23] = "Response" 

#get the number of participants
length(unique(dat$Username))

##Add version number and batch number
dat$Version = rep("F")
dat$Batch = rep("1")

##Drop unused columns
f = dat[ , -c(2:17, 19:20, 22, 24:32)]

#Drop everything but the six recall recall lists
F_Final = data.frame()

for (i in unique(f$Username)){
  
  print(i)
  
  temp = subset(f,
                f$Username == i)
  temp = temp[c(759:894), ]
  
  F_Final = rbind(F_Final, temp)
  
}

##Get the list types
subF = subset(F_Final,
              F_Final$Trial_Type == "Study")

list_names = unique(subF$List_Type)

##Get the participant responses
F_Final = subset(F_Final,
                 F_Final$Trial_Type == "freerecall")

##Now add in the correct list type
F_Final$List_Type = list_names

####Combine batch 1####
batch1 = rbind(A_Final, B_Final, C_Final, D_Final, E_Final, F_Final)

####Now let's do batch two!####

####Version A####
setwd("C:/Users/nickm.000/Documents/GitHub/lrd/free_recall_SHINY/Test Data RGN/Batch 2/Subjects Guess First/Good Data/Version A - Guess First")

files = list.files(pattern = "*.csv")

##Now combine all the csvs into one file
#Get the files names
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#Fix column names
names(dat) = substring(names(dat[1:11]), 3)

colnames(dat)[18] = "Trial_Type"
colnames(dat)[21] = "List_Type" 
colnames(dat)[23] = "Response" 

#get the number of participants
length(unique(dat$Username))

##Add version number and batch number
dat$Version = rep("A")
dat$Batch = rep("2")

##Drop unused columns
A = dat[ , -c(2:17, 19:20, 22, 24:32)]

#Drop everything but the six recall recall lists
A_Final = data.frame()

for (i in unique(A$Username)){
  
  print(i)
  
  temp = subset(A,
                A$Username == i)
  temp = temp[-c(1:322, 460:nrow(temp)), ]
  
  A_Final = rbind(A_Final, temp)
  
}

##Get the list types
subA = subset(A_Final,
              A_Final$Trial_Type == "Study")

list_names = unique(subA$List_Type)

##Get the participant responses
A_Final = subset(A_Final,
                 A_Final$Trial_Type == "freerecall")

##Now add in the correct list type
A_Final$List_Type = list_names

####Version B####
setwd("C:/Users/nickm.000/Documents/GitHub/lrd/free_recall_SHINY/Test Data RGN/Batch 2/Subjects Guess First/Good Data/Version B - Guess First")

files = list.files(pattern = "*.csv")

##Now combine all the csvs into one file
#Get the files names
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#Fix column names
names(dat) = substring(names(dat[1:11]), 3)

colnames(dat)[18] = "Trial_Type"
colnames(dat)[21] = "List_Type" 
colnames(dat)[23] = "Response" 

#get the number of participants
length(unique(dat$Username))

##Add version number and batch number
dat$Version = rep("B")
dat$Batch = rep("2")

##Drop unused columns
B = dat[ , -c(2:17, 19:20, 22, 24:32)]

#Drop everything but the six recall recall lists
B_Final = data.frame()

for (i in unique(B$Username)){
  
  print(i)
  
  temp = subset(B,
                B$Username == i)
  temp = temp[-c(1:322, 460:nrow(temp)), ]
  
  B_Final = rbind(B_Final, temp)
  
}

##Get the list types
subB = subset(B_Final,
              B_Final$Trial_Type == "Study")

list_names = unique(subB$List_Type)

##Get the participant responses
B_Final = subset(B_Final,
                 B_Final$Trial_Type == "freerecall")

##Now add in the correct list type
B_Final$List_Type = list_names

####Now for version C####
setwd("C:/Users/nickm.000/Documents/GitHub/lrd/free_recall_SHINY/Test Data RGN/Batch 2/Subjects Recall First/Good Data/Version C - Recall First")

files = list.files(pattern = "*.csv")

##Now combine all the csvs into one file
#Get the files names
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#Fix column names
names(dat) = substring(names(dat[1:11]), 3)

colnames(dat)[18] = "Trial_Type"
colnames(dat)[21] = "List_Type" 
colnames(dat)[23] = "Response" 

#get the number of participants
length(unique(dat$Username))

##Add version number and batch number
dat$Version = rep("C")
dat$Batch = rep("2")

##Drop unused columns
C = dat[ , -c(2:17, 19:20, 22, 24:32)]

#Drop everything but the six recall recall lists
C_Final = data.frame()

for (i in unique(C$Username)){
  
  print(i)
  
  temp = subset(C,
                C$Username == i)
  temp = temp[c(1:138), ]
  
  C_Final = rbind(C_Final, temp)
  
}

##Get the list types
subC = subset(C_Final,
              C_Final$Trial_Type == "Study")

list_names = unique(subC$List_Type)

##Get the participant responses
C_Final = subset(C_Final,
                 C_Final$Trial_Type == "freerecall")

##Now add in the correct list type
C_Final$List_Type = list_names

####Now for Version D####
setwd("C:/Users/nickm.000/Documents/GitHub/lrd/free_recall_SHINY/Test Data RGN/Batch 2/Subjects Recall First/Good Data/Version D - Recall First")

files = list.files(pattern = "*.csv")

##Now combine all the csvs into one file
#Get the files names
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#Fix column names
names(dat) = substring(names(dat[1:11]), 3)

colnames(dat)[18] = "Trial_Type"
colnames(dat)[21] = "List_Type" 
colnames(dat)[23] = "Response" 

#get the number of participants
length(unique(dat$Username))

##Add version number and batch number
dat$Version = rep("D")
dat$Batch = rep("2")

##Drop unused columns
D = dat[ , -c(2:17, 19:20, 22, 24:32)]

#Drop everything but the six recall recall lists
D_Final = data.frame()

for (i in unique(D$Username)){
  
  print(i)
  
  temp = subset(D,
                D$Username == i)
  temp = temp[c(1:138), ]
  
  D_Final = rbind(D_Final, temp)
  
}

##Get the list types
subD = subset(D_Final,
              D_Final$Trial_Type == "Study")

list_names = unique(subD$List_Type)

##Get the participant responses
D_Final = subset(D_Final,
                 D_Final$Trial_Type == "freerecall")

##Now add in the correct list type
D_Final$List_Type = list_names

####Now do Version E####
setwd("C:/Users/nickm.000/Documents/GitHub/lrd/free_recall_SHINY/Test Data RGN/Batch 2/Subjects Restudy First/Good Data/Version E - Restudy First")

files = list.files(pattern = "*.csv")

##Now combine all the csvs into one file
#Get the files names
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#Fix column names
names(dat) = substring(names(dat[1:11]), 3)

dat = dat[ , -17]

colnames(dat)[18] = "Trial_Type"
colnames(dat)[21] = "List_Type" 
colnames(dat)[23] = "Response" 

#get the number of participants
length(unique(dat$Username))

##Add version number and batch number
dat$Version = rep("E")
dat$Batch = rep("2")

##Drop unused columns
E = dat[ , -c(2:17, 19:20, 22, 24:32)]

#Drop everything but the six recall recall lists
E_Final = data.frame()

for (i in unique(E$Username)){
  
  print(i)
  
  temp = subset(E,
                E$Username == i)
  temp = temp[c(759:894), ]
  
  E_Final = rbind(E_Final, temp)
  
}

##Get the list types
subE = subset(E_Final,
              E_Final$Trial_Type == "Study")

list_names = unique(subE$List_Type)

##Get the participant responses
E_Final = subset(E_Final,
                 E_Final$Trial_Type == "freerecall")

##Now add in the correct list type
E_Final$List_Type = list_names

####Now do Version F####
setwd("C:/Users/nickm.000/Documents/GitHub/lrd/free_recall_SHINY/Test Data RGN/Batch 2/Subjects Restudy First/Good Data/Version F - Restudy First")

files = list.files(pattern = "*.csv")

##Now combine all the csvs into one file
#Get the files names
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#Fix column names
names(dat) = substring(names(dat[1:11]), 3)

colnames(dat)[18] = "Trial_Type"
colnames(dat)[21] = "List_Type" 
colnames(dat)[23] = "Response" 

#get the number of participants
length(unique(dat$Username))

##Add version number and batch number
dat$Version = rep("F")
dat$Batch = rep("2")

##Drop unused columns
f = dat[ , -c(2:17, 19:20, 22, 24:32)]

#Drop everything but the six recall recall lists
F_Final = data.frame()

for (i in unique(f$Username)){
  
  print(i)
  
  temp = subset(f,
                f$Username == i)
  temp = temp[c(759:894), ]
  
  F_Final = rbind(F_Final, temp)
  
}

##Get the list types
subF = subset(F_Final,
              F_Final$Trial_Type == "Study")

list_names = unique(subF$List_Type)

##Get the participant responses
F_Final = subset(F_Final,
                 F_Final$Trial_Type == "freerecall")

##Now add in the correct list type
F_Final$List_Type = list_names

####Now combine batch 2####
batch2 = rbind(A_Final, B_Final, C_Final, D_Final, E_Final, F_Final)

####Now combine both batches
Final = rbind(batch1, batch2)

setwd("C:/Users/nickm.000/Documents/GitHub/lrd/free_recall_SHINY/Test Data RGN")

#write.csv(Final, file = "arranged.csv", row.names = F)