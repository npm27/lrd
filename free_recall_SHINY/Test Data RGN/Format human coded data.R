####Format the human coded data####
library(readxl)
ver.A = read_xlsx("Huff et al human coded.xlsx", sheet = "Version A - Guess First")
ver.B = read_xlsx("Huff et al human coded.xlsx", sheet = "Version B - Guess First")
ver.C = read_xlsx("Huff et al human coded.xlsx", sheet = "Version C - Recall First")
ver.D = read_xlsx("Huff et al human coded.xlsx", sheet = "Version D - Recall First")
ver.E = read_xlsx("Huff et al human coded.xlsx", sheet = "Version E - Restudy First")
ver.F = read_xlsx("Huff et al human coded.xlsx", sheet = "Version F - Restudy First")

library(reshape)
library(data.table)

####Version A####
##List 1
A1 = ver.A[3:22, 1:21]

colnames(A1)[1] = "Key"

A1.long = melt(A1, id.vars =  "Key")

colnames(A1.long)[2] = "Username"
colnames(A1.long)[3] = "Score"

A1.long$Version = rep("A")
A1.long$List = rep("1")
A1.long$list_type = rep("Cat")

##List 2
A2 = ver.A[41:60, 1:21]

colnames(A2)[1] = "Key"

A2.long = melt(A2, id.vars =  "Key")

colnames(A2.long)[2] = "Username"
colnames(A2.long)[3] = "Score"

A2.long$Version = rep("A")
A2.long$List = rep("2")
A2.long$list_type = rep("Ad_hoc")

##List 3
A3 = ver.A[77:96, 1:21]

colnames(A3)[1] = "Key"

A3.long = melt(A3, id.vars =  "Key")

colnames(A3.long)[2] = "Username"
colnames(A3.long)[3] = "Score"

A3.long$Version = rep("A")
A3.long$List = rep("3")
A3.long$list_type = rep("Unrel")

##List 4
A4 = ver.A[114:133, 1:21]

colnames(A4)[1] = "Key"

A4.long = melt(A4, id.vars =  "Key")

colnames(A4.long)[2] = "Username"
colnames(A4.long)[3] = "Score"

A4.long$Version = rep("A")
A4.long$List = rep("4")
A4.long$list_type = rep("Unrel")

##List 5
A5 = ver.A[156:175, 1:21]

colnames(A5)[1] = "Key"

A5.long = melt(A5, id.vars =  "Key")

colnames(A5.long)[2] = "Username"
colnames(A5.long)[3] = "Score"

A5.long$Version = rep("A")
A5.long$List = rep("5")
A5.long$list_type = rep("Cat")

##List 6
A6 = ver.A[191:210, 1:21]

colnames(A6)[1] = "Key"

A6.long = melt(A6, id.vars =  "Key")

colnames(A6.long)[2] = "Username"
colnames(A6.long)[3] = "Score"

A6.long$Version = rep("A")
A6.long$List = rep("6")
A6.long$list_type = rep("Ad_hoc")

##Combine all the A's
A_Final = rbind(A1.long, A2.long, A3.long, A4.long, A5.long, A6.long)

####Now Version B####
##List 1
B1 = ver.B[3:22, 1:21]

colnames(B1)[1] = "Key"

B1.long = melt(B1, id.vars =  "Key")

colnames(B1.long)[2] = "Username"
colnames(B1.long)[3] = "Score"

B1.long$Version = rep("B")
B1.long$List = rep("1")
B1.long$list_type = rep("Unrel")

##List 2
B2 = ver.B[43:62, 1:21]

colnames(B2)[1] = "Key"

B2.long = melt(B2, id.vars =  "Key")

colnames(B2.long)[2] = "Username"
colnames(B2.long)[3] = "Score"

B2.long$Version = rep("B")
B2.long$List = rep("2")
B2.long$list_type = rep("Ad_hoc")

##List 3
B3 = ver.B[74:93, 1:21]

colnames(B3)[1] = "Key"

B3.long = melt(B3, id.vars =  "Key")

colnames(B3.long)[2] = "Username"
colnames(B3.long)[3] = "Score"

B3.long$Version = rep("B")
B3.long$List = rep("3")
B3.long$list_type = rep("Ad_hoc")

##List 4
B4 = ver.B[109:128, 1:21]

colnames(B4)[1] = "Key"

B4.long = melt(B4, id.vars =  "Key")

colnames(B4.long)[2] = "Username"
colnames(B4.long)[3] = "Score"

B4.long$Version = rep("B")
B4.long$List = rep("4")
B4.long$list_type = rep("Cat")

##List 5
B5 = ver.B[139:158, 1:21]

colnames(B5)[1] = "Key"

B5.long = melt(B5, id.vars =  "Key")

colnames(B5.long)[2] = "Username"
colnames(B5.long)[3] = "Score"

B5.long$Version = rep("B")
B5.long$List = rep("5")
B5.long$list_type = rep("Cat")

##List 6
B6 = ver.B[175:194, 1:21]

colnames(B6)[1] = "Key"

B6.long = melt(B6, id.vars =  "Key")

colnames(B6.long)[2] = "Username"
colnames(B6.long)[3] = "Score"

B6.long$Version = rep("B")
B6.long$List = rep("6")
B6.long$list_type = rep("Unrel")

##Put all the B's together
B_Final = rbind(B1.long, B2.long, B3.long, B4.long, B5.long, B6.long)

####Now do Version C####
##List 1
C1 = ver.C[3:22, 1:22]

colnames(C1)[1] = "Key"

C1.long = melt(C1, id.vars =  "Key")

colnames(C1.long)[2] = "Username"
colnames(C1.long)[3] = "Score"

C1.long$Version = rep("C")
C1.long$List = rep("1")
C1.long$list_type = rep("Cat")

##List 2
C2 = ver.C[43:62, 1:22]

colnames(C2)[1] = "Key"

C2.long = melt(C2, id.vars =  "Key")

colnames(C2.long)[2] = "Username"
colnames(C2.long)[3] = "Score"

C2.long$Version = rep("C")
C2.long$List = rep("2")
C2.long$list_type = rep("Ad_hoc")

##List 3
C3 = ver.C[80:99, 1:22]

colnames(C3)[1] = "Key"

C3.long = melt(C3, id.vars =  "Key")

colnames(C3.long)[2] = "Username"
colnames(C3.long)[3] = "Score"

C3.long$Version = rep("C")
C3.long$List = rep("3")
C3.long$list_type = rep("Unrel")

##List 4
C4 = ver.C[115:134, 1:22]

colnames(C4)[1] = "Key"

C4.long = melt(C4, id.vars =  "Key")

colnames(C4.long)[2] = "Username"
colnames(C4.long)[3] = "Score"

C4.long$Version = rep("C")
C4.long$List = rep("4")
C4.long$list_type = rep("Unrel")

##List 5
C5 = ver.C[154:173, 1:22]

colnames(C5)[1] = "Key"

C5.long = melt(C5, id.vars =  "Key")

colnames(C5.long)[2] = "Username"
colnames(C5.long)[3] = "Score"

C5.long$Version = rep("C")
C5.long$List = rep("5")
C5.long$list_type = rep("Cat")

##List 6
C6 = ver.C[194:213, 1:22]

colnames(C6)[1] = "Key"

C6.long = melt(C6, id.vars =  "Key")

colnames(C6.long)[2] = "Username"
colnames(C6.long)[3] = "Score"

C6.long$Version = rep("C")
C6.long$List = rep("6")
C6.long$list_type = rep("Ad_hoc")

##combine all the c's
C_Final = rbind(C1.long, C2.long, C3.long, C4.long, C5.long, C6.long)

####Now for Version D####
##List 1
D1 = ver.D[3:22, 1:20]

colnames(D1)[1] = "Key"

D1.long = melt(D1, id.vars =  "Key")

colnames(D1.long)[2] = "Username"
colnames(D1.long)[3] = "Score"

D1.long$Version = rep("D")
D1.long$List = rep("1")
D1.long$list_type = rep("Unrel")

##List 2
D2 = ver.D[42:61, 1:20]

colnames(D2)[1] = "Key"

D2.long = melt(D2, id.vars =  "Key")

colnames(D2.long)[2] = "Username"
colnames(D2.long)[3] = "Score"

D2.long$Version = rep("D")
D2.long$List = rep("2")
D2.long$list_type = rep("Ad_hoc")

##List 3
D3 = ver.D[79:98, 1:20]

colnames(D3)[1] = "Key"

D3.long = melt(D3, id.vars =  "Key")

colnames(D3.long)[2] = "Username"
colnames(D3.long)[3] = "Score"

D3.long$Version = rep("D")
D3.long$List = rep("3")
D3.long$list_type = rep("Ad_hoc")

##List 4
D4 = ver.D[119:138, 1:20]

colnames(D4)[1] = "Key"

D4.long = melt(D4, id.vars =  "Key")

colnames(D4.long)[2] = "Username"
colnames(D4.long)[3] = "Score"

D4.long$Version = rep("D")
D4.long$List = rep("4")
D4.long$list_type = rep("Cat")

##List 5
D5 = ver.D[153:172, 1:20]

colnames(D5)[1] = "Key"

D5.long = melt(D5, id.vars =  "Key")

colnames(D5.long)[2] = "Username"
colnames(D5.long)[3] = "Score"

D5.long$Version = rep("D")
D5.long$List = rep("5")
D5.long$list_type = rep("Cat")

##List 6
D6 = ver.D[188:207, 1:20]

colnames(D6)[1] = "Key"

D6.long = melt(D6, id.vars =  "Key")

colnames(D6.long)[2] = "Username"
colnames(D6.long)[3] = "Score"

D6.long$Version = rep("D")
D6.long$List = rep("6")
D6.long$list_type = rep("Unrel")

##Combine all the D's
D_Final = rbind(D1.long, D2.long, D3.long, D4.long, D5.long, D6.long)

####Now for Version E####
E1 = ver.E[3:22, 1:20]

colnames(E1)[1] = "Key"

E1.long = melt(E1, id.vars =  "Key")

colnames(E1.long)[2] = "Username"
colnames(E1.long)[3] = "Score"

E1.long$Version = rep("E")
E1.long$List = rep("1")
E1.long$list_type = rep("Cat")

##List 2
E2 = ver.E[38:57, 1:20]

colnames(E2)[1] = "Key"

E2.long = melt(E2, id.vars =  "Key")

colnames(E2.long)[2] = "Username"
colnames(E2.long)[3] = "Score"

E2.long$Version = rep("E")
E2.long$List = rep("2")
E2.long$list_type = rep("Ad_hoc")

##List 3
E3 = ver.E[74:93, 1:20]

colnames(E3)[1] = "Key"

E3.long = melt(E3, id.vars =  "Key")

colnames(E3.long)[2] = "Username"
colnames(E3.long)[3] = "Score"

E3.long$Version = rep("E")
E3.long$List = rep("3")
E3.long$list_type = rep("Unrel")

##List 4
E4 = ver.E[116:135, 1:20]

colnames(E4)[1] = "Key"

E4.long = melt(E4, id.vars =  "Key")

colnames(E4.long)[2] = "Username"
colnames(E4.long)[3] = "Score"

E4.long$Version = rep("E")
E4.long$List = rep("4")
E4.long$list_type = rep("Unrel")

##List 5
E5 = ver.E[163:182, 1:20]

colnames(E5)[1] = "Key"

E5.long = melt(E5, id.vars =  "Key")

colnames(E5.long)[2] = "Username"
colnames(E5.long)[3] = "Score"

E5.long$Version = rep("E")
E5.long$List = rep("5")
E5.long$list_type = rep("Cat")

##List 6
E6 = ver.E[206:225, 1:20]

colnames(E6)[1] = "Key"

E6.long = melt(E6, id.vars =  "Key")

colnames(E6.long)[2] = "Username"
colnames(E6.long)[3] = "Score"

E6.long$Version = rep("E")
E6.long$List = rep("6")
E6.long$list_type = rep("Ad_hoc")

##Combine all the E's
E_Final = rbind(E1.long, E2.long, E3.long, E4.long, E5.long, E6.long)

####Now do the F's####
##List 1
F1 = ver.F[3:22, 1:22]

colnames(F1)[1] = "Key"

F1.long = melt(F1, id.vars =  "Key")

colnames(F1.long)[2] = "Username"
colnames(F1.long)[3] = "Score"

F1.long$Version = rep("F")
F1.long$List = rep("1")
F1.long$list_type = rep("Unrel")

##List 2
F2 = ver.F[53:72, 1:22]

colnames(F2)[1] = "Key"

F2.long = melt(F2, id.vars =  "Key")

colnames(F2.long)[2] = "Username"
colnames(F2.long)[3] = "Score"

F2.long$Version = rep("F")
F2.long$List = rep("2")
F2.long$list_type = rep("Ad_hoc")

##List 3
F3 = ver.F[91:110, 1:22]

colnames(F3)[1] = "Key"

F3.long = melt(F3, id.vars =  "Key")

colnames(F3.long)[2] = "Username"
colnames(F3.long)[3] = "Score"

F3.long$Version = rep("F")
F3.long$List = rep("3")
F3.long$list_type = rep("Ad_hoc")

##List 4
F4 = ver.F[126:145, 1:22]

colnames(F4)[1] = "Key"

F4.long = melt(F4, id.vars =  "Key")

colnames(F4.long)[2] = "Username"
colnames(F4.long)[3] = "Score"

F4.long$Version = rep("F")
F4.long$List = rep("4")
F4.long$list_type = rep("Cat")

##List 5
F5 = ver.F[160:179, 1:22]

colnames(F5)[1] = "Key"

F5.long = melt(F5, id.vars =  "Key")

colnames(F5.long)[2] = "Username"
colnames(F5.long)[3] = "Score"

F5.long$Version = rep("F")
F5.long$List = rep("5")
F5.long$list_type = rep("Cat")

##List 6
F6 = ver.F[198:217, 1:22]

colnames(F6)[1] = "Key"

F6.long = melt(F6, id.vars =  "Key")

colnames(F6.long)[2] = "Username"
colnames(F6.long)[3] = "Score"

F6.long$Version = rep("F")
F6.long$List = rep("6")
F6.long$list_type = rep("Unrel")

##Combine all the F's
F_Final = rbind(F1.long, F2.long, F3.long, F4.long, F5.long, F6.long)

####Combine everything and write to csv####
Final = rbind(A_Final, B_Final, C_Final, D_Final, E_Final, F_Final)

write.csv(Final, file = "Human Coded Arranged.csv", row.names = F)
