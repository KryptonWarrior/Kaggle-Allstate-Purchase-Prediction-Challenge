# Call SQL package in R (sqldf)
library(sqldf)

#import data to R
SampleTest <- read.csv("~/Desktop/R Codes/11group_test.csv", sep=",", header=TRUE)

colnames(SampleTest)[23] <- "F0"

k <- 1
df <- data.frame( customer_ID = numeric(0), A_Repl0 = numeric(0), B_Repl0 = numeric(0), C_Repl0 = numeric(0),
                  D_Repl0 = numeric(0), E_Repl0 = numeric(0), F_Repl0 = numeric(0), G_Repl0 = numeric(0) )


while ( k < nrow(SampleTest) ) {
        
        vec <- c()
        countA <- 0
        countB <- 0
        countC <- 0
        countD <- 0
        countE <- 0
        countF0 <- 0
        countG <- 0
        
        vec[1] <- SampleTest$customer_ID[k]
        
        
        for(i in 1:2) {
                
                #Comparing the last 2 lines.
                if (SampleTest$A[k + i - 1] == SampleTest$A[k+i]) {
                        countA <- countA 
                        vec[2] <- countA

                }
                else {
                        countA <- countA + 1
                        vec[2] <- countA
                }
                
                if (SampleTest$B[k + i - 1] == SampleTest$B[k+i]) {
                        countB <- countB 
                        vec[3] <- countB
                }
                else {
                        countB <- countB + 1
                        vec[3] <- countB
                }
                if (SampleTest$C[k + i - 1] == SampleTest$C[k+i]) {
                        countC <- countC 
                        vec[4] <- countC
                }
                else {
                        countC <- countC + 1
                        vec[4] <- countC
                }
                if (SampleTest$D[k + i - 1] == SampleTest$D[k+i]) {
                        countD <- countD 
                        vec[5] <- countD
                }
                else {
                        countD <- countD + 1
                        vec[5] <- countD
                }
                if (SampleTest$E[k + i - 1] == SampleTest$E[k+i]) {
                        countE <- countE 
                        vec[6] <- countE
                }
                else {
                        countE <- countE + 1
                        vec[6] <- countE
                }
                if (SampleTest$F0[k + i - 1] == SampleTest$F0[k+i]) {
                        countF0 <- countF0 
                        vec[7] <- countF0
                }
                else {
                        countF0 <- countF0 + 1
                        vec[7] <- countF0
                }
                if (SampleTest$G[k + i - 1] == SampleTest$G[k+i]) {
                        countG <- countG 
                        vec[8] <- countG
                }
                else {
                        countG <- countG + 1
                        vec[8] <- countG
                }
                
        }
        
        df[k, ] = vec
        k <- k + 11
        
}

df <- df[complete.cases(df),]

write.csv(df, file = '011customer_seqMatchTest.csv', row.names = FALSE)
