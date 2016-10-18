# Call SQL package in R (sqldf)
library(sqldf)

#import data to R
SampleTest <- read.csv("C:/Users/Skamali/Desktop/R Codes/group_pur8.csv", sep=",", header=TRUE)


colnames(SampleTest)[23] <- "F0"

k <- 1
df <- data.frame( customer_ID = numeric(0), A_lastHistory = numeric(0), B_lastHistory = numeric(0), 
                  C_lastHistory = numeric(0), D_lastHistory = numeric(0), E_lastHistory = numeric(0), 
                  F_lastHistory = numeric(0), G_lastHistory = numeric(0) )


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
        
        n <- 7
        for(i in n:1) {
                
                #Comparing the last 2 lines.
                if (SampleTest$A[k + i - 1] == SampleTest$A[k+n]) {
                        countA <- countA + 1
                        vec[2] <- countA

                }
                else {
                        countA <- countA + 0
                        vec[2] <- countA
                }
                
                if (SampleTest$B[k + i - 1] == SampleTest$B[k+n]) {
                        countB <- countB + 1
                        vec[3] <- countB
                }
                else {
                        countB <- countB + 0
                        vec[3] <- countB
                }
                if (SampleTest$C[k + i - 1] == SampleTest$C[k+n]) {
                        countC <- countC + 1
                        vec[4] <- countC
                }
                else {
                        countC <- countC + 0
                        vec[4] <- countC
                }
                if (SampleTest$D[k + i - 1] == SampleTest$D[k+n]) {
                        countD <- countD + 1
                        vec[5] <- countD
                }
                else {
                        countD <- countD + 0
                        vec[5] <- countD
                }
                if (SampleTest$E[k + i - 1] == SampleTest$E[k+n]) {
                        countE <- countE + 1
                        vec[6] <- countE
                }
                else {
                        countE <- countE + 0
                        vec[6] <- countE
                }
                if (SampleTest$F0[k + i - 1] == SampleTest$F0[k+n]) {
                        countF0 <- countF0 + 1
                        vec[7] <- countF0
                }
                else {
                        countF0 <- countF0 + 0
                        vec[7] <- countF0
                }
                if (SampleTest$G[k + i - 1] == SampleTest$G[k+n]) {
                        countG <- countG + 1
                        vec[8] <- countG
                }
                else {
                        countG <- countG + 0
                        vec[8] <- countG
                }
                
        }
        
        df[k, ] = vec
        k <- k + 8
        
}

df <- df[complete.cases(df),]

write.csv(df, file = '008customer_MatchHistory.csv', row.names = FALSE)

