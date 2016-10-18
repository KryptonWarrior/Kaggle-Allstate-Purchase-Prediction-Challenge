# Call SQL package in R (sqldf)
library(sqldf)

#import data to R
SampleTest <- read.csv("~/Desktop/R Codes/11group_test.csv", sep=",", header=TRUE)


colnames(SampleTest)[23] <- "F0"

k <- 1
df <- data.frame( customer_ID = numeric(0), A_mostRepeat  = numeric(0), B_mostRepeat = numeric(0), 
                  C_mostRepeat = numeric(0), D_mostRepeat = numeric(0), E_mostRepeat = numeric(0), 
                  F_mostRepeat = numeric(0), G_mostRepeat = numeric(0) )


while ( k < nrow(SampleTest) ) {
        
        vec <- c()
        countA0 <- 0; countA1 <- 0; countA2 <- 0;
        countB0 <- 0; countB1 <- 0;
        countC1 <- 0; countC2 <- 0; countC3 <- 0; countC4 <- 0;
        countD1 <- 0; countD2 <- 0; countD3 <- 0;
        countE0 <- 0; countE1 <- 0;
        countF0 <- 0; countF1 <- 0; countF2 <- 0; countF3<- 0;
        countG1 <- 0; countG2 <- 0; countG3 <- 0; countG4 <- 0;
        
        vec[1] <- SampleTest$customer_ID[k]
        
        n <- 11
        for(i in 1:n) {
                
                if (SampleTest$A[k + i - 1] == 0) {
                        
                        countA0 <- countA0 + 1

                        
                } else if (SampleTest$A[k + i - 1] == 1) {
                        
                        countA1 <- countA1 + 1 
                        
                } else {
                        
                        countA2 <- countA2 + 1      
                } 

                if (SampleTest$B[k + i - 1] == 0) {
                        
                        countB0 <- countB0 + 1
                        
                } else {
                        
                        countB1 <- countB1 + 1 
                }
                
                if (SampleTest$C[k + i - 1] == 1) {
                        
                        countC1 <- countC1 + 1 
                        
                } else if (SampleTest$C[k + i - 1] == 2) {
                        
                        countC2 <- countC2 + 1
                        
                } else if (SampleTest$C[k + i - 1] == 3) {
                        
                        countC3 <- countC3 + 1 
                        
                } else {
                        
                        countC4 <- countC4 + 1
                }
                
                if (SampleTest$D[k + i - 1] == 1) {
                        
                        countD1 <- countD1 + 1  
                        
                } else if (SampleTest$D[k + i - 1] == 2) {
                        
                        countD2 <- countD2 + 1
                        
                } else {
                        
                        countD3 <- countD3 + 1
                }
                
                if (SampleTest$E[k + i - 1] == 0) {
                        
                        countE0 <- countE0 + 1 
                        
                } else {
                        
                        countE1 <- countE1 + 1 
                        
                }
                
                if (SampleTest$F0[k + i - 1] == 0) {
                        
                        countF0 <- countF0 + 1 
                        
                } else if (SampleTest$F0[k + i - 1] == 1) {
                        
                        countF1 <- countF1 + 1
                        
                } else if (SampleTest$F0[k + i - 1] == 2) {
                        
                        countF2 <- countF2 + 1 
                        
                } else {
                        
                        countF3 <- countF3 + 1
                }
                
                if (SampleTest$G[k + i - 1] == 1) {
                        
                        countG1 <- countG1 + 1 
                        
                } else if (SampleTest$G[k + i - 1] == 2) {
                        
                        countG2 <- countG2 + 1
                        
                } else if (SampleTest$G[k + i - 1] == 3) {
                        
                        countG3 <- countG3 + 1
                        
                } else {
                        countG4 <- countG4 + 1
                }
                
        }
        #-----------A-----0,1,2------
        #----------------------------
        if ( (countA0 > countA1) && (countA0 > countA2) ) {
                vec[2] <- 0
        } else if ( (countA1 > countA0) && (countA1 > countA2) ) {
                vec[2] <- 1
        } else if ( (countA2 > countA0) && (countA2 > countA1) ) {
                vec[2] <- 2
        } else {
                vec[2] <- SampleTest$A[k + n -1]
        }
        #-----------B-----0,1--------
        #----------------------------
        if (countB0 > countB1) {
                vec[3] <- 0
        } else if (countB1 > countB0) {
                vec[3] <- 1
        } else {
                vec[3] <- SampleTest$B[k + n -1]
        }
        #-----------C----1,2,3,4-----
        #----------------------------
        if ( (countC1 > countC2) && (countC1 > countC3) && (countC1 > countC4) ) {
                vec[4] <- 1
        } else if ( (countC2 > countC1) && (countC2 > countC3) && (countC2 > countC4) ) {
                vec[4] <- 2
        } else if ( (countC3 > countC1) && (countC3 > countC2) && (countC3 > countC4) ) {
                vec[4] <- 3
        } else if ( (countC4 > countC1) && (countC4 > countC2) && (countC4 > countC3) ) {
                vec[4] <- 4
        } else {
                vec[4] <- SampleTest$C[k + n-1]
        }
        #-----------D----1,2,3-------
        #----------------------------
        if ( (countD1 > countD2) && (countD1 > countD3) ) {
                vec[5] <- 1
        } else if ( (countD2 > countD1) && (countD2 > countD3) ) {
                vec[5] <- 2
        } else if ( (countD3 > countD1) && (countD3 > countD2) ) {
                vec[5] <- 3
        } else {
                vec[5] <- SampleTest$D[k + n-1]
        }
        #-----------E------0,1-------
        #----------------------------
        if (countE0 > countE1) {
                vec[6] <- 0
        } else if (countE1 > countE0) {
                vec[6] <- 1
        } else {
                vec[6] <- SampleTest$E[k + n-1]
        }
        #-----------F----0,1,2,3-----
        #----------------------------
        if ( (countF0 > countF1) && (countF0 > countF2) && (countF0 > countF3) ) {
                vec[7] <- 0
        } else if ( (countF1 > countF0) && (countF1 > countF2) && (countF1 > countF3) ) {
                vec[7] <- 1
        } else if ( (countF2 > countF0) && (countF2 > countF1) && (countF2 > countF3) ) {
                vec[7] <- 2
        } else if ( (countF3 > countF0) && (countF3 > countF1) && (countF3 > countF2) ) {
                vec[7] <- 3
        } else {
                vec[7] <- SampleTest$F0[k + n - 1]
        }
        #-----------G-----1,2,3,4----
        #----------------------------
        if ( (countG1 > countG2) && (countG1 > countG3) && (countG1 > countG4) ) {
                vec[8] <- 1
        } else if ( (countG2 > countG1) && (countG2 > countG3) && (countG2 > countG4) ) {
                vec[8] <- 2
        } else if ( (countG3 > countG1) && (countG3 > countG2) && (countG3 > countG4) ) {
                vec[8] <- 3
        } else if ( (countG4 > countG1) && (countG4 > countG2) && (countG4 > countG3) ) {
                vec[8] <- 4
        } else {
                vec[8] <- SampleTest$G[k + n-1]
        }
        
        df[k, ] = vec
        k <- k + 11
        
}

df <- df[complete.cases(df),]

write.csv(df, file = '011CustomerTest_MostRepeat.csv', row.names = FALSE)

