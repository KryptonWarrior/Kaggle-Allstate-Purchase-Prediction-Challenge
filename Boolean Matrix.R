# Call SQL package in R (sqldf)
library(sqldf)

#import data to R
SampleTest <- read.csv("C:/Users/Skamali/Desktop/R Codes/06last1_l_train.csv", sep=",", header=TRUE)

colnames(SampleTest)[23] <- "F0"

Boolean_Matrix <- data.frame( customer_ID = numeric(0), 
                              A0 = numeric(0), A1 = numeric(0), A2 = numeric(0), 
                              B0 = numeric(0), B1 = numeric(0), 
                              C1 = numeric(0), C2 = numeric(0), C3 = numeric(0), C4 = numeric(0),
                              D1 = numeric(0), D2 = numeric(0), D3 = numeric(0),
                              E0 = numeric(0), E1 = numeric(0),
                              F0 = numeric(0), F1 = numeric(0), F2 = numeric(0), F3 = numeric(0),
                              G1 = numeric(0), G2 = numeric(0), G3 = numeric(0), G4 = numeric(0) )

k <- 1

while ( k <= nrow(SampleTest)) {
        
        vec <- c()
        vec[1] <- SampleTest$customer_ID[k]
        
        #Comparing the last 2 lines.
        if (SampleTest$A[k] == 0) {
                
                vec[2] <- 1
                vec[3] <- 0
                vec[4] <- 0   
                
        } else if (SampleTest$A[k] == 1) {
                
                vec[2] <- 0
                vec[3] <- 1
                vec[4] <- 0
                
        } else {
                vec[2] <- 0
                vec[3] <- 0
                vec[4] <- 1      
        } 
        
        if (SampleTest$B[k] == 0) {
                
                vec[5] <- 1
                vec[6] <- 0  
                
        } else {
                
                vec[5] <- 0
                vec[6] <- 1
                
        }
        
        if (SampleTest$C[k] == 1) {
                
                vec[7] <- 1
                vec[8] <- 0
                vec[9] <- 0
                vec[10] <- 0
                
        } else if (SampleTest$C[k] == 2) {
                
                vec[7] <- 0
                vec[8] <- 1
                vec[9] <- 0
                vec[10] <- 0
                
        } else if (SampleTest$C[k] == 3) {
                
                vec[7] <- 0
                vec[8] <- 0
                vec[9] <- 1
                vec[10] <- 0
                
        } else {
                
                vec[7] <- 0
                vec[8] <- 0
                vec[9] <- 0
                vec[10] <- 1
        }
        
        if (SampleTest$D[k] == 1) {
                
                vec[11] <- 1
                vec[12] <- 0
                vec[13] <- 0   
                
        } else if (SampleTest$D[k] == 2) {
                
                vec[11] <- 0
                vec[12] <- 1
                vec[13] <- 0
                
        } else {
                
                vec[11] <- 0
                vec[12] <- 0
                vec[13] <- 1
        }
        
        if (SampleTest$E[k] == 0) {
                
                vec[14] <- 1
                vec[15] <- 0 
                
        } else {
                
                vec[14] <- 0
                vec[15] <- 1
                
        }
        
        if (SampleTest$F0[k] == 0) {
                
                vec[16] <- 1
                vec[17] <- 0
                vec[18] <- 0
                vec[19] <- 0
                
        } else if (SampleTest$F0[k] == 1) {
                
                vec[16] <- 0
                vec[17] <- 1
                vec[18] <- 0
                vec[19] <- 0
                
        } else if (SampleTest$F0[k] == 2) {
                
                vec[16] <- 0
                vec[17] <- 0
                vec[18] <- 1
                vec[19] <- 0
                
        } else {
                vec[16] <- 0
                vec[17] <- 0
                vec[18] <- 0
                vec[19] <- 1
        }
        
        if (SampleTest$G[k] == 1) {
                vec[20] <- 1
                vec[21] <- 0
                vec[22] <- 0
                vec[23] <- 0
                
        } else if (SampleTest$G[k] == 2) {
                
                vec[20] <- 0
                vec[21] <- 1
                vec[22] <- 0
                vec[23] <- 0
                
        } else if (SampleTest$G[k] == 3) {
                
                vec[20] <- 0
                vec[21] <- 0
                vec[22] <- 1
                vec[23] <- 0
                
        } else {
                vec[20] <- 0
                vec[21] <- 0
                vec[22] <- 0
                vec[23] <- 1
        }

        Boolean_Matrix[k, ] = vec
        k <- k + 1  
        
}

m <- Boolean_Matrix [, 2:23]

COR <- cor(m)

write.csv(COR, file = '06last1_CORTrain.csv', row.names = FALSE)

