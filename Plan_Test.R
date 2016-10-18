# Call SQL package in R (sqldf)
library(sqldf)

#import data to R
last0_test <- read.csv("~/Desktop/R Codes/CustomerTest_MostRepeat.csv", sep=",", header=TRUE)

#Initialized the empty data frame for final answer
Plan_test <- data.frame( customer_ID = numeric(0), plan = numeric(0), stringsAsFactors=F )

#colnames(last0_test)[11] <- "F0"

#Write Plan options in form on final result
for (i in 1:nrow(last0_test)) {
        
        k <- last0_test$customer_ID[i]

        m <- last0_test$A_mostRepeat[i]*1000000 + last0_test$B_mostRepeat[i]*100000 + last0_test$C_mostRepeat[i]*10000 + 
                last0_test$D_mostRepeat[i]*1000 + last0_test$E_mostRepeat[i]*100 + last0_test$F_mostRepeat[i]*10 + 
                last0_test$G_mostRepeat[i]
                
        Plan_test <- rbind(Plan_test,c(k, m)) 
            
}

colnames(Plan_test) <- c("customer_ID", "plan")

#write customer purchase by try 3 on a new excel sheet
write.csv(Plan_test, file = 'plan_CustomerTest_MostRepeat.csv', row.names = FALSE)
