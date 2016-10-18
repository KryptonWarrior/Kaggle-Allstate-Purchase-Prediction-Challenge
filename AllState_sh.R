# 1)

rm(list = ls())

Train = read.csv("C:/Users/shuang/Desktop/Kaggle Allstate Contest/train_new.csv", sep=",", header=TRUE)
#Train = read.csv("C:/Users/shuang/Desktop/Kaggle Allstate Contest/train.csv", sep=",", header=TRUE, na.strings=c("","NA"))
#Testold = read.csv("C:/Users/shuang/Desktop/Kaggle Allstate Contest/test_v2.csv",sep=",", header=TRUE)
Test = read.csv("C:/Users/shuang/Desktop/Kaggle Allstate Contest/test_new.csv",sep=",", header=TRUE)

# These adjustments are put in place to fit the current code
Train = Train[,c(-1)]
Train = Train[order(Train$customer_ID,Train$shopping_pt),]

Test = Test[,c(-1)]
Test = Test[order(Test$customer_ID,Test$shopping_pt),]
#write.csv(Test, file = "C:/Users/shuang/Desktop/Kaggle Allstate Contest/TestQuicky.csv")

# 2)
Traintmp = Train[,c(3,18:24)] #purchased or not and all coverage selection
Traintotal = dim(Traintmp)[1] #665,249
Traincase = sum(Traintmp[,1]) #97,009
Traintmp1 = matrix(0,Traincase,14)
Traintmp2 = matrix(0,Traincase,1)
colnames(Traintmp1) = c("Aavg","Bavg","Cavg","Davg","Eavg","Favg","Gavg",
				"Alast","Blast","Clst","Dlast","Elast","Flast","Glast")
colnames(Traintmp2) = "NofQuotes"

Testtmp = Test[,c(1,18:24)] #Select only CustomerID and the coverage selection
Testcase = length(unique(Testtmp[,1])) #55,716
Testtotal = dim(Testtmp)[1] #198,856
Testtmp1 = matrix(0,Testcase,14)
Testtmp2 = matrix(0,Testcase,1)
colnames(Testtmp1) = c("Aavg","Bavg","Cavg","Davg","Eavg","Favg","Gavg",
				"Alast","Blast","Clst","Dlast","Elast","Flast","Glast")
colnames(Testtmp2) = "NofQuotes"

# 3) BSSSS

## Traintmp2 & Testtmp2 (Calculating # of Quotes)

rt = (Traintmp[,1] == 1) #purchased line
c = 0
p = 1
for (i in 1:Traintotal) {
	c = c + 1
	if (rt[i]) {
		Traintmp2[p] = c - 1
		p = p + 1
		c = 0
	}
}
Traintmp = Traintmp[Traintmp[,1] == 0,2:8] #now it has row.names, coverage
Traintotal = dim(Traintmp)[1] #568,240 (total # of quotes)

c1 = 0
p1 = 1
t = Testtmp[,1]
for (i in 1:(Testtotal-1)) {
	c1 = c1 + 1
	if (t[i] != t[i+1]) {
		Testtmp2[p1] = c1
		p1 = p1 + 1
		c1 = 0
	}
}
Testtmp2[p1] = Testtotal - sum(Testtmp2) #Calculating # of quotes
Testtmp = Testtmp[,-1]

## Traintmp1 & Testtmp1 (12 min) - Calculating Coverage Avg, with rounding; and Coverage Last

sum = 0
for (i in 1:Traincase) {
	sum = sum + Traintmp2[i]
	Traintmp1[i,1:7] = as.numeric(round(colMeans(Traintmp[(sum-Traintmp2[i]+1):sum,])))
	Traintmp1[i,8:14] = as.numeric(Traintmp[sum,1:7])
}

sum1 = 0
for (i in 1:Testcase) {
	sum1 = sum1 + Testtmp2[i]
	Testtmp1[i,1:7] = as.numeric(round(colMeans(Testtmp[(sum1-Testtmp2[i]+1):sum1,])))
	Testtmp1[i,8:14] = as.numeric(Testtmp[sum1,1:7])
}

## Traintmp3 & Testtmp3 (combining # of quotes with covg avg and covg last)

Traintmp3 = cbind(Traintmp2,Traintmp1)
Testtmp3 = cbind(Testtmp2,Testtmp1)

# 4)

## Train3 & Test3 (2 min) #Calculating last record

Ind = matrix(FALSE,dim(Train)[1],1)

for (i in 1:(dim(Train)[1]-1)) {
	if (Train[i,3] == 0 && Train[i+1,3] == 1) {
		Ind[i] = TRUE
	}
}

Train1 = cbind(Ind,Train)
Train2 = Train1[Train1[,1],] #Get only one record per customer (when last record = 'true')
Train3 = cbind(Train2,Traintmp3) #Taintmp3 is the calculated coverage data
Train4 = Train3[,c(-1:-4,-19:-25)] #eliminate some fields from the training data

Ind1 = matrix(FALSE,dim(Test)[1],1)
count = 0
for (i in 1:Testcase) {
	count = count + Testtmp2[i]
	Ind1[count] = TRUE
}
Test1 = cbind(Ind1,Test)
Test2 = Test1[Test1[,1],]
Test3 = cbind(Test2,Testtmp3)
Test4 = Test3[,c(-1:-4,-19:-25)]

#baseline<-cbind(Test3[,2],
#                Test4[,24]*1000000
#                +Test4[,25]*100000
#                +Test4[,26]*10000
#                +Test4[,27]*1000
#                +Test4[,28]*100
#                +Test4[,29]*10
#                +Test4[,30])

## NeedPred
NeedPred = Train[Train[,3] == 1,18:24] #selecting the Purchase line
Train5 = cbind(Train4,NeedPred)

# converting variables into factor format for classification random tree
Train5$Aavg<-as.factor(Train5$Aavg)
Train5$Bavg<-as.factor(Train5$Bavg)
Train5$Cavg<-as.factor(Train5$Cavg)
Train5$Davg<-as.factor(Train5$Davg)
Train5$Eavg<-as.factor(Train5$Eavg)
Train5$Favg<-as.factor(Train5$Favg)
Train5$Gavg<-as.factor(Train5$Gavg)
Train5$Alast<-as.factor(Train5$Alast)
Train5$Blast<-as.factor(Train5$Blast)
colnames(Train5)[26]<-"Clast"
Train5$Clast<-as.factor(Train5$Clast)
Train5$Dlast<-as.factor(Train5$Dlast)
Train5$Elast<-as.factor(Train5$Elast)
Train5$Flast<-as.factor(Train5$Flast)
Train5$Glast<-as.factor(Train5$Glast)
Train5$A<-as.factor(Train5$A)
Train5$B<-as.factor(Train5$B)
Train5$C<-as.factor(Train5$C)
Train5$D<-as.factor(Train5$D)
Train5$E<-as.factor(Train5$E)
colnames(Train5)[36]<-"Ftemp" #remember to use this name when predicting
Train5$Ftemp<-as.factor(Train5$Ftemp)
Train5$G<-as.factor(Train5$G)
Train5$risk_factor<-as.factor(Train5$risk_factor)
Train5$C_previous<-as.factor(Train5$C_previous)

Test4$Alast<-as.factor(Test4$Alast)
Test4$Blast<-as.factor(Test4$Blast)
colnames(Test4)[26]<-"Clast"
Test4$Clast<-as.factor(Test4$Clast)
Test4$Dlast<-as.factor(Test4$Dlast)
Test4$Elast<-as.factor(Test4$Elast)
Test4$Flast<-as.factor(Test4$Flast)
Test4$Glast<-as.factor(Test4$Glast)
Test4$Aavg<-as.factor(Test4$Aavg)
Test4$Bavg<-as.factor(Test4$Bavg)
Test4$Cavg<-as.factor(Test4$Cavg)
Test4$Davg<-as.factor(Test4$Davg)
Test4$Eavg<-as.factor(Test4$Eavg)
Test4$Favg<-as.factor(Test4$Favg)
Test4$Gavg<-as.factor(Test4$Gavg)

Test4$C_previous<-as.factor(Test4$C_previous)
Test4$risk_factor<-as.factor(Test4$risk_factor)

## Save files

write.csv(Train5, file = "C:/Users/shuang/Desktop/Kaggle Allstate Contest/Trainxnew.csv")
write.csv(Test4, file = "C:/Users/shuang/Desktop/Kaggle Allstate Contest/Testxnew.csv")

#cor(Train[,18:24]) - A&E,F; B&E; C&D
#      A          B           C          D         E           F          G
#A 1.00000000 0.15803975  0.19051949 0.18935059 0.3246183  0.53118570 0.04533461
#B 0.15803975 1.00000000  0.09361113 0.12265594 0.4612710  0.12823086 0.02482582
#C 0.19051949 0.09361113  1.00000000 0.63953022 0.1960552 -0.01218753 0.10792490
#D 0.18935059 0.12265594  0.63953022 1.00000000 0.1922799  0.03031655 0.10339772
#E 0.32461825 0.46127097  0.19605525 0.19227994 1.0000000  0.15957927 0.10227253
#F 0.53118570 0.12823086 -0.01218753 0.03031655 0.1595793  1.00000000 0.07131848
#G 0.04533461 0.02482582  0.10792490 0.10339772 0.1022725  0.07131848 1.00000000


# 5)

## Prediction for Coverages
install.packages("randomForest")
library(randomForest)

ATrain = Train5[,c(8:9,15:16,17,24,31)] #car_value, risk_factor, cost, NofQuotes,Aavg, Alast, A
ATrain2 = Train5[,c(8:9,15:16,17,24,29,31)] #adding Flast
ATrain3 = Train5[,c(8:9,15:16,17,24,28,29,31)] #adding Flast and Elast
BTrain = Train5[,c(8:9,15:16,18,25,28,32)] #adding Elast
BTrain2 = Train5[,c(8:9,15:16,18,28,32)] #deleting Blast, just to understand the behavior
GTrain = Train5[,c(8:9,15:16,17,26:30,37)] #did not include G_avg, but E,D,F,G_last

ATrain.rf = randomForest(A ~ ., data = ATrain, na.action = na.omit) 
ATrain2.rf = randomForest(A ~ ., data = ATrain2, na.action = na.omit) 
ATrain3.rf = randomForest(A ~ ., data = ATrain3, na.action = na.omit) 

BTrain.rf = randomForest(B ~ ., data = BTrain, na.action = na.omit)
BTrain2.rf = randomForest(B ~ ., data = BTrain2, na.action = na.omit)
GTrain.rf = randomForest(G ~ ., data = GTrain, na.action = na.omit)

write.csv(ATrain3, file = "C:/Users/shuang/Desktop/Kaggle Allstate Contest/ATrain3.csv")
print(ATrain.rf)
round(importance(ATrain.rf), 2)

Apredict = predict(ATrain.rf,Test4)
#Apredict2 = predict(ATrain2.rf,Test4)
Apredict3 = predict(ATrain3.rf,Test4)
Bpredict = predict(BTrain.rf,Test4)
#Bpredict2 = predict(BTrain2.rf,Test4)
Gpredict = predict(GTrain.rf,Test4)

round(importance(BTrain2.rf), 2)

#Output the prediction in correct format
Adiff=sum(as.numeric(Apredict3)-as.numeric(Apredict)) #5
Adiff2=sum(as.numeric(Apredict3)-as.numeric(Test4[,24])) #-3
Adiff3=sum(as.numeric(Apredict)-as.numeric(Test4[,24])) # 2
Aview = cbind(Apredict3,Test4[,24])
Bdiff=sum(as.numeric(Bpredict)-as.numeric(Test4[,25])) #0
Gdiff = sum(as.numeric(Gpredict)-as.numeric(Test4[,30]))

baselineG<-cbind(Test3[,2],
                 (as.numeric(Test4[,24])-1)*1000000
                 +(as.numeric(Test4[,25])-1)*100000
                 +(as.numeric(Test4[,26]))*10000
                 +(as.numeric(Test4[,27]))*1000
                 +(as.numeric(Test4[,28])-1)*100
                 +(as.numeric(Test4[,29])-1)*10
                 +(as.numeric(Gpredict)))
#View(baseline)
colnames(baselineG) <- c("customer_ID", "plan")
write.csv(baselineG, file = "C:/Users/shuang/Desktop/Kaggle Allstate Contest/baselineG.csv")