#remove all the objects stored
rm(list=ls())

#set current working directory
setwd("C:/R programs")

#Current working directory
getwd()

#reading CSV
credit = read.csv("credit-card-data.csv", header = T)

##################################Exploratory data & Initial data preparation######################################

head(credit)
#Getting the structure of the dataset
str(credit)

#Getting the number of variables and obervation in the datasets
dim(credit)

credit$CUST_ID = NULL
credit$CASH_ADVANCE_TRX = as.numeric(credit$CASH_ADVANCE_TRX)
credit$PURCHASES_TRX = as.numeric(credit$PURCHASES_TRX)
credit$TENURE = as.numeric(credit$TENURE)


##################################Missing Values Analysis###############################################
#getting missing values
missing_val = data.frame(apply(credit,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(credit)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
write.csv(missing_val, "Miising_perc.csv", row.names = F)
credit$MINIMUM_PAYMENTS
sum(is.na(credit$MINIMUM_PAYMENTS))

#lets take a known value & convert it to na, then we'll impute the same using diff. techniques
#say, we take credit[4,13]
#actual value= 7500
#mean= 4494.114
#median= 3000
#knn= 4525.811
#so, knn is closest to the missing value, hence selected for this dataset

#first, I'll convert the known value to NA
#credit[4,13] = NA

#Mean Method
#credit$CREDIT_LIMIT[is.na(credit$CREDIT_LIMIT)] = mean(credit$CREDIT_LIMIT, na.rm = T)

#Median method
#credit$CREDIT_LIMIT[is.na(credit$CREDIT_LIMIT)] = median(credit$CREDIT_LIMIT, na.rm = T)

#KNN Imputation

library(DMwR)
credit = knnImputation(credit, k = 3)
sum(is.na(credit))


##########################################################Deriving new KPIs################################################################

credit$Monthly_Avg_PURCHASES = credit$PURCHASES/credit$TENURE
credit$Monthly_CASH_ADVANCE = credit$CASH_ADVANCE/credit$TENURE
credit$LIMIT_USAGE = credit$BALANCE/credit$CREDIT_LIMIT
credit$PAYMENTS_MIN_PAYMENTS_RATIO = credit$PAYMENTS/credit$MINIMUM_PAYMENTS


#purchase type
credit[,4:5]
credit$purchase_type = with(credit,1:length(credit$BALANCE))
for (i in 1:nrow(credit)) {
  #print(i)
  if(credit$ONEOFF_PURCHASES[i]==0 & credit$INSTALLMENTS_PURCHASES[i]>0){
    credit$purchase_type[i] = "Installment"
  }
  if(credit$ONEOFF_PURCHASES[i]>0 & credit$INSTALLMENTS_PURCHASES[i]==0){
    credit$purchase_type[i] = "Oneoff"
  }
  if(credit$ONEOFF_PURCHASES[i]==0 & credit$INSTALLMENTS_PURCHASES[i]==0){
    credit$purchase_type[i] = "None"
  }
  if(credit$ONEOFF_PURCHASES[i]>0 & credit$INSTALLMENTS_PURCHASES[i]>0){
    credit$purchase_type[i] = "Both"
  }
}


#########################################################Insights from Derived KPIs################################################################

#here, I'll take the mean of each importat variables for different purchase type which I'll use in further analysis
cr_selected = aggregate(credit[, 18:21], list(credit$purchase_type), mean)
colnames(cr_selected) = c("purchase_type","Monthly_Avg_PURCHASES","Monthly_CASH_ADVANCE","LIMIT_USAGE","PAYMENTS_MIN_PAYMENTS_RATIO")

library(ggplot2)
ggplot(cr_selected, aes(fill=purchase_type, x=PAYMENTS_MIN_PAYMENTS_RATIO, y = purchase_type)) + 
  geom_bar(position="dodge", stat="identity")
#insight1: customers with installment purchases are paying more dues compared to other groups

ggplot(cr_selected, aes(fill=purchase_type, x=Monthly_CASH_ADVANCE, y = purchase_type)) + 
  geom_bar(position="dodge", stat="identity")
#Insight 2: Customers who do neither one-off nor installment purchases, take more cash on advance

ggplot(cr_selected, aes(fill=purchase_type, x=LIMIT_USAGE, y = purchase_type)) + 
  geom_bar(position="dodge", stat="identity")
#Insight 3: Customers who don't do installment purchases, spend more on their credit card

ggplot(cr_selected, aes(fill=purchase_type, x=Monthly_Avg_PURCHASES, y = purchase_type)) + 
  geom_bar(position="dodge", stat="identity")
#insight4: customers with installment purchases are doing less purchase compared to other groups




########################################################Outlier Treatment##################################################################

summary(credit$BALANCE)

#Detection of outliers using boxplot
boxplot(credit$BALANCE, xlab="BALANCE")
boxplot(credit$BALANCE_FREQUENCY,xlab = "BALANCE_FREQUENCY")
boxplot(credit$PURCHASES,xlab ="PURCHASES")
boxplot(credit$ONEOFF_PURCHASES,xlab ="ONEOFF_PURCHASES")
boxplot(credit$INSTALLMENTS_PURCHASES,xlab ="INSTALLMENTS_PURCHASES")
boxplot(credit$CASH_ADVANCE,xlab ="CASH_ADVANCE")
boxplot(credit$PURCHASES_FREQUENCY,xlab ="PURCHASES_FREQUENCY")
boxplot(credit$ONEOFF_PURCHASES_FREQUENCY,xlab ="ONEOFF_PURCHASES_FREQUENCY")
boxplot(credit$PURCHASES_INSTALLMENTS_FREQUENCY,xlab ="PURCHASES_INSTALLMENTS_FREQUENCY")
boxplot(credit$CASH_ADVANCE_FREQUENCY,xlab ="CASH_ADVANCE_FREQUENCY")
boxplot(credit$CASH_ADVANCE_TRX,xlab ="CASH_ADVANCE_TRX")
boxplot(credit$PURCHASES_TRX,xlab ="PURCHASES_TRX")
boxplot(credit$CREDIT_LIMIT,xlab ="CREDIT_LIMIT")
boxplot(credit$PAYMENTS,xlab ="PAYMENTS")
boxplot(credit$MINIMUM_PAYMENTS,xlab ="MINIMUM_PAYMENTS")
boxplot(credit$PRC_FULL_PAYMENT,xlab ="PRC_FULL_PAYMENT")
boxplot(credit$TENURE,xlab ="TENURE")
boxplot(credit$Monthly_Avg_PURCHASES,xlab ="Monthly_Avg_PURCHASES")
boxplot(credit$Monthly_CASH_ADVANCE,xlab ="Monthly_CASH_ADVANCE")
boxplot(credit$LIMIT_USAGE,xlab ="LIMIT_USAGE")
boxplot(credit$PAYMENTS_MIN_PAYMENTS_RATIO,xlab ="PAYMENTS_MIN_PAYMENTS_RATIO")

#It's obvious, that these outliers were not made by human errors & they create a significant association, and in turn, will contribute to the end result
#That's why, I'm not dropping or imputing them with central tendency, I'll use square- root transform to get rid of the extreme values of the dataset

#here, I'm subsetting the KPI variables, before doing the transform, which will be required later to reveal the behavioural segments of credit card holders
cre_original = credit[,18:22]


#square-root transform
credit[,1:21] = credit[,1:21]^(1/2)

#Here, I'll exclude those variables from cr_log, which have been used to derive KPIs, apart from ONEOFF_PURCHASES & INSTALLMENTS_PURCHASES, as these 2 variables don't have a linear relation with their derived KPI, purchase_type
cr_pre = credit[c('BALANCE_FREQUENCY','ONEOFF_PURCHASES', 'INSTALLMENTS_PURCHASES',
                  'PURCHASES_FREQUENCY', 'ONEOFF_PURCHASES_FREQUENCY',
                  'PURCHASES_INSTALLMENTS_FREQUENCY', 'CASH_ADVANCE_FREQUENCY',
                  'CASH_ADVANCE_TRX', 'PURCHASES_TRX', 'PRC_FULL_PAYMENT',
                  'Monthly_Avg_PURCHASES', 'Monthly_CASH_ADVANCE', 'LIMIT_USAGE',
                  'PAYMENTS_MIN_PAYMENTS_RATIO','purchase_type')]


########################################Data Preparation for Machine Learning Algorithm#################################################

#Converting categorical variables
str(cr_pre)
#from character to factor
class(cr_pre$purchase_type)
cr_pre$purchase_type = as.factor(cr_pre$purchase_type)

#As I have categorical variable, I'll need to convert them into numeric variables using dummies
library(dummies)
credit.new1 = dummy.data.frame(cr_pre, names = c("purchase_type") , sep = ".")
str(credit.new1)

credit.new1$purchase_type.Both = as.numeric(credit.new1$purchase_type.Both)
credit.new1$purchase_type.Installment = as.numeric(credit.new1$purchase_type.Installment)
credit.new1$purchase_type.None = as.numeric(credit.new1$purchase_type.None)
credit.new1$purchase_type.Oneoff = as.numeric(credit.new1$purchase_type.Oneoff)

head(credit.new1)



#here, I'll convert the categorical variable in cre_original for future use
class(cre_original$purchase_type)
cre_original$purchase_type = as.factor(cre_original$purchase_type)

library(dummies)
cre_original = dummy.data.frame(cre_original, names = c("purchase_type") , sep = ".")
str(cre_original)

cre_original$purchase_type.Both = as.numeric(cre_original$purchase_type.Both)
cre_original$purchase_type.Installment = as.numeric(cre_original$purchase_type.Installment)
cre_original$purchase_type.None = as.numeric(cre_original$purchase_type.None)
cre_original$purchase_type.Oneoff = as.numeric(cre_original$purchase_type.Oneoff)


########################################################Correlation Analysis################################################################

#let's check for multicollinearity between vectors
credit_cor = cor(credit.new1)
write.csv(credit_cor, "Correlation_matrix.csv")
col = colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = credit_cor, col = col, symm = TRUE)
#so, there's multicollinearity in the data, I'll apply PCA to reduce dimensionality


########################################################Dimensionality Reduction using PCA################################################################


# standardizing the data
cr_scaled = scale(credit.new1)
str(cr_scaled)
write.csv(cr_scaled, "standardized data.csv")

#applying PCA
library(factoextra)
cred.pca = prcomp(cr_scaled, center = TRUE, scale. = FALSE)
fviz_eig(cred.pca)
#So, I can see that 5 variables explain more than 85% of the variance(approximately), I select 5 components
cred_res = cred.pca$rotation
cred_res1 = as.data.frame(cred_res)
cred_res_final = cred_res1[,1:5]# these data gives us eigen vectors for each component

########################################################Clustering################################################################
#determine no of clusters
wss = (nrow(cr_scaled)-1)*sum(apply(cr_scaled,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(cr_scaled,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
#from the plot, I select no. of cluster=5

#building clusters using k-means clustering 
clusters = kmeans(cred_res_final, centers = 5)

#Merging generated clusters with existing dataframe with KPI variables
credit_new2 = cbind(cre_original,km_clust_5=clusters$cluster)
str(credit_new2)
credit_new2 = as.data.frame(credit_new2, stringsAsFactors=FALSE)


########################################Behaviroul Analysis & Segmentation of credit card holders##############################################

#here, I'm taking the mean of all the variables in credit_new2 except the cluster variable, for all the different clusters
cr_clust = aggregate(credit_new2[, 1:8], list(credit_new2$km_clust_5), mean)
colnames(cr_clust) = c("Cluster","Monthly_Avg_PURCHASES","Monthly_CASH_ADVANCE","LIMIT_USAGE","PAYMENTS_MIN_PAYMENTS_RATIO","purchase_type.Both","purchase_type.Installment","purchase_type.None","purchase_type.Oneoff")

library(ggplot2)
ggplot(cr_clust, aes(fill=Cluster, x=Cluster, y = Monthly_CASH_ADVANCE)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(cr_clust, aes(fill=Cluster, x=Cluster, y = LIMIT_USAGE)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(cr_clust, aes(fill=Cluster, x=Cluster, y = Monthly_Avg_PURCHASES)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(cr_clust, aes(fill=Cluster, x=Cluster, y = PAYMENTS_MIN_PAYMENTS_RATIO)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(cr_clust, aes(fill=Cluster, x=Cluster, y = purchase_type.Installment)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(cr_clust, aes(fill=Cluster, x=Cluster, y = purchase_type.Oneoff)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(cr_clust, aes(fill=Cluster, x=Cluster, y = purchase_type.Both)) + 
  geom_bar(position="dodge", stat="identity")

ggplot(cr_clust, aes(fill=Cluster, x=Cluster, y = purchase_type.None)) + 
  geom_bar(position="dodge", stat="identity")



###############################################behavioural segments of credit card holders##############################################################

#CUSTOMERS FROM CLUSTER 1: They're taking medium advance cash from their card & doing only one-off purchases. Their average purchase using this credit card is less, they're spending medium amount & are not paying their dues in time. 
#CUSTOMERS FROM CLUSTER 2: They're taking less amount of advance cash from their card & doing both types of purchases. Their average purchase is highest among groups. They're spending highest amount & they're making high minimum payments. 
#CUSTOMERS FROM CLUSTER 3: They're taking medium advance cash from their card & doing both type of purchases. Their average purchase using this credit card is medium. They're spending medium amount on their card & are not paying their dues in time.  
#CUSTOMERS FROM CLUSTER 4: They're taking highest amount of advance cash from their card & not doing any purchases. They're spending least amount on their card & are making medium minimum payments compared to others. 
#CUSTOMERS FROM CLUSTER 5: They're taking least amount of advance cash from their card & doing only instalment purchases. They're doing less amount of purchase. They're spending high amount on their card & are not paying their dues in time. 