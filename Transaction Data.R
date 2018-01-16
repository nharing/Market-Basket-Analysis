install.packages('arules',dependencies=TRUE)
install.packages('arulesViz',dependencies=TRUE)
install.packages('Rgraphviz',dependencies=TRUE)
install.packages('TSP',dependencies=TRUE)
install.packages('gdata',dependencies=TRUE)
install.packages('ggplot2')
install.packages('whisker')
install.packages('mclust')
install.packages('robustbase')
install.packages('dplyr',dependencies=TRUE)# ?? not working
library(Rgraphviz)
library(TSP)
library(gdata)
library(ggplot2)
library(whisker)
library(mclust)
library(robustbase)
library(dplyr)

library(arules)
library(arulesViz)

#data()
data()
data(Groceries) # real life Groceries data

Groceries

class(Groceries) # transactional data. all trans data store data in matrix format in Object$data NOTE

summary(Groceries)

Groceries@data  #this is how data is stored in pipeline format
                #sparse format in which data is stored in real time
                # because in real world, storing those thousands of data daily, where there are mostly 0s and 1s,
                # it is waste of memory to store those 0s and 1s like that.
                # it has to be sored efficiently
                #Groceries@data is stored in ngCMatrix format


#ngCMatrix : created 169 rows and 9835 columns. More efficient way of storing data
#ngRMatric : as we used to in normal sheets, where transaction no. in rows and products in columns


Groceries@itemInfo      # shows all products and their categories and such
                        # doesnot give any information about transaction made
                       # labels is what customer sees.
                      # level 1 is higher level categorization
                      # level 2 is further-categorization
                      # Groceries is a transaction class type data base and not a data frame
                      # Transaction type data store data in the @data attribute, which is ngCMatrix format
                        # Information about items in stored in the @itemInfo , which is a data frame


Groceries@itemInfo[80,]

Groceries@data[1,] #shows 1st colum or 1st product purchased NOTE -----
Groceries@data[,1]  #shows first transaction

class(Groceries@itemInfo)
class(Groceries@data)


Groceries@data[1,]  # which transactions have bought the 1st product
Groceries@itemInfo[1,] # 1st product

#better way of all this is below
#show to get an idea with 1 command in a transaction ,which are the items that have been sold for that transaction
test<-Groceries@itemInfo;test  # data frame , nice excel type of representation



#look at groceries@iteminfo labels
Groceries@itemInfo[,"labels"] # note 169 rows, this line shows labels of 169 products
Groceries@data[,1]           # 169 TRUEs and FALSEs note. This is the 1st transaction. which ever product he has brought, it will be TRUE there and FALSE elsewhere

Groceries@itemInfo[Groceries@data[,1],"labels"] # where there are TRUEs, 
                                                #only those product's labels have been picked up, 
                                                 #names of products first person( 1st trans) has bought




#i dont want space in between the output of prev command. what to do?show with comma
paste(Groceries@itemInfo[Groceries@data[,1],"labels"],collapse=",")


#show first 20 transactions and items separated by comma
for(i in 1:20){
  print(paste(Groceries@itemInfo[Groceries@data[,i],"labels"],collapse=","))
}
#better not to use for loop. so use below apply


apply(Groceries@data[,1:10],2,function(r){ # 2 because i am interested in cols, 1st trans 2nd trans and so on..
  paste(Groceries@itemInfo[r,"labels"],collapse=",")
 })



# APRIORI ~~~~

itemsets<-apriori(Groceries,parameter=list(support=0.02,target="frequent itemsets"))
                 #target ="freq..." is same as  in excel supprt for veg,baby. look at diff item sets combination.
                 # note: this does not give any rule as of now.
                 # just combination of different products for generating X. ( if it was rule, it had give X -> Y)
                # you can give rules instead of freq itemsets: it will give you rules

summary(itemsets)

inspect(head(sort(itemsets,by="support"),10))
inspect(tail(sort(itemsets,by="support"),10))



#show only 1 item length 
#itemsets<-apriori(Groceries,parameters=list(minlen=1,maxlen=1,support=0.02,target="frequent itemsets"))


#rules
groceriesRules<-apriori(Groceries,parameter=list(minlen=1,maxlen=4,support=0.002,confidence=0.6,target="rules"))#note: lhs+rhs will be 4 max.
summary(groceriesRules)
inspect(head(sort(groceriesRules,by="lift"),10))



#Q) what are customers likely to buy before buying whole milk?

groceriesRules<-apriori(Groceries,parameter=list(minlen=1,maxlen=4,support=0.002,confidence=0.6,target="rules")
                        ,appearance=list(default="lhs",rhs="whole milk"))
summary(groceriesRules)
inspect(head(sort(groceriesRules,by="lift"),10))

#Q) what are customers likely to buy if they purchase whole milk? do after class
groceriesRules1<-apriori(Groceries,parameter=list(minlen=1,maxlen=4,support=0.002,confidence=0.6,target="rules"),appearance=list(default="rhs",lhs="whole milk"))
summary(groceriesRules1)
inspect(head(sort(groceriesRules1,by="lift"),10))  ## ????? cross check with others


# now show graphically the observations we got
myRules<-sort(groceriesRules,decreasing=TRUE,by="confidence");class(myRules)
inspect(myRules[1:5])
plot(myRules) 
plot(myRules@quality)  # NOTE this graph


highLiftRules <- head(sort(myRules,by="lift"),5)
plot(highLiftRules,method="graph",control=list(type="items")) # note: method : graph



#We will use the Transaction.xls file, create a transaction object and check the rules

transData <- read.csv("../R/transaction_data.csv")
class(transData)

#convert this to transaction object

myTransactions <- as(transData,"transactions")# convert to transaction object

class(myTransactions)  # same object type as that of Groceries   
                       #NOTE APRIORI needs transaction object. else it cannot work .
                       # transactions is not a native data type, hence we could nto as.transactions()
                       # Transactions object is a higher form of data type
                       # can any data frame be converted to transaction data frame? no. only if the data is transaction type in that data frame.


transItemsSets<- apriori(myTransactions,parameter=list(minlen=1,maxlen=1,support=0.02,target="frequent itemsets"))

inspect(head(sort(transItemsSets,by="support"),10)) 
   # now compare this result with the Transaction_Data.xls ( 9th DAY)



#rule with max lift
myTransRules<-apriori(myTransactions,parameter=list(minlen=1,maxlen=3,support=0.002,confidence=0.01,target="rules"))
inspect(head(sort(myTransRules,by="lift"),10))
    #in the output, ignore rule 1 and 2, as they are meaningless rules .
    #because something something leads to Thursday!!?? doesnot make sense
   # so the 1st meaningful rule is rule no. 3:-> {thursday,baby}->{dvds}
   # we had got the same result in the excel as well R61:T61
  #note in output of this, ignore 1st rule. it is meaningless 
  # next is Thursday,dvd-> baby. see we had got the same in the excel in R61:T61 field
  
  
myTransactions@data  
myTransactions@itemInfo


