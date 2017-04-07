library(twitteR)
library(plotly)
library(httr)
library(NLP)
library(tm)
library(quanteda)

cKey=""
cSecret=""
accessToken=""
accessSecret=""
setup_twitter_oauth(cKey, cSecret, accessToken, accessSecret)

userObj=getUser("@narendramodi")
userT=userTimeline(userObj, n=500, includeRts=TRUE)
userT.DF=twListToDF(userT)

cor1=Corpus(VectorSource(userT.DF$text))
#corpus is nothing but collection of text documents for tm package

cor1 = tm_map(cor1, stripWhitespace)
cor1 = tm_map(cor1, removePunctuation) 
cor1 = tm_map(cor1, tolower) 
cor1 = tm_map(cor1, PlainTextDocument)
cor1 = tm_map(cor1, removeWords, stopwords("english")) 
cor1 = tm_map(cor1, removeNumbers)
cor1 = tm_map(cor1, stemDocument, language="english")


data=read.csv("D:/tech seminar/project/tweet-classification-master/tdata2.csv", header=T)

#tdata2 is tdata1 with weights. Weights are as follows 
# "1" if the word is general
# "3" if word is specific to one or few class label 


'
y1=data$business
y2=data$entertainment
y3=data$travel
y4=data$sports
y5=data$health
y6=data$technology
y7=data$politics
y8=data$animals
y9=data$shopping
'
y0=c('$$$$%%%$$$')
y1=as.character(data$business)
y2=as.character(data$entertainment)
y3=as.character(data$travel)
y4=as.character(data$sports)
y5=as.character(data$health)
y6=as.character(data$technology)
y7=as.character(data$politics)
y8=as.character(data$animals)
y9=as.character(data$shopping)


w1=data$wbusiness
w2=data$wentertainment
w3=data$wtravel
w4=data$wsports
w5=data$whealth
w6=data$wtechnology
w7=data$wpolitics
w8=data$wanimals
w9=data$wshopping



t1 <- setNames(w1, y1) 
t2 <- setNames(w2, y2) 
t3 <- setNames(w3, y3) 
t4 <- setNames(w4, y4) 
t5 <- setNames(w5, y5) 
t6 <- setNames(w6, y6) 
t7 <- setNames(w7, y7) 
t8 <- setNames(w8, y8) 
t9 <- setNames(w9, y9) 

weights=c(t1,t2,t3,t4,t5,t6,t7,t8,t9)
weights = weights[!is.na(weights)]

tweets=c()
list_tweet=cor1$content[[1]]
for(i in 1:length(list_tweet)){tweets[i]=list_tweet[i]}

#dimension=dim.data.frame(cor1[[1]])
#for(i in 1:dimension[2]){tweets[i]=cor1[[i]]}


tweets=as.character(tweets)
#write.csv(tweets,"xyz.csv")

#mydfm <- dfm(tweets, ignoredFeatures = stopwords("english"), verbose = FALSE)
mydfm <- dfm(tweets, verbose = FALSE)
idx <- which(names(weights) %in% colnames(mydfm))
mydfm[, names(weights)[idx]] <-  mydfm[, names(weights)[idx]] %*% diag(weights[idx])
mydfm=as.matrix(mydfm)
mydfm=as.dfm(mydfm)



myDict <- dictionary(list(na=y0, business=y1, entertainment= y2, travel=y3, sports=y4, health=y5, technology=y6, politics=y7, animals=y8, shopping=y9))



res=dfm_lookup(mydfm, myDict,valuetype = "glob")
resDF=as.data.frame(res)
resList=colnames(resDF)[apply(resDF,1,which.max)]
#resList
#Can convert dfm to dtm by using convert()




l0=length(which(resList== "na")) 
l1=length(which(resList == "business")) 
l2=length(which(resList == "entertainment")) 
l3=length(which(resList == "travel")) 
l4=length(which(resList == "sports")) 
l5=length(which(resList == "health")) 
l6=length(which(resList == "technology")) 
l7=length(which(resList == "politics")) 
l8=length(which(resList == "animals")) 
l9=length(which(resList == "shopping")) 


values=c(l1,l2,l3,l4,l5,l6,l7,l8,l9)
#values=c(l0,l1,l2,l3,l4,l5,l6,l7,l8,l9)

labels=c("Business", "Entertainment", "Travel","Sports","Health", "Technology","Politics","Animals","Shopping")
#labels=c("NA","Business", "Entertainment", "Travel","Sports","Health", "Technology","Politics","Animals","Shopping")


ds = data.frame(labels = labels, values = values)
plot_ly(ds, labels = labels, values = values, type = "pie") 
layout(title = userObj$name)




