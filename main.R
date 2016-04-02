setDict("C:/Program Files (x86)/WordNet/2.1/dict")
library(twitteR)
library(NLP)
library(tm)
library(httr)
library(quanteda)
library(wordnet)
library(plotrix)
library(SnowballC) 
library(plotly)
setDict("C:/Program Files (x86)/WordNet/2.1/dict")

cKey=""
cSecret=""
accessToken=""
accessSecret=""
setup_twitter_oauth(cKey, cSecret, accessToken, accessSecret)

userObj=getUser("@BeingSalmanKhan")
userT=userTimeline(userObj, n=500, includeRts=TRUE)
userT.DF=twListToDF(userT)

cor1=Corpus(VectorSource(userT.DF$text))

#do better cleaning

cor1 = tm_map(cor1, stripWhitespace)
cor1 = tm_map(cor1, removePunctuation) 
cor1 = tm_map(cor1, tolower) 
cor1 = tm_map(cor1, PlainTextDocument)
cor1 = tm_map(cor1, removeWords, stopwords("english")) 
cor1 = tm_map(cor1, removeNumbers)
cor1 = tm_map(cor1, stemDocument, language="english")


data=read.csv("D:/R/selfStudy/tdata1.csv", header=T)

y1=as.character(data$business)
y2=as.character(data$entertainment)
y3=as.character(data$travel)
y4=as.character(data$sports)
y5=as.character(data$health)
y6=as.character(data$technology)
y7=as.character(data$politics)
y8=as.character(data$animals)
y9=as.character(data$shopping)


#try to get synonyms of synonyms
#try to get synonyms of 2-3 words at the same time
x1=synonyms("business","NOUN")
x2=synonyms("entertainment","NOUN")
x3=synonyms("travel","NOUN")
x4=synonyms("sports","NOUN")
x5=synonyms("health","NOUN")
x6=synonyms("technology","NOUN")
x7=synonyms("politics","NOUN")
x8=synonyms("animals","NOUN")
x9=synonyms("shopping","NOUN")


z1=c(x1,y1)
z2=c(x2,y2)
z3=c(x3,y3)
z4=c(x4,y4)
z5=c(x5,y5)
z6=c(x6,y6)
z7=c(x7,y7)
z8=c(x8,y8)
z9=c(x9,y9)

z0=c("...")
z1=unique(z1)
z2=unique(z2)
z3=unique(z3)
z4=unique(z4)
z5=unique(z5)
z6=unique(z6)
z7=unique(z7)
z8=unique(z8)
z9=unique(z9)

#func=function(x){if(max(x)==0) return(0) else return(max(x))}

tweets=c()
dimension=dim.data.frame(cor1)
for(i in 1:dimension[2]){tweets[i]=cor1[[i]]}


tweets=as.character(tweets)

myDict <- dictionary(list(na=z0, business=z1, entertainment = z2, travel=z3, sports=z4, health=z5, technology=z6, politics=z7, animals=z8, shopping=z9))
myDfm <- dfm(tweets, ignoredFeatures = stopwords("english"), verbose = FALSE)

res=applyDictionary(myDfm, myDict, valuetype = "glob")
resDF=as.data.frame(res)
resList=colnames(resDF)[apply(resDF,1,which.max)]
resList


#step2 ADD hell lot of words to the tdata1.csv and remove non suitable words
#step4 piechart, graph


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



#pie(slice,labels=label,explode=0.5,col=rainbow(length(label)),main="Pie Chart: Different Area of Interest")
