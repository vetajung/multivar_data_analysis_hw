# Assignment1 - 2015170378 Jung Eun young
# Association Rules -------------------------------------------------------
# arules and arulesViz packages install
install.packages("arules")
install.packages("arulesViz")
install.packages("wordcloud")

library(arules)
library(arulesViz)
library(wordcloud)

# Part 1: Transform a data file into transaction format
mooc_dataset <- read.csv("big_student_clear_third_version.csv", header=TRUE)

#[Step1] 
#<Q1>
#Phase1
Institute <- mooc_dataset$institute
Course <-mooc_dataset$course_id
Region <- mooc_dataset$final_cc_cname_DI
Degree <- mooc_dataset$LoE_DI

##Phase2
Region <- factor(gsub(' ','',Region)) #공백처리
Region <- factor(gsub(',','',Region)) #'OtherNorth&CentralAmer.,Caribbean'의 ','가 csv변환시 문제가 되어 제거
Degree <- factor(gsub(' ','',Degree)) #item생성 시 less than Secondary가 less로 잘리는 문제 해결

###Phase3
RawTransactions <- paste(Institute,Course,Region,Degree,sep="_")

####Phase4
MOOC_transactions <- paste(mooc_dataset$userid_DI,RawTransactions,sep =" ")

#####Phase5
write.table(MOOC_transactions,"MOOC_User_Course.csv", sep=",", quote = FALSE, row.names = FALSE, col.names=FALSE)

#[Step2]
#<Q2-1>
tmp_single <- read.transactions("MOOC_User_Course.csv", format = "single",
                                cols = c(1,2), rm.duplicates=TRUE)
inspect(tmp_single[1:10])
summary(tmp_single)

#<Q2-2>
itemName <- itemLabels(tmp_single)
itemCount <- itemFrequency(tmp_single)*nrow(tmp_single)

col <- brewer.pal(7,"Dark2")
wordcloud(words = itemName, freq = itemCount,min.freq = 2000, scale = c(1, 0.2), col = col , random.order = FALSE,family = "Rockwell")

#<Q2-3>
itemFrequencyPlot(tmp_single, support = 0.01, cex.names=0.5, main ="Item frequency plot above support 1%") 

#[Step3]
#<Q3-1>
nMat <- matrix(c(1:16),nrow=4,ncol=4,dimnames=list(c("support=0.001", "        0.0015","        0.002","        0.0025"),c("confidence=0.05", "0.06", "0.07","0.08")))
for (i in 1:4){
  for(j in 1:4){
    nMat[i,j]=length(apriori(tmp_single, parameter=list(support=as.double(0.0005*(i+1)), confidence=as.double(0.01*(j+4)))))
  }
}
as.table(nMat)

#<Q3-2>
rules <- apriori(tmp_single, parameter=list(support=0.001, confidence=0.05))
inspect(sort(rules, by="support")[1])
inspect(sort(rules, by="confidence")[1])
inspect(sort(rules, by="lift")[1])
df <- as(rules,"data.frame")
df[,6]<-df[,2]*df[,3]*df[,4] #support*confidence*lift
df[order(-df[,6])[1:3],]

# Plot the rules
plot(rules, method="graph",cex=0.3)
plot(rules[1:10], method="graph",cex=0.3)
inspect(rules[1:10])

