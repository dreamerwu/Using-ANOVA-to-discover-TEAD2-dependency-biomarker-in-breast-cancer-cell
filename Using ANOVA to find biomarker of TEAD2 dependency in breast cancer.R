data=read.delim("D:/demo/demo.txt",head=T,sep="\t")
y=data$TEAD2
j=2
i=2
output=matrix(,ncol=2,nrow=ncol(data))

while (i < (ncol(data))+1) {

x=data[,i:i]
ANOVA=aov(y~x,data=data)
s=summary(ANOVA)
p=unlist(s)['Pr(>F)1']
pp=as.matrix(p)
output[j:j,1:1]=pp[1:1,1:1]


i=i+1
j=j+1
}
write.csv(output,"D:/demo/result.csv")





#single ANOVA
x=data$ERBB3
y=data$TEAD2
result=aov(y~x,data=data)
summary(result)



