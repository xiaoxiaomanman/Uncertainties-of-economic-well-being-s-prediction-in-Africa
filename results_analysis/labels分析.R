library(ggplot2)
library(RColorBrewer)

# -----------------------按照各个国家做的样本wealthpooled的箱线图---------------------

DHS <- read.csv("E:/africa_poverty_clean-main/data/dhs_clusters.csv", header = TRUE)
colourCount = length(unique(DHS$country))
getPalette = colorRampPalette(brewer.pal(12, "Set3"))
plot <- ggplot(DHS,aes(x = factor(country),y = wealthpooled, fill = factor(country)))+
  geom_boxplot() +
  scale_fill_manual(values = getPalette(colourCount)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("各个国家的财富指数")
print(plot)        


#---------------------------- 看各个国家财富指数的分位数------------------------------
# 1、按照各个国家做的样本wealthpooled分位数的热图
library(dplyr)
group_names <- c('angola', 'benin', 'burkina_faso', 'cameroon', 'cote_d_ivoire',
                 'democratic_republic_of_congo', 'ethiopia', 'ghana', 'guinea', 'kenya',
                 'lesotho', 'malawi', 'mali', 'mozambique', 'nigeria', 'rwanda', 'senegal',
                 'sierra_leone', 'tanzania', 'togo', 'uganda', 'zambia', 'zimbabwe')

DHS <- read.csv("E:/africa_poverty_clean-main/data/dhs_clusters.csv", header = TRUE)
data <- mutate(DHS,quantile100 = findInterval(wealthpooled,quantile(wealthpooled,probs = seq(0,1,0.01)),rightmost.closed = TRUE))
data <- data %>%
  group_by(country) %>%
  summarise(quantile100)

library(reshape2)
table_l <- as.data.frame(table(data))

for (i in group_names){
  table_l$Freq[which(table_l$country==i)] <- table_l$Freq[which(table_l$country==i)]*length(DHS$country)/sum(DHS$country==i)
}

table_w <- dcast(data=table_l,
                   country~quantile100 #左侧是要保留的字段，右侧是要分割的分类变量
                   )
rownames(table_w)<-table_w[,1]
table_w<-table_w[,-1]

library(pheatmap)
pheatmap(table_w,cluster_col = FALSE,scale = "none",border_color="gray")
pheatmap(table_w,cluster_col = FALSE,scale = "row",border_color="gray")
pheatmap(table_w,cluster_col = FALSE,scale = "row",border_color="gray",display_numbers = TRUE)
pheatmap(table_w,cluster_col = FALSE,scale = "column",border_color="gray",display_numbers = TRUE)
pheatmap(table_w,cluster_col = FALSE,border_color="gray",display_numbers = TRUE)



# 2、按照各个国家做的样本wealthpooled分位数的核密度图
library(ggplot2)
for (i in group_names){
  df <- data[which(data$country== i),]
  plot <- ggplot(df,aes(quantile100,fill=factor(country)))+geom_density(alpha=0.2,size=0.25,bw=20)
  print(plot)
  ggsave(plot,filename = paste0(i,".png"),width = 12,height = 9)
}



# 3、各个国家wealthpooled分位数的平均值
library(dplyr)
group_names <- c('angola', 'benin', 'burkina_faso', 'cameroon', 'cote_d_ivoire',
                 'democratic_republic_of_congo', 'ethiopia', 'ghana', 'guinea', 'kenya',
                 'lesotho', 'malawi', 'mali', 'mozambique', 'nigeria', 'rwanda', 'senegal',
                 'sierra_leone', 'tanzania', 'togo', 'uganda', 'zambia', 'zimbabwe')

DHS <- read.csv("E:/africa_poverty_clean-main/data/dhs_clusters.csv", header = TRUE)
data1 <- mutate(DHS,quantile100 = findInterval(wealthpooled,quantile(wealthpooled,probs = seq(0,1,0.01)),rightmost.closed = TRUE))
data1 <- data1 %>%
  group_by(country) %>%
  summarise(mean(quantile100))


# 4、各个国家wealthpooled平均值的分位数
DHS <- read.csv("E:/africa_poverty_clean-main/data/dhs_clusters.csv", header = TRUE)
data2 <- DHS[,c(1,7)] %>% group_by(country) %>% summarise(mean(wealthpooled))
data2 <- as.data.frame(data2[,2])
rownames(data2)<-group_names
colnames(data2)<-'wealthpooled'
data2 <- mutate(data2,quantile100 = findInterval(wealthpooled,quantile(wealthpooled,probs = seq(0,1,0.01)),rightmost.closed = TRUE))



#----------------------------RF不如Ridge国家的岭回归残差------------------------------

kenya <- which(RES$country=='kenya'| RES$country=='malawi'|RES$country=='mozambique'|RES$country=='tanzania'|RES$country=='uganda')

kenya <- which(RES$country=='kenya')
kenya <- RES[kenya,]
plot <- ggplot(kenya,aes(x=res,fill = factor(country),color=factor(country)))+
  geom_density(alpha=0.2,bw=1,colour="black",size=0.25)+
  theme(
    text=element_text(size = 15,color = "black")
  )+
  xlim(-4,4)
print(plot) 

malawi <- which(RES$country=='malawi')
malawi <- RES[malawi,]
plot <- ggplot(malawi,aes(x=res,fill = factor(country),color=factor(country)))+
  geom_density(alpha=0.2,bw=1,colour="black",size=0.25)+
  theme(
    text=element_text(size = 15,color = "black")
  )+
  xlim(-4,4)
print(plot) 

mozambique <- which(RES$country=='mozambique')
mozambique <- RES[mozambique,]
plot <- ggplot(mozambique,aes(x=res,fill = factor(country),color=factor(country)))+
  geom_density(alpha=0.2,bw=1,colour="black",size=0.25)+
  theme(
    text=element_text(size = 15,color = "black")
  )+
  xlim(-4,4)
print(plot) 


tanzania <- which(RES$country=='tanzania')
tanzania <- RES[tanzania,]
plot <- ggplot(tanzania,aes(x=res,fill = factor(country),color=factor(country)))+
  geom_density(alpha=0.2,bw=1,colour="black",size=0.25)+
  theme(
    text=element_text(size = 15,color = "black")
  )+
  xlim(-4,4)
print(plot) 


uganda <- which(RES$country=='uganda')
uganda <- RES[uganda,]
plot <- ggplot(uganda,aes(x=res,fill = factor(country),color=factor(country)))+
  geom_density(alpha=0.2,bw=1,colour="black",size=0.25)+
  theme(
    text=element_text(size = 15,color = "black")
  )+
  xlim(-4,4)
print(plot) 
