setwd("C:/Users/AlmaZ/RStudioProjects/R/LR6")
df <- read.csv("Airplane_Crushes.csv", sep=",", header=T,
               fileEncoding="UTF-8", )


oper_vs_fatal <- data.frame(tapply(df$Fatalities, df$Operator, sum))
rownames(oper_vs_fatal)[1] <- 'Unknown'

barplot(names=row.names(oper_vs_fatal), oper_vs_fatal[, 1], xlab="Оператор", 
        ylab="Кол-во смертей", main="Кол-во смертей по операторам")



df$Year <- substring(df$Date, 7)
dif_pars_per_year <- data.frame(tapply(df$Fatalities, df$Year, sum))

dif_pars_per_year$Fatalities <- dif_pars_per_year[, 1]
dif_pars_per_year[, 1] <- rownames(dif_pars_per_year)
names(dif_pars_per_year)[1] <- 'Year'
rownames(dif_pars_per_year) <- c(1:length(dif_pars_per_year[, 1]))

dif_pars_per_year <- cbind(dif_pars_per_year, Crashes=tapply(df$Date, df$Year, length))
dif_pars_per_year <- cbind(dif_pars_per_year, Aboard=tapply(df$Aboard, df$Year, sum))
df$Ground <- as.numeric(df$Ground)
dif_pars_per_year <- cbind(dif_pars_per_year, Survived=tapply(df$Ground, df$Year, sum))

df <- df[-length(df)]
df_clean <- na.omit(dif_pars_per_year)


barplot(names=df_clean$Year, df_clean$Crashes, xlab="Год",
        ylab="Кол-во крушений", main="Кол-во крушений в год", las=2)

barplot(names=df_clean$Year, df_clean$Aboard, xlab="Год", ylim=c(0, 3500),
        ylab="Кол-во людей", main="Кол-во людей на борту в год", las=2, col='goldenrod')

survived_no_zero <- subset(df_clean, df_clean$Survived != 0, drop = TRUE)
par(mfrow=c(1, 2))
barplot(names=survived_no_zero$Year, survived_no_zero$Survived, xlab="Год", ylim=c(0, 300),
        ylab="Кол-во людей", main="Кол-во выживших в год", las=2, col='green')

barplot(names=df_clean$Year, df_clean$Fatalities, xlab="Год", ylim=c(0, 3000),
        ylab="Кол-во людей", main="Кол-во погибших в год", las=2, col='red')

par(mfrow=c(1, 1))

# Оператор с наибольшим кол-вом крушений
tmp <- tapply(df$Fatalities, df$Operator, length)
tmp[which.max(tmp)]


# Тип воздушного суда с наибольшим кол-вом крушений
tmp <- tapply(df$Fatalities, df$Type, length)
tmp[which.max(tmp)]
rm(tmp)



df_clust <- df[, -c(1:9)]
df_clust <- na.omit(df_clust)
df_clust <- df_clust[, -4]
df_deaths <- df_clust

maxs <- apply(df_clust, 2, max)
mins <- apply(df_clust, 2, min)
df_clust <- scale(df_clust, center = mins, scale = maxs - mins)
df_clust <- data.frame(df_clust)


dist.crashes <- dist(df_clust[, 1:3])
clust.crashes <- hclust(dist.crashes, "ward.D")
plot(clust.crashes, labels=F)
rect.hclust(clust.crashes, k = 5, border='red')


clusters <- cutree(clust.crashes, k = 5)
df_deaths <- data.frame(df_deaths, clusters)


# library(cluster)
# library(factoextra)
# 
# # метод локтя
# fviz_nbclust(df_clust[1:3], kmeans, method = 'wss')


# каменная осыпь
plot(1:(nrow(df_clust)-1), clust.crashes$height, type='b', main='Каменная осыпь')



cl1 <- colMeans(df_clust[clusters == 1, ])
cl2 <- colMeans(df_clust[clusters == 2, ])
cl3 <- colMeans(df_clust[clusters == 3, ])
cl4 <- colMeans(df_clust[clusters == 4, ])
cl5 <- colMeans(df_clust[clusters == 5, ])

df_plot <- data.frame(cl1, cl2, cl3, cl4, cl5)
df_plot1 <- t(df_plot)
df_plot <- t(df_plot1)
barplot(df_plot, col=c('gold','red','green'), ylim=c(0, 0.5))
legend('topleft', colnames(df_clust), fill = c('gold','red','green'))


nrow(df_clust[clusters == 1, ])
nrow(df_clust[clusters == 2, ])
nrow(df_clust[clusters == 3, ])
nrow(df_clust[clusters == 4, ])
nrow(df_clust[clusters == 5, ])

sum(df_deaths[clusters == 1, 'Fatalities'])
sum(df_deaths[clusters == 2, 'Fatalities'])
sum(df_deaths[clusters == 3, 'Fatalities'])
sum(df_deaths[clusters == 4, 'Fatalities'])
sum(df_deaths[clusters == 5, 'Fatalities'])


df_tmp <- df_deaths[-which.max(df_deaths$Fatalities), ]
df_tmp <- df_tmp[-which.max(df_tmp$Fatalities), ]
df_tmp <- df_tmp[-which.max(df_tmp$Fatalities), ]
boxplot(Fatalities~clusters, data=df_tmp, xlab='Clusters', 
        ylab = 'Fatalities', frame = F, col = rainbow(5))


library (lattice)
df_tmp <- df_deaths[-which.max(df_deaths$Aboard), ]
df_tmp <- df_tmp[-which.max(df_tmp$Aboard), ]
df_tmp <- df_tmp[-which.max(df_tmp$Aboard), ]
xyplot(Fatalities~Aboard, group = clusters, data = df_tmp, auto.key = TRUE, col = rainbow(5))

cloud(Fatalities ~  Aboard* Ground, group = clusters, data = df_deaths, 
      auto.key = TRUE, col = rainbow(5))
