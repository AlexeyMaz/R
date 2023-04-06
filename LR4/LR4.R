# Румыния - Гребля академическая
setwd("C:/Users/AlmaZ/RStudioProjects/R/LR4")
library(readxl)

df_m <- read_excel("LR4_greb_m.xlsx")
df_f <- read_excel("LR4_greb_f.xlsx")

# количество призовых мест
all_pl_m <- sapply(df_m[df_m$Year >= 1992, -1], sum)
all_pl_f <- sapply(df_f[df_m$Year >= 1993, -1], sum)

par(mfrow=c(1,2))
barplot(all_pl_m, names=c(1:8), col="blue", xlab="Место", 
        ylab="Количество мест (1-8)", ylim=c(0,10), main="Мужчины (за последние 30 лет)")
barplot(all_pl_f, names=c(1:8), col="hotpink", xlab="Место",
        ylab="Количество мест (1-8)", ylim = c(0, 10), main="Женщины (за последние 30 лет)")

# первые места
first_pl_m <- df_m[df_m$Gold > 0 & df_m$Year >= 1992, c(1:2)]
first_pl_f <- df_f[df_f$Gold > 0 & df_f$Year >= 1993, c(1:2)]

pie(first_pl_m$Gold, labels=first_pl_m$Gold, 
    col=topo.colors(length(first_pl_m$Year)), 
    main = "Количество золотых медалей (мужчины)\nза все время")
legend(-1.3, 1.1, first_pl_m$Year, cex=0.7, 
       fill = topo.colors(length(first_pl_m$Year)))

pie(first_pl_f$Gold, labels=first_pl_f$Gold, 
    col=topo.colors(length(first_pl_f$Year)), 
    main = "Количество золотых медалей (женщины)\nза все время")
legend(-1.6, 1.5, first_pl_f$Year, cex=0.7, 
       fill = topo.colors(length(first_pl_f$Year)))

# призовые места
prize_pl_m <- data.frame(Год=df_m[df_m$Year >= 1992, 1], 
                         Призовые=rowSums(df_m[df_m$Year >= 1992, 2:4]))

prize_pl_f <- data.frame(Год=df_f[df_f$Year >= 1992, 1], 
                         Призовые=rowSums(df_f[df_f$Year >= 1992, 2:4]))

par(mfrow=c(1,1))
plot(prize_pl_m, type="b", pch=21, col="blue", xaxt="n", ylim=c(0,7), xlab="Год",
     main="Призовые места Румынии по академической гребле за последние 30 лет")
lines(prize_pl_f, type="o", pch=21, col="hotpink")
legend(min(prize_pl_m$Year), 7, c("Мужчины", "Женщины"), fill=c("blue", "hotpink"))
axis(side=1, at=prize_pl_m$Year)



last6_gold <- read_excel("LR4_gold.xlsx")


pl_col <- c('red', 'green', 'gold', '#BA43B4', 'gray', 'black', 'blue')
plot(last6_gold$Год, last6_gold$Россия, type="b", 
     pch=18, col=pl_col[1], xaxt="n", ylim=c(0, 40), 
     xlab="Год проведения олимпиады", ylab="Количество золотых медалей", 
     main="Динамика количества победителей за 6 последних олимпиад")
lines(last6_gold$Год, last6_gold$Китай, type="o", pch=18, col=pl_col[2])
lines(last6_gold$Год, last6_gold$Италия, type="o", pch=18, col=pl_col[3])
lines(last6_gold$Год, last6_gold$Япония, type="o", pch=18, col=pl_col[4])
lines(last6_gold$Год, last6_gold$Нидерланды, type="o", pch=18, col=pl_col[5])
lines(last6_gold$Год, last6_gold$Испания, type="o", pch=18, col=pl_col[6])
lines(last6_gold$Год, last6_gold$Беларусь, type="o", pch=18, col=pl_col[7])
axis(side=1, at=last6_gold$Год)
legend(max(last6_gold$Год) - 1.5, 41.5, cex = 0.55,
       colnames(last6_gold)[-1], fill=pl_col)


last6_prize <- read_excel("LR4_prize.xlsx")

plot(last6_prize$Год, last6_prize$Россия, type="b", 
     pch=18, col=pl_col[1], xaxt="n", ylim=c(0, 100), 
     xlab="Год проведения олимпиады", ylab="Количество призовых мест", 
     main="Динамика количества призовых мест за 6 последних олимпиад")
lines(last6_prize$Год, last6_prize$Китай, type="o", pch=18, col=pl_col[2])
lines(last6_prize$Год, last6_prize$Италия, type="o", pch=18, col=pl_col[3])
lines(last6_prize$Год, last6_prize$Япония, type="o", pch=18, col=pl_col[4])
lines(last6_prize$Год, last6_prize$Нидерланды, type="o", pch=18, col=pl_col[5])
lines(last6_prize$Год, last6_prize$Испания, type="o", pch=18, col=pl_col[6])
lines(last6_prize$Год, last6_prize$Беларусь, type="o", pch=18, col=pl_col[7])
axis(side=1, at=last6_prize$Год)
legend(max(last6_prize$Год) - 1.5, 104, cex = 0.55,
       colnames(last6_prize)[-1], fill=pl_col)


last6_m <- tail(prize_pl_m, 6)
last6_f <- tail(prize_pl_f, 6)

par(mfrow=c(1,3))
plot(last6_m, type="b", pch=18, col="blue", xaxt="n", ylim=c(0,3), 
     xlab="Год проведения олимпиады", ylab="Количество призовых мест",
     main="Динамика изменения кол-ва призовых мест Румынии\nпо академической гребле за последние 6 олимпиад")
lines(last6_f, type="o", pch=18, col="hotpink")
legend(max(last6_f$Year) - 10.4, 3.12, cex=0.7 ,c("Мужчины", "Женщины"), fill=c("blue", "hotpink"))
axis(side=1, at=last6_m$Year)


last6_mf = data.frame(Призовые_М=last6_m$Призовые, Призовые_Ж=last6_f$Призовые)
barplot(t(as.matrix(last6_mf)), beside=T, 
        xlab="Год проведения олимпиады", 
        ylab="Количество призовых мест", names=last6_f$Year, col=c("blue", "hotpink"), 
        main="Динамика изменения кол-ва призовых мест Румынии\nпо академической гребле за последние 6 олимпиад")

last6_mf_sum <- sapply(last6_mf, sum)
pie(last6_mf_sum, labels=c(last6_mf_sum["Призовые_М"], last6_mf_sum["Призовые_Ж"]), 
    col=c("blue", "hotpink"), radius = 1.5,
    main="Суммарное кол-во призовых мест у М и Ж из Румынии\nпо академической гребле за последние 6 олимпиад")
legend(-1.6, 2.1, c('Мужчины', 'Женщины'), cex=0.7, 
       fill = c("blue", "hotpink"))