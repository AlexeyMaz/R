setwd("C:/Users/AlmaZ/RStudioProjects/R/LR8")

# longley
paste0('Количество переменных = ', length(longley))
paste0('Объем выборки = ', nrow(longley))
summary(longley)
cor(longley)

plot(longley$Employed, longley$Unemployed, type='l', xlab = 'Работающее население',
     ylab = 'Неработающее население', 
     main = 'Тенденция отношения работающего населения к неработающему')

plot(longley$Year, longley$Employed, type='l', xlab = 'Год',
     ylab = 'Работающее население', 
     main = 'Тенденция количества работающего населения по годам')

plot(longley$Year, longley$Unemployed, type='l', xlab = 'Год',
     ylab = 'Неработающее население', 
     main = 'Тенденция количества неработающего населения по годам')

plot(longley$Year, longley$Armed.Forces, type='l', xlab = 'Год',
     ylab = 'Численность ВС', 
     main = 'Тенденция численности ВС по годам')

plot(longley$Year, longley$GNP, type='l', xlab = 'Год',
     ylab = 'Объем ВВП', 
     main = 'Объем ВВП по годам')

plot(longley$Unemployed, longley$Armed.Forces, xlab = 'Неработающее население',
     ylab = 'Численность ВС', 
     main = 'Отношение количества неработающего населения к численности ВС')


library(car)
par(mfrow=c(2, 2))
qqPlot(longley$GNP, main = 'Проверка распределения ВВП на нормальность')
qqPlot(longley$Employed, main = 'Проверка распределения работающего
       населения на нормальность')
qqPlot(longley$Unemployed, main = 'Проверка распределения неработающего
       населения на нормальность')
qqPlot(longley$Armed.Forces, main = 'Проверка распределения численности ВС на нормальность')

shapiro.test(longley$GNP)
shapiro.test(longley$Employed)
shapiro.test(longley$Unemployed)
shapiro.test(longley$Armed.Forces)


par(mfrow=c(1, 1))
cor(longley$GNP, log(longley$GNP),method="spearman")
plot(longley$GNP, log(longley$GNP), type='l', xlab = 'ВВП',
     ylab = 'log(ВВП)', 
     main = 'Сравнение данных о ВВП с логарифмом от этих данных')



df <- read.csv("Belarus.csv", sep=",", header=T,
              fileEncoding="UTF-8")
df <- df[, -c(1:3)]; df <- t(df)
colnames(df) <- df[1, ]; df <- df[-1, -c(23:27)]
df[df == '..'] <- NA; df <- as.data.frame(df)
df <- data.frame(lapply(df, as.numeric)); rownames(df) <- 1989:2017


library(car)
scatterplotMatrix(df[, c(1:4)], spread=FALSE, lty.smooth=2,
                  main="Матрица диаграмм рассеяния")

plot(rownames(df), df[, 2], type = 'l', xlab = 'Год', ylab = 'Прирост ВВП (в %)')

plot(df$SP.POP.GROW, df[, 2], xlab = 'Прирост населения', ylab = 'Прирост ВВП',
     main = 'Отношение прироста населения к приросту ВВП')
cor.test(df$SP.POP.GROW, df[, 2])

plot(df$SP.POP.GROW, df$SL.UEM.BASC.ZS, xlab = 'Прирост населения', 
     ylab = 'Безработица среди граждан с базовым образованием', 
     main = 'Отношение прироста населения к динамике безработицы')
# cor.test(df$SP.POP.GROW, df$SL.UEM.BASC.ZS)


par(mfrow=c(1, 2))
plot(df$SP.POP.GROW, df$SP.DYN.LE00.IN, xlab = 'Прирост населения', 
     ylab = 'Продолжительность жизни', 
     main = 'Отношение прироста населения к продолжительности жизни')
cor.test(df$SP.POP.GROW, df$SP.DYN.LE00.IN)

plot(df$SP.POP.GROW, df$SP.DYN.CDRT.IN, xlab = 'Прирост населения', 
     ylab = 'Смертность (на 1000 чел.)', 
     main = 'Отношение прироста населения к смертности')
cor.test(df$SP.POP.GROW, df$SP.DYN.CDRT.IN)


plot(df$SE.TER.CUAT.BA.ZS, df$NE.EXP.GNFS.KD.ZG, xlab = 'Люди с высшим образованием', 
     ylab = 'Экспорт товаров',
     main = 'Отношение количества людей с высшим образованием 
     к объему технологичного производства')
# cor.test(df$SE.TER.CUAT.BA.ZS, df$NE.EXP.GNFS.KD.ZG)

plot(df$SE.TER.CUAT.BA.ZS, df$NV.MNF.TECH.ZS.UN, xlab = 'Люди с высшим образованием', 
     ylab = 'Высокотехнологичное производство', 
     main = 'Отношение количества людей с высшим образованием к общему экспорту')
# cor.test(df$SE.TER.CUAT.BA.ZS, df$NV.MNF.TECH.ZS.UN)


par(mfrow=c(1, 1))
plot(df$SE.XPD.TOTL.GD.ZS, df$SE.TER.CUAT.BA.FE.ZS, xlab = 'Расходы на высшее образование', 
     ylab = 'Кумулятивный прирост бакалавров среди женщин (в %)',
     main = 'Отношение расходов на образование к 
     кумулятивному приросту бакалавров среди женщин')
# cor.test(df$SE.XPD.TOTL.GD.ZS, df$SE.TER.CUAT.BA.FE.ZS)

plot(df$SE.TER.CUAT.BA.ZS, df$IP.JRN.ARTC.SC, xlab = 'Люди с высшим образованием', 
     ylab = 'Статьи в научных и технических журналах', 
     main = 'Отношение количества людей с высшим образованием к количеству статей 
     в научных и технических журналах')
# cor.test(df$SE.TER.CUAT.BA.ZS, df$IP.JRN.ARTC.SC)



library(ellipse)
plotcorr(cor(df))

image(1:ncol(cor(df)), 1:nrow(cor(df)), cor(df), col = rainbow(22), axes = F,
      xlab = '', ylab = '')
axis(1, at = 1:ncol(cor(df)), labels=colnames(cor(df)), las = 2)
axis(2, at = 1:nrow(cor(df)), labels=rownames(cor(df)), las = 1)



fit <- lm(SP.DYN.LE00.IN ~ SP.POP.GROW, df)

plot(df$SP.POP.GROW, df$SP.DYN.LE00.IN, xlab="Прирост населения", 
     ylab="Продолжительность жизни", col="blue", 
     main = 'Соотношение прироста населения к продолжительности жизни')
abline(fit, col="red")
scatterplotMatrix(df[, c(12, 13)], spread=FALSE, lty.smooth=2)


plot(df$SP.POP.GROW, predict(fit), xlab="Прирост населения", ylab="Продолжительность жизни", 
     main = 'Предсказание о продолжительности жизни по приросту населения')
abline(fit, col = "green")
