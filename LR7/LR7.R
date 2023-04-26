setwd("C:/Users/AlmaZ/RStudioProjects/R/LR7")

df <- read.csv("athlete_events.csv", sep=",", header=T,
               fileEncoding="UTF-8", )

# все гребцы
rowing <- df[df$Sport == 'Rowing' & !is.na(df$Weight), ]
h0 <- mean(df[!df$Sport == 'Rowing' & !is.na(df$Weight), ]$Weight)

t.test(rowing$Weight, mu=h0)


taekwondo_M <- df[df$Sport == 'Taekwondo' & !is.na(df$Weight) & df$Sex == 'M', ]
taekwondo_F <- df[df$Sport == 'Taekwondo' & !is.na(df$Weight) & df$Sex == 'F', ]
judo_M <- df[df$Sport == 'Judo' & !is.na(df$Weight) & df$Sex == 'M', ]
judo_F <- df[df$Sport == 'Judo' & !is.na(df$Weight) & df$Sex == 'F', ]


library(car)
par(mfrow=c(1, 2))
qqPlot(judo_M$Weight, main='Вес мужчин дзюдоистов')
qqPlot(judo_F$Weight, main='Вес женщин дзюдоистов')

hist(judo_M$Weight, freq = FALSE, ylim = c(0, 0.025),
     main = "Вес мужчин дзюдоистов")
lines(density(judo_M$Weight), col = "red", lwd = 2)

hist(judo_F$Weight, freq = FALSE, main = "Вес женщин дзюдоистов")
lines(density(judo_F$Weight), col = "red", lwd = 2)

shapiro.test(judo_M$Weight)
shapiro.test(judo_F$Weight)



qqPlot(taekwondo_M$Weight, main='Вес мужчин таэквондистов')
qqPlot(taekwondo_F$Weight, main='Вес женщин таэквондистов')

hist(taekwondo_M$Weight, freq = FALSE, main = "Вес мужчин таэквондистов")
lines(density(taekwondo_M$Weight), col = "red", lwd = 2)

hist(taekwondo_F$Weight, freq = FALSE, main = "Вес женщин таэквондистов")
lines(density(taekwondo_F$Weight), col = "red", lwd = 2)

shapiro.test(taekwondo_M$Weight)
shapiro.test(taekwondo_F$Weight)


judo_taekwondo_M <- rbind(judo_M, taekwondo_M)
judo_taekwondo_F <- rbind(judo_F, taekwondo_F)

bartlett.test(Weight ~ Sport, data=judo_taekwondo_M)
bartlett.test(Weight ~ Sport, data=judo_taekwondo_F)

t.test(Weight ~ Sport, data=judo_taekwondo_M)
t.test(Weight ~ Sport, data=judo_taekwondo_F)
