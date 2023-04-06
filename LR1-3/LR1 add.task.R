x <- sample(c(0, 1), 100, replace=T, prob=c(.3, .7))
x

z <- data.frame(table(x))
colnames(z) <- c('Элемент', 'Частота')
z