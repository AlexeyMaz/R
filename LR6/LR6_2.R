rm(clust.crashes, df_plot, df_plot1, df_tmp, df_deaths, dif_pars_per_year,
   oper_vs_fatal, survived_no_zero, cl1, cl2, cl3, cl4, cl5, dist.crashes, maxs, mins)


library(klaR)

ind <- sample(2, nrow(df_clust), replace=TRUE, prob=c(0.7, 0.3))

df_clust_train <- data.frame(df_clust, clusters)
df_clust_train <- df_clust_train[ind == 1, ]
df_clust_train$clusters <- as.factor(df_clust_train$clusters)

df_clust_test <- df_clust[ind == 2, ]
clusters_test <- clusters[ind == 2]
df_clust_test <- data.frame(df_clust_test, clusters=clusters_test)


naive_crashes <- NaiveBayes(clusters~., data=df_clust_train)
naive_crashes$tables


par(mfrow=c(3,1))
plot(naive_crashes,lwd = 2, legendplot=FALSE)


pred_b <- predict(naive_crashes, df_clust_train[, -4])$class
table(Факт = df_clust_train$clusters, Прогноз = pred_b)

acc_b <- mean(pred_b == df_clust_train$clusters)
paste0("Точность = ", round(100*acc_b, 2), "%")



library(party)
myFormula <- clusters ~ .
crashes_ctree <- ctree(myFormula, data=df_clust_train)


pred_t <- predict(crashes_ctree, df_clust_train[, -4])
table(Факт = df_clust_train$clusters, Прогноз = pred_t)

acc_t <- mean(pred_t == df_clust_train$clusters)
paste0("Точность = ", round(100*acc_t, 2), "%")


dev.off()
plot(crashes_ctree)



library(randomForest)
rf <- randomForest(clusters ~ ., data=df_clust_train, ntree=100, proximity=TRUE)

pred_rf <- predict(crashes_ctree, df_clust_train[, -4])
table(Факт = df_clust_train$clusters, Прогноз = pred_rf)

acc_rf <- mean(pred_rf == df_clust_train$clusters)
paste0("Точность = ", round(100*acc_rf, 2), "%")