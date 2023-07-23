#Installing packages
install.packages('ggpubr')
install.packages('mclust')
install.packages('factoextra')
install.packages('fpc')

#Enable libraries
library('ggpubr')
library('mclust')
library('factoextra')
library('fpc')
library('cluster')

#Read data
data = read.csv('SCF07.csv')
data = data[,2:3]
#Plot data
ggscatter(data, x = 'lsam', y = 'linc') +
  geom_density2d()

#Model-based clustering (EM)
cl = Mclust(data, G = 1:12)
summary(cl)
fviz_mclust_bic(cl, palette = 'Set3', main = 'EM model selection',
                legend = 'right')
fviz_mclust(cl, 'classification', geom = 'point', stand = FALSE,
            ellipse.type = 'convex', pointsize = 1.3, palette = 'Set3',
            main = 'EM')
fviz_mclust(cl, 'uncertainty', stand = FALSE,
             ellipse.type = 'convex', palette = 'Set3',
            main = 'EM')

#EM analysis
hist(cl$uncertainty, main = 'Histogram of Uncertainty', xlab = 'Uncertainty')
cluster_table <- table(cl$classification)
cluster_prop <- prop.table(cluster_table)
cluster_means <- aggregate(data, by = list(cl$classification), mean)
bar_colors <- ifelse(cluster_means[, 'lsam'] < 4, 'cyan', 'gold')
bp = barplot(cluster_prop, main = 'Relative Frequency Distributions between Clusters', 
        xlab = 'Cluster', ylab = 'Relative Frequency', col = bar_colors)
text(bp, 0, labels = round(cluster_means$linc, 1), pos = 3)
legend(4, 0.12, title = 'Savings', legend = c('No', 'Yes'), 
       col = c('cyan', 'gold'), pch = 15)


#K-Medoids clustering - find best k
best_k = vector(mode = 'integer', length = 3)
for (i in 1:3) {
  sm = data[sample(nrow(data), 3400), ]
  pdftitle = paste('sample_sca', i, '.pdf', sep='')
  ggscatter(sm, x = 'lsam', y = 'linc') + geom_density2d()
  ggsave(pdftitle)
  pdftitle = paste('sample_sil', i, '.pdf', sep = '')
  plot = fviz_nbclust(sm, cluster::pam, method = 'silhouette', k.max = 12)
  ggsave(pdftitle)
  info = plot$data
  best_k[i] = as.numeric(info$clusters[which.max(info$y)])
}
best_k

#K-Medoids clustering - apply to dataset
plot = pam(data, k = 2, metric = 'euclidean', stand = FALSE)
fviz_cluster(plot, palette = 'Dark2', ellipse.type = 'norm',
             stand = FALSE, geom = 'point', pointsize = 1.3,
             main = '2-Medoids classification'
             ) + theme(axis.line = element_line(),
                       panel.background = element_blank())

#KM analysis
km_cluster_table <- table(plot$clustering)
km_cluster_prop <- prop.table(km_cluster_table)
km_cluster_means <- aggregate(data, by = list(plot$clustering), mean)
bar_colors <- ifelse(km_cluster_means[, 'lsam'] < 6, 'cyan', 'gold')
bp = barplot(km_cluster_prop, main = 'Relative Frequency Distributions between Clusters', 
             xlab = 'Cluster', ylab = 'Relative Frequency', col = bar_colors)
text(bp, 0, labels = round(km_cluster_means$linc, 1), pos = 3)
legend(0.55, 0.35, title = 'Savings', legend = c('No', 'Yes'), 
       col = c('cyan', 'gold'), pch = 15)

