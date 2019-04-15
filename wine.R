#---------------------------- 1: DOWNLOADING FILE --------------------------------#

# Getting data from website
if(!file.exists("./data")){dir.create("./data")}
url1 <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv'
url2 <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv'
download.file(url1, destfile = 'data/red.csv')
download.file(url2, destfile = 'data/white.csv')


#------------------------ 2: READING ACT & FEAT DATA -----------------------------#
red <- read.csv('data/red.csv', sep=';', header=T)
white <- read.csv('data/white.csv', sep=';', header=T)

red$label <- "red"
white$label <- "white"

wine <- rbind(red,white)

any(is.na(wine))
#--- histograms ---#

pl <- ggplot(wine, aes(x=residual.sugar, fill=label, color=label)) +
      geom_histogram(bins=100, position="identity", alpha=0.6)

pl + scale_color_manual(values=c("#8B0000", "#fffdd0", "#56B4E9"))+
      scale_fill_manual(values=c("#8B0000", "#fffdd0", "#56B4E9"))


pl2 <- ggplot(wine, aes(x=citric.acid, fill=label, color=label)) +
      geom_histogram(bins=100, position="identity", alpha=0.6)

pl2 + scale_color_manual(values=c("#8B0000", "#fffdd0", "#56B4E9"))+
      scale_fill_manual(values=c("#8B0000", "#fffdd0", "#56B4E9"))


pl3 <- ggplot(wine, aes(x=alcohol, fill=label, color=label)) +
      geom_histogram(bins=100, position="identity", alpha=0.6)

pl3 + scale_color_manual(values=c("#8B0000", "#fffdd0", "#56B4E9"))+
      scale_fill_manual(values=c("#8B0000", "#fffdd0", "#56B4E9"))

#--- scatterplot  ---#

pl4 <- ggplot(wine, aes(citric.acid,residual.sugar, color =label)) + 
      geom_point(size = 3, alpha= .3)

pl4 + scale_color_manual(values=c("#8B0000", "#fffdd0", "#56B4E9"))+
      scale_fill_manual(values=c("#8B0000", "#fffdd0", "#56B4E9")) +
      theme(panel.background = element_rect(fill = "#696969",
                                            colour = "#696969",
                                            size = 0.5, linetype = "solid")) +
      theme(legend.key = element_rect(color = '#696969', fill = '#696969'))

pl5 <- ggplot(wine, aes(volatile.acidity,residual.sugar, color =label)) + 
      geom_point(size = 3, alpha= .3)

pl5 + scale_color_manual(values=c("#8B0000", "#fffdd0", "#56B4E9"))+
      scale_fill_manual(values=c("#8B0000", "#fffdd0", "#56B4E9")) +
      theme(panel.background = element_rect(fill = "#696969",
                                            colour = "#696969",
                                            size = 0.5, linetype = "solid")) +
      theme(legend.key = element_rect(color = '#696969', fill = '#696969'))

#--- model ---#
set.seed(101)
clus.data <- wine[,-13]
head(clus.data)

wine.cluster <- kmeans(clus.data,centers = 2, nstart = 15)
print(wine.cluster)

#--- evaluation ---#
table(wine$label,wine.cluster$cluster)
