data<-read.table('C:/Users/nikit/Desktop/workmad/dataset_22.csv', header = TRUE, sep = ',', stringsAsFactors = FALSE)
View(data)
data$Y <- as.factor(data$Y)
data$COLOR <- as.factor(data$COLOR)
data$SPINE <- as.factor(data$SPINE)
str(data)
install.packages("psych")
library(psych)
summary(data)
describe(data)
my_mode <- function(x) {
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}
my_mode(data$WIDTH)
my_mode(data$SATELLTS)
my_mode(data$WEIGHT)
my_mode(data$CATWIDTH)
data1<-subset(data,data$Y==0)
data2<-subset(data,data$Y==1)
summary(data1)
describe(data1)
my_mode(data1$WIDTH)
my_mode(data1$SATELLTS)
my_mode(data1$WEIGHT)
my_mode(data1$CATWIDTH)
summary(data2)
describe(data2)
my_mode(data2$WIDTH)
my_mode(data2$SATELLTS)
my_mode(data2$WEIGHT)
my_mode(data2$CATWIDTH)
plot_builder <- function(column_one, column_two, main, xlab, ylab) {
  plot(data.frame(column_one, column_two),
       pch = 16,
       main = main,
       xlab = xlab,
       ylab = ylab
  )
}
plot_builder(data$SATELLTS,data$WIDTH,"Диаграмма рассеяния", " Ширина панциря ", "Количество партнеров")
plot_builder(data$SATELLTS,data$WEIGHT,"Диаграмма рассеяния", " Вес краба", "Количество партнеров")



pie_builder <- function(table_row, main, colors, descr_colors) {
  summarized <- c(summary(table_row))
  piepercent <- round(100 * summarized / sum(summarized), 1)
  pie(summarized,
      piepercent,
      radius = 0.75,
      main = main,
      col = colors,
      clockwise = TRUE
  )
  legend("topright", descr_colors, cex = 1.5, fill = colors)
}
pie_builder(data1$COLOR,
            "Распределение по цвету краба",
            c("#AAFFCC", "#FFAACC", "#AACCFF", "#AAAAAA"),
            c("2", "3", "4", "5")
)
pie_builder(data2$COLOR,
            "Распределение по цвету краба",
            c("#AAFFCC", "#FFAACC", "#AACCFF", "#AAAAAA"),
            c("2", "3", "4", "5")
)
barplot_builder <- function(table_row, main) {
  barplot(table(table_row),
          main = main,
          border = "red",
          col = "blue",
          density = 10
  )
}
barplot_builder(data1$COLOR, "Цвет краба")
barplot_builder(data2$COLOR, "Цвет краба")
boxplot_builder <- function(table_row_k, table_row_n, main, xlab, ylab, data) {
  boxplot(table_row_n ~ table_row_k,
          main = main,
          xlab = xlab,
          ylab = ylab,
          col = "#AACCFF",
          data = data
  )
}
boxplot_builder(data$COLOR, data$SATELLTS, "Диаграмма размаха", "Цвет краба", "Количество партнеров", data)
hist(data2$SATELLTS,
     freq=FALSE,
     breaks=20,
     col = "#AACCFF",
     xlab="Диапазон количества партнеров",
     main="Гистограмма распределения"
)
pairs(~WIDTH+SATELLTS+WEIGHT+CATWIDTH, data = data, main = "Matrix graph")
chisq_builder <- function(data) {
  factor_names <- names(data[,unlist(lapply(data, is.factor))])
  chi_results <- matrix(NA,
                        nrow = length(factor_names),
                        ncol = length(factor_names)
  )
  colnames(chi_results) <- c(factor_names)
  rownames(chi_results) <- c(factor_names)
  for (col_name in factor_names){
    for (col_name2 in factor_names) {
      chi_results[col_name,col_name2] <- chisq.test(table(data[,col_name],data[,col_name2]))$p.value
    } 
  }
  View(chi_results)
} 
chisq_builder(data1)
chisq_builder(data2)
pearson_builder <- function(data) {
  factor_names <- names(data[,unlist(lapply(data, is.factor))])
  fisher_results <- matrix(NA,
                           nrow = length(factor_names),
                           ncol = length(factor_names)
  )
  colnames(fisher_results) <- c(factor_names)
  rownames(fisher_results) <- c(factor_names)
  for (col_name in factor_names){
    for (col_name2 in factor_names) {
      fisher_results[col_name, col_name2] <- fisher.test(table(data[,col_name], data[,col_name2]), workspace = 2e8)$p.value
    } 
  }
  View(fisher_results)
}
pearson_builder(data1)
pearson_builder(data2)
aov_model <- aov(data$SATELLTS ~ data$COLOR, data = data)
summary(aov_model)










cor_builder <- function(data) {
  M <- data[,unlist(lapply(data, is.numeric))]
  N1 <- cor(M,use="pairwise.complete.obs", method="pearson")
  N2 <- cor(M,use="pairwise.complete.obs",method="spearman")
  N3 <- cor(M,use="pairwise.complete.obs",method="kendall")
  print("Pearson")
  print(N1)
  print("Spearman")
  print(N2)
  print("Kendall")
  print(N3)
}
cor_builder(data1)
cor_builder(data2)
install.packages("BiocManager")
BiocManager::install("graph")
install.packages("ggm")
install.packages("corrplot")
library(ggm)
library(corrplot)
pcor_builder <- function(data, depend) {
  M <- data[,unlist(lapply(data, is.numeric))]
  pcor(depend, cov(M))
}
pcor_builder(data1, c(1,4))
pcor_builder(data2, c(1,4))

corrplot_builder <- function(data, method) {
  colors <- c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA")
  M <- data[,unlist(lapply(data, is.numeric))]
  N1 <- cor(M, use="pairwise.complete.obs", method = method)
  col <- colorRampPalette(colors)
  corrplot(N1, method="color", col=NULL,  
           type="upper", order="hclust", 
           addCoef.col = "black", tl.col="black", tl.srt=45,
           sig.level = 0.01, insig = "blank",
           diag=FALSE 
  )
}
corrplot_builder(data,"pearson")
corrplot_builder(data2,"pearson")
