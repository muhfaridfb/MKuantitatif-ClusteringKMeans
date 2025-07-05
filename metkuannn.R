# Memuat library yang diperlukan
library(cluster)
library(ggplot2)
library(factoextra)
library(purrr)
library(dplyr)
library(tidyr)

# Memuat Data
data <- read.csv("C:/Users/Muh Farid FB/Downloads/StudentsPerformance_with_headers.csv")

# Memilih hanya beberapa variabel tertentu
data_selected <- data %>%
  select(Dohavepartner, Weeklystudyhours, Readingfrequency, Attendancetoclasses, Takingnotesinclasses, Listeninginclasses, CumulativeGPAinthelastsemester)

# Basic Data Exploration
head(data_selected)
str(data_selected)
summary(data_selected)

# Checking Missing Values
colSums(is.na(data_selected))

# Handle missing values by imputing with column means
data_numeric <- data_selected %>% mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Membuat Plot
data_melted <- data_numeric %>% pivot_longer(everything(), names_to = "Variable", values_to = "Value")

# Mengeksekusi Plot
ggplot(data_melted, aes(x = Value, fill = Variable)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.5) +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Histogram for Each Variable", fill = "Variable") +
  ylab("Count")

# Scale
data_scaled <- scale(data_numeric)
cov_matrix <- cov(data_scaled)
View(data_scaled)

# Menghitung Total Dalam Klaster Jumlah Kuadrat
wss <- function(k) {
  kmeans(data_numeric, k, nstart = 10)$tot.withinss
}

# Menghitung dan Plot WSS untuk K = 1 Hingga K = 15
k.values <- 1:15
wss_values <- map_dbl(k.values, wss)

# Determine Optimal Number of Clusters Using Elbow Method
fviz_nbclust(data_scaled, kmeans, method = "wss") +
  labs(title = "Metode Elbow untuk Menentukan Jumlah Cluster Optimal")

# Melakukan Clustering Tanpa Praproses PCA
# Melakukan K-Means dengan Jumlah Klaster yang Dipilih K=3
set.seed(123)
final_kmeans1 <- kmeans(data_numeric, 3)

# Visualisasi Klaster
fviz_cluster(final_kmeans1, data = data_numeric)

# Untuk final_kmeans1 (Tanpa PCA)
total_ss1 <- sum((data_numeric - colMeans(data_numeric))^2)
between_ss1 <- final_kmeans1$tot.withinss
percentage1 <- (total_ss1 - between_ss1) / total_ss1 * 100

# Menampilkan Persentase
cat("Persentase between_SS/total_SS Tanpa PCA: ", percentage1, "%\n")


# Melakukan Clustering Menggunakan Praproses PCA
# Melakukan PCA pada Data
pca <- prcomp(data_numeric, scale. = TRUE)
summary(pca)

# Mengubah Hasil PCA Menjadi Dataframe
df.pca <- as.data.frame(pca$x[,1:2])
df.pca

# Melakukan K-Means dengan Jumlah Klaster yang Dipilih K=5
set.seed(123)
final_kmeans3 <- kmeans(df.pca, 5)

# Visualisasi Klaster
fviz_cluster(final_kmeans5, data = df.pca) 

# Untuk final_kmeans2 (Dengan PCA)
total_ss3 <- sum((df.pca - colMeans(df.pca))^2)
between_ss3 <- final_kmeans3$tot.withinss
percentage3 <- (total_ss3 - between_ss3) / total_ss3 * 100

# Menampilkan Persentase
cat("Persentase between_SS/total_SS Dengan PCA: ", percentage3, "%\n")

# Menghitung rata-rata setiap variabel untuk setiap klaster
cluster_means1 <- aggregate(data_numeric, list(final_kmeans1$cluster), mean)
cluster_means3 <- aggregate(df.pca, list(final_kmeans3$cluster), mean)

# Menampilkan Hasil
print(cluster_means1)
print(cluster_means3)

# Visualisasi Korelasi dengan Heatmap
cor_matrix <- cor(data_numeric)
heatmap_melted <- melt(cor_matrix)
ggplot(heatmap_melted, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) + 
  coord_fixed()

# Visualisasi PCA dengan Warna Klaster
pca_df <- as.data.frame(pca$x[, 1:2])
pca_df$Cluster<- as.factor(final_kmeans1$cluster)

ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) + 
  geom_point(size = 3) +  
  ggtitle("PCA Plot by Cluster") + 
  theme_minimal()
