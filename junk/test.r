# ==========================================================
# PRAKTIKUM 11 - METODE KUANTITATIF
# Clustering Dasar Menggunakan R (Dataset: mtcars)
# ==========================================================

# --- 1. SETUP ENVIRONMENT & PACKAGES ---
packages <- c("cluster", "factoextra", "dendextend", "gridExtra")
install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}
invisible(sapply(packages, install_if_missing))

# --- 1. EKSPLORASI DATA & PRA-PEMROSESAN ---
cat("\n--- 1. Eksplorasi Data & Pra-pemrosesan ---\n")
data("mtcars")
mtcars_scaled <- scale(mtcars)

cat("Ringkasan Data Asli:\n")
print(summary(mtcars))
cat("\nRingkasan Data Normalisasi:\n")
print(summary(mtcars_scaled))

cat("\nJawaban 1c:\nData perlu distandarkan agar variabel dengan skala besar (seperti hp dan disp) tidak mendominasi perhitungan jarak dibandingkan variabel berskala kecil (seperti wt), sehingga skala menjadi seragam.\n")

# --- 2. JARAK ANTAR DATA ---
cat("\n--- 2. Jarak antar Data ---\n")
dist_euclidean <- dist(mtcars_scaled, method = "euclidean")
dist_manhattan <- dist(mtcars_scaled, method = "manhattan")

mat_euclidean <- as.matrix(dist_euclidean)
mat_manhattan <- as.matrix(dist_manhattan)

cat(sprintf("Jarak Euclidean (Objek 1 & 2): %.4f\n", mat_euclidean[1, 2]))
cat(sprintf("Jarak Manhattan (Objek 1 & 2): %.4f\n", mat_manhattan[1, 2]))

cat("\nJawaban 2b:\nNilai jarak tersebut merepresentasikan tingkat kemiripan antar observasi. Semakin kecil nilainya, semakin mirip karakteristik kedua objek tersebut.\n")

# --- 3. ELBOW METHOD & K-MEANS ---
cat("\n--- 3. Elbow Method & K-Means ---\n")
set.seed(123)
wss <- vector()
silhouette_scores <- numeric()

for (k in 1:10) {
  km <- kmeans(mtcars_scaled, centers = k, nstart = 25)
  wss[k] <- km$tot.withinss
}

for (k in 2:10) {
  km <- kmeans(mtcars_scaled, centers = k, nstart = 25)
  sil <- silhouette(km$cluster, dist_euclidean)
  silhouette_scores[k] <- mean(sil[, 3])
}

par(mfrow = c(1, 2))
plot(1:10, wss, type = "b", pch = 19, col = "blue", xlab = "K", ylab = "Total WCSS", main = "Elbow Method")
plot(2:10, silhouette_scores[2:10], type = "b", pch = 19, col = "red", xlab = "K", ylab = "Rata-rata Silhouette", main = "Silhouette Analysis")

k_opt <- 3
cat("\nJawaban 3b:\nK optimal yang paling tepat adalah 3. Berdasarkan grafik Elbow, penurunan WCSS mulai melambat secara signifikan di K=3 (membentuk siku). Hal ini juga didukung oleh rata-rata Silhouette Score yang tinggi pada titik tersebut.\n")

km_2 <- kmeans(mtcars_scaled, centers = k_opt - 1, nstart = 25)
km_3 <- kmeans(mtcars_scaled, centers = k_opt, nstart = 25)
km_4 <- kmeans(mtcars_scaled, centers = k_opt + 1, nstart = 25)

p2 <- fviz_cluster(km_2, data = mtcars_scaled, main = "k = 2")
p3 <- fviz_cluster(km_3, data = mtcars_scaled, main = "k = 3")
p4 <- fviz_cluster(km_4, data = mtcars_scaled, main = "k = 4")
grid.arrange(p2, p3, p4, ncol = 3)

cat("\nJawaban 3c:\nBerdasarkan visualisasi K=3, cluster terlihat terpisah dengan cukup jelas dan pengelompokannya terlihat rapi dengan tumpang tindih (overlap) yang minim.\n")

# --- 4. EVALUASI K-MEANS ---
cat("\n--- 4. Evaluasi K-Means (K=3) ---\n")
cat(sprintf("Total Sum of Squares (totss): %.2f\n", km_3$totss))
cat(sprintf("Within-cluster (tot.withinss): %.2f\n", km_3$tot.withinss))
cat(sprintf("Between-cluster (betweenss): %.2f\n", km_3$betweenss))

proporsi <- km_3$betweenss / km_3$totss
cat(sprintf("\nJawaban 4b - Proporsi variasi yang dijelaskan: %.2f%%\n", proporsi * 100))

# --- 5. HIERARCHICAL CLUSTERING ---
cat("\n--- 5. Hierarchical Clustering ---\n")
hc_complete <- hclust(dist_euclidean, method = "complete")
hc_average  <- hclust(dist_euclidean, method = "average")
hc_single   <- hclust(dist_euclidean, method = "single")

par(mfrow = c(1, 3))
plot(hc_complete, main = "Complete Linkage", cex = 0.6)
plot(hc_average, main = "Average Linkage", cex = 0.6)
plot(hc_single, main = "Single Linkage", cex = 0.6)

cor_comp <- cor(dist_euclidean, cophenetic(hc_complete))
cor_avg  <- cor(dist_euclidean, cophenetic(hc_average))
cor_sing <- cor(dist_euclidean, cophenetic(hc_single))

cat("\nCophenetic Correlation:\n")
cat(sprintf("Complete: %.4f | Average: %.4f | Single: %.4f\n", cor_comp, cor_avg, cor_sing))

cat("\nJawaban 5b:\nBentuk dendrogram Complete lebih kompak, Single cenderung membentuk rantai (chaining), dan Average berada di antaranya. Berdasarkan Cophenetic Correlation, metode Average adalah yang lebih baik karena nilainya paling mendekati 1, menandakan representasi jarak yang paling akurat terhadap data asli.\n")

hc_cut <- cutree(hc_average, k = 3)
cat("\nDistribusi cluster (Average Linkage, k=3):\n")
print(table(hc_cut))

cat("\nJawaban 5c:\nBerdasarkan pemotongan dendrogram menjadi 3 cluster, distribusi jumlah observasi antar cluster tidak sepenuhnya seimbang. Namun, hal ini wajar dalam clustering karena pengelompokan didasarkan pada kedekatan karakteristik data, bukan sekadar membagi data sama rata.\n")