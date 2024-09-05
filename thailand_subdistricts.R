library(apcluster)
library(tidyverse)
library(ggrepel)

thailand_subdictrict <- read.csv("thailand_subdistricts.csv")

# 1. Data Transformation ----
# Remove the name_id column to prepare data for clustering
feature_counts_matrix <- as.matrix(thailand_subdictrict[,11:33])
# Logarithm function
feature_counts_log <- log1p(feature_counts_matrix)

# 2. Affinity Propagation ----
# Calculate the Euclidean similarity (negative squared Euclidean distance)
euclidean_similarity <- negDistMat(feature_counts_log, method = "euclidean", r = 2)

# Perform Affinity Propagation clustering
ap_result <- apcluster(euclidean_similarity,
                       q = 0.0005, # Try a lower q value for fewer clusters
                       seed = 22551,
                       details = TRUE)

apcluster::plot(ap_result)

# Extract clustering labels for each name_id
clusters <- apcluster::labels(ap_result)

# Replace cluster indices with corresponding name_id
sub_names <- thailand_subdictrict$subdistrict
clusters_sub_names <- sub_names[as.numeric(clusters)]

# Exemplar names and colours
exemplar_names <- data.frame(exemplar =
                                     c("Kham Tanot","Nong Kum", "San Sai", "Khueang Kham", "Lom Raet", "Ang Thong", "Lumphli", "Na Mueang", "Nong Hoi", "Wiang"),
                             exemplar_name = 
                                     c("Minimal Hinterland","Basic (A) Hinterland", "Basic (N) Hinterland", "Developing Hinterland", "Necessity Zone", "Green Enclave", "Historical Site", "Urban Village", "City Centre", "Commercial Hub"),
                             color = c("#FFF8D6","#F7E1AE", "#A4D0A4","#617A55","#8ACDD7","#3887BE","#874CCC","#9F0D7F","#FF5BAE","#FF0000")
)

# Add exemplar column to the original feature_counts dataframe
thailand_ap <- thailand_subdictrict |>
        mutate(exemplar = clusters_sub_names) |>
        left_join(exemplar_names, by = "exemplar")

# Extract exemplars
thailand_exemplars <- thailand_ap |>
        dplyr::filter(subdistrict == exemplar)

# Calculate province_exemplar and cluster_poverty as before
province_exemplar <- thailand_ap |>
        group_by(province, exemplar_name) |>
        summarise(count = n()) |>
        spread(key = exemplar_name, value = count, fill = 0) |>
        column_to_rownames(var = "province")

# 3. PCA visualisation ----
# Perform PCA for dimensionality reduction
pca_result <- prcomp(feature_counts_log,
                     scale. = FALSE,
                     center = TRUE)
summary(pca_result)

# Prepare the data
pca_rotations <- pca_result[["rotation"]]
pca_data <- data.frame(pca_result$x[, 1:2]*-1)

# Add cluster and exemplar columns to the PCA data
pca_data$exemplar_name <- thailand_ap$exemplar_name
pca_data$cluster <- clusters_sub_names
# Define a mapping of numbers to exemplar names
exemplar_name_mapping <- setNames(colnames(province_exemplar), 1:10)

# Ensure the labels are correctly assigned to exemplar_name
pca_data$label <- ifelse(thailand_ap$subdistrict == clusters_sub_names, thailand_ap$exemplar_name, "")

# Replace numeric labels with corresponding exemplar names
pca_data$label <- sapply(pca_data$label, function(x) {
        if (x %in% names(exemplar_name_mapping)) {
                exemplar_name_mapping[[x]]
        } else {
                x
        }
})

# Calculate the proportion of variance explained by the first two principal components
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
variance_explained_pc1 <- round(variance_explained[1] * 100, 2)
variance_explained_pc2 <- round(variance_explained[2] * 100, 2)

# Function to compute convex hull for clusters
compute_hull <- function(df) df[chull(df$PC1, df$PC2), ]

# Compute the convex hull for each cluster
hulls <- pca_data |>
        group_by(cluster) |>
        do(compute_hull(.))

# Specify colours
colors <- exemplar_names$color
names(colors) <- exemplar_names$exemplar_name

# Plotting
ggplot(pca_data, aes(x = PC1, y = PC2, color = exemplar_name)) +
        geom_point(size = 3, alpha = 0.3) +
        geom_polygon(data = hulls, aes(x = PC1, y = PC2, fill = exemplar_name, group = cluster),
                     alpha = 0.2) +
        scale_color_manual(values = colors) +
        scale_fill_manual(values = colors) +
        geom_label_repel(data = pca_data[pca_data$label != "", ], aes(label = label, fill = exemplar_name), 
                         alpha = 0.8,
                         color = "black",
                         size = 4, hjust = 0, vjust = 0,
                         force = 20) +
        labs(title = "Principal Component Analysis",
             x = paste("PC1 (", variance_explained_pc1, "%) Intensity of Amenities", sep = ""),
             y = paste("PC 2 (", variance_explained_pc2, "%) Natural — Commercial", sep = "")) +
        theme_light() +
        guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 2)) +
        theme(legend.position = "none")

