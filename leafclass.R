#Leaf Plant Classification

#https://datascienceplus.com/leaf-plant-classification-an-exploratory-analysis-part-1/
#https://www.r-bloggers.com/leaf-plant-classification-statistical-learning-model-part-2/

#######################
### Exploratory Analysis ###
#######################
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(corrplot))
suppressPackageStartupMessages(library(Hmisc))

setwd("/home/upsman/machineLearnAI/leafClassification")

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00241/100%20leaves%20plant%20species.zip"
temp_file <- tempfile()
download.file(url, temp_file)
margin_file <- "100 leaves plant species/data_Mar_64.txt"
shape_file <- "100 leaves plant species/data_Sha_64.txt"
texture_file <- "100 leaves plant species/data_Tex_64.txt"
files_to_unzip <- c(margin_file, shape_file, texture_file)
unzip(temp_file, files = files_to_unzip, exdir=".", overwrite = TRUE)

margin_data <- read.csv(margin_file, header=FALSE, sep=",", stringsAsFactors = TRUE)
shape_data <- read.csv(shape_file, header=FALSE, sep=",", stringsAsFactors = TRUE)
texture_data <- read.csv(texture_file, header=FALSE, sep=",", stringsAsFactors = TRUE)

dim(margin_data)
dim(shape_data)
dim(texture_data)
#We notice that the texture dataset has one row less. Such issue will be fixed at a later moment.
sum(complete.cases(margin_data)) == nrow(margin_data)
sum(complete.cases(shape_data)) == nrow(shape_data)
sum(complete.cases(texture_data)) == nrow(texture_data)
#No NA's value are present. Column naming is necessary due to the absence of header.
n_features <- ncol(margin_data) - 1
colnames(margin_data) <- c("species", paste("margin", as.character(1:n_features), sep=""))
margin_data$species <- factor(margin_data$species)

n_features <- ncol(shape_data) - 1
colnames(shape_data) <- c("species", paste("shape", as.character(1:n_features), sep=""))
shape_data$species <- factor(shape_data$species)

n_features <- ncol(texture_data) - 1
colnames(texture_data) <- c("species", paste("texture", as.character(1:n_features), sep=""))
texture_data$species <- factor(texture_data$species)

# We count the number of entries for each species within each dataset.

margin_count <- sapply(base::split(margin_data, margin_data$species), nrow)
shape_count <- sapply(base::split(shape_data, shape_data$species), nrow)
texture_count <- sapply(base::split(texture_data, texture_data$species), nrow)

# That in order to identify what species is associated to the missing entry inside the texture dataset.

which(margin_count != texture_count)
## Acer Campestre 
##              1

which(shape_count != texture_count)
## Acer Campestre 
##              1

# The texture data missing entry is associated to Acer Campestre species. Adding an identifier column to all datasets to allow for datasets merging (joining).

margin_data <- mutate(margin_data, id = 1:nrow(margin_data))
shape_data <- mutate(shape_data, id = 1:nrow(shape_data))
texture_data <- mutate(texture_data, id = 1:nrow(texture_data))

# Imputation

dd <- data.frame(matrix(nrow=1, ncol = 66))
colnames(dd) <- colnames(texture_data)
dd$species <- "Acer Campestre"
dd$id <- 16

temp_texture_data <- rbind(texture_data[1:15,], dd)
features <- setdiff(colnames(temp_texture_data), c("species", "id"))
imputed <- sapply(features, function(x) { as.numeric(impute(temp_texture_data[, x], median)[16])})
temp_texture_data[16, names(imputed)] <- imputed

texture_data <- rbind(temp_texture_data, texture_data[-(1:15),])
texture_data <- mutate(texture_data, id = 1:nrow(texture_data))
dim(texture_data)
## [1] 1600   66

# Here is what we got at the end.

str(margin_data)
str(shape_data)
str(texture_data)

# Correlation Analysis

# Since margin, shape and texture covariates are quantitative variables, it is of interest to evaluate correlation among such leaf features.
# 
# We do it by taking advantage of the correlation plot. We show that for the margin1 feature.

m_l <- split(margin_data, margin_data$species)

extract_feature <- function(m_l, feature) {
  f <- lapply(m_l, function(x) { x[,feature] })
  do.call(cbind, f)
}

thefeature <- "margin1"
m <- extract_feature(m_l, thefeature)
cor_mat <- cor(m)
corrplot(cor_mat, method = "circle", type="upper", tl.cex=0.3)
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
most_correlated <- function(dataset, feature, threshold) {
  m_l <- split(dataset, dataset$species)
  m <- extract_feature(m_l, feature)
  rcorr_m <- rcorr(m)
  flat_cor <- flattenCorrMatrix(rcorr_m$r, rcorr_m$P)
  attr(flat_cor, "variable") <- feature
  flat_cor <- flat_cor %>% filter(p < 0.05) %>% filter(abs(cor) > threshold)
  flat_cor[,-4] # getting rid of the p-value column
}
corr_margin2 <- most_correlated(margin_data, "margin2", 0.7)
corr_margin2
# If we want to collect all the correlation matrixes for the margin_data, here is how we can do.
margin_names <- setdiff(colnames(margin_data), c("species", "id"))
margin_corr_l <- lapply(margin_names, function(x) {most_correlated(margin_data, x, 0.7)})
names(margin_corr_l) <- margin_names

# Let us have a look at a correlation matrix as item of such list.
margin_corr_l[["margin32"]]

# Here we do the same for shape and texture features datasets. For shape dataset we show shape3 feature correlation matrix result.
shape_names <- setdiff(colnames(shape_data), c("species", "id"))
shape_corr_l <- lapply(shape_names, function(x) {most_correlated(shape_data, x, 0.7)})
names(shape_corr_l) <- shape_names
shape_corr_l[["shape3"]]

# For texture dataset we show texture19 feature correlation matrix result.
texture_names <- setdiff(colnames(texture_data), c("species", "id"))
texture_corr_l <- lapply(texture_names, function(x) {most_correlated(texture_data, x, 0.7)})
names(texture_corr_l) <- texture_names
texture_corr_l[["texture19"]]

# Furthermore, by collecting the number of rows of such correlation matrixes, the most correlated features among the one hundred leaf plant species can be put in evidence.
t <- sapply(margin_corr_l, nrow)
margin_c <- data.frame(feature = names(t), value = t)

t <- sapply(shape_corr_l, nrow)
shape_c <- data.frame(feature = names(t), value = t)

t <- sapply(texture_corr_l, nrow)
texture_c <- data.frame(feature = names(t), value = t)

ggplot(data = margin_c, aes(x=feature, y=value, fill = feature)) + theme_bw() + theme(legend.position = "none") + geom_histogram(stat='identity') + coord_flip()
ggplot(data = shape_c, aes(x=feature, y=value, fill = feature)) + theme_bw() + theme(legend.position = "none") + geom_histogram(stat='identity') + coord_flip()
ggplot(data = texture_c, aes(x=feature, y=value, fill = feature)) + theme_bw() + theme(legend.position = "none") + geom_histogram(stat='identity') + coord_flip()
# Boxplots are shown to highlight differences in features among species. At the purpose, we define the following utility function.

species_boxplot <- function(dataset, variable) {
  p <- ggplot(data = dataset, aes(x = species, y = eval(parse(text=variable)), fill= species)) + theme_bw() + theme(legend.position = "none") + geom_boxplot() + ylab(parse(text=variable))
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle(paste(variable, "among species", sep = " "))
  p
}
# Margin feature boxplot

# For each margin feature, a boxplot as shown below can be generated. Herein, the boxplot associated to the margin1 feature.
species_boxplot(margin_data, "margin1")
# If you are interested in having a summary report, you may take advantage of the following line of code.
with(margin_data, tapply(margin1, species, summary))

# Shape feature boxplot
# We show the boxplot for shape features by considering the shape20 as example.
species_boxplot(shape_data, "shape20")

species_boxplot(texture_data, "texture31")

# Saving the current enviroment for further analysis.
save.image(file='PlantLeafEnvironment.RData')

#######################
### Statistical Learning Model ###
#######################
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
set.seed(1023)

plant_leaf_model <- function(leaf_dataset) {
  
  train_idx <- createDataPartition(leaf_dataset$species, p = 0.7, list = FALSE)
  trControl <- trainControl(method = "repeatedcv",  number = 10, verboseIter = FALSE, repeats = 5)
  
  relevant_features <- setdiff(colnames(leaf_dataset), c("id", "species"))
  
  feat_sum <- paste(relevant_features, collapse = "+")
  frm <- as.formula(paste("species ~ ", feat_sum))
  
  train_set <- leaf_dataset[train_idx,]
  test_set <- leaf_dataset[-train_idx,]
  
  lda_fit <- train(frm, 
                   data = train_set,
                   method ="lda", 
                   tuneLength = 10,
                   preProcess = c("BoxCox", "center", "scale", "pca"),
                   trControl = trControl,
                   metric = 'Accuracy')
  
  lda_test_pred <- predict(lda_fit, test_set)
  
  cfm <- confusionMatrix(lda_test_pred, test_set$species)
  
  list(model=lda_fit, confusionMatrix = cfm)
}

# Margin dataset model
# We build our model for margin dataset only.
result <- plant_leaf_model(margin_data)

# Model fit results.
result$model

# Testset confusion matrix.
result$confusionMatrix$overall[1:4]


# We take notice of accuracy for final report.
margin_data_accuracy <- result$confusionMatrix$overall[1]

# Further details can be printed out by:
result$model$finalModel
varImp(result$model)

# that we do not show for brevity.
# Shape dataset model

# We build our model for shape dataset only.
result <- plant_leaf_model(shape_data)

# Model fit results.
result$model

# Testset confusion matrix.
result$confusionMatrix$overall[1:4]

# We take notice of accuracy for final report.
shape_data_accuracy <- result$confusionMatrix$overall[1]


# Texture dataset model

# We build our model for texture dataset only.
result <- plant_leaf_model(texture_data)

# Model fit results.
result$model
# Testset confusion matrix.
result$confusionMatrix$overall[1:4]
# We take notice of accuracy for final report.
texture_data_accuracy <- result$confusionMatrix$overall[1]

# Margin+Shape datasets model

# We build our model for margin and shape datasets.
margin_shape_data <- left_join(margin_data, shape_data[,-which(colnames(texture_data) %in% c("species"))], by=c("id"))
result <- plant_leaf_model(margin_shape_data)

# Model fit results.
result$model


# Testset confusion matrix.
result$confusionMatrix$overall[1:4]

# We take notice of accuracy for final report.
margin_shape_data_accuracy <- result$confusionMatrix$overall[1]

# Margin+Texture datasets model

# We build our model for margin and texture datasets.
margin_texture_data <- left_join(margin_data, texture_data[,-which(colnames(texture_data) %in% c("species"))], by=c("id"))
result <- plant_leaf_model(margin_texture_data)

# Model fit results.
result$model

# Testset confusion matrix.
result$confusionMatrix$overall[1:4]

# We take notice of accuracy for final report.
margin_texture_data_accuracy <- result$confusionMatrix$overall[1]


# Shape+Texture datasets model

# We build our model for shape and texture datasets.
shape_texture_data <- left_join(shape_data, texture_data[,-which(colnames(texture_data) %in% c("species"))], by=c("id"))
result <- plant_leaf_model(shape_texture_data)

# Model fit results.
result$model

# testset confusion matrix.
result$confusionMatrix$overall[1:4]

# We take notice of accuracy for final report.
shape_texture_data_accuracy <- result$confusionMatrix$overall[1]


# Margin+Shape+Texture datasets model

# We build our model for all three datasets.
leaves_data <- left_join(margin_data, shape_texture_data[,-which(colnames(texture_data) %in% c("species"))], by = c("id"))
result <- plant_leaf_model(leaves_data)

# Model fit results.
result$model

# Testset confusion matrix.
result$confusionMatrix$overall[1:4]
# We take notice of accuracy for final report.
leaf_data_accuracy <- result$confusionMatrix$overall[1]



### Final Results

# Finally, we gather all results in a data frame and show for comparison. The V symbol indicates when a dataset is selected for model building.
LDA_accuracy = c(shape_data_accuracy, texture_data_accuracy, margin_data_accuracy,
                 shape_texture_data_accuracy, margin_shape_data_accuracy, margin_texture_data_accuracy,
                 leaf_data_accuracy)
LDA_accuracy <- 100*LDA_accuracy # as percentage

results <- data.frame(SHA = c("V", " ", " ", "V", "V", " ", "V"),
                      TEX = c(" ", "V", " ", "V", " ", "V", "V"),
                      MAR = c(" ", " ", "V", " ", "V", "V", "V"),
                      prop_KNN_accuracy = c(62.13, 72.94, 75.00, 86.19, 87.19, 93.38, 96.81),
                      wprop_KNN__accuracy = c(61.88, 72.75, 75.75, 86.06, 86.75, 93.31, 96.69),
                      LDA_accuracy = LDA_accuracy
)

# Here are ref.[1] results compared to ours.
results













