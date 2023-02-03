library(ranger)
library(MASS)
library(Matrix)
library(ggplot2)
library(dplyr)
library(reshape2)
library(glmnet)
library(scales)
library(rfinterval)


group_names <- c('angola', 'benin', 'burkina_faso', 'cameroon', 'cote_d_ivoire',
                 'democratic_republic_of_congo', 'ethiopia', 'ghana', 'guinea', 'kenya',
                 'lesotho', 'malawi', 'mali', 'mozambique', 'nigeria', 'rwanda', 'senegal',
                 'sierra_leone', 'tanzania', 'togo', 'uganda', 'zambia', 'zimbabwe')


DHS <- read.csv("/africa_poverty_clean-main/data/dhs_clusters_19047.csv", header = TRUE)
labels <- DHS[,"wealthpooled"]
group_labels <- DHS[,'country_labels']+1

#------------------------------- Random Forest PCA features --------------------------------------

RandomForest <- function(labels, group_names, feature_path, output_path){
  N = length(labels)
  all_preds <- matrix(0,length(labels))
  all_country_MSE_hyperparams <- matrix(0,length(group_names),5)
  
  
  for (f in group_names){
    cat('reading',f,'...')
    filePath <- paste0("/africa_poverty_clean-main/outputs/dhs_ooc/PCA/",feature_path,'/',f,'.csv')
    data <- read.csv(filePath, header = FALSE)
    features <- as.matrix(data)
    
    cat('Group:', f, '\n')
    i <- which(group_names == f)
    
    test_indices <- which(group_labels == i)
    train_indices <- which(group_labels != i)
    
    X <- features[train_indices,]
    y <- labels[train_indices]
    
    test_X <- features[test_indices,]
    test_y <- labels[test_indices]
    
    train_data <- data.frame(X = X, y = y)
    test_data <- data.frame(X = test_X, y = test_y)
    
    # hyperparameter grid search V1
    hyper_grid <- expand.grid(
      num.trees =c(30,50,100,150,200), 
      mtry = seq(1,dim(features)[2]),
      max.depth= c(5,10,15),
      MSE = 0
    )
    
    cat("组合数：", nrow(hyper_grid), '\n')
    preds <- matrix(0,nrow(hyper_grid),length(test_y))
    
    for (j in 1:nrow(hyper_grid)){
      cat("第", j, "行：", '\n')
      
      models <- ranger(
        formula = y ~ .,
        data = train_data,
        num.trees = hyper_grid$num.trees[j],
        mtry = hyper_grid$mtry[j],
        # min.node.size= hyper_grid$min.node.size[j],
        max.depth=hyper_grid$max.depth[j],
        importance = 'impurity',
        seed = 123
      )
      
      # 预测值t_preds
      test_preds <- predict(models, data = test_data)$predictions
      preds[j,] <- test_preds
      hyper_grid$MSE[j] <- mean((test_preds - test_y)^2)
      
    }
    
    # 我们将结果依据MSE由小至大排列，取模型成效前十名印出
    print(hyper_grid[order(hyper_grid$MSE),]%>%head(10))
    
    # 保存MSE最小的对应的预测值
    all_preds[test_indices] <- preds[which.min(hyper_grid$MSE),]
    
    # 保存国家和最小MSE
    all_country_MSE_hyperparams[i,1] <- f
    all_country_MSE_hyperparams[i,2] <- min(hyper_grid$MSE)
    all_country_MSE_hyperparams[i,3:5] <- as.matrix(hyper_grid[which.min(hyper_grid$MSE),1:3])
    
  }
  
  # 保存all_country_MSE_hyperparams
  colnames(all_country_MSE_hyperparams) <- c('test_country','MSE','num.trees','mtry','max.depth')
  RF_path = paste0('/africa_poverty_clean-main/outputs/dhs_ooc/PCA/',output_path,'/RF_logo_pca')
  dir.create(RF_path)
  write.csv(all_country_MSE_hyperparams, paste0(RF_path,'/RF_all_country_MSE_hyperparams.csv'),row.names=FALSE)  
  
  # 保存all_preds
  write.csv(all_preds, paste0(RF_path,'/RF_all_preds.csv'),row.names=FALSE)  
  
  RF_PCA_list<- list(all_country_MSE_hyperparams = all_country_MSE_hyperparams,
                     all_preds = all_preds
                     )
  
  return(RF_PCA_list)
}

RF_ms_PCA <- RandomForest(labels = labels, group_names = group_names, feature_path = 'features_ms_pca', output_path = 'resnet_ms_pca') 
RF_nl_PCA <- RandomForest(labels = labels, group_names = group_names, feature_path = 'features_nl_pca', output_path = 'resnet_nl_pca') 
RF_msnl_PCA <- RandomForest(labels = labels, group_names = group_names, feature_path = 'features_msnl_pca', output_path = 'resnet_msnl_pca') 


#------------------------------- Random Forest PCA_0.99 features ----------------------------------

RandomForest <- function(labels, group_names, feature_path, output_path){
  N = length(labels)
  all_preds <- matrix(0,length(labels))
  all_country_MSE_hyperparams <- matrix(0,length(group_names),5)
  
  
  for (f in group_names){
    cat('reading',f,'...')
    filePath <- paste0("/africa_poverty_clean-main/outputs/dhs_ooc/PCA_099/",feature_path,'/',f,'.csv')
    data <- read.csv(filePath, header = FALSE)
    features <- as.matrix(data)
    
    cat('Group:', f, '\n')
    i <- which(group_names == f)
    
    test_indices <- which(group_labels == i)
    train_indices <- which(group_labels != i)
    
    X <- features[train_indices,]
    y <- labels[train_indices]
    
    test_X <- features[test_indices,]
    test_y <- labels[test_indices]
    
    train_data <- data.frame(X = X, y = y)
    test_data <- data.frame(X = test_X, y = test_y)
    
    # hyperparameter grid search V1
    hyper_grid <- expand.grid(
      num.trees =c(30,50,100,150,200), 
      mtry = seq(1,dim(features)[2]),
      max.depth= c(5,10,15),
      MSE = 0
    )
    
    cat("组合数：", nrow(hyper_grid), '\n')
    preds <- matrix(0,nrow(hyper_grid),length(test_y))
    
    for (j in 1:nrow(hyper_grid)){
      cat("第", j, "行：", '\n')
      
      models <- ranger(
        formula = y ~ .,
        data = train_data,
        num.trees = hyper_grid$num.trees[j],
        mtry = hyper_grid$mtry[j],
        # min.node.size= hyper_grid$min.node.size[j],
        max.depth=hyper_grid$max.depth[j],
        importance = 'impurity',
        seed = 123
      )
      
      # 预测值t_preds
      test_preds <- predict(models, data = test_data)$predictions
      preds[j,] <- test_preds
      hyper_grid$MSE[j] <- mean((test_preds - test_y)^2)
      
    }
    
    # 我们将结果依据MSE由小至大排列，取模型成效前十名印出
    print(hyper_grid[order(hyper_grid$MSE),]%>%head(10))
    
    # 保存MSE最小的对应的预测值
    all_preds[test_indices] <- preds[which.min(hyper_grid$MSE),]
    
    # 保存国家和最小MSE
    all_country_MSE_hyperparams[i,1] <- f
    all_country_MSE_hyperparams[i,2] <- min(hyper_grid$MSE)
    all_country_MSE_hyperparams[i,3:5] <- as.matrix(hyper_grid[which.min(hyper_grid$MSE),1:3])
    
  }
  
  # 保存all_country_MSE_hyperparams
  colnames(all_country_MSE_hyperparams) <- c('test_country','MSE','num.trees','mtry','max.depth')
  RF_path = paste0('/africa_poverty_clean-main/outputs/dhs_ooc/PCA_099/',output_path,'/RF_logo_pca099')
  dir.create(RF_path)
  write.csv(all_country_MSE_hyperparams, paste0(RF_path,'/RF_all_country_MSE_hyperparams.csv'),row.names=FALSE)  
  
  # 保存all_preds
  write.csv(all_preds, paste0(RF_path,'/RF_all_preds.csv'),row.names=FALSE)  
  
  RF_PCA_list<- list(all_country_MSE_hyperparams = all_country_MSE_hyperparams,
                     all_preds = all_preds
  )
  
  return(RF_PCA_list)
}

RF_ms_PCA_099 <- RandomForest(labels = labels, group_names = group_names, feature_path = 'features_ms_pca099', output_path = 'resnet_ms_pca099') 
RF_nl_PCA_099 <- RandomForest(labels = labels, group_names = group_names, feature_path = 'features_nl_pca099', output_path = 'resnet_nl_pca099') 
RF_msnl_PCA_099 <- RandomForest(labels = labels, group_names = group_names, feature_path = 'features_msnl_pca099', output_path = 'resnet_msnl_pca099') 


#------------------------------- Random Forest UMAP features --------------------------------------

RandomForest <- function(labels, group_names, feature_path, output_path){
  N = length(labels)
  all_preds <- matrix(0,length(labels))
  all_country_MSE_hyperparams <- matrix(0,length(group_names),5)
  
  
  for (f in group_names){
    cat('reading',f,'...')
    filePath <- paste0("/africa_poverty_clean-main/outputs/dhs_ooc/UMAP/",feature_path,'/',f,'.csv')
    data <- read.csv(filePath, header = FALSE)
    features <- as.matrix(data)
    
    cat('Group:', f, '\n')
    i <- which(group_names == f)
    
    test_indices <- which(group_labels == i)
    train_indices <- which(group_labels != i)
    
    X <- features[train_indices,]
    y <- labels[train_indices]
    
    test_X <- features[test_indices,]
    test_y <- labels[test_indices]
    
    train_data <- data.frame(X = X, y = y)
    test_data <- data.frame(X = test_X, y = test_y)
    
    # hyperparameter grid search V1
    hyper_grid <- expand.grid(
      num.trees =c(30,50,100,150,200), 
      mtry = seq(1,dim(features)[2]),
      max.depth= c(5,10,15),
      MSE = 0
    )
    
    cat("组合数：", nrow(hyper_grid), '\n')
    preds <- matrix(0,nrow(hyper_grid),length(test_y))
    
    for (j in 1:nrow(hyper_grid)){
      cat("第", j, "行：", '\n')
      
      models <- ranger(
        formula = y ~ .,
        data = train_data,
        num.trees = hyper_grid$num.trees[j],
        mtry = hyper_grid$mtry[j],
        max.depth=hyper_grid$max.depth[j],
        importance = 'impurity',
        seed = 123
      )
      
      # 预测值t_preds
      test_preds <- predict(models, data = test_data)$predictions
      preds[j,] <- test_preds
      hyper_grid$MSE[j] <- mean((test_preds - test_y)^2)
      
    }
    
    # 我们将结果依据MSE由小至大排列，取模型成效前十名印出
    print(hyper_grid[order(hyper_grid$MSE),]%>%head(10))
    
    # 保存MSE最小的对应的预测值
    all_preds[test_indices] <- preds[which.min(hyper_grid$MSE),]
    
    # 保存国家和最小MSE
    all_country_MSE_hyperparams[i,1] <- f
    all_country_MSE_hyperparams[i,2] <- min(hyper_grid$MSE)
    all_country_MSE_hyperparams[i,3:5] <- as.matrix(hyper_grid[which.min(hyper_grid$MSE),1:3])
    
  }
  
  # 保存all_country_MSE_hyperparams
  colnames(all_country_MSE_hyperparams) <- c('test_country','MSE','num.trees','mtry','max.depth')
  RF_path = paste0('/africa_poverty_clean-main/outputs/dhs_ooc/UMAP/',output_path,'/RF_logo_UMAP')
  dir.create(RF_path)
  write.csv(all_country_MSE_hyperparams, paste0(RF_path,'/RF_all_country_MSE_hyperparams.csv'),row.names=FALSE)  
  
  # 保存all_preds
  write.csv(all_preds, paste0(RF_path,'/RF_all_preds.csv'),row.names=FALSE)  
  
  RF_PCA_list<- list(all_country_MSE_hyperparams = all_country_MSE_hyperparams,
                     all_preds = all_preds
  )
  
  return(RF_PCA_list)
}

RF_ms_UMAP <- RandomForest(labels = labels, group_names = group_names, feature_path = 'features_ms_UMAP', output_path = 'resnet_ms_UMAP') 
RF_nl_UMAP <- RandomForest(labels = labels, group_names = group_names, feature_path = 'features_nl_UMAP', output_path = 'resnet_nl_UMAP') 
RF_msnl_UMAP <- RandomForest(labels = labels, group_names = group_names, feature_path = 'features_msnl_UMAP', output_path = 'resnet_msnl_UMAP') 


