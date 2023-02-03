library(xgboost)
library(Matrix)
library(dplyr)

group_names <- c('angola', 'benin', 'burkina_faso', 'cameroon', 'cote_d_ivoire',
                 'democratic_republic_of_congo', 'ethiopia', 'ghana', 'guinea', 'kenya',
                 'lesotho', 'malawi', 'mali', 'mozambique', 'nigeria', 'rwanda', 'senegal',
                 'sierra_leone', 'tanzania', 'togo', 'uganda', 'zambia', 'zimbabwe')



DHS <- read.csv("/africa_poverty_clean-main/data/dhs_clusters_19047.csv", header = TRUE)
labels <- DHS[,"wealthpooled"]
group_labels <- DHS[,'country_labels']+1


#------------------------------- XGBoost PCA features --------------------------------------

XGBoost <- function(labels, group_names, feature_path, output_path){
  N = length(labels)
  all_preds <- matrix(0,length(labels))
  all_country_MSE_hyperparams <- matrix(0,length(group_names),6)
  
  
  for (f in group_names){
    cat('reading',f,'...')
    filePath <- paste0("/africa_poverty_clean-main/outputs/dhs_ooc/PCA/",feature_path,'/',f,'.csv')
    data <- read.csv(filePath, header = FALSE)
    features <- as.matrix(data)
    
    cat('Group:', f, '\n')
    i <- which(group_names == f)
    
    test_indices <- which(group_labels == i)
    train_indices <- which(group_labels != i)
    
    X <- as.matrix(features[train_indices,])
    y <- labels[train_indices]
    
    test_X <- as.matrix(features[test_indices,])
    test_y <- labels[test_indices]
    
    dtrain <- xgb.DMatrix(data = X, label = y) 
    dtest <- xgb.DMatrix(data = test_X, label = test_y)
    
    
    hyper_grid <- expand.grid(
      eta =c(0.01, 0.005, 0.1),
      max_depth= c(1, 2),
      subsample = c(0.1,0.2,0.5, 1),
      nrounds = c(300,400,500,1000),
      MSE = 0
    )
    
    # 大部分
    # hyper_grid <- expand.grid(
    #   eta =c(0.005,0.01),
    #   max_depth= c(2, 4, 6),
    #   subsample = c(0.2,0.5,1),
    #   nrounds = c(1000,2000,3000),
    #   MSE = 0
    # )
    
    # # democratic_republic_of_congo
    # hyper_grid <- expand.grid(
    #   eta =c(0.1),
    #   max_depth= c(1),
    #   subsample = c(0.1),
    #   nrounds = c(1000),
    #   MSE = 0
    # )
    # 
    # # ghana
    # hyper_grid <- expand.grid(
    #   eta =c(0.01),
    #   max_depth= c(4),
    #   subsample = c(0.5),
    #   nrounds = c(300),
    #   MSE = 0
    # )
    # 
    # # mali
    # hyper_grid <- expand.grid(
    #   eta =c(0.004,0.005),
    #   max_depth= c(4),
    #   subsample = c(1),
    #   nrounds = c(400),
    #   MSE = 0
    #   )
    # 
    # # mozambique
    # hyper_grid <- expand.grid(
    #   eta =c(0.01, 0.05, 0.1),
    #   max_depth= c(1, 2, 4),
    #   subsample = c(0.2,0.5, 1),
    #   nrounds = c(100,300,1000),
    #   MSE = 0
    # )
    # 
    # # senegal
    # hyper_grid <- expand.grid(
    #   eta =c(0.004,0.003,0.005),
    #   max_depth= c(2),
    #   subsample = c(0.2,0.5),
    #   nrounds = c(300),
    #   MSE = 0
    # )
    # 
    # # togo
    # hyper_grid <- expand.grid(
    #   eta =c(0.005),
    #   max_depth= c(2),
    #   subsample = c(0.1,0.2),
    #   nrounds = c(300,400,500),
    #   MSE = 0
    # )
    # 
    # # uganda 
    # # hyper_grid <- expand.grid(
    # #   eta =c(0.01),
    # #   max_depth= c(1),
    # #   subsample = c(1),
    # #   nrounds = c(500),
    # #   MSE = 0
    # # )
    
    
    cat("组合数：", nrow(hyper_grid), '\n')
    preds <- matrix(0,nrow(hyper_grid),length(test_y))
    
    for (j in 1:nrow(hyper_grid)){
      cat("第", j, "行：", '\n')
      
      set.seed(123)
      xgb <- xgboost(data = dtrain, 
                     eta = hyper_grid$eta[j],
                     max_depth = hyper_grid$max_depth[j],
                     subsample = hyper_grid$subsample[j],
                     nrounds = hyper_grid$nrounds[j])

      
      # 预测值t_preds
      test_preds <- predict(xgb, newdata = dtest)
      preds[j,] <- test_preds
      hyper_grid$MSE[j] <- mean((test_preds - test_y)^2)
      
    }
    
    # 我们将结果依据MSE由小至大排列，取模型成效前十名印出
    print(hyper_grid[order(hyper_grid$MSE),]%>%head(10))
    
    
    # 保存MSE最小的对应的预测值
    all_preds[test_indices] <- preds[which.min(hyper_grid$MSE),]
    #write.csv(preds[which.min(hyper_grid$MSE),],paste('E:/africa_poverty_clean-main/outputs/dhs_ooc/resnet_msnl_concat_pca/XGBoost_pca/R_XGBoost_preds/',f,'.csv'),row.names=FALSE)
    
    # 保存国家和最小MSE
    all_country_MSE_hyperparams[i,1] <- f
    all_country_MSE_hyperparams[i,2] <- min(hyper_grid$MSE)
    all_country_MSE_hyperparams[i,3:6] <- as.matrix(hyper_grid[which.min(hyper_grid$MSE),1:4])
    
  }
  
  
  # 保存all_country_MSE_hyperparams
  colnames(all_country_MSE_hyperparams) <- c('test_country','MSE','eta','max_depth','subsample','nrounds')
  XGBoost_path = paste0('/africa_poverty_clean-main/outputs/dhs_ooc/PCA/',output_path,'/XGBoost_logo_pca')
  dir.create(XGBoost_path)
  write.csv(all_country_MSE_hyperparams, paste0(XGBoost_path,'/XGBoost_all_country_MSE_hyperparams_pca.csv'),row.names=FALSE)  
  
  
  # 保存all_preds
  write.csv(all_preds, paste0(XGBoost_path,'/XGBoost_all_preds_pca.csv'),row.names=FALSE) 
  
  XGBoost_PCA_list<- list(all_country_MSE_hyperparams = all_country_MSE_hyperparams,
                          all_preds = all_preds)
  
  return(XGBoost_PCA_list)
}

XGBoost_ms_PCA <- XGBoost(labels = labels, group_names = group_names, feature_path = 'features_ms_pca', output_path = 'resnet_ms_pca')
XGBoost_nl_PCA <- XGBoost(labels = labels, group_names = group_names, feature_path = 'features_nl_pca', output_path = 'resnet_nl_pca') 
XGBoost_msnl_PCA <- XGBoost(labels = labels, group_names = group_names, feature_path = 'features_msnl_pca', output_path = 'resnet_msnl_pca') 

#------------------------------- XGBoost PCA_0.99 features --------------------------------------

XGBoost <- function(labels, group_names, feature_path, output_path){
  N = length(labels)
  all_preds <- matrix(0,length(labels))
  all_country_MSE_hyperparams <- matrix(0,length(group_names),6)
  
  
  for (f in group_names){
    cat('reading',f,'...')
    filePath <- paste0("/africa_poverty_clean-main/outputs/dhs_ooc/PCA_099/",feature_path,'/',f,'.csv')
    data <- read.csv(filePath, header = FALSE)
    features <- as.matrix(data)
    
    cat('Group:', f, '\n')
    i <- which(group_names == f)
    
    test_indices <- which(group_labels == i)
    train_indices <- which(group_labels != i)
    
    X <- as.matrix(features[train_indices,])
    y <- labels[train_indices]
    
    test_X <- as.matrix(features[test_indices,])
    test_y <- labels[test_indices]
    
    dtrain <- xgb.DMatrix(data = X, label = y) 
    dtest <- xgb.DMatrix(data = test_X, label = test_y)
    
    
    hyper_grid <- expand.grid(
      eta =c(0.01, 0.005, 0.1),
      max_depth= c(1, 2),
      subsample = c(0.1,0.2,0.5, 1),
      nrounds = c(300,400,500,1000),
      MSE = 0
    )
    
    
    cat("组合数：", nrow(hyper_grid), '\n')
    preds <- matrix(0,nrow(hyper_grid),length(test_y))
    
    for (j in 1:nrow(hyper_grid)){
      cat("第", j, "行：", '\n')
      
      set.seed(123)
      xgb <- xgboost(data = dtrain, 
                     eta = hyper_grid$eta[j],
                     max_depth = hyper_grid$max_depth[j],
                     subsample = hyper_grid$subsample[j],
                     nrounds = hyper_grid$nrounds[j])
      
      
      # 预测值t_preds
      test_preds <- predict(xgb, newdata = dtest)
      preds[j,] <- test_preds
      hyper_grid$MSE[j] <- mean((test_preds - test_y)^2)
      
    }
    
    # 我们将结果依据MSE由小至大排列，取模型成效前十名印出
    print(hyper_grid[order(hyper_grid$MSE),]%>%head(10))
    
    
    # 保存MSE最小的对应的预测值
    all_preds[test_indices] <- preds[which.min(hyper_grid$MSE),]
    #write.csv(preds[which.min(hyper_grid$MSE),],paste('E:/africa_poverty_clean-main/outputs/dhs_ooc/resnet_msnl_concat_pca/XGBoost_pca/R_XGBoost_preds/',f,'.csv'),row.names=FALSE)
    
    # 保存国家和最小MSE
    all_country_MSE_hyperparams[i,1] <- f
    all_country_MSE_hyperparams[i,2] <- min(hyper_grid$MSE)
    all_country_MSE_hyperparams[i,3:6] <- as.matrix(hyper_grid[which.min(hyper_grid$MSE),1:4])
    
  }
  
  
  # 保存all_country_MSE_hyperparams
  colnames(all_country_MSE_hyperparams) <- c('test_country','MSE','eta','max_depth','subsample','nrounds')
  XGBoost_path = paste0('/africa_poverty_clean-main/outputs/dhs_ooc/PCA_099/',output_path,'/XGBoost_logo_pca099')
  dir.create(XGBoost_path)
  write.csv(all_country_MSE_hyperparams, paste0(XGBoost_path,'/XGBoost_all_country_MSE_hyperparams_pca099.csv'),row.names=FALSE)  
  
  
  # 保存all_preds
  write.csv(all_preds, paste0(XGBoost_path,'/XGBoost_all_preds_pca099.csv'),row.names=FALSE) 
  
  XGBoost_PCA_list<- list(all_country_MSE_hyperparams = all_country_MSE_hyperparams,
                          all_preds = all_preds)
  
  return(XGBoost_PCA_list)
}

XGBoost_ms_PCA_099 <- XGBoost(labels = labels, group_names = group_names, feature_path = 'features_ms_pca099', output_path = 'resnet_ms_pca099')
XGBoost_nl_PCA_099 <- XGBoost(labels = labels, group_names = group_names, feature_path = 'features_nl_pca099', output_path = 'resnet_nl_pca099') 
XGBoost_msnl_PCA_099 <- XGBoost(labels = labels, group_names = group_names, feature_path = 'features_msnl_pca099', output_path = 'resnet_msnl_pca099') 

#------------------------------- XGBoost UMAP features --------------------------------------
XGBoost <- function(labels, group_names, feature_path, output_path){
  N = length(labels)
  all_preds <- matrix(0,length(labels))
  all_country_MSE_hyperparams <- matrix(0,length(group_names),6)
  
  
  for (f in group_names){
    cat('reading',f,'...')
    filePath <- paste0("/africa_poverty_clean-main/outputs/dhs_ooc/UMAP/",feature_path,'/',f,'.csv')
    data <- read.csv(filePath, header = FALSE)
    features <- as.matrix(data)
    
    cat('Group:', f, '\n')
    i <- which(group_names == f)
    
    test_indices <- which(group_labels == i)
    train_indices <- which(group_labels != i)
    
    X <- as.matrix(features[train_indices,])
    y <- labels[train_indices]
    
    test_X <- as.matrix(features[test_indices,])
    test_y <- labels[test_indices]
    
    dtrain <- xgb.DMatrix(data = X, label = y) 
    dtest <- xgb.DMatrix(data = test_X, label = test_y)
    
    
    hyper_grid <- expand.grid(
      eta =c(0.01, 0.005, 0.1),
      max_depth= c(1, 2),
      subsample = c(0.1,0.2,0.5, 1),
      nrounds = c(300,400,500,1000),
      MSE = 0
    )
    
    # 大部分
    # hyper_grid <- expand.grid(
    #   eta =c(0.005,0.01),
    #   max_depth= c(2, 4, 6),
    #   subsample = c(0.2,0.5,1),
    #   nrounds = c(1000,2000,3000),
    #   MSE = 0
    # )
    
    # # democratic_republic_of_congo
    # hyper_grid <- expand.grid(
    #   eta =c(0.1),
    #   max_depth= c(1),
    #   subsample = c(0.1),
    #   nrounds = c(1000),
    #   MSE = 0
    # )
    # 
    # # ghana
    # hyper_grid <- expand.grid(
    #   eta =c(0.01),
    #   max_depth= c(4),
    #   subsample = c(0.5),
    #   nrounds = c(300),
    #   MSE = 0
    # )
    # 
    # # mali
    # hyper_grid <- expand.grid(
    #   eta =c(0.004,0.005),
    #   max_depth= c(4),
    #   subsample = c(1),
    #   nrounds = c(400),
    #   MSE = 0
    #   )
    # 
    # # mozambique
    # hyper_grid <- expand.grid(
    #   eta =c(0.01, 0.05, 0.1),
    #   max_depth= c(1, 2, 4),
    #   subsample = c(0.2,0.5, 1),
    #   nrounds = c(100,300,1000),
    #   MSE = 0
    # )
    # 
    # # senegal
    # hyper_grid <- expand.grid(
    #   eta =c(0.004,0.003,0.005),
    #   max_depth= c(2),
    #   subsample = c(0.2,0.5),
    #   nrounds = c(300),
    #   MSE = 0
    # )
    # 
    # # togo
    # hyper_grid <- expand.grid(
    #   eta =c(0.005),
    #   max_depth= c(2),
    #   subsample = c(0.1,0.2),
    #   nrounds = c(300,400,500),
    #   MSE = 0
    # )
    # 
    # # uganda 
    # # hyper_grid <- expand.grid(
    # #   eta =c(0.01),
    # #   max_depth= c(1),
    # #   subsample = c(1),
    # #   nrounds = c(500),
    # #   MSE = 0
    # # )
    
    
    cat("组合数：", nrow(hyper_grid), '\n')
    preds <- matrix(0,nrow(hyper_grid),length(test_y))
    
    for (j in 1:nrow(hyper_grid)){
      cat("第", j, "行：", '\n')
      
      set.seed(123)
      xgb <- xgboost(data = dtrain, 
                     eta = hyper_grid$eta[j],
                     max_depth = hyper_grid$max_depth[j],
                     subsample = hyper_grid$subsample[j],
                     nrounds = hyper_grid$nrounds[j])
      
      
      # 预测值t_preds
      test_preds <- predict(xgb, newdata = dtest)
      preds[j,] <- test_preds
      hyper_grid$MSE[j] <- mean((test_preds - test_y)^2)
      
    }
    
    # 我们将结果依据MSE由小至大排列，取模型成效前十名印出
    print(hyper_grid[order(hyper_grid$MSE),]%>%head(10))
    
    
    # 保存MSE最小的对应的预测值
    all_preds[test_indices] <- preds[which.min(hyper_grid$MSE),]
    #write.csv(preds[which.min(hyper_grid$MSE),],paste('E:/africa_poverty_clean-main/outputs/dhs_ooc/resnet_msnl_concat_pca/XGBoost_pca/R_XGBoost_preds/',f,'.csv'),row.names=FALSE)
    
    # 保存国家和最小MSE
    all_country_MSE_hyperparams[i,1] <- f
    all_country_MSE_hyperparams[i,2] <- min(hyper_grid$MSE)
    all_country_MSE_hyperparams[i,3:6] <- as.matrix(hyper_grid[which.min(hyper_grid$MSE),1:4])
    
  }
  
  
  # 保存all_country_MSE_hyperparams
  colnames(all_country_MSE_hyperparams) <- c('test_country','MSE','eta','max_depth','subsample','nrounds')
  XGBoost_path = paste0('/africa_poverty_clean-main/outputs/dhs_ooc/UMAP/',output_path,'/XGBoost_logo_UMAP')
  dir.create(XGBoost_path)
  write.csv(all_country_MSE_hyperparams, paste0(XGBoost_path,'/XGBoost_all_country_MSE_hyperparams_UMAP.csv'),row.names=FALSE)  
  
  
  # 保存all_preds
  write.csv(all_preds, paste0(XGBoost_path,'/XGBoost_all_preds_UMAP.csv'),row.names=FALSE) 
  
  XGBoost_UMAP_list<- list(all_country_MSE_hyperparams = all_country_MSE_hyperparams,
                          all_preds = all_preds)
  
  return(XGBoost_UMAP_list)
}

XGBoost_ms_UMAP <- XGBoost(labels = labels, group_names = group_names, feature_path = 'features_ms_UMAP', output_path = 'resnet_ms_UMAP')
XGBoost_nl_UMAP <- XGBoost(labels = labels, group_names = group_names, feature_path = 'features_nl_UMAP', output_path = 'resnet_nl_UMAP') 
XGBoost_msnl_UMAP <- XGBoost(labels = labels, group_names = group_names, feature_path = 'features_msnl_UMAP', output_path = 'resnet_msnl_UMAP') 


