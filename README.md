## Revealing uncertainties of economic well-being’s prediction in Africa with remote sensing imagery data

This repository includes the part of code and data necessary to reproduce the results and figures for the article "Revealing uncertainties of economic well-being’s prediction in Africa with remote sensing imagery data"



## Table of Contents

* [Computing Requirements](#computing-requirements)
* [Getting Data](#getting-data)
* [Feature Extraction Network](#feature-extraction-network)
* [Point Prediction](#point-prediction)
* [Interval Prediction](#interval-prediction)
* [Results Analysis](#results-analysis)

## Computing Requirements

This code was tested on a system with the following specifications:

- operating system: Ubuntu 16.04.6 LTS 
- CPU: Intel(R) Core(TM)i9-9900K CPU @ 3.60GHz
- memory (RAM): 125GB
- disk storage: 500GB
- GPU: 1x NVIDIA Titan Xp

The main software requirements are Python 3.7 with TensorFlow r1.15, and R 4.1.2 


## Getting Data

  - `data/dhs_clusters_19047.xls`are the wealth index (following Yeh, C., Perez, A., Driscoll, A. et al. Using publicly available satellite imagery and deep learning to understand economic well-being in Africa. Nat Commun 11, 2583 (2020).)
  - `data/shapefiles/` are images download by Google Earth Engine
  - Follow the files in the `preprocessing/` directory to download the necessary satellite images, shapefiles, and trained model checkpoints.

## Feature Extraction Network

- Run the `extract_features.py` script to use the trained CNN models to extract feature vectors from each satellite image.

## Point Prediction

Follow the  files in the `point_prediction/` to train point prediction models.

models      | files     
------------|----------------------
Ridge     | `dhs_resnet_ridge`、`dhs_resnet_ridge_pca`、`dhs_resnet_ridge_UMAP`                
RandomForest | `point_prediction/RandomForest`               
XGBoost    | `point_prediction/XGBoost` 
NGBoost    | `interval_prediction/NGB`



## Interval Prediction 

Follow the  files in the `interval_prediction/` to train point prediction models.

models      | files     
------------|----------------------
Jacknife+ after Bootstrap     | `J+aB_interval_predict_PCA_RandomForest`、`J+aB_interval_predict_PCA_Ridge`、`J+aB_interval_predict_PCA_XGBoost`、`J+aB_interval_predict_UMAP_RandomForest`、`J+aB_interval_predict_UMAP_Ridge`、`J+aB_interval_predict_UMAP_XGBoost`              
Split Conform | `interval_prediction/SC`           
NGBoost    | `interval_prediction/NGB` 

## Results Analysis

Use the following notebooks to analyze the results:
  - `results_analysis/point_analysis.py`
  - `results_analysis/interval_analysis.py`

The analysis results are stored in `output/dhs_ooc/results`
