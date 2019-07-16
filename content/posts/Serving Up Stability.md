---
author: Anukriti Gupta
date: "2017-09-15"
linktitle: Serving Up Stability
tags:
- MachineLearning
- Hackathon
title: Serving Up Stability - HALICON OPTUM Datahack
weight: 10
---

## BACKGROUND

Service Outage is when system fails to provide or perform its primary function. 

## PROBLEM

The Problem is to predict service outage or probability associated with it at certain point of time


## Data

 * CPU Related – CPUPercent, SystemModeCPUPercent, UserModeCPUPercent
 * Network Related - NetworkPacketRate ,NetworkInboundPacketRate , NetworkOutboundPacketRate 
 * Space Related - ActiveProcesses RunQueue PeakDiskUtilization PhysicalDiskIORate PhysicalDiskByteRate                  MemoryPageoutRate Memory Utilization SwapSpaceUtilization PeakFilesystemSpaceUtilization 

## Approach

 * *Feature extraction:* Time series features are extracted from the data using the date-time field like day, month,         weekday and hour etc. coming as some of the most significant variables in the model
 * *Feature selection :* The key step post feature extraction was to choose features which were best predicting the     service outages. Correlation of service outages with key features were observed and only features having positively associated with outage were selected compared to usual approach of having all the features
 * *Model Used:*  The Random forest classifier was used to predict the probabilities of service outage. It’s an decision tree ensemble method which takes mandate from different trees to predict target variable. Different iteration of tree depth and number of tree were tested for best OOB error rate and optimally 200 Decision trees with depth of 5 gave best result

## Model Accuracy
 AUC - 98.06954
 
 ![Fig 1. Variable Importance](/img/SUS.png)
 