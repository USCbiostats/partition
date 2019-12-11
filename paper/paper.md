---
author:
- name: Malcolm Barrett
  affiliation: Department of Preventive Medicine, University of Southern California
  orcid: 0000-0003-0299-5825
- name: Joshua Millstein
  affiliation: Department of Preventive Medicine, University of Southern California
date: '2019-12-10'
title: 'Partition: A fast and flexible framework for agglomerative partitioning in
  R'
output:
  distill::distill_article:
    keep_md: true
tags: ''
bibliography: references.bib
---



# Summary 

Data are increasingly wider; in modern genomics, for example, high-resolution genetic data captures much more information but also much more noise and redundancy. While the depth of this data is essential for science, it also introduces difficulties in computation and interpretability. Data reduction techniques, like principal components analysis and k-means clustering, are a vital part of addressing these issues, but they sometimes face further problems in scalability and information loss. The partition framework is an approach to data reduction that is flexible, scalable, and interpretable, developed to address information loss while maintaining speed. 
The partition R package, a fast and flexible tool for data reduction, implements the partition framework. partition uses an approach called Direct-Measure-Reduce to create agglomerative partitions that maintain the user-specified minimum level of information. Each variable begins as its own cluster. The user specifies the level of information that each reduction must retain, and potentially reduced variables only cluster if they meet this requirement. The reduced variables are also interpretable: original variables map to one and only one variable in the reduced data set. partition is flexible, as well: how variables are selected as candidates to reduce, how information loss is measured, and the way data is reduced can all be customized.

The design of the partition package is what allows the speed and flexibility of the data reduction; we use an approach called Direct-Measure-Reduce to modularize the partition framework. Partitioners tell the partition algorithm 1) what to try to reduce 2) how to measure how much information is lost from the reduction, and 3) how to reduce the data. In partition, functions that handle 1) are thus called directors, functions that handle 2) are called metrics, and functions that handle 3) are called reducers. partition has several pre-specified partitioners for agglomerative data reduction, but this approach is also quite flexible. 

The default partitioner uses a correlation-based distance matrix to find the pair of variables with the smallest distance between them, intraclass correlation (ICC) to measure information explained by the reduced variable, and scaled row means to reduce variables with a sufficient minimum ICC. This approach is fast reliable, but other data reduction strategies, such as principal components analysis, are also implemented in partition. As the framework is agnostic to how reductions are being directed, measured, or reduced, custom partitioners are easy to implement. Users may apply tools from base R or other R packages at any of the three stages of the partition.

While many tools exist in R for data reduction, including many functions in base R, partition offers a fast and flexible tool that addresses the need for interpretability and information retention in reduced variables. Wide data, such as high-resolution genetic data, can be quickly reduced without sacrificing information. Additionally, as each feature of the data maps to a single cluster, the resulting partition remains interpretable for inference. partition is also flexible, adapting to the needs of many data reduction strategies.

# Funding and Support

This work is supported by the National Cancer Institute (NCI), Award Number 5P01CA196569.

# References
