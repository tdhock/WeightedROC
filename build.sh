#!/bin/bash
cd ..
rm -rf WeightedROC-release
cp -r WeightedROC WeightedROC-release
##grep -v WeightedROCData WeightedROC/DESCRIPTION | grep -v Remotes > WeightedROC-release/DESCRIPTION
##rm WeightedROC-release/tests/testthat/test-WeightedROCData.R
PKG_TGZ=$(R CMD build WeightedROC-release|grep building|sed 's/.*‘//'|sed 's/’.*//')
R CMD INSTALL $PKG_TGZ
R CMD check --as-cran $PKG_TGZ
