# POTAUGAEM

## Reference Paper
http://www.scirp.org/Journal/PaperInformation.aspx?PaperID=80106

##  Live predicting site
https://mokuzenshingo.github.io/GANNTrader/Untitled.html

##  Performance analysis
https://mokuzenshingo.shinyapps.io/return_analysis/

## How to training

set working directory
``` setwd(./R/) ```

### step 1. train the agents

* script file : train.R
* output file : ./data/result/symbolname_@@.rds

Commands for batch processing  
``` R —slave —vanilla —args “outputfile_number” < train.R ``` 

### step 2. bind training results

* script file : bind.R
* output file : ./data/symbolname_train500.rds

### step 3. validation of training results

* script file : valid.R
* output file : ./data/valid/symbolname_valid_@@.rds

### step 4. bind validation results

* script file : bind.R
* output file : ./data/symbolname_valid500.rds

### step 5. select the generation for testing

* script file : valid_select.R
* output file : ./data/symbolname_bestGeneration.rds

### step 6. test ensemble method

* script file : test_ense.R
* output file : ./data/symbolname_ense_orderbook.rds

### step 7. test average performance

* script file : test_avg.R
* output file : ./data/symbolname_avg_orderbook.rds

### step 8. Comparison of result between the Ensemble and the Average)

* script file : report.R






