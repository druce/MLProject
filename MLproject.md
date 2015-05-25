MLproject
========================================================
author: Druce Vertes
date: 5/24/2015

ML Project
========================================================

- Load data from pml-training.csv
Load data
========================================================


```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
# Load data from pml-training.csv
colclasses = c("numeric","character","numeric","numeric","character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","character")
pmlframe = read.csv("pml-training.csv", 
                    header=TRUE, sep=",",
                    colClasses=colclasses, 
                    na.strings = c("NA","#DIV/0!"))

# convert timestamp from string
pmlframe$cvtd_timestamp = as.POSIXct(pmlframe$cvtd_timestamp, tz="", "%m/%d/%Y %H:%M")
```

Clean up data
========================================================

- The summary rows and columns seem the most useful. 
- Let's filter out the non-summary rows and columns

Filter data
========================================================


```r
# filter columns
pcakeep = c("roll_belt","pitch_belt","yaw_belt","total_accel_belt", "kurtosis_roll_belt","kurtosis_picth_belt", "skewness_roll_belt","skewness_roll_belt.1", "max_roll_belt","max_picth_belt","max_yaw_belt", "min_roll_belt","min_pitch_belt","min_yaw_belt", "amplitude_roll_belt","amplitude_pitch_belt", "var_total_accel_belt","avg_roll_belt","stddev_roll_belt", "var_roll_belt","avg_pitch_belt","stddev_pitch_belt", "var_pitch_belt","avg_yaw_belt","stddev_yaw_belt", "var_yaw_belt","gyros_belt_x","gyros_belt_y", "gyros_belt_z","accel_belt_x","accel_belt_y", "accel_belt_z","magnet_belt_x","magnet_belt_y", "magnet_belt_z","roll_arm","pitch_arm", "yaw_arm","total_accel_arm","var_accel_arm", "avg_roll_arm","stddev_roll_arm","var_roll_arm", "avg_pitch_arm","stddev_pitch_arm","var_pitch_arm", "avg_yaw_arm","stddev_yaw_arm","var_yaw_arm", "gyros_arm_x","gyros_arm_y","gyros_arm_z", "accel_arm_x","accel_arm_y","accel_arm_z", "magnet_arm_x","magnet_arm_y","magnet_arm_z", "kurtosis_roll_arm","kurtosis_picth_arm","kurtosis_yaw_arm", "skewness_roll_arm","skewness_pitch_arm","skewness_yaw_arm", "max_roll_arm","max_picth_arm","max_yaw_arm", "min_roll_arm","min_pitch_arm","min_yaw_arm", "amplitude_roll_arm","amplitude_pitch_arm","amplitude_yaw_arm", "roll_dumbbell","pitch_dumbbell","yaw_dumbbell", "kurtosis_roll_dumbbell","kurtosis_picth_dumbbell", "skewness_roll_dumbbell","skewness_pitch_dumbbell", "max_roll_dumbbell","max_picth_dumbbell","max_yaw_dumbbell", "min_roll_dumbbell","min_pitch_dumbbell","min_yaw_dumbbell", "amplitude_roll_dumbbell","amplitude_pitch_dumbbell", "total_accel_dumbbell","var_accel_dumbbell","avg_roll_dumbbell", "stddev_roll_dumbbell","var_roll_dumbbell","avg_pitch_dumbbell", "stddev_pitch_dumbbell","var_pitch_dumbbell","avg_yaw_dumbbell", "stddev_yaw_dumbbell","var_yaw_dumbbell","gyros_dumbbell_x", "gyros_dumbbell_y","gyros_dumbbell_z","accel_dumbbell_x", "accel_dumbbell_y","accel_dumbbell_z","magnet_dumbbell_x", "magnet_dumbbell_y","magnet_dumbbell_z","roll_forearm", "pitch_forearm","yaw_forearm","kurtosis_roll_forearm", "kurtosis_picth_forearm","skewness_roll_forearm", "skewness_pitch_forearm","max_roll_forearm", "max_picth_forearm","max_yaw_forearm","min_roll_forearm", "min_pitch_forearm","min_yaw_forearm","amplitude_roll_forearm", "amplitude_pitch_forearm","total_accel_forearm", "var_accel_forearm","avg_roll_forearm","stddev_roll_forearm", "var_roll_forearm","avg_pitch_forearm","stddev_pitch_forearm", "var_pitch_forearm","avg_yaw_forearm","stddev_yaw_forearm", "var_yaw_forearm","gyros_forearm_x","gyros_forearm_y", "gyros_forearm_z","accel_forearm_x","accel_forearm_y", "accel_forearm_z","magnet_forearm_x","magnet_forearm_y", "magnet_forearm_z","classe")
```
Filter part 2
========================================================

```r
# filter down to summary rows
pmlsummary = pmlframe[pmlframe$new_window == "yes", ]
# filter columns
pmlsummary = pmlsummary[, pcakeep]
```

Cleanup for predict
========================================================

- set prediction class to numeric
- remove NAs

Cleanup code
========================================================

```r
# filter down to summary rows
pmlsummary = pmlframe[pmlframe$new_window == "yes", ]
# filter columns
pmlsummary = pmlsummary[, pcakeep]
pmlsummary2=pmlsummary
pmlsummary2$numclass=0
pmlsummary2[pmlsummary2$classe=="A", "numclass"] <- 1
pmlsummary2[pmlsummary2$classe=="B", "numclass"] <- 2
pmlsummary2[pmlsummary2$classe=="C", "numclass"] <- 3
pmlsummary2[pmlsummary2$classe=="D", "numclass"] <- 4
pmlsummary2[pmlsummary2$classe=="E", "numclass"] <- 5
pmlsummary2 = pmlsummary2[-144]
pmlsummary2[is.na(pmlsummary2)] <- 0
```

Predict with boost
========================================================


```r
modelFitGbm <- train(pmlsummary2$numclass ~ .,method="gbm",data=pmlsummary2)
```

```
## Loading required package: gbm
## Loading required package: survival
## 
## Attaching package: 'survival'
## 
## The following object is masked from 'package:caret':
## 
##     cluster
## 
## Loading required package: splines
## Loading required package: parallel
## Loaded gbm 2.1.1
## Loading required package: plyr
```

```
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9593             nan     0.1000    0.1321
##      2        1.8506             nan     0.1000    0.0891
##      3        1.7704             nan     0.1000    0.0808
##      4        1.7006             nan     0.1000    0.0749
##      5        1.6355             nan     0.1000    0.0555
##      6        1.5800             nan     0.1000    0.0547
##      7        1.5406             nan     0.1000    0.0251
##      8        1.4858             nan     0.1000    0.0373
##      9        1.4401             nan     0.1000    0.0421
##     10        1.3970             nan     0.1000    0.0351
##     20        1.1001             nan     0.1000    0.0059
##     40        0.7596             nan     0.1000    0.0156
##     60        0.5680             nan     0.1000    0.0035
##     80        0.4669             nan     0.1000    0.0006
##    100        0.3880             nan     0.1000   -0.0009
##    120        0.3357             nan     0.1000    0.0014
##    140        0.2889             nan     0.1000    0.0002
##    150        0.2708             nan     0.1000    0.0002
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9184             nan     0.1000    0.1632
##      2        1.7582             nan     0.1000    0.1278
##      3        1.6306             nan     0.1000    0.1075
##      4        1.5099             nan     0.1000    0.1019
##      5        1.4200             nan     0.1000    0.0864
##      6        1.3331             nan     0.1000    0.0825
##      7        1.2405             nan     0.1000    0.0826
##      8        1.1707             nan     0.1000    0.0672
##      9        1.1177             nan     0.1000    0.0478
##     10        1.0628             nan     0.1000    0.0439
##     20        0.7201             nan     0.1000    0.0114
##     40        0.4304             nan     0.1000    0.0039
##     60        0.3054             nan     0.1000    0.0015
##     80        0.2249             nan     0.1000    0.0006
##    100        0.1742             nan     0.1000    0.0002
##    120        0.1398             nan     0.1000   -0.0004
##    140        0.1159             nan     0.1000   -0.0002
##    150        0.1049             nan     0.1000    0.0007
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.8956             nan     0.1000    0.1701
##      2        1.7075             nan     0.1000    0.1706
##      3        1.5328             nan     0.1000    0.1740
##      4        1.4075             nan     0.1000    0.1189
##      5        1.2918             nan     0.1000    0.1067
##      6        1.1924             nan     0.1000    0.0997
##      7        1.1084             nan     0.1000    0.0733
##      8        1.0451             nan     0.1000    0.0454
##      9        0.9764             nan     0.1000    0.0642
##     10        0.9187             nan     0.1000    0.0420
##     20        0.5373             nan     0.1000    0.0174
##     40        0.2944             nan     0.1000   -0.0018
##     60        0.1903             nan     0.1000   -0.0025
##     80        0.1301             nan     0.1000   -0.0000
##    100        0.0959             nan     0.1000   -0.0004
##    120        0.0719             nan     0.1000   -0.0008
##    140        0.0567             nan     0.1000   -0.0003
##    150        0.0498             nan     0.1000    0.0002
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0740             nan     0.1000    0.1507
##      2        1.9386             nan     0.1000    0.1187
##      3        1.8377             nan     0.1000    0.1172
##      4        1.7635             nan     0.1000    0.0699
##      5        1.6735             nan     0.1000    0.0699
##      6        1.6140             nan     0.1000    0.0439
##      7        1.5402             nan     0.1000    0.0573
##      8        1.5118             nan     0.1000    0.0029
##      9        1.4586             nan     0.1000    0.0514
##     10        1.4124             nan     0.1000    0.0416
##     20        1.1357             nan     0.1000    0.0107
##     40        0.8022             nan     0.1000    0.0067
##     60        0.6352             nan     0.1000   -0.0040
##     80        0.5282             nan     0.1000   -0.0002
##    100        0.4406             nan     0.1000    0.0011
##    120        0.3839             nan     0.1000    0.0007
##    140        0.3354             nan     0.1000   -0.0001
##    150        0.3168             nan     0.1000   -0.0012
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0307             nan     0.1000    0.2006
##      2        1.8387             nan     0.1000    0.1862
##      3        1.7013             nan     0.1000    0.1267
##      4        1.5899             nan     0.1000    0.0807
##      5        1.4821             nan     0.1000    0.0997
##      6        1.4087             nan     0.1000    0.0599
##      7        1.3249             nan     0.1000    0.0769
##      8        1.2516             nan     0.1000    0.0649
##      9        1.1860             nan     0.1000    0.0557
##     10        1.1288             nan     0.1000    0.0441
##     20        0.7924             nan     0.1000    0.0126
##     40        0.4823             nan     0.1000    0.0040
##     60        0.3351             nan     0.1000    0.0022
##     80        0.2561             nan     0.1000   -0.0011
##    100        0.2026             nan     0.1000    0.0001
##    120        0.1633             nan     0.1000    0.0010
##    140        0.1327             nan     0.1000   -0.0003
##    150        0.1201             nan     0.1000   -0.0009
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0050             nan     0.1000    0.2388
##      2        1.8022             nan     0.1000    0.1863
##      3        1.6318             nan     0.1000    0.1560
##      4        1.4897             nan     0.1000    0.1304
##      5        1.3709             nan     0.1000    0.1068
##      6        1.2672             nan     0.1000    0.0911
##      7        1.1858             nan     0.1000    0.0676
##      8        1.1036             nan     0.1000    0.0692
##      9        1.0396             nan     0.1000    0.0595
##     10        0.9799             nan     0.1000    0.0467
##     20        0.5942             nan     0.1000    0.0151
##     40        0.3262             nan     0.1000    0.0044
##     60        0.2144             nan     0.1000   -0.0013
##     80        0.1475             nan     0.1000    0.0006
##    100        0.1065             nan     0.1000    0.0006
##    120        0.0788             nan     0.1000   -0.0009
##    140        0.0610             nan     0.1000   -0.0004
##    150        0.0540             nan     0.1000   -0.0003
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.8952             nan     0.1000    0.1322
##      2        1.7837             nan     0.1000    0.1136
##      3        1.6853             nan     0.1000    0.0948
##      4        1.6095             nan     0.1000    0.0769
##      5        1.5504             nan     0.1000    0.0537
##      6        1.5035             nan     0.1000    0.0436
##      7        1.4564             nan     0.1000    0.0399
##      8        1.4092             nan     0.1000    0.0261
##      9        1.3713             nan     0.1000    0.0278
##     10        1.3378             nan     0.1000    0.0207
##     20        1.0827             nan     0.1000    0.0093
##     40        0.7615             nan     0.1000   -0.0010
##     60        0.5893             nan     0.1000    0.0042
##     80        0.4836             nan     0.1000   -0.0001
##    100        0.4106             nan     0.1000    0.0020
##    120        0.3618             nan     0.1000   -0.0002
##    140        0.3200             nan     0.1000   -0.0014
##    150        0.3042             nan     0.1000   -0.0008
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.8700             nan     0.1000    0.1334
##      2        1.7119             nan     0.1000    0.1360
##      3        1.5764             nan     0.1000    0.1212
##      4        1.4684             nan     0.1000    0.0915
##      5        1.3850             nan     0.1000    0.0664
##      6        1.3106             nan     0.1000    0.0616
##      7        1.2406             nan     0.1000    0.0605
##      8        1.1812             nan     0.1000    0.0418
##      9        1.1279             nan     0.1000    0.0434
##     10        1.0771             nan     0.1000    0.0383
##     20        0.7343             nan     0.1000    0.0197
##     40        0.4574             nan     0.1000    0.0068
##     60        0.3174             nan     0.1000    0.0018
##     80        0.2400             nan     0.1000   -0.0004
##    100        0.1934             nan     0.1000   -0.0002
##    120        0.1594             nan     0.1000    0.0003
##    140        0.1318             nan     0.1000   -0.0000
##    150        0.1202             nan     0.1000   -0.0007
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.8322             nan     0.1000    0.1944
##      2        1.6556             nan     0.1000    0.1574
##      3        1.4931             nan     0.1000    0.1425
##      4        1.3671             nan     0.1000    0.0891
##      5        1.2517             nan     0.1000    0.0933
##      6        1.1624             nan     0.1000    0.0732
##      7        1.0938             nan     0.1000    0.0508
##      8        1.0254             nan     0.1000    0.0637
##      9        0.9660             nan     0.1000    0.0495
##     10        0.9108             nan     0.1000    0.0488
##     20        0.5687             nan     0.1000    0.0086
##     40        0.3184             nan     0.1000   -0.0010
##     60        0.2133             nan     0.1000   -0.0038
##     80        0.1537             nan     0.1000    0.0003
##    100        0.1156             nan     0.1000   -0.0006
##    120        0.0866             nan     0.1000   -0.0001
##    140        0.0667             nan     0.1000    0.0002
##    150        0.0583             nan     0.1000   -0.0002
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.1002             nan     0.1000    0.1282
##      2        1.9771             nan     0.1000    0.1088
##      3        1.8974             nan     0.1000    0.0707
##      4        1.8019             nan     0.1000    0.1016
##      5        1.7213             nan     0.1000    0.0742
##      6        1.6652             nan     0.1000    0.0565
##      7        1.6050             nan     0.1000    0.0524
##      8        1.5525             nan     0.1000    0.0433
##      9        1.5098             nan     0.1000    0.0441
##     10        1.4781             nan     0.1000    0.0170
##     20        1.1947             nan     0.1000    0.0113
##     40        0.8760             nan     0.1000   -0.0009
##     60        0.6944             nan     0.1000    0.0032
##     80        0.5792             nan     0.1000   -0.0027
##    100        0.5006             nan     0.1000   -0.0017
##    120        0.4347             nan     0.1000   -0.0003
##    140        0.3895             nan     0.1000   -0.0015
##    150        0.3676             nan     0.1000    0.0006
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0441             nan     0.1000    0.1674
##      2        1.8887             nan     0.1000    0.1491
##      3        1.7460             nan     0.1000    0.1308
##      4        1.6326             nan     0.1000    0.0956
##      5        1.5387             nan     0.1000    0.0881
##      6        1.4703             nan     0.1000    0.0465
##      7        1.4086             nan     0.1000    0.0493
##      8        1.3357             nan     0.1000    0.0579
##      9        1.2661             nan     0.1000    0.0513
##     10        1.2027             nan     0.1000    0.0495
##     20        0.8637             nan     0.1000    0.0165
##     40        0.5314             nan     0.1000    0.0058
##     60        0.3888             nan     0.1000    0.0037
##     80        0.2986             nan     0.1000    0.0004
##    100        0.2414             nan     0.1000   -0.0005
##    120        0.1991             nan     0.1000   -0.0005
##    140        0.1659             nan     0.1000   -0.0000
##    150        0.1518             nan     0.1000    0.0001
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0205             nan     0.1000    0.2023
##      2        1.8313             nan     0.1000    0.1653
##      3        1.6732             nan     0.1000    0.1425
##      4        1.5379             nan     0.1000    0.1081
##      5        1.4299             nan     0.1000    0.0870
##      6        1.3317             nan     0.1000    0.0873
##      7        1.2435             nan     0.1000    0.0701
##      8        1.1637             nan     0.1000    0.0668
##      9        1.0892             nan     0.1000    0.0686
##     10        1.0201             nan     0.1000    0.0608
##     20        0.6418             nan     0.1000    0.0150
##     40        0.3766             nan     0.1000    0.0015
##     60        0.2391             nan     0.1000    0.0017
##     80        0.1683             nan     0.1000    0.0009
##    100        0.1216             nan     0.1000    0.0002
##    120        0.0925             nan     0.1000   -0.0006
##    140        0.0692             nan     0.1000    0.0004
##    150        0.0605             nan     0.1000   -0.0006
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0163             nan     0.1000    0.1523
##      2        1.8932             nan     0.1000    0.1299
##      3        1.7833             nan     0.1000    0.1019
##      4        1.6979             nan     0.1000    0.0785
##      5        1.6276             nan     0.1000    0.0504
##      6        1.5692             nan     0.1000    0.0450
##      7        1.5177             nan     0.1000    0.0489
##      8        1.4670             nan     0.1000    0.0444
##      9        1.4256             nan     0.1000    0.0344
##     10        1.3924             nan     0.1000    0.0273
##     20        1.1267             nan     0.1000    0.0158
##     40        0.8065             nan     0.1000    0.0040
##     60        0.6469             nan     0.1000    0.0023
##     80        0.5371             nan     0.1000    0.0038
##    100        0.4550             nan     0.1000    0.0010
##    120        0.3968             nan     0.1000   -0.0025
##    140        0.3519             nan     0.1000    0.0003
##    150        0.3286             nan     0.1000    0.0001
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9709             nan     0.1000    0.2164
##      2        1.8149             nan     0.1000    0.1258
##      3        1.6747             nan     0.1000    0.1462
##      4        1.5486             nan     0.1000    0.1158
##      5        1.4417             nan     0.1000    0.0891
##      6        1.3534             nan     0.1000    0.0620
##      7        1.2834             nan     0.1000    0.0556
##      8        1.2216             nan     0.1000    0.0516
##      9        1.1648             nan     0.1000    0.0432
##     10        1.1143             nan     0.1000    0.0381
##     20        0.7939             nan     0.1000    0.0140
##     40        0.4944             nan     0.1000   -0.0011
##     60        0.3440             nan     0.1000    0.0023
##     80        0.2528             nan     0.1000   -0.0025
##    100        0.1935             nan     0.1000    0.0007
##    120        0.1563             nan     0.1000    0.0024
##    140        0.1287             nan     0.1000   -0.0004
##    150        0.1180             nan     0.1000   -0.0003
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9565             nan     0.1000    0.1872
##      2        1.7744             nan     0.1000    0.1762
##      3        1.6117             nan     0.1000    0.1242
##      4        1.4669             nan     0.1000    0.1245
##      5        1.3531             nan     0.1000    0.0994
##      6        1.2496             nan     0.1000    0.0874
##      7        1.1638             nan     0.1000    0.0698
##      8        1.0914             nan     0.1000    0.0603
##      9        1.0244             nan     0.1000    0.0570
##     10        0.9663             nan     0.1000    0.0455
##     20        0.6148             nan     0.1000    0.0176
##     40        0.3357             nan     0.1000   -0.0007
##     60        0.2111             nan     0.1000    0.0019
##     80        0.1422             nan     0.1000   -0.0007
##    100        0.1024             nan     0.1000   -0.0003
##    120        0.0792             nan     0.1000   -0.0012
##    140        0.0595             nan     0.1000   -0.0001
##    150        0.0528             nan     0.1000    0.0002
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0981             nan     0.1000    0.1814
##      2        1.9520             nan     0.1000    0.1336
##      3        1.8249             nan     0.1000    0.1300
##      4        1.7286             nan     0.1000    0.0913
##      5        1.6492             nan     0.1000    0.0706
##      6        1.5813             nan     0.1000    0.0579
##      7        1.5220             nan     0.1000    0.0564
##      8        1.4710             nan     0.1000    0.0480
##      9        1.4082             nan     0.1000    0.0472
##     10        1.3684             nan     0.1000    0.0445
##     20        1.0563             nan     0.1000    0.0104
##     40        0.7450             nan     0.1000    0.0034
##     60        0.5880             nan     0.1000    0.0012
##     80        0.4875             nan     0.1000   -0.0011
##    100        0.4272             nan     0.1000    0.0010
##    120        0.3696             nan     0.1000   -0.0000
##    140        0.3276             nan     0.1000   -0.0020
##    150        0.3104             nan     0.1000   -0.0009
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0546             nan     0.1000    0.2153
##      2        1.8742             nan     0.1000    0.1716
##      3        1.7123             nan     0.1000    0.1589
##      4        1.5851             nan     0.1000    0.1204
##      5        1.4708             nan     0.1000    0.0945
##      6        1.3756             nan     0.1000    0.0773
##      7        1.2898             nan     0.1000    0.0773
##      8        1.2170             nan     0.1000    0.0613
##      9        1.1608             nan     0.1000    0.0447
##     10        1.1182             nan     0.1000    0.0301
##     20        0.7617             nan     0.1000    0.0121
##     40        0.4708             nan     0.1000    0.0030
##     60        0.3299             nan     0.1000   -0.0017
##     80        0.2498             nan     0.1000   -0.0003
##    100        0.1958             nan     0.1000   -0.0005
##    120        0.1558             nan     0.1000    0.0010
##    140        0.1263             nan     0.1000   -0.0007
##    150        0.1148             nan     0.1000   -0.0004
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0129             nan     0.1000    0.2202
##      2        1.8094             nan     0.1000    0.1978
##      3        1.6256             nan     0.1000    0.1462
##      4        1.4630             nan     0.1000    0.1379
##      5        1.3391             nan     0.1000    0.1051
##      6        1.2191             nan     0.1000    0.1090
##      7        1.1255             nan     0.1000    0.0760
##      8        1.0478             nan     0.1000    0.0657
##      9        0.9781             nan     0.1000    0.0552
##     10        0.9224             nan     0.1000    0.0435
##     20        0.5847             nan     0.1000    0.0056
##     40        0.3357             nan     0.1000    0.0060
##     60        0.2283             nan     0.1000   -0.0013
##     80        0.1630             nan     0.1000   -0.0002
##    100        0.1191             nan     0.1000   -0.0001
##    120        0.0874             nan     0.1000   -0.0001
##    140        0.0673             nan     0.1000   -0.0005
##    150        0.0583             nan     0.1000   -0.0003
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9976             nan     0.1000    0.1730
##      2        1.8775             nan     0.1000    0.1288
##      3        1.7722             nan     0.1000    0.0981
##      4        1.6883             nan     0.1000    0.0696
##      5        1.6201             nan     0.1000    0.0749
##      6        1.5566             nan     0.1000    0.0647
##      7        1.5030             nan     0.1000    0.0490
##      8        1.4435             nan     0.1000    0.0542
##      9        1.4015             nan     0.1000    0.0378
##     10        1.3607             nan     0.1000    0.0336
##     20        1.0758             nan     0.1000    0.0197
##     40        0.7797             nan     0.1000    0.0097
##     60        0.6095             nan     0.1000    0.0027
##     80        0.4981             nan     0.1000    0.0008
##    100        0.4193             nan     0.1000    0.0005
##    120        0.3614             nan     0.1000    0.0012
##    140        0.3124             nan     0.1000    0.0000
##    150        0.2918             nan     0.1000   -0.0016
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9441             nan     0.1000    0.1911
##      2        1.7643             nan     0.1000    0.1825
##      3        1.6046             nan     0.1000    0.1414
##      4        1.4984             nan     0.1000    0.1139
##      5        1.4093             nan     0.1000    0.0727
##      6        1.3233             nan     0.1000    0.0837
##      7        1.2345             nan     0.1000    0.0674
##      8        1.1731             nan     0.1000    0.0487
##      9        1.1121             nan     0.1000    0.0502
##     10        1.0626             nan     0.1000    0.0417
##     20        0.7499             nan     0.1000    0.0059
##     40        0.4673             nan     0.1000    0.0021
##     60        0.3274             nan     0.1000    0.0010
##     80        0.2430             nan     0.1000   -0.0010
##    100        0.1934             nan     0.1000   -0.0029
##    120        0.1564             nan     0.1000   -0.0011
##    140        0.1320             nan     0.1000   -0.0006
##    150        0.1207             nan     0.1000    0.0003
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9074             nan     0.1000    0.2415
##      2        1.7102             nan     0.1000    0.1828
##      3        1.5549             nan     0.1000    0.1411
##      4        1.4129             nan     0.1000    0.1250
##      5        1.2961             nan     0.1000    0.1037
##      6        1.1962             nan     0.1000    0.0794
##      7        1.1115             nan     0.1000    0.0688
##      8        1.0527             nan     0.1000    0.0283
##      9        0.9860             nan     0.1000    0.0527
##     10        0.9327             nan     0.1000    0.0341
##     20        0.5656             nan     0.1000    0.0119
##     40        0.3124             nan     0.1000    0.0041
##     60        0.1941             nan     0.1000    0.0017
##     80        0.1343             nan     0.1000    0.0000
##    100        0.0957             nan     0.1000    0.0005
##    120        0.0724             nan     0.1000   -0.0014
##    140        0.0564             nan     0.1000   -0.0010
##    150        0.0499             nan     0.1000   -0.0004
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9700             nan     0.1000    0.1535
##      2        1.8414             nan     0.1000    0.1366
##      3        1.7245             nan     0.1000    0.1190
##      4        1.6315             nan     0.1000    0.0870
##      5        1.5585             nan     0.1000    0.0680
##      6        1.4937             nan     0.1000    0.0513
##      7        1.4436             nan     0.1000    0.0509
##      8        1.4021             nan     0.1000    0.0317
##      9        1.3558             nan     0.1000    0.0461
##     10        1.3188             nan     0.1000    0.0314
##     20        1.0371             nan     0.1000    0.0110
##     40        0.7471             nan     0.1000    0.0080
##     60        0.5882             nan     0.1000    0.0010
##     80        0.4926             nan     0.1000   -0.0029
##    100        0.4216             nan     0.1000    0.0028
##    120        0.3704             nan     0.1000    0.0003
##    140        0.3307             nan     0.1000    0.0002
##    150        0.3116             nan     0.1000    0.0016
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9157             nan     0.1000    0.2037
##      2        1.7353             nan     0.1000    0.1473
##      3        1.6039             nan     0.1000    0.1141
##      4        1.4704             nan     0.1000    0.1311
##      5        1.3632             nan     0.1000    0.1074
##      6        1.2752             nan     0.1000    0.0830
##      7        1.2053             nan     0.1000    0.0515
##      8        1.1353             nan     0.1000    0.0623
##      9        1.0734             nan     0.1000    0.0503
##     10        1.0405             nan     0.1000    0.0201
##     20        0.7007             nan     0.1000    0.0241
##     40        0.4496             nan     0.1000    0.0006
##     60        0.3302             nan     0.1000   -0.0003
##     80        0.2528             nan     0.1000   -0.0010
##    100        0.1974             nan     0.1000   -0.0014
##    120        0.1598             nan     0.1000   -0.0007
##    140        0.1328             nan     0.1000    0.0005
##    150        0.1210             nan     0.1000   -0.0011
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9072             nan     0.1000    0.2402
##      2        1.6988             nan     0.1000    0.2228
##      3        1.5299             nan     0.1000    0.1462
##      4        1.3977             nan     0.1000    0.1138
##      5        1.2667             nan     0.1000    0.1065
##      6        1.1657             nan     0.1000    0.0830
##      7        1.0709             nan     0.1000    0.0921
##      8        0.9999             nan     0.1000    0.0568
##      9        0.9348             nan     0.1000    0.0603
##     10        0.8700             nan     0.1000    0.0611
##     20        0.5412             nan     0.1000    0.0120
##     40        0.3216             nan     0.1000   -0.0018
##     60        0.2161             nan     0.1000    0.0012
##     80        0.1535             nan     0.1000    0.0012
##    100        0.1116             nan     0.1000   -0.0001
##    120        0.0845             nan     0.1000   -0.0012
##    140        0.0627             nan     0.1000   -0.0003
##    150        0.0540             nan     0.1000   -0.0001
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9781             nan     0.1000    0.1211
##      2        1.8718             nan     0.1000    0.1103
##      3        1.7791             nan     0.1000    0.0829
##      4        1.7014             nan     0.1000    0.0765
##      5        1.6397             nan     0.1000    0.0595
##      6        1.5894             nan     0.1000    0.0369
##      7        1.5285             nan     0.1000    0.0487
##      8        1.4872             nan     0.1000    0.0357
##      9        1.4470             nan     0.1000    0.0149
##     10        1.4061             nan     0.1000    0.0380
##     20        1.1212             nan     0.1000    0.0212
##     40        0.8063             nan     0.1000    0.0082
##     60        0.6195             nan     0.1000    0.0002
##     80        0.5056             nan     0.1000   -0.0002
##    100        0.4239             nan     0.1000    0.0017
##    120        0.3643             nan     0.1000    0.0005
##    140        0.3225             nan     0.1000   -0.0001
##    150        0.3043             nan     0.1000   -0.0021
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9476             nan     0.1000    0.1559
##      2        1.7899             nan     0.1000    0.1551
##      3        1.6558             nan     0.1000    0.1278
##      4        1.5452             nan     0.1000    0.1010
##      5        1.4464             nan     0.1000    0.0866
##      6        1.3709             nan     0.1000    0.0620
##      7        1.3056             nan     0.1000    0.0500
##      8        1.2438             nan     0.1000    0.0494
##      9        1.1926             nan     0.1000    0.0412
##     10        1.1377             nan     0.1000    0.0426
##     20        0.8059             nan     0.1000    0.0079
##     40        0.4860             nan     0.1000    0.0044
##     60        0.3386             nan     0.1000    0.0017
##     80        0.2564             nan     0.1000   -0.0012
##    100        0.2040             nan     0.1000    0.0002
##    120        0.1638             nan     0.1000   -0.0005
##    140        0.1375             nan     0.1000   -0.0008
##    150        0.1251             nan     0.1000   -0.0002
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9091             nan     0.1000    0.1870
##      2        1.7278             nan     0.1000    0.1820
##      3        1.5979             nan     0.1000    0.0998
##      4        1.4708             nan     0.1000    0.0986
##      5        1.3553             nan     0.1000    0.1064
##      6        1.2681             nan     0.1000    0.0778
##      7        1.1876             nan     0.1000    0.0398
##      8        1.1199             nan     0.1000    0.0473
##      9        1.0501             nan     0.1000    0.0546
##     10        0.9797             nan     0.1000    0.0625
##     20        0.5992             nan     0.1000    0.0087
##     40        0.3252             nan     0.1000    0.0027
##     60        0.2016             nan     0.1000    0.0016
##     80        0.1387             nan     0.1000    0.0002
##    100        0.0993             nan     0.1000   -0.0003
##    120        0.0778             nan     0.1000   -0.0006
##    140        0.0602             nan     0.1000   -0.0006
##    150        0.0539             nan     0.1000    0.0001
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.1625             nan     0.1000    0.1456
##      2        2.0317             nan     0.1000    0.1285
##      3        1.9401             nan     0.1000    0.0777
##      4        1.8567             nan     0.1000    0.0853
##      5        1.7788             nan     0.1000    0.0689
##      6        1.7103             nan     0.1000    0.0647
##      7        1.6572             nan     0.1000    0.0477
##      8        1.6172             nan     0.1000    0.0282
##      9        1.5657             nan     0.1000    0.0405
##     10        1.5189             nan     0.1000    0.0393
##     20        1.1583             nan     0.1000    0.0251
##     40        0.8126             nan     0.1000    0.0072
##     60        0.6202             nan     0.1000    0.0063
##     80        0.5085             nan     0.1000   -0.0019
##    100        0.4297             nan     0.1000    0.0001
##    120        0.3737             nan     0.1000    0.0001
##    140        0.3260             nan     0.1000   -0.0012
##    150        0.3074             nan     0.1000   -0.0005
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0949             nan     0.1000    0.2021
##      2        1.8964             nan     0.1000    0.1831
##      3        1.7487             nan     0.1000    0.1287
##      4        1.6239             nan     0.1000    0.1192
##      5        1.5061             nan     0.1000    0.1005
##      6        1.4214             nan     0.1000    0.0670
##      7        1.3290             nan     0.1000    0.0746
##      8        1.2630             nan     0.1000    0.0579
##      9        1.1933             nan     0.1000    0.0519
##     10        1.1275             nan     0.1000    0.0611
##     20        0.7648             nan     0.1000    0.0067
##     40        0.4589             nan     0.1000    0.0030
##     60        0.3235             nan     0.1000    0.0041
##     80        0.2414             nan     0.1000    0.0006
##    100        0.1879             nan     0.1000    0.0002
##    120        0.1485             nan     0.1000   -0.0000
##    140        0.1237             nan     0.1000   -0.0004
##    150        0.1116             nan     0.1000    0.0004
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0482             nan     0.1000    0.2521
##      2        1.8437             nan     0.1000    0.2089
##      3        1.6665             nan     0.1000    0.1694
##      4        1.5098             nan     0.1000    0.1513
##      5        1.3830             nan     0.1000    0.1137
##      6        1.2715             nan     0.1000    0.0961
##      7        1.1735             nan     0.1000    0.0815
##      8        1.0837             nan     0.1000    0.0784
##      9        1.0110             nan     0.1000    0.0534
##     10        0.9441             nan     0.1000    0.0479
##     20        0.5723             nan     0.1000    0.0108
##     40        0.2980             nan     0.1000    0.0059
##     60        0.1913             nan     0.1000   -0.0022
##     80        0.1317             nan     0.1000    0.0004
##    100        0.0974             nan     0.1000   -0.0004
##    120        0.0732             nan     0.1000   -0.0003
##    140        0.0556             nan     0.1000   -0.0001
##    150        0.0485             nan     0.1000   -0.0005
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0951             nan     0.1000    0.1350
##      2        1.9736             nan     0.1000    0.1376
##      3        1.8830             nan     0.1000    0.1004
##      4        1.8137             nan     0.1000    0.0689
##      5        1.7533             nan     0.1000    0.0577
##      6        1.7103             nan     0.1000    0.0251
##      7        1.6585             nan     0.1000    0.0475
##      8        1.6023             nan     0.1000    0.0454
##      9        1.5634             nan     0.1000    0.0292
##     10        1.5058             nan     0.1000    0.0465
##     20        1.1744             nan     0.1000    0.0195
##     40        0.8230             nan     0.1000    0.0047
##     60        0.6413             nan     0.1000    0.0009
##     80        0.5267             nan     0.1000   -0.0003
##    100        0.4470             nan     0.1000    0.0003
##    120        0.3868             nan     0.1000    0.0006
##    140        0.3432             nan     0.1000    0.0012
##    150        0.3223             nan     0.1000    0.0008
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0047             nan     0.1000    0.2037
##      2        1.8504             nan     0.1000    0.1426
##      3        1.7180             nan     0.1000    0.1226
##      4        1.6099             nan     0.1000    0.1051
##      5        1.5116             nan     0.1000    0.0967
##      6        1.4333             nan     0.1000    0.0671
##      7        1.3291             nan     0.1000    0.0918
##      8        1.2555             nan     0.1000    0.0519
##      9        1.2019             nan     0.1000    0.0375
##     10        1.1398             nan     0.1000    0.0564
##     20        0.7708             nan     0.1000    0.0160
##     40        0.4890             nan     0.1000    0.0059
##     60        0.3484             nan     0.1000    0.0035
##     80        0.2618             nan     0.1000    0.0005
##    100        0.2016             nan     0.1000    0.0003
##    120        0.1535             nan     0.1000   -0.0015
##    140        0.1236             nan     0.1000    0.0000
##    150        0.1111             nan     0.1000    0.0001
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9597             nan     0.1000    0.2306
##      2        1.7646             nan     0.1000    0.1781
##      3        1.6070             nan     0.1000    0.1580
##      4        1.4569             nan     0.1000    0.1248
##      5        1.3310             nan     0.1000    0.1156
##      6        1.2500             nan     0.1000    0.0744
##      7        1.1399             nan     0.1000    0.0899
##      8        1.0671             nan     0.1000    0.0653
##      9        1.0012             nan     0.1000    0.0480
##     10        0.9413             nan     0.1000    0.0442
##     20        0.5770             nan     0.1000    0.0163
##     40        0.3347             nan     0.1000   -0.0008
##     60        0.2160             nan     0.1000   -0.0010
##     80        0.1499             nan     0.1000   -0.0000
##    100        0.1099             nan     0.1000   -0.0005
##    120        0.0834             nan     0.1000    0.0004
##    140        0.0652             nan     0.1000   -0.0005
##    150        0.0573             nan     0.1000   -0.0003
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0499             nan     0.1000    0.1533
##      2        1.9121             nan     0.1000    0.1213
##      3        1.8056             nan     0.1000    0.1054
##      4        1.7191             nan     0.1000    0.0799
##      5        1.6500             nan     0.1000    0.0600
##      6        1.5901             nan     0.1000    0.0538
##      7        1.5426             nan     0.1000    0.0375
##      8        1.4949             nan     0.1000    0.0369
##      9        1.4572             nan     0.1000    0.0316
##     10        1.4173             nan     0.1000    0.0310
##     20        1.1256             nan     0.1000    0.0125
##     40        0.8013             nan     0.1000    0.0062
##     60        0.6360             nan     0.1000   -0.0016
##     80        0.5272             nan     0.1000    0.0025
##    100        0.4509             nan     0.1000    0.0022
##    120        0.3944             nan     0.1000   -0.0040
##    140        0.3466             nan     0.1000   -0.0024
##    150        0.3272             nan     0.1000   -0.0011
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9956             nan     0.1000    0.2019
##      2        1.8172             nan     0.1000    0.1755
##      3        1.6731             nan     0.1000    0.1429
##      4        1.5571             nan     0.1000    0.1007
##      5        1.4563             nan     0.1000    0.0965
##      6        1.3724             nan     0.1000    0.0648
##      7        1.2906             nan     0.1000    0.0522
##      8        1.2352             nan     0.1000    0.0433
##      9        1.1760             nan     0.1000    0.0503
##     10        1.1217             nan     0.1000    0.0337
##     20        0.7946             nan     0.1000    0.0082
##     40        0.4976             nan     0.1000    0.0062
##     60        0.3570             nan     0.1000    0.0018
##     80        0.2723             nan     0.1000    0.0009
##    100        0.2105             nan     0.1000   -0.0006
##    120        0.1718             nan     0.1000    0.0005
##    140        0.1404             nan     0.1000   -0.0001
##    150        0.1283             nan     0.1000    0.0002
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9744             nan     0.1000    0.2030
##      2        1.7702             nan     0.1000    0.2050
##      3        1.5927             nan     0.1000    0.1659
##      4        1.4539             nan     0.1000    0.1387
##      5        1.3422             nan     0.1000    0.0907
##      6        1.2420             nan     0.1000    0.0879
##      7        1.1454             nan     0.1000    0.0842
##      8        1.0710             nan     0.1000    0.0637
##      9        1.0085             nan     0.1000    0.0470
##     10        0.9424             nan     0.1000    0.0599
##     20        0.6053             nan     0.1000    0.0032
##     40        0.3280             nan     0.1000    0.0048
##     60        0.2120             nan     0.1000   -0.0019
##     80        0.1463             nan     0.1000    0.0014
##    100        0.1057             nan     0.1000    0.0006
##    120        0.0816             nan     0.1000   -0.0007
##    140        0.0641             nan     0.1000   -0.0005
##    150        0.0575             nan     0.1000   -0.0007
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0566             nan     0.1000    0.1397
##      2        1.9274             nan     0.1000    0.1280
##      3        1.8227             nan     0.1000    0.0853
##      4        1.7291             nan     0.1000    0.0849
##      5        1.6634             nan     0.1000    0.0695
##      6        1.6062             nan     0.1000    0.0395
##      7        1.5451             nan     0.1000    0.0437
##      8        1.4922             nan     0.1000    0.0508
##      9        1.4472             nan     0.1000    0.0467
##     10        1.4050             nan     0.1000    0.0375
##     20        1.1145             nan     0.1000    0.0149
##     40        0.8199             nan     0.1000    0.0069
##     60        0.6517             nan     0.1000   -0.0020
##     80        0.5454             nan     0.1000   -0.0001
##    100        0.4657             nan     0.1000    0.0004
##    120        0.4087             nan     0.1000   -0.0019
##    140        0.3628             nan     0.1000   -0.0003
##    150        0.3419             nan     0.1000   -0.0006
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9975             nan     0.1000    0.1736
##      2        1.8368             nan     0.1000    0.1679
##      3        1.6831             nan     0.1000    0.1384
##      4        1.5701             nan     0.1000    0.0971
##      5        1.4725             nan     0.1000    0.0743
##      6        1.3772             nan     0.1000    0.0761
##      7        1.3072             nan     0.1000    0.0575
##      8        1.2394             nan     0.1000    0.0659
##      9        1.1803             nan     0.1000    0.0381
##     10        1.1269             nan     0.1000    0.0395
##     20        0.7928             nan     0.1000    0.0121
##     40        0.4960             nan     0.1000    0.0051
##     60        0.3480             nan     0.1000    0.0007
##     80        0.2682             nan     0.1000   -0.0011
##    100        0.2158             nan     0.1000   -0.0037
##    120        0.1732             nan     0.1000   -0.0012
##    140        0.1399             nan     0.1000   -0.0004
##    150        0.1270             nan     0.1000   -0.0008
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9601             nan     0.1000    0.2146
##      2        1.7661             nan     0.1000    0.1980
##      3        1.5972             nan     0.1000    0.1679
##      4        1.4772             nan     0.1000    0.0985
##      5        1.3567             nan     0.1000    0.1091
##      6        1.2532             nan     0.1000    0.0893
##      7        1.1676             nan     0.1000    0.0814
##      8        1.0988             nan     0.1000    0.0614
##      9        1.0282             nan     0.1000    0.0575
##     10        0.9686             nan     0.1000    0.0506
##     20        0.6120             nan     0.1000    0.0165
##     40        0.3403             nan     0.1000    0.0035
##     60        0.2258             nan     0.1000   -0.0025
##     80        0.1577             nan     0.1000   -0.0022
##    100        0.1125             nan     0.1000    0.0004
##    120        0.0822             nan     0.1000   -0.0004
##    140        0.0630             nan     0.1000    0.0002
##    150        0.0550             nan     0.1000   -0.0003
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.1047             nan     0.1000    0.2118
##      2        1.9300             nan     0.1000    0.1905
##      3        1.7953             nan     0.1000    0.1439
##      4        1.6835             nan     0.1000    0.1300
##      5        1.5898             nan     0.1000    0.0911
##      6        1.5234             nan     0.1000    0.0651
##      7        1.4643             nan     0.1000    0.0423
##      8        1.4092             nan     0.1000    0.0509
##      9        1.3594             nan     0.1000    0.0407
##     10        1.3185             nan     0.1000    0.0295
##     20        1.0088             nan     0.1000    0.0133
##     40        0.7228             nan     0.1000    0.0054
##     60        0.5737             nan     0.1000   -0.0012
##     80        0.4741             nan     0.1000    0.0005
##    100        0.4063             nan     0.1000   -0.0023
##    120        0.3622             nan     0.1000    0.0008
##    140        0.3237             nan     0.1000   -0.0002
##    150        0.3073             nan     0.1000   -0.0067
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0644             nan     0.1000    0.2381
##      2        1.8554             nan     0.1000    0.2206
##      3        1.6739             nan     0.1000    0.1612
##      4        1.5324             nan     0.1000    0.1141
##      5        1.4137             nan     0.1000    0.1037
##      6        1.3142             nan     0.1000    0.0892
##      7        1.2222             nan     0.1000    0.0792
##      8        1.1510             nan     0.1000    0.0575
##      9        1.0776             nan     0.1000    0.0733
##     10        1.0161             nan     0.1000    0.0507
##     20        0.7053             nan     0.1000    0.0101
##     40        0.4495             nan     0.1000   -0.0003
##     60        0.3180             nan     0.1000   -0.0020
##     80        0.2441             nan     0.1000   -0.0007
##    100        0.1934             nan     0.1000   -0.0002
##    120        0.1559             nan     0.1000   -0.0008
##    140        0.1297             nan     0.1000    0.0001
##    150        0.1193             nan     0.1000   -0.0007
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0466             nan     0.1000    0.2893
##      2        1.8118             nan     0.1000    0.2107
##      3        1.6330             nan     0.1000    0.1514
##      4        1.4805             nan     0.1000    0.1354
##      5        1.3462             nan     0.1000    0.1374
##      6        1.2321             nan     0.1000    0.1113
##      7        1.1307             nan     0.1000    0.0879
##      8        1.0391             nan     0.1000    0.0855
##      9        0.9647             nan     0.1000    0.0571
##     10        0.9044             nan     0.1000    0.0496
##     20        0.5392             nan     0.1000    0.0170
##     40        0.2945             nan     0.1000    0.0020
##     60        0.1895             nan     0.1000    0.0001
##     80        0.1354             nan     0.1000    0.0001
##    100        0.1001             nan     0.1000   -0.0001
##    120        0.0769             nan     0.1000   -0.0005
##    140        0.0592             nan     0.1000   -0.0006
##    150        0.0519             nan     0.1000    0.0001
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0868             nan     0.1000    0.1283
##      2        1.9467             nan     0.1000    0.1495
##      3        1.8693             nan     0.1000    0.0753
##      4        1.7692             nan     0.1000    0.0931
##      5        1.6835             nan     0.1000    0.0764
##      6        1.6114             nan     0.1000    0.0656
##      7        1.5615             nan     0.1000    0.0246
##      8        1.5105             nan     0.1000    0.0391
##      9        1.4567             nan     0.1000    0.0551
##     10        1.4047             nan     0.1000    0.0484
##     20        1.0947             nan     0.1000    0.0194
##     40        0.7466             nan     0.1000    0.0098
##     60        0.5776             nan     0.1000    0.0053
##     80        0.4813             nan     0.1000    0.0012
##    100        0.4141             nan     0.1000   -0.0011
##    120        0.3590             nan     0.1000   -0.0019
##    140        0.3190             nan     0.1000    0.0002
##    150        0.2999             nan     0.1000    0.0005
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0374             nan     0.1000    0.2096
##      2        1.8708             nan     0.1000    0.1834
##      3        1.7154             nan     0.1000    0.1497
##      4        1.5695             nan     0.1000    0.1245
##      5        1.4563             nan     0.1000    0.0917
##      6        1.3766             nan     0.1000    0.0626
##      7        1.2947             nan     0.1000    0.0715
##      8        1.2341             nan     0.1000    0.0556
##      9        1.1798             nan     0.1000    0.0446
##     10        1.1435             nan     0.1000    0.0168
##     20        0.7581             nan     0.1000    0.0098
##     40        0.4612             nan     0.1000   -0.0020
##     60        0.3345             nan     0.1000    0.0033
##     80        0.2582             nan     0.1000    0.0011
##    100        0.2136             nan     0.1000   -0.0027
##    120        0.1711             nan     0.1000   -0.0016
##    140        0.1421             nan     0.1000   -0.0007
##    150        0.1290             nan     0.1000    0.0001
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9992             nan     0.1000    0.2377
##      2        1.7966             nan     0.1000    0.2033
##      3        1.6166             nan     0.1000    0.1586
##      4        1.4682             nan     0.1000    0.1259
##      5        1.3353             nan     0.1000    0.1019
##      6        1.2312             nan     0.1000    0.0975
##      7        1.1329             nan     0.1000    0.0881
##      8        1.0606             nan     0.1000    0.0656
##      9        1.0013             nan     0.1000    0.0293
##     10        0.9278             nan     0.1000    0.0642
##     20        0.5573             nan     0.1000    0.0142
##     40        0.3013             nan     0.1000    0.0002
##     60        0.1984             nan     0.1000    0.0009
##     80        0.1391             nan     0.1000   -0.0009
##    100        0.1044             nan     0.1000   -0.0011
##    120        0.0799             nan     0.1000    0.0005
##    140        0.0612             nan     0.1000   -0.0004
##    150        0.0522             nan     0.1000   -0.0005
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.1527             nan     0.1000    0.1221
##      2        2.0289             nan     0.1000    0.1087
##      3        1.9366             nan     0.1000    0.0799
##      4        1.8720             nan     0.1000    0.0615
##      5        1.8014             nan     0.1000    0.0622
##      6        1.7367             nan     0.1000    0.0518
##      7        1.6802             nan     0.1000    0.0467
##      8        1.6281             nan     0.1000    0.0392
##      9        1.5771             nan     0.1000    0.0417
##     10        1.5322             nan     0.1000    0.0320
##     20        1.1893             nan     0.1000    0.0265
##     40        0.8587             nan     0.1000    0.0060
##     60        0.6560             nan     0.1000    0.0038
##     80        0.5442             nan     0.1000   -0.0006
##    100        0.4640             nan     0.1000    0.0005
##    120        0.3997             nan     0.1000   -0.0002
##    140        0.3446             nan     0.1000    0.0015
##    150        0.3235             nan     0.1000    0.0001
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0999             nan     0.1000    0.1880
##      2        1.9160             nan     0.1000    0.1612
##      3        1.7720             nan     0.1000    0.1401
##      4        1.6477             nan     0.1000    0.1044
##      5        1.5438             nan     0.1000    0.0919
##      6        1.4702             nan     0.1000    0.0515
##      7        1.3990             nan     0.1000    0.0492
##      8        1.3330             nan     0.1000    0.0570
##      9        1.2678             nan     0.1000    0.0434
##     10        1.2158             nan     0.1000    0.0398
##     20        0.8289             nan     0.1000    0.0246
##     40        0.5061             nan     0.1000    0.0034
##     60        0.3602             nan     0.1000    0.0051
##     80        0.2688             nan     0.1000   -0.0017
##    100        0.2086             nan     0.1000   -0.0002
##    120        0.1680             nan     0.1000    0.0012
##    140        0.1373             nan     0.1000   -0.0011
##    150        0.1246             nan     0.1000   -0.0008
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0481             nan     0.1000    0.2122
##      2        1.8622             nan     0.1000    0.1771
##      3        1.7016             nan     0.1000    0.1355
##      4        1.5600             nan     0.1000    0.1219
##      5        1.4384             nan     0.1000    0.1044
##      6        1.3211             nan     0.1000    0.0898
##      7        1.2370             nan     0.1000    0.0806
##      8        1.1583             nan     0.1000    0.0646
##      9        1.0941             nan     0.1000    0.0428
##     10        1.0366             nan     0.1000    0.0413
##     20        0.6415             nan     0.1000    0.0245
##     40        0.3385             nan     0.1000    0.0074
##     60        0.2236             nan     0.1000   -0.0001
##     80        0.1532             nan     0.1000   -0.0003
##    100        0.1091             nan     0.1000    0.0001
##    120        0.0819             nan     0.1000   -0.0001
##    140        0.0647             nan     0.1000   -0.0003
##    150        0.0573             nan     0.1000    0.0000
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0010             nan     0.1000    0.1569
##      2        1.8551             nan     0.1000    0.1584
##      3        1.7342             nan     0.1000    0.1179
##      4        1.6354             nan     0.1000    0.1072
##      5        1.5511             nan     0.1000    0.0909
##      6        1.4918             nan     0.1000    0.0601
##      7        1.4370             nan     0.1000    0.0501
##      8        1.3947             nan     0.1000    0.0252
##      9        1.3490             nan     0.1000    0.0423
##     10        1.3118             nan     0.1000    0.0361
##     20        1.0489             nan     0.1000    0.0167
##     40        0.7691             nan     0.1000    0.0028
##     60        0.6035             nan     0.1000    0.0016
##     80        0.4943             nan     0.1000    0.0016
##    100        0.4177             nan     0.1000   -0.0000
##    120        0.3645             nan     0.1000    0.0008
##    140        0.3180             nan     0.1000    0.0003
##    150        0.2993             nan     0.1000   -0.0003
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9674             nan     0.1000    0.2135
##      2        1.7895             nan     0.1000    0.1571
##      3        1.6477             nan     0.1000    0.1387
##      4        1.5125             nan     0.1000    0.1331
##      5        1.3999             nan     0.1000    0.0967
##      6        1.3071             nan     0.1000    0.0836
##      7        1.2338             nan     0.1000    0.0707
##      8        1.1637             nan     0.1000    0.0654
##      9        1.1037             nan     0.1000    0.0503
##     10        1.0562             nan     0.1000    0.0343
##     20        0.7418             nan     0.1000    0.0042
##     40        0.4638             nan     0.1000    0.0072
##     60        0.3292             nan     0.1000    0.0007
##     80        0.2446             nan     0.1000    0.0014
##    100        0.1940             nan     0.1000   -0.0010
##    120        0.1564             nan     0.1000   -0.0001
##    140        0.1290             nan     0.1000   -0.0008
##    150        0.1157             nan     0.1000    0.0007
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9420             nan     0.1000    0.2303
##      2        1.7393             nan     0.1000    0.1821
##      3        1.5708             nan     0.1000    0.1484
##      4        1.4181             nan     0.1000    0.1268
##      5        1.2974             nan     0.1000    0.1081
##      6        1.1970             nan     0.1000    0.0987
##      7        1.1111             nan     0.1000    0.0690
##      8        1.0247             nan     0.1000    0.0893
##      9        0.9593             nan     0.1000    0.0515
##     10        0.9082             nan     0.1000    0.0374
##     20        0.5754             nan     0.1000    0.0115
##     40        0.3184             nan     0.1000    0.0049
##     60        0.1980             nan     0.1000    0.0006
##     80        0.1376             nan     0.1000    0.0003
##    100        0.0998             nan     0.1000   -0.0006
##    120        0.0742             nan     0.1000    0.0001
##    140        0.0552             nan     0.1000   -0.0003
##    150        0.0484             nan     0.1000   -0.0001
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9438             nan     0.1000    0.1684
##      2        1.8103             nan     0.1000    0.1175
##      3        1.7068             nan     0.1000    0.1090
##      4        1.6191             nan     0.1000    0.0739
##      5        1.5535             nan     0.1000    0.0664
##      6        1.5010             nan     0.1000    0.0471
##      7        1.4419             nan     0.1000    0.0447
##      8        1.3953             nan     0.1000    0.0331
##      9        1.3487             nan     0.1000    0.0405
##     10        1.3136             nan     0.1000    0.0156
##     20        1.0365             nan     0.1000    0.0206
##     40        0.7391             nan     0.1000    0.0056
##     60        0.5702             nan     0.1000    0.0058
##     80        0.4640             nan     0.1000    0.0037
##    100        0.3972             nan     0.1000   -0.0030
##    120        0.3395             nan     0.1000   -0.0029
##    140        0.2971             nan     0.1000   -0.0013
##    150        0.2771             nan     0.1000    0.0002
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9002             nan     0.1000    0.1919
##      2        1.7436             nan     0.1000    0.1392
##      3        1.6008             nan     0.1000    0.1460
##      4        1.4880             nan     0.1000    0.1012
##      5        1.3813             nan     0.1000    0.0943
##      6        1.2951             nan     0.1000    0.0801
##      7        1.2225             nan     0.1000    0.0621
##      8        1.1667             nan     0.1000    0.0421
##      9        1.1133             nan     0.1000    0.0424
##     10        1.0646             nan     0.1000    0.0364
##     20        0.7349             nan     0.1000    0.0161
##     40        0.4540             nan     0.1000    0.0032
##     60        0.3226             nan     0.1000    0.0012
##     80        0.2360             nan     0.1000   -0.0009
##    100        0.1851             nan     0.1000    0.0002
##    120        0.1515             nan     0.1000    0.0005
##    140        0.1247             nan     0.1000    0.0003
##    150        0.1137             nan     0.1000    0.0002
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.8752             nan     0.1000    0.2097
##      2        1.6987             nan     0.1000    0.1488
##      3        1.5410             nan     0.1000    0.1474
##      4        1.4091             nan     0.1000    0.1140
##      5        1.3060             nan     0.1000    0.0781
##      6        1.2075             nan     0.1000    0.0912
##      7        1.1257             nan     0.1000    0.0752
##      8        1.0435             nan     0.1000    0.0613
##      9        0.9886             nan     0.1000    0.0447
##     10        0.9214             nan     0.1000    0.0597
##     20        0.5651             nan     0.1000    0.0198
##     40        0.3060             nan     0.1000    0.0000
##     60        0.1954             nan     0.1000    0.0032
##     80        0.1363             nan     0.1000    0.0014
##    100        0.0974             nan     0.1000   -0.0006
##    120        0.0728             nan     0.1000   -0.0004
##    140        0.0565             nan     0.1000   -0.0007
##    150        0.0507             nan     0.1000   -0.0005
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9525             nan     0.1000    0.1695
##      2        1.8158             nan     0.1000    0.1438
##      3        1.6960             nan     0.1000    0.0965
##      4        1.6156             nan     0.1000    0.0832
##      5        1.5348             nan     0.1000    0.0811
##      6        1.4700             nan     0.1000    0.0615
##      7        1.4185             nan     0.1000    0.0459
##      8        1.3750             nan     0.1000    0.0416
##      9        1.3386             nan     0.1000    0.0255
##     10        1.3092             nan     0.1000    0.0235
##     20        1.0400             nan     0.1000    0.0154
##     40        0.7846             nan     0.1000    0.0033
##     60        0.6259             nan     0.1000   -0.0028
##     80        0.5137             nan     0.1000    0.0009
##    100        0.4384             nan     0.1000    0.0015
##    120        0.3783             nan     0.1000    0.0006
##    140        0.3330             nan     0.1000   -0.0029
##    150        0.3125             nan     0.1000    0.0009
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9246             nan     0.1000    0.1937
##      2        1.7396             nan     0.1000    0.1569
##      3        1.6060             nan     0.1000    0.1354
##      4        1.4996             nan     0.1000    0.1031
##      5        1.3969             nan     0.1000    0.0849
##      6        1.3348             nan     0.1000    0.0496
##      7        1.2584             nan     0.1000    0.0659
##      8        1.1931             nan     0.1000    0.0627
##      9        1.1324             nan     0.1000    0.0491
##     10        1.0920             nan     0.1000    0.0186
##     20        0.7701             nan     0.1000    0.0126
##     40        0.4780             nan     0.1000   -0.0003
##     60        0.3315             nan     0.1000    0.0041
##     80        0.2422             nan     0.1000   -0.0009
##    100        0.1895             nan     0.1000   -0.0008
##    120        0.1484             nan     0.1000   -0.0021
##    140        0.1222             nan     0.1000   -0.0002
##    150        0.1117             nan     0.1000   -0.0006
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.8957             nan     0.1000    0.1864
##      2        1.7147             nan     0.1000    0.1853
##      3        1.5560             nan     0.1000    0.1222
##      4        1.4325             nan     0.1000    0.1108
##      5        1.3126             nan     0.1000    0.0991
##      6        1.2194             nan     0.1000    0.0722
##      7        1.1384             nan     0.1000    0.0645
##      8        1.0588             nan     0.1000    0.0661
##      9        1.0014             nan     0.1000    0.0470
##     10        0.9554             nan     0.1000    0.0314
##     20        0.5916             nan     0.1000    0.0147
##     40        0.3202             nan     0.1000    0.0046
##     60        0.2078             nan     0.1000   -0.0005
##     80        0.1460             nan     0.1000   -0.0025
##    100        0.1052             nan     0.1000   -0.0002
##    120        0.0813             nan     0.1000   -0.0008
##    140        0.0647             nan     0.1000   -0.0004
##    150        0.0565             nan     0.1000   -0.0011
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0252             nan     0.1000    0.1557
##      2        1.9134             nan     0.1000    0.1107
##      3        1.8093             nan     0.1000    0.0942
##      4        1.7248             nan     0.1000    0.0726
##      5        1.6541             nan     0.1000    0.0703
##      6        1.5997             nan     0.1000    0.0481
##      7        1.5475             nan     0.1000    0.0448
##      8        1.4927             nan     0.1000    0.0566
##      9        1.4454             nan     0.1000    0.0354
##     10        1.4089             nan     0.1000    0.0223
##     20        1.0853             nan     0.1000    0.0195
##     40        0.7533             nan     0.1000    0.0085
##     60        0.5783             nan     0.1000    0.0009
##     80        0.4696             nan     0.1000    0.0015
##    100        0.4019             nan     0.1000   -0.0019
##    120        0.3503             nan     0.1000    0.0003
##    140        0.3138             nan     0.1000   -0.0010
##    150        0.2959             nan     0.1000   -0.0008
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9691             nan     0.1000    0.1848
##      2        1.8090             nan     0.1000    0.1547
##      3        1.6516             nan     0.1000    0.1493
##      4        1.5179             nan     0.1000    0.1284
##      5        1.4062             nan     0.1000    0.1047
##      6        1.3218             nan     0.1000    0.0670
##      7        1.2434             nan     0.1000    0.0687
##      8        1.1771             nan     0.1000    0.0479
##      9        1.1324             nan     0.1000    0.0239
##     10        1.0769             nan     0.1000    0.0444
##     20        0.7180             nan     0.1000    0.0193
##     40        0.4272             nan     0.1000    0.0022
##     60        0.3078             nan     0.1000    0.0018
##     80        0.2370             nan     0.1000    0.0022
##    100        0.1810             nan     0.1000   -0.0013
##    120        0.1487             nan     0.1000    0.0000
##    140        0.1243             nan     0.1000    0.0000
##    150        0.1132             nan     0.1000    0.0014
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9215             nan     0.1000    0.2262
##      2        1.7282             nan     0.1000    0.1924
##      3        1.5536             nan     0.1000    0.1642
##      4        1.4165             nan     0.1000    0.1107
##      5        1.3059             nan     0.1000    0.0890
##      6        1.1833             nan     0.1000    0.0950
##      7        1.0962             nan     0.1000    0.0714
##      8        1.0123             nan     0.1000    0.0691
##      9        0.9552             nan     0.1000    0.0441
##     10        0.9000             nan     0.1000    0.0460
##     20        0.5471             nan     0.1000    0.0125
##     40        0.3089             nan     0.1000   -0.0033
##     60        0.2011             nan     0.1000   -0.0007
##     80        0.1408             nan     0.1000    0.0005
##    100        0.1038             nan     0.1000   -0.0008
##    120        0.0770             nan     0.1000   -0.0009
##    140        0.0577             nan     0.1000   -0.0007
##    150        0.0496             nan     0.1000   -0.0005
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0334             nan     0.1000    0.1533
##      2        1.9032             nan     0.1000    0.1368
##      3        1.7966             nan     0.1000    0.1024
##      4        1.7196             nan     0.1000    0.0749
##      5        1.6485             nan     0.1000    0.0496
##      6        1.5888             nan     0.1000    0.0449
##      7        1.5359             nan     0.1000    0.0409
##      8        1.4967             nan     0.1000    0.0276
##      9        1.4570             nan     0.1000    0.0316
##     10        1.4166             nan     0.1000    0.0353
##     20        1.1011             nan     0.1000    0.0150
##     40        0.7828             nan     0.1000    0.0002
##     60        0.6131             nan     0.1000    0.0019
##     80        0.5106             nan     0.1000   -0.0021
##    100        0.4429             nan     0.1000   -0.0002
##    120        0.3936             nan     0.1000    0.0001
##    140        0.3481             nan     0.1000    0.0002
##    150        0.3293             nan     0.1000   -0.0031
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9730             nan     0.1000    0.1967
##      2        1.7956             nan     0.1000    0.1641
##      3        1.6489             nan     0.1000    0.1283
##      4        1.5312             nan     0.1000    0.1102
##      5        1.4342             nan     0.1000    0.0798
##      6        1.3439             nan     0.1000    0.0875
##      7        1.2658             nan     0.1000    0.0727
##      8        1.1994             nan     0.1000    0.0557
##      9        1.1376             nan     0.1000    0.0509
##     10        1.0923             nan     0.1000    0.0284
##     20        0.7557             nan     0.1000    0.0162
##     40        0.4726             nan     0.1000    0.0013
##     60        0.3538             nan     0.1000   -0.0001
##     80        0.2672             nan     0.1000    0.0024
##    100        0.2117             nan     0.1000   -0.0022
##    120        0.1732             nan     0.1000    0.0007
##    140        0.1442             nan     0.1000    0.0008
##    150        0.1303             nan     0.1000   -0.0003
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9467             nan     0.1000    0.2236
##      2        1.7532             nan     0.1000    0.2023
##      3        1.5880             nan     0.1000    0.1328
##      4        1.4489             nan     0.1000    0.1245
##      5        1.3261             nan     0.1000    0.1099
##      6        1.2282             nan     0.1000    0.0803
##      7        1.1443             nan     0.1000    0.0604
##      8        1.0745             nan     0.1000    0.0514
##      9        1.0099             nan     0.1000    0.0475
##     10        0.9533             nan     0.1000    0.0353
##     20        0.5777             nan     0.1000    0.0117
##     40        0.3289             nan     0.1000    0.0032
##     60        0.2254             nan     0.1000   -0.0023
##     80        0.1544             nan     0.1000   -0.0005
##    100        0.1160             nan     0.1000   -0.0001
##    120        0.0871             nan     0.1000   -0.0000
##    140        0.0685             nan     0.1000   -0.0002
##    150        0.0602             nan     0.1000    0.0002
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9763             nan     0.1000    0.1306
##      2        1.8682             nan     0.1000    0.1065
##      3        1.7956             nan     0.1000    0.0564
##      4        1.7181             nan     0.1000    0.0580
##      5        1.6513             nan     0.1000    0.0595
##      6        1.5922             nan     0.1000    0.0516
##      7        1.5474             nan     0.1000    0.0438
##      8        1.4956             nan     0.1000    0.0514
##      9        1.4550             nan     0.1000    0.0314
##     10        1.4178             nan     0.1000    0.0241
##     20        1.1260             nan     0.1000    0.0080
##     40        0.8119             nan     0.1000    0.0080
##     60        0.6376             nan     0.1000   -0.0002
##     80        0.5252             nan     0.1000   -0.0001
##    100        0.4427             nan     0.1000    0.0010
##    120        0.3890             nan     0.1000    0.0002
##    140        0.3422             nan     0.1000   -0.0009
##    150        0.3210             nan     0.1000    0.0012
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9233             nan     0.1000    0.1726
##      2        1.7786             nan     0.1000    0.1345
##      3        1.6648             nan     0.1000    0.1041
##      4        1.5572             nan     0.1000    0.1023
##      5        1.4570             nan     0.1000    0.0889
##      6        1.3690             nan     0.1000    0.0787
##      7        1.2859             nan     0.1000    0.0733
##      8        1.2229             nan     0.1000    0.0510
##      9        1.1636             nan     0.1000    0.0508
##     10        1.1162             nan     0.1000    0.0393
##     20        0.8048             nan     0.1000    0.0119
##     40        0.5190             nan     0.1000   -0.0000
##     60        0.3620             nan     0.1000    0.0018
##     80        0.2728             nan     0.1000    0.0001
##    100        0.2121             nan     0.1000    0.0012
##    120        0.1685             nan     0.1000    0.0005
##    140        0.1375             nan     0.1000   -0.0004
##    150        0.1255             nan     0.1000    0.0003
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.8743             nan     0.1000    0.2182
##      2        1.6958             nan     0.1000    0.1502
##      3        1.5439             nan     0.1000    0.1343
##      4        1.4229             nan     0.1000    0.0985
##      5        1.3257             nan     0.1000    0.0923
##      6        1.2309             nan     0.1000    0.0736
##      7        1.1517             nan     0.1000    0.0590
##      8        1.0921             nan     0.1000    0.0530
##      9        1.0307             nan     0.1000    0.0550
##     10        0.9763             nan     0.1000    0.0460
##     20        0.6488             nan     0.1000    0.0146
##     40        0.3654             nan     0.1000    0.0009
##     60        0.2283             nan     0.1000    0.0009
##     80        0.1579             nan     0.1000   -0.0018
##    100        0.1178             nan     0.1000   -0.0002
##    120        0.0904             nan     0.1000   -0.0015
##    140        0.0694             nan     0.1000   -0.0000
##    150        0.0613             nan     0.1000    0.0000
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0530             nan     0.1000    0.1622
##      2        1.9166             nan     0.1000    0.1271
##      3        1.8016             nan     0.1000    0.1001
##      4        1.7110             nan     0.1000    0.0767
##      5        1.6655             nan     0.1000    0.0287
##      6        1.5930             nan     0.1000    0.0706
##      7        1.5249             nan     0.1000    0.0597
##      8        1.4907             nan     0.1000    0.0167
##      9        1.4439             nan     0.1000    0.0388
##     10        1.4056             nan     0.1000    0.0269
##     20        1.0967             nan     0.1000    0.0149
##     40        0.7515             nan     0.1000    0.0116
##     60        0.5858             nan     0.1000    0.0053
##     80        0.4808             nan     0.1000   -0.0001
##    100        0.4154             nan     0.1000   -0.0020
##    120        0.3645             nan     0.1000    0.0007
##    140        0.3262             nan     0.1000   -0.0003
##    150        0.3120             nan     0.1000    0.0002
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0258             nan     0.1000    0.2345
##      2        1.8365             nan     0.1000    0.1662
##      3        1.6951             nan     0.1000    0.1474
##      4        1.5551             nan     0.1000    0.1256
##      5        1.4455             nan     0.1000    0.1079
##      6        1.3534             nan     0.1000    0.0785
##      7        1.2746             nan     0.1000    0.0587
##      8        1.2183             nan     0.1000    0.0336
##      9        1.1512             nan     0.1000    0.0590
##     10        1.0913             nan     0.1000    0.0472
##     20        0.7120             nan     0.1000    0.0172
##     40        0.4393             nan     0.1000    0.0055
##     60        0.3123             nan     0.1000    0.0012
##     80        0.2441             nan     0.1000   -0.0053
##    100        0.1993             nan     0.1000   -0.0011
##    120        0.1646             nan     0.1000   -0.0004
##    140        0.1359             nan     0.1000    0.0005
##    150        0.1243             nan     0.1000   -0.0015
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9798             nan     0.1000    0.2281
##      2        1.7856             nan     0.1000    0.1889
##      3        1.6164             nan     0.1000    0.1305
##      4        1.4588             nan     0.1000    0.1415
##      5        1.3397             nan     0.1000    0.1115
##      6        1.2260             nan     0.1000    0.1081
##      7        1.1464             nan     0.1000    0.0789
##      8        1.0651             nan     0.1000    0.0763
##      9        0.9821             nan     0.1000    0.0701
##     10        0.9272             nan     0.1000    0.0445
##     20        0.5388             nan     0.1000    0.0208
##     40        0.3005             nan     0.1000   -0.0052
##     60        0.1984             nan     0.1000    0.0003
##     80        0.1408             nan     0.1000   -0.0001
##    100        0.1078             nan     0.1000   -0.0008
##    120        0.0820             nan     0.1000   -0.0001
##    140        0.0647             nan     0.1000    0.0000
##    150        0.0584             nan     0.1000   -0.0007
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9996             nan     0.1000    0.1637
##      2        1.8671             nan     0.1000    0.1369
##      3        1.7626             nan     0.1000    0.0889
##      4        1.6703             nan     0.1000    0.1040
##      5        1.5907             nan     0.1000    0.0709
##      6        1.5284             nan     0.1000    0.0616
##      7        1.4672             nan     0.1000    0.0527
##      8        1.4148             nan     0.1000    0.0513
##      9        1.3719             nan     0.1000    0.0331
##     10        1.3306             nan     0.1000    0.0327
##     20        1.0403             nan     0.1000    0.0169
##     40        0.7611             nan     0.1000    0.0067
##     60        0.6167             nan     0.1000   -0.0044
##     80        0.5220             nan     0.1000   -0.0001
##    100        0.4467             nan     0.1000    0.0026
##    120        0.3931             nan     0.1000   -0.0007
##    140        0.3474             nan     0.1000   -0.0002
##    150        0.3279             nan     0.1000    0.0005
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9436             nan     0.1000    0.1962
##      2        1.7703             nan     0.1000    0.1661
##      3        1.6175             nan     0.1000    0.1429
##      4        1.5111             nan     0.1000    0.0966
##      5        1.3942             nan     0.1000    0.1018
##      6        1.3095             nan     0.1000    0.0681
##      7        1.2319             nan     0.1000    0.0747
##      8        1.1598             nan     0.1000    0.0572
##      9        1.1116             nan     0.1000    0.0399
##     10        1.0635             nan     0.1000    0.0420
##     20        0.7554             nan     0.1000    0.0081
##     40        0.4931             nan     0.1000    0.0017
##     60        0.3521             nan     0.1000   -0.0022
##     80        0.2702             nan     0.1000   -0.0017
##    100        0.2142             nan     0.1000    0.0006
##    120        0.1721             nan     0.1000    0.0010
##    140        0.1400             nan     0.1000   -0.0008
##    150        0.1275             nan     0.1000    0.0001
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9131             nan     0.1000    0.2381
##      2        1.7122             nan     0.1000    0.1544
##      3        1.5333             nan     0.1000    0.1729
##      4        1.4031             nan     0.1000    0.1275
##      5        1.2823             nan     0.1000    0.0966
##      6        1.1786             nan     0.1000    0.0987
##      7        1.0997             nan     0.1000    0.0690
##      8        1.0344             nan     0.1000    0.0490
##      9        0.9813             nan     0.1000    0.0395
##     10        0.9313             nan     0.1000    0.0362
##     20        0.5852             nan     0.1000    0.0098
##     40        0.3392             nan     0.1000    0.0026
##     60        0.2232             nan     0.1000    0.0020
##     80        0.1570             nan     0.1000    0.0000
##    100        0.1191             nan     0.1000    0.0000
##    120        0.0914             nan     0.1000   -0.0008
##    140        0.0704             nan     0.1000   -0.0005
##    150        0.0622             nan     0.1000   -0.0004
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        2.0124             nan     0.1000    0.1113
##      2        1.9044             nan     0.1000    0.1067
##      3        1.8141             nan     0.1000    0.0683
##      4        1.7388             nan     0.1000    0.0707
##      5        1.6789             nan     0.1000    0.0494
##      6        1.6277             nan     0.1000    0.0426
##      7        1.5814             nan     0.1000    0.0413
##      8        1.5314             nan     0.1000    0.0482
##      9        1.4938             nan     0.1000    0.0118
##     10        1.4456             nan     0.1000    0.0438
##     20        1.1476             nan     0.1000    0.0227
##     40        0.8135             nan     0.1000    0.0077
##     60        0.6378             nan     0.1000    0.0040
##     80        0.5195             nan     0.1000    0.0007
##    100        0.4390             nan     0.1000    0.0003
##    120        0.3795             nan     0.1000    0.0012
##    140        0.3326             nan     0.1000    0.0002
##    150        0.3129             nan     0.1000    0.0005
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9405             nan     0.1000    0.1962
##      2        1.7828             nan     0.1000    0.1443
##      3        1.6474             nan     0.1000    0.1222
##      4        1.5473             nan     0.1000    0.0925
##      5        1.4554             nan     0.1000    0.0872
##      6        1.3732             nan     0.1000    0.0637
##      7        1.2946             nan     0.1000    0.0672
##      8        1.2315             nan     0.1000    0.0481
##      9        1.1782             nan     0.1000    0.0496
##     10        1.1313             nan     0.1000    0.0355
##     20        0.7834             nan     0.1000    0.0126
##     40        0.4737             nan     0.1000    0.0132
##     60        0.3222             nan     0.1000    0.0037
##     80        0.2395             nan     0.1000    0.0008
##    100        0.1853             nan     0.1000   -0.0005
##    120        0.1469             nan     0.1000    0.0010
##    140        0.1144             nan     0.1000   -0.0007
##    150        0.1048             nan     0.1000   -0.0006
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9239             nan     0.1000    0.2115
##      2        1.7317             nan     0.1000    0.1683
##      3        1.5893             nan     0.1000    0.1170
##      4        1.4577             nan     0.1000    0.1175
##      5        1.3457             nan     0.1000    0.0885
##      6        1.2481             nan     0.1000    0.0811
##      7        1.1715             nan     0.1000    0.0646
##      8        1.0954             nan     0.1000    0.0656
##      9        1.0136             nan     0.1000    0.0835
##     10        0.9379             nan     0.1000    0.0718
##     20        0.5909             nan     0.1000    0.0110
##     40        0.3037             nan     0.1000    0.0012
##     60        0.1925             nan     0.1000    0.0004
##     80        0.1337             nan     0.1000   -0.0013
##    100        0.0958             nan     0.1000    0.0000
##    120        0.0714             nan     0.1000   -0.0002
##    140        0.0540             nan     0.1000   -0.0005
##    150        0.0476             nan     0.1000   -0.0005
## 
## Iter   TrainDeviance   ValidDeviance   StepSize   Improve
##      1        1.9719             nan     0.1000    0.1929
##      2        1.7816             nan     0.1000    0.1860
##      3        1.6160             nan     0.1000    0.1337
##      4        1.4972             nan     0.1000    0.0982
##      5        1.3923             nan     0.1000    0.0821
##      6        1.2940             nan     0.1000    0.0807
##      7        1.2092             nan     0.1000    0.0700
##      8        1.1297             nan     0.1000    0.0641
##      9        1.0732             nan     0.1000    0.0335
##     10        1.0144             nan     0.1000    0.0468
##     20        0.6614             nan     0.1000    0.0085
##     40        0.4099             nan     0.1000    0.0010
##     60        0.3006             nan     0.1000   -0.0016
##     80        0.2229             nan     0.1000   -0.0032
##    100        0.1715             nan     0.1000   -0.0017
##    120        0.1372             nan     0.1000   -0.0021
##    140        0.1098             nan     0.1000   -0.0007
##    150        0.1007             nan     0.1000   -0.0014
```
Table of boost results
========================================================


```r
predictGbm = round(predict(modelFitGbm, pmlsummary2))
table(predictGbm, pmlsummary2$numclass)
```

```
##           
## predictGbm  1  2  3  4  5
##          1 98  1  0  0  0
##          2 11 73  3  0  0
##          3  0  5 66  9  0
##          4  0  0  1 60 14
##          5  0  0  0  0 65
```
Plot boost results
========================================================

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

Predict with random forest
========================================================


```r
modelFitRF <- train(pmlsummary2$numclass ~ .,method="rf",data=pmlsummary2)
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): The
## response has five or fewer unique values.  Are you sure you want to do
## regression?
```
Table of random forest results
========================================================


```r
predictRF = round(predict(modelFitRF, pmlsummary2))
table(predictRF, pmlsummary2$numclass)
```

```
##          
## predictRF   1   2   3   4   5
##         1 101   0   0   0   0
##         2   8  78   0   0   0
##         3   0   1  70   3   0
##         4   0   0   0  66  17
##         5   0   0   0   0  62
```
Plot random forest results
========================================================

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 
Test data
========================================================

- At this point I load up the test data  
- Test data does not have summary rows, and summary columns are NA
- So this model fails on test data.


