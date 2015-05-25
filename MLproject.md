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
Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.8809             nan     0.1000    0.1657
     2        1.7356             nan     0.1000    0.1473
     3        1.6347             nan     0.1000    0.1128
     4        1.5502             nan     0.1000    0.0772
     5        1.4767             nan     0.1000    0.0830
     6        1.4195             nan     0.1000    0.0558
     7        1.3674             nan     0.1000    0.0485
     8        1.3232             nan     0.1000    0.0376
     9        1.2804             nan     0.1000    0.0305
    10        1.2486             nan     0.1000    0.0179
    20        0.9960             nan     0.1000    0.0065
    40        0.7198             nan     0.1000    0.0019
    60        0.5750             nan     0.1000    0.0035
    80        0.4868             nan     0.1000   -0.0063
   100        0.4169             nan     0.1000    0.0003
   120        0.3649             nan     0.1000   -0.0018
   140        0.3238             nan     0.1000   -0.0004
   150        0.3063             nan     0.1000   -0.0009

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.8468             nan     0.1000    0.2052
     2        1.6754             nan     0.1000    0.1691
     3        1.5320             nan     0.1000    0.1267
     4        1.4051             nan     0.1000    0.1089
     5        1.3035             nan     0.1000    0.0988
     6        1.2198             nan     0.1000    0.0672
     7        1.1577             nan     0.1000    0.0603
     8        1.0952             nan     0.1000    0.0569
     9        1.0446             nan     0.1000    0.0450
    10        1.0003             nan     0.1000    0.0347
    20        0.7154             nan     0.1000    0.0078
    40        0.4491             nan     0.1000    0.0044
    60        0.3219             nan     0.1000    0.0037
    80        0.2436             nan     0.1000   -0.0001
   100        0.1919             nan     0.1000   -0.0001
   120        0.1599             nan     0.1000   -0.0022
   140        0.1278             nan     0.1000   -0.0009
   150        0.1172             nan     0.1000   -0.0011

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.8211             nan     0.1000    0.2135
     2        1.6227             nan     0.1000    0.1727
     3        1.4582             nan     0.1000    0.1548
     4        1.3379             nan     0.1000    0.1000
     5        1.2195             nan     0.1000    0.0958
     6        1.1237             nan     0.1000    0.0870
     7        1.0423             nan     0.1000    0.0749
     8        0.9708             nan     0.1000    0.0649
     9        0.9147             nan     0.1000    0.0433
    10        0.8568             nan     0.1000    0.0415
    20        0.5409             nan     0.1000    0.0186
    40        0.3238             nan     0.1000    0.0063
    60        0.2271             nan     0.1000   -0.0006
    80        0.1645             nan     0.1000   -0.0010
   100        0.1210             nan     0.1000    0.0007
   120        0.0920             nan     0.1000   -0.0008
   140        0.0711             nan     0.1000   -0.0006
   150        0.0632             nan     0.1000   -0.0000

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0455             nan     0.1000    0.1452
     2        1.9214             nan     0.1000    0.1101
     3        1.8223             nan     0.1000    0.0857
     4        1.7453             nan     0.1000    0.0771
     5        1.6833             nan     0.1000    0.0544
     6        1.6274             nan     0.1000    0.0498
     7        1.5732             nan     0.1000    0.0488
     8        1.5270             nan     0.1000    0.0363
     9        1.4826             nan     0.1000    0.0349
    10        1.4487             nan     0.1000    0.0211
    20        1.1662             nan     0.1000    0.0127
    40        0.8364             nan     0.1000    0.0105
    60        0.6554             nan     0.1000    0.0014
    80        0.5420             nan     0.1000    0.0006
   100        0.4605             nan     0.1000    0.0009
   120        0.4025             nan     0.1000    0.0020
   140        0.3553             nan     0.1000   -0.0020
   150        0.3346             nan     0.1000   -0.0008

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9940             nan     0.1000    0.1912
     2        1.8392             nan     0.1000    0.1544
     3        1.7017             nan     0.1000    0.1398
     4        1.5878             nan     0.1000    0.1029
     5        1.4988             nan     0.1000    0.0770
     6        1.4111             nan     0.1000    0.0790
     7        1.3512             nan     0.1000    0.0392
     8        1.2775             nan     0.1000    0.0607
     9        1.2137             nan     0.1000    0.0623
    10        1.1590             nan     0.1000    0.0415
    20        0.8040             nan     0.1000    0.0108
    40        0.5036             nan     0.1000    0.0028
    60        0.3592             nan     0.1000    0.0009
    80        0.2711             nan     0.1000    0.0003
   100        0.2109             nan     0.1000    0.0010
   120        0.1724             nan     0.1000   -0.0016
   140        0.1415             nan     0.1000   -0.0004
   150        0.1284             nan     0.1000   -0.0008

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9741             nan     0.1000    0.2160
     2        1.7661             nan     0.1000    0.1925
     3        1.6129             nan     0.1000    0.1340
     4        1.4784             nan     0.1000    0.1163
     5        1.3619             nan     0.1000    0.1170
     6        1.2749             nan     0.1000    0.0711
     7        1.1840             nan     0.1000    0.0754
     8        1.1057             nan     0.1000    0.0608
     9        1.0381             nan     0.1000    0.0524
    10        0.9800             nan     0.1000    0.0427
    20        0.6108             nan     0.1000    0.0196
    40        0.3353             nan     0.1000   -0.0005
    60        0.2190             nan     0.1000   -0.0001
    80        0.1529             nan     0.1000   -0.0006
   100        0.1132             nan     0.1000   -0.0007
   120        0.0865             nan     0.1000   -0.0002
   140        0.0681             nan     0.1000   -0.0007
   150        0.0591             nan     0.1000    0.0001

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.2025             nan     0.1000    0.1938
     2        2.0546             nan     0.1000    0.1309
     3        1.9402             nan     0.1000    0.1183
     4        1.8448             nan     0.1000    0.0998
     5        1.7774             nan     0.1000    0.0632
     6        1.7007             nan     0.1000    0.0714
     7        1.6375             nan     0.1000    0.0553
     8        1.5905             nan     0.1000    0.0318
     9        1.5440             nan     0.1000    0.0324
    10        1.4977             nan     0.1000    0.0303
    20        1.1766             nan     0.1000    0.0238
    40        0.8408             nan     0.1000    0.0072
    60        0.6479             nan     0.1000    0.0046
    80        0.5366             nan     0.1000    0.0008
   100        0.4611             nan     0.1000   -0.0002
   120        0.4039             nan     0.1000   -0.0023
   140        0.3582             nan     0.1000    0.0008
   150        0.3368             nan     0.1000    0.0012

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.1444             nan     0.1000    0.2534
     2        1.9474             nan     0.1000    0.1681
     3        1.8017             nan     0.1000    0.1206
     4        1.6635             nan     0.1000    0.1239
     5        1.5442             nan     0.1000    0.1075
     6        1.4476             nan     0.1000    0.0830
     7        1.3664             nan     0.1000    0.0616
     8        1.2953             nan     0.1000    0.0608
     9        1.2234             nan     0.1000    0.0678
    10        1.1608             nan     0.1000    0.0595
    20        0.7864             nan     0.1000    0.0209
    40        0.4963             nan     0.1000    0.0051
    60        0.3535             nan     0.1000    0.0022
    80        0.2653             nan     0.1000    0.0007
   100        0.2116             nan     0.1000   -0.0009
   120        0.1678             nan     0.1000    0.0005
   140        0.1369             nan     0.1000    0.0006
   150        0.1255             nan     0.1000   -0.0003

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.1114             nan     0.1000    0.2619
     2        1.8793             nan     0.1000    0.2141
     3        1.6942             nan     0.1000    0.1725
     4        1.5354             nan     0.1000    0.1390
     5        1.3997             nan     0.1000    0.1231
     6        1.2894             nan     0.1000    0.1002
     7        1.1912             nan     0.1000    0.0811
     8        1.0928             nan     0.1000    0.0830
     9        1.0150             nan     0.1000    0.0708
    10        0.9528             nan     0.1000    0.0465
    20        0.5915             nan     0.1000    0.0181
    40        0.3323             nan     0.1000    0.0008
    60        0.2172             nan     0.1000    0.0000
    80        0.1523             nan     0.1000    0.0013
   100        0.1067             nan     0.1000   -0.0002
   120        0.0787             nan     0.1000   -0.0000
   140        0.0593             nan     0.1000   -0.0003
   150        0.0522             nan     0.1000   -0.0004

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.1411             nan     0.1000    0.1993
     2        1.9866             nan     0.1000    0.1313
     3        1.8784             nan     0.1000    0.1001
     4        1.7831             nan     0.1000    0.0894
     5        1.7087             nan     0.1000    0.0623
     6        1.6372             nan     0.1000    0.0628
     7        1.5830             nan     0.1000    0.0575
     8        1.5334             nan     0.1000    0.0418
     9        1.4809             nan     0.1000    0.0497
    10        1.4388             nan     0.1000    0.0336
    20        1.1247             nan     0.1000    0.0142
    40        0.7581             nan     0.1000    0.0064
    60        0.5823             nan     0.1000    0.0020
    80        0.4772             nan     0.1000   -0.0018
   100        0.4052             nan     0.1000    0.0000
   120        0.3559             nan     0.1000   -0.0013
   140        0.3147             nan     0.1000   -0.0017
   150        0.2967             nan     0.1000   -0.0014

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0658             nan     0.1000    0.2317
     2        1.8594             nan     0.1000    0.1777
     3        1.6856             nan     0.1000    0.1597
     4        1.5476             nan     0.1000    0.1260
     5        1.4350             nan     0.1000    0.1110
     6        1.3255             nan     0.1000    0.0851
     7        1.2403             nan     0.1000    0.0720
     8        1.1744             nan     0.1000    0.0622
     9        1.1108             nan     0.1000    0.0576
    10        1.0660             nan     0.1000    0.0384
    20        0.7098             nan     0.1000    0.0195
    40        0.4331             nan     0.1000    0.0004
    60        0.3141             nan     0.1000   -0.0004
    80        0.2389             nan     0.1000    0.0010
   100        0.1920             nan     0.1000    0.0001
   120        0.1543             nan     0.1000   -0.0005
   140        0.1276             nan     0.1000   -0.0004
   150        0.1148             nan     0.1000   -0.0003

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0272             nan     0.1000    0.2633
     2        1.8073             nan     0.1000    0.2403
     3        1.6160             nan     0.1000    0.1642
     4        1.4590             nan     0.1000    0.1488
     5        1.3293             nan     0.1000    0.1193
     6        1.2111             nan     0.1000    0.1138
     7        1.1206             nan     0.1000    0.0861
     8        1.0291             nan     0.1000    0.0742
     9        0.9668             nan     0.1000    0.0533
    10        0.9092             nan     0.1000    0.0485
    20        0.5238             nan     0.1000    0.0156
    40        0.2903             nan     0.1000    0.0028
    60        0.1981             nan     0.1000    0.0009
    80        0.1445             nan     0.1000   -0.0002
   100        0.1080             nan     0.1000    0.0004
   120        0.0820             nan     0.1000   -0.0012
   140        0.0648             nan     0.1000   -0.0005
   150        0.0579             nan     0.1000   -0.0006

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0057             nan     0.1000    0.1290
     2        1.8838             nan     0.1000    0.1182
     3        1.7901             nan     0.1000    0.1065
     4        1.7150             nan     0.1000    0.0763
     5        1.6445             nan     0.1000    0.0506
     6        1.5886             nan     0.1000    0.0408
     7        1.5453             nan     0.1000    0.0219
     8        1.4962             nan     0.1000    0.0394
     9        1.4564             nan     0.1000    0.0294
    10        1.4192             nan     0.1000    0.0250
    20        1.1498             nan     0.1000    0.0174
    40        0.8413             nan     0.1000    0.0071
    60        0.6648             nan     0.1000    0.0008
    80        0.5460             nan     0.1000    0.0044
   100        0.4633             nan     0.1000   -0.0008
   120        0.4080             nan     0.1000    0.0008
   140        0.3561             nan     0.1000   -0.0013
   150        0.3333             nan     0.1000   -0.0001

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9688             nan     0.1000    0.1697
     2        1.8224             nan     0.1000    0.1457
     3        1.6933             nan     0.1000    0.1042
     4        1.5898             nan     0.1000    0.0870
     5        1.4940             nan     0.1000    0.0989
     6        1.4090             nan     0.1000    0.0701
     7        1.3361             nan     0.1000    0.0646
     8        1.2792             nan     0.1000    0.0453
     9        1.2325             nan     0.1000    0.0344
    10        1.1781             nan     0.1000    0.0363
    20        0.8425             nan     0.1000    0.0157
    40        0.5118             nan     0.1000    0.0031
    60        0.3477             nan     0.1000    0.0001
    80        0.2606             nan     0.1000   -0.0015
   100        0.1999             nan     0.1000    0.0004
   120        0.1596             nan     0.1000    0.0005
   140        0.1295             nan     0.1000   -0.0019
   150        0.1154             nan     0.1000   -0.0011

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9511             nan     0.1000    0.1942
     2        1.7847             nan     0.1000    0.1302
     3        1.6365             nan     0.1000    0.1103
     4        1.5130             nan     0.1000    0.0850
     5        1.4111             nan     0.1000    0.0756
     6        1.3090             nan     0.1000    0.0848
     7        1.2272             nan     0.1000    0.0678
     8        1.1524             nan     0.1000    0.0563
     9        1.0891             nan     0.1000    0.0470
    10        1.0342             nan     0.1000    0.0458
    20        0.6692             nan     0.1000    0.0135
    40        0.3662             nan     0.1000    0.0002
    60        0.2291             nan     0.1000    0.0019
    80        0.1563             nan     0.1000    0.0000
   100        0.1114             nan     0.1000   -0.0001
   120        0.0838             nan     0.1000   -0.0005
   140        0.0643             nan     0.1000   -0.0009
   150        0.0554             nan     0.1000    0.0002

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0687             nan     0.1000    0.1757
     2        1.9349             nan     0.1000    0.1466
     3        1.8157             nan     0.1000    0.1083
     4        1.7169             nan     0.1000    0.0874
     5        1.6386             nan     0.1000    0.0634
     6        1.5776             nan     0.1000    0.0573
     7        1.5236             nan     0.1000    0.0500
     8        1.4741             nan     0.1000    0.0404
     9        1.4208             nan     0.1000    0.0385
    10        1.3753             nan     0.1000    0.0436
    20        1.0727             nan     0.1000    0.0148
    40        0.7760             nan     0.1000   -0.0009
    60        0.6124             nan     0.1000    0.0019
    80        0.5064             nan     0.1000    0.0023
   100        0.4365             nan     0.1000    0.0006
   120        0.3876             nan     0.1000   -0.0014
   140        0.3392             nan     0.1000   -0.0005
   150        0.3185             nan     0.1000   -0.0004

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0206             nan     0.1000    0.2365
     2        1.8309             nan     0.1000    0.1630
     3        1.6815             nan     0.1000    0.1415
     4        1.5558             nan     0.1000    0.1045
     5        1.4496             nan     0.1000    0.1041
     6        1.3589             nan     0.1000    0.0975
     7        1.2814             nan     0.1000    0.0685
     8        1.2287             nan     0.1000    0.0240
     9        1.1702             nan     0.1000    0.0468
    10        1.1225             nan     0.1000    0.0246
    20        0.7755             nan     0.1000    0.0032
    40        0.4869             nan     0.1000    0.0053
    60        0.3486             nan     0.1000    0.0034
    80        0.2601             nan     0.1000    0.0003
   100        0.2011             nan     0.1000   -0.0006
   120        0.1621             nan     0.1000    0.0000
   140        0.1341             nan     0.1000    0.0002
   150        0.1225             nan     0.1000    0.0004

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9975             nan     0.1000    0.2166
     2        1.7827             nan     0.1000    0.2107
     3        1.6189             nan     0.1000    0.1492
     4        1.4710             nan     0.1000    0.1408
     5        1.3509             nan     0.1000    0.1040
     6        1.2601             nan     0.1000    0.0581
     7        1.1651             nan     0.1000    0.0775
     8        1.0893             nan     0.1000    0.0592
     9        1.0204             nan     0.1000    0.0608
    10        0.9665             nan     0.1000    0.0369
    20        0.6205             nan     0.1000    0.0218
    40        0.3421             nan     0.1000    0.0017
    60        0.2242             nan     0.1000   -0.0004
    80        0.1512             nan     0.1000    0.0008
   100        0.1098             nan     0.1000    0.0003
   120        0.0850             nan     0.1000   -0.0008
   140        0.0634             nan     0.1000    0.0002
   150        0.0568             nan     0.1000   -0.0005

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9985             nan     0.1000    0.1529
     2        1.8672             nan     0.1000    0.1172
     3        1.7623             nan     0.1000    0.0993
     4        1.6713             nan     0.1000    0.0825
     5        1.6061             nan     0.1000    0.0625
     6        1.5504             nan     0.1000    0.0481
     7        1.5105             nan     0.1000    0.0262
     8        1.4591             nan     0.1000    0.0463
     9        1.4178             nan     0.1000    0.0287
    10        1.3752             nan     0.1000    0.0379
    20        1.0837             nan     0.1000    0.0156
    40        0.7787             nan     0.1000    0.0037
    60        0.6103             nan     0.1000    0.0031
    80        0.4859             nan     0.1000    0.0027
   100        0.4036             nan     0.1000    0.0009
   120        0.3468             nan     0.1000    0.0006
   140        0.3017             nan     0.1000   -0.0004
   150        0.2835             nan     0.1000   -0.0002

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9470             nan     0.1000    0.1994
     2        1.7619             nan     0.1000    0.1631
     3        1.6352             nan     0.1000    0.1298
     4        1.5077             nan     0.1000    0.1050
     5        1.4086             nan     0.1000    0.0866
     6        1.3243             nan     0.1000    0.0745
     7        1.2654             nan     0.1000    0.0577
     8        1.1974             nan     0.1000    0.0630
     9        1.1393             nan     0.1000    0.0474
    10        1.0943             nan     0.1000    0.0300
    20        0.7551             nan     0.1000    0.0095
    40        0.4531             nan     0.1000    0.0077
    60        0.3172             nan     0.1000   -0.0003
    80        0.2433             nan     0.1000    0.0002
   100        0.1893             nan     0.1000    0.0016
   120        0.1549             nan     0.1000   -0.0006
   140        0.1287             nan     0.1000   -0.0009
   150        0.1150             nan     0.1000    0.0001

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9122             nan     0.1000    0.2339
     2        1.7114             nan     0.1000    0.1744
     3        1.5499             nan     0.1000    0.1576
     4        1.4121             nan     0.1000    0.1231
     5        1.3062             nan     0.1000    0.0881
     6        1.2029             nan     0.1000    0.0824
     7        1.1184             nan     0.1000    0.0761
     8        1.0645             nan     0.1000    0.0307
     9        1.0137             nan     0.1000    0.0288
    10        0.9511             nan     0.1000    0.0535
    20        0.5889             nan     0.1000    0.0109
    40        0.3065             nan     0.1000    0.0023
    60        0.2065             nan     0.1000   -0.0002
    80        0.1515             nan     0.1000   -0.0006
   100        0.1149             nan     0.1000   -0.0003
   120        0.0892             nan     0.1000   -0.0012
   140        0.0691             nan     0.1000   -0.0002
   150        0.0594             nan     0.1000   -0.0002

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0699             nan     0.1000    0.1992
     2        1.9130             nan     0.1000    0.1595
     3        1.7813             nan     0.1000    0.1304
     4        1.6833             nan     0.1000    0.0989
     5        1.5992             nan     0.1000    0.0883
     6        1.5295             nan     0.1000    0.0712
     7        1.4680             nan     0.1000    0.0530
     8        1.4136             nan     0.1000    0.0431
     9        1.3662             nan     0.1000    0.0364
    10        1.3234             nan     0.1000    0.0406
    20        1.0028             nan     0.1000    0.0169
    40        0.7089             nan     0.1000    0.0037
    60        0.5565             nan     0.1000    0.0016
    80        0.4616             nan     0.1000   -0.0015
   100        0.3972             nan     0.1000   -0.0015
   120        0.3495             nan     0.1000    0.0007
   140        0.3073             nan     0.1000    0.0001
   150        0.2905             nan     0.1000    0.0007

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0289             nan     0.1000    0.2286
     2        1.8351             nan     0.1000    0.2060
     3        1.6682             nan     0.1000    0.1572
     4        1.5236             nan     0.1000    0.1332
     5        1.4002             nan     0.1000    0.1019
     6        1.2989             nan     0.1000    0.0929
     7        1.2089             nan     0.1000    0.0791
     8        1.1412             nan     0.1000    0.0637
     9        1.0824             nan     0.1000    0.0491
    10        1.0276             nan     0.1000    0.0456
    20        0.7080             nan     0.1000    0.0134
    40        0.4369             nan     0.1000    0.0026
    60        0.3136             nan     0.1000    0.0036
    80        0.2408             nan     0.1000    0.0006
   100        0.1880             nan     0.1000    0.0009
   120        0.1480             nan     0.1000   -0.0002
   140        0.1214             nan     0.1000   -0.0005
   150        0.1118             nan     0.1000   -0.0005

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9979             nan     0.1000    0.2424
     2        1.7698             nan     0.1000    0.2008
     3        1.5742             nan     0.1000    0.1700
     4        1.4285             nan     0.1000    0.1354
     5        1.3044             nan     0.1000    0.1153
     6        1.1847             nan     0.1000    0.1069
     7        1.0959             nan     0.1000    0.0785
     8        1.0082             nan     0.1000    0.0741
     9        0.9368             nan     0.1000    0.0602
    10        0.8753             nan     0.1000    0.0485
    20        0.5624             nan     0.1000    0.0075
    40        0.3194             nan     0.1000    0.0019
    60        0.2109             nan     0.1000    0.0013
    80        0.1467             nan     0.1000   -0.0015
   100        0.1082             nan     0.1000   -0.0012
   120        0.0814             nan     0.1000   -0.0003
   140        0.0628             nan     0.1000   -0.0007
   150        0.0545             nan     0.1000   -0.0006

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0014             nan     0.1000    0.1797
     2        1.8597             nan     0.1000    0.1393
     3        1.7469             nan     0.1000    0.1018
     4        1.6536             nan     0.1000    0.1036
     5        1.5710             nan     0.1000    0.0708
     6        1.5107             nan     0.1000    0.0611
     7        1.4491             nan     0.1000    0.0584
     8        1.3976             nan     0.1000    0.0419
     9        1.3613             nan     0.1000    0.0313
    10        1.3266             nan     0.1000    0.0265
    20        1.0686             nan     0.1000    0.0151
    40        0.8010             nan     0.1000    0.0056
    60        0.6352             nan     0.1000    0.0025
    80        0.5253             nan     0.1000    0.0022
   100        0.4464             nan     0.1000   -0.0008
   120        0.3855             nan     0.1000   -0.0013
   140        0.3390             nan     0.1000   -0.0015
   150        0.3180             nan     0.1000   -0.0008

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9572             nan     0.1000    0.2002
     2        1.7821             nan     0.1000    0.1755
     3        1.6297             nan     0.1000    0.1399
     4        1.5110             nan     0.1000    0.1306
     5        1.3998             nan     0.1000    0.0872
     6        1.3145             nan     0.1000    0.0684
     7        1.2441             nan     0.1000    0.0504
     8        1.1757             nan     0.1000    0.0453
     9        1.1252             nan     0.1000    0.0423
    10        1.0811             nan     0.1000    0.0276
    20        0.7723             nan     0.1000    0.0043
    40        0.4994             nan     0.1000    0.0077
    60        0.3454             nan     0.1000   -0.0000
    80        0.2574             nan     0.1000    0.0001
   100        0.2031             nan     0.1000    0.0007
   120        0.1609             nan     0.1000    0.0002
   140        0.1332             nan     0.1000   -0.0006
   150        0.1195             nan     0.1000   -0.0004

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9393             nan     0.1000    0.2151
     2        1.7379             nan     0.1000    0.1755
     3        1.5714             nan     0.1000    0.1450
     4        1.4359             nan     0.1000    0.1297
     5        1.3231             nan     0.1000    0.0939
     6        1.2234             nan     0.1000    0.0740
     7        1.1357             nan     0.1000    0.0731
     8        1.0641             nan     0.1000    0.0538
     9        0.9993             nan     0.1000    0.0522
    10        0.9426             nan     0.1000    0.0377
    20        0.5915             nan     0.1000    0.0176
    40        0.3287             nan     0.1000    0.0003
    60        0.2108             nan     0.1000    0.0018
    80        0.1452             nan     0.1000    0.0008
   100        0.1057             nan     0.1000   -0.0000
   120        0.0798             nan     0.1000   -0.0002
   140        0.0606             nan     0.1000   -0.0012
   150        0.0537             nan     0.1000   -0.0003

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9962             nan     0.1000    0.1478
     2        1.8722             nan     0.1000    0.1467
     3        1.7725             nan     0.1000    0.1045
     4        1.6845             nan     0.1000    0.0882
     5        1.6131             nan     0.1000    0.0589
     6        1.5619             nan     0.1000    0.0424
     7        1.5131             nan     0.1000    0.0468
     8        1.4709             nan     0.1000    0.0419
     9        1.4413             nan     0.1000    0.0182
    10        1.4044             nan     0.1000    0.0376
    20        1.1651             nan     0.1000    0.0160
    40        0.8559             nan     0.1000    0.0003
    60        0.6773             nan     0.1000   -0.0064
    80        0.5579             nan     0.1000    0.0019
   100        0.4749             nan     0.1000   -0.0015
   120        0.4094             nan     0.1000    0.0028
   140        0.3531             nan     0.1000   -0.0037
   150        0.3284             nan     0.1000    0.0005

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9632             nan     0.1000    0.1886
     2        1.8034             nan     0.1000    0.1862
     3        1.6715             nan     0.1000    0.1109
     4        1.5686             nan     0.1000    0.0841
     5        1.4713             nan     0.1000    0.0847
     6        1.3977             nan     0.1000    0.0635
     7        1.3230             nan     0.1000    0.0633
     8        1.2664             nan     0.1000    0.0406
     9        1.2109             nan     0.1000    0.0375
    10        1.1641             nan     0.1000    0.0328
    20        0.8311             nan     0.1000    0.0216
    40        0.5072             nan     0.1000    0.0076
    60        0.3572             nan     0.1000    0.0026
    80        0.2694             nan     0.1000   -0.0003
   100        0.2052             nan     0.1000   -0.0009
   120        0.1585             nan     0.1000   -0.0015
   140        0.1287             nan     0.1000    0.0005
   150        0.1164             nan     0.1000   -0.0012

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9277             nan     0.1000    0.1896
     2        1.7483             nan     0.1000    0.1664
     3        1.5880             nan     0.1000    0.1464
     4        1.4593             nan     0.1000    0.1133
     5        1.3551             nan     0.1000    0.0880
     6        1.2628             nan     0.1000    0.0668
     7        1.1812             nan     0.1000    0.0769
     8        1.1102             nan     0.1000    0.0563
     9        1.0460             nan     0.1000    0.0527
    10        0.9879             nan     0.1000    0.0492
    20        0.6228             nan     0.1000    0.0139
    40        0.3556             nan     0.1000    0.0051
    60        0.2277             nan     0.1000    0.0008
    80        0.1547             nan     0.1000    0.0006
   100        0.1128             nan     0.1000   -0.0005
   120        0.0825             nan     0.1000   -0.0002
   140        0.0638             nan     0.1000   -0.0003
   150        0.0556             nan     0.1000   -0.0004

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.1038             nan     0.1000    0.1652
     2        1.9742             nan     0.1000    0.1211
     3        1.8673             nan     0.1000    0.1017
     4        1.7793             nan     0.1000    0.0860
     5        1.7095             nan     0.1000    0.0677
     6        1.6666             nan     0.1000    0.0319
     7        1.6070             nan     0.1000    0.0555
     8        1.5587             nan     0.1000    0.0389
     9        1.5113             nan     0.1000    0.0341
    10        1.4695             nan     0.1000    0.0232
    20        1.1685             nan     0.1000    0.0102
    40        0.8419             nan     0.1000    0.0081
    60        0.6744             nan     0.1000    0.0038
    80        0.5537             nan     0.1000    0.0008
   100        0.4695             nan     0.1000    0.0018
   120        0.4111             nan     0.1000   -0.0002
   140        0.3568             nan     0.1000   -0.0010
   150        0.3386             nan     0.1000    0.0006

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0660             nan     0.1000    0.2050
     2        1.9037             nan     0.1000    0.1434
     3        1.7520             nan     0.1000    0.1304
     4        1.6363             nan     0.1000    0.1043
     5        1.5337             nan     0.1000    0.1034
     6        1.4340             nan     0.1000    0.0815
     7        1.3643             nan     0.1000    0.0545
     8        1.2991             nan     0.1000    0.0512
     9        1.2411             nan     0.1000    0.0362
    10        1.1885             nan     0.1000    0.0382
    20        0.8350             nan     0.1000    0.0142
    40        0.5245             nan     0.1000    0.0024
    60        0.3654             nan     0.1000    0.0022
    80        0.2775             nan     0.1000    0.0006
   100        0.2174             nan     0.1000    0.0012
   120        0.1709             nan     0.1000    0.0001
   140        0.1400             nan     0.1000   -0.0005
   150        0.1278             nan     0.1000   -0.0005

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0360             nan     0.1000    0.2317
     2        1.8339             nan     0.1000    0.2123
     3        1.6716             nan     0.1000    0.1401
     4        1.5306             nan     0.1000    0.1323
     5        1.4111             nan     0.1000    0.1084
     6        1.3186             nan     0.1000    0.0836
     7        1.2425             nan     0.1000    0.0646
     8        1.1578             nan     0.1000    0.0659
     9        1.0801             nan     0.1000    0.0551
    10        1.0306             nan     0.1000    0.0421
    20        0.6509             nan     0.1000    0.0182
    40        0.3525             nan     0.1000    0.0021
    60        0.2230             nan     0.1000    0.0000
    80        0.1524             nan     0.1000    0.0004
   100        0.1121             nan     0.1000   -0.0006
   120        0.0798             nan     0.1000    0.0003
   140        0.0605             nan     0.1000   -0.0002
   150        0.0528             nan     0.1000    0.0001

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.1169             nan     0.1000    0.1543
     2        2.0026             nan     0.1000    0.1117
     3        1.8963             nan     0.1000    0.0934
     4        1.8199             nan     0.1000    0.0703
     5        1.7399             nan     0.1000    0.0877
     6        1.6780             nan     0.1000    0.0576
     7        1.6245             nan     0.1000    0.0381
     8        1.5703             nan     0.1000    0.0547
     9        1.5290             nan     0.1000    0.0273
    10        1.4742             nan     0.1000    0.0406
    20        1.1430             nan     0.1000    0.0228
    40        0.8050             nan     0.1000    0.0040
    60        0.6312             nan     0.1000    0.0052
    80        0.5210             nan     0.1000   -0.0012
   100        0.4461             nan     0.1000    0.0006
   120        0.3945             nan     0.1000    0.0006
   140        0.3473             nan     0.1000    0.0001
   150        0.3300             nan     0.1000    0.0000

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0690             nan     0.1000    0.1982
     2        1.8935             nan     0.1000    0.1723
     3        1.7644             nan     0.1000    0.1309
     4        1.6386             nan     0.1000    0.1224
     5        1.5257             nan     0.1000    0.0934
     6        1.4335             nan     0.1000    0.0766
     7        1.3591             nan     0.1000    0.0656
     8        1.3078             nan     0.1000    0.0282
     9        1.2549             nan     0.1000    0.0219
    10        1.1943             nan     0.1000    0.0495
    20        0.7836             nan     0.1000    0.0217
    40        0.4803             nan     0.1000    0.0075
    60        0.3365             nan     0.1000    0.0005
    80        0.2492             nan     0.1000    0.0007
   100        0.1995             nan     0.1000   -0.0002
   120        0.1618             nan     0.1000    0.0003
   140        0.1329             nan     0.1000   -0.0002
   150        0.1220             nan     0.1000    0.0008

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0432             nan     0.1000    0.2123
     2        1.8512             nan     0.1000    0.1720
     3        1.6878             nan     0.1000    0.1297
     4        1.5422             nan     0.1000    0.1151
     5        1.4091             nan     0.1000    0.1173
     6        1.3026             nan     0.1000    0.0938
     7        1.1970             nan     0.1000    0.0866
     8        1.1176             nan     0.1000    0.0670
     9        1.0462             nan     0.1000    0.0521
    10        0.9862             nan     0.1000    0.0551
    20        0.5939             nan     0.1000    0.0102
    40        0.3269             nan     0.1000   -0.0015
    60        0.2083             nan     0.1000    0.0020
    80        0.1471             nan     0.1000    0.0004
   100        0.1099             nan     0.1000   -0.0010
   120        0.0828             nan     0.1000   -0.0007
   140        0.0637             nan     0.1000   -0.0012
   150        0.0555             nan     0.1000   -0.0006

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0348             nan     0.1000    0.1613
     2        1.8878             nan     0.1000    0.1302
     3        1.7749             nan     0.1000    0.1207
     4        1.6779             nan     0.1000    0.1020
     5        1.6014             nan     0.1000    0.0826
     6        1.5339             nan     0.1000    0.0558
     7        1.4740             nan     0.1000    0.0584
     8        1.4266             nan     0.1000    0.0400
     9        1.3798             nan     0.1000    0.0438
    10        1.3493             nan     0.1000    0.0108
    20        1.0777             nan     0.1000    0.0109
    40        0.7880             nan     0.1000    0.0089
    60        0.6225             nan     0.1000    0.0034
    80        0.5169             nan     0.1000    0.0044
   100        0.4326             nan     0.1000   -0.0012
   120        0.3739             nan     0.1000   -0.0028
   140        0.3257             nan     0.1000    0.0014
   150        0.3063             nan     0.1000    0.0002

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9844             nan     0.1000    0.1643
     2        1.7963             nan     0.1000    0.1737
     3        1.6534             nan     0.1000    0.1451
     4        1.5315             nan     0.1000    0.1160
     5        1.4275             nan     0.1000    0.0838
     6        1.3329             nan     0.1000    0.0753
     7        1.2622             nan     0.1000    0.0583
     8        1.2016             nan     0.1000    0.0517
     9        1.1307             nan     0.1000    0.0654
    10        1.0854             nan     0.1000    0.0313
    20        0.7624             nan     0.1000    0.0245
    40        0.4804             nan     0.1000   -0.0010
    60        0.3330             nan     0.1000   -0.0003
    80        0.2425             nan     0.1000    0.0009
   100        0.1827             nan     0.1000   -0.0006
   120        0.1405             nan     0.1000   -0.0004
   140        0.1122             nan     0.1000    0.0006
   150        0.1002             nan     0.1000    0.0001

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9564             nan     0.1000    0.2251
     2        1.7669             nan     0.1000    0.1779
     3        1.5947             nan     0.1000    0.1613
     4        1.4503             nan     0.1000    0.1354
     5        1.3202             nan     0.1000    0.1258
     6        1.2174             nan     0.1000    0.0872
     7        1.1288             nan     0.1000    0.0671
     8        1.0485             nan     0.1000    0.0556
     9        0.9849             nan     0.1000    0.0583
    10        0.9267             nan     0.1000    0.0423
    20        0.5611             nan     0.1000    0.0055
    40        0.3005             nan     0.1000   -0.0003
    60        0.1900             nan     0.1000   -0.0008
    80        0.1274             nan     0.1000   -0.0010
   100        0.0867             nan     0.1000    0.0008
   120        0.0637             nan     0.1000    0.0002
   140        0.0482             nan     0.1000   -0.0004
   150        0.0421             nan     0.1000   -0.0005

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0566             nan     0.1000    0.2082
     2        1.9148             nan     0.1000    0.1417
     3        1.8004             nan     0.1000    0.1003
     4        1.6941             nan     0.1000    0.0988
     5        1.6187             nan     0.1000    0.0723
     6        1.5569             nan     0.1000    0.0503
     7        1.5035             nan     0.1000    0.0400
     8        1.4515             nan     0.1000    0.0382
     9        1.4061             nan     0.1000    0.0360
    10        1.3609             nan     0.1000    0.0464
    20        1.0627             nan     0.1000    0.0144
    40        0.7597             nan     0.1000    0.0024
    60        0.5690             nan     0.1000    0.0010
    80        0.4608             nan     0.1000    0.0012
   100        0.3881             nan     0.1000    0.0003
   120        0.3341             nan     0.1000   -0.0000
   140        0.2939             nan     0.1000   -0.0017
   150        0.2748             nan     0.1000   -0.0016

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0231             nan     0.1000    0.2199
     2        1.8445             nan     0.1000    0.1795
     3        1.6922             nan     0.1000    0.1547
     4        1.5581             nan     0.1000    0.1338
     5        1.4738             nan     0.1000    0.0650
     6        1.3725             nan     0.1000    0.0857
     7        1.2923             nan     0.1000    0.0642
     8        1.2132             nan     0.1000    0.0656
     9        1.1475             nan     0.1000    0.0583
    10        1.0839             nan     0.1000    0.0475
    20        0.7349             nan     0.1000    0.0125
    40        0.4302             nan     0.1000    0.0014
    60        0.2905             nan     0.1000    0.0016
    80        0.2212             nan     0.1000    0.0009
   100        0.1773             nan     0.1000   -0.0007
   120        0.1431             nan     0.1000   -0.0004
   140        0.1183             nan     0.1000   -0.0010
   150        0.1085             nan     0.1000   -0.0004

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0053             nan     0.1000    0.2175
     2        1.7961             nan     0.1000    0.1434
     3        1.6092             nan     0.1000    0.1599
     4        1.4618             nan     0.1000    0.1197
     5        1.3472             nan     0.1000    0.0846
     6        1.2216             nan     0.1000    0.1161
     7        1.1372             nan     0.1000    0.0758
     8        1.0553             nan     0.1000    0.0676
     9        0.9729             nan     0.1000    0.0826
    10        0.9167             nan     0.1000    0.0434
    20        0.5539             nan     0.1000    0.0149
    40        0.2780             nan     0.1000    0.0046
    60        0.1857             nan     0.1000    0.0015
    80        0.1302             nan     0.1000   -0.0008
   100        0.0960             nan     0.1000   -0.0011
   120        0.0742             nan     0.1000   -0.0009
   140        0.0579             nan     0.1000   -0.0006
   150        0.0506             nan     0.1000    0.0002

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0848             nan     0.1000    0.1612
     2        1.9442             nan     0.1000    0.1411
     3        1.8393             nan     0.1000    0.1067
     4        1.7458             nan     0.1000    0.0951
     5        1.6555             nan     0.1000    0.0673
     6        1.5780             nan     0.1000    0.0753
     7        1.5238             nan     0.1000    0.0456
     8        1.4673             nan     0.1000    0.0504
     9        1.4246             nan     0.1000    0.0225
    10        1.3747             nan     0.1000    0.0461
    20        1.0413             nan     0.1000    0.0237
    40        0.7372             nan     0.1000    0.0056
    60        0.5880             nan     0.1000    0.0038
    80        0.5030             nan     0.1000   -0.0010
   100        0.4376             nan     0.1000   -0.0028
   120        0.3842             nan     0.1000   -0.0010
   140        0.3455             nan     0.1000   -0.0006
   150        0.3248             nan     0.1000    0.0014

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0249             nan     0.1000    0.2137
     2        1.8406             nan     0.1000    0.1808
     3        1.6956             nan     0.1000    0.1287
     4        1.5437             nan     0.1000    0.1374
     5        1.4203             nan     0.1000    0.1055
     6        1.3294             nan     0.1000    0.0739
     7        1.2491             nan     0.1000    0.0574
     8        1.1790             nan     0.1000    0.0616
     9        1.1304             nan     0.1000    0.0344
    10        1.0765             nan     0.1000    0.0443
    20        0.7170             nan     0.1000    0.0085
    40        0.4613             nan     0.1000    0.0037
    60        0.3338             nan     0.1000    0.0037
    80        0.2553             nan     0.1000    0.0014
   100        0.2090             nan     0.1000    0.0000
   120        0.1698             nan     0.1000   -0.0003
   140        0.1406             nan     0.1000    0.0003
   150        0.1270             nan     0.1000    0.0000

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0077             nan     0.1000    0.2314
     2        1.7901             nan     0.1000    0.2039
     3        1.6106             nan     0.1000    0.1759
     4        1.4430             nan     0.1000    0.1524
     5        1.3297             nan     0.1000    0.1091
     6        1.2178             nan     0.1000    0.0949
     7        1.1231             nan     0.1000    0.0834
     8        1.0444             nan     0.1000    0.0633
     9        0.9640             nan     0.1000    0.0668
    10        0.9055             nan     0.1000    0.0541
    20        0.5707             nan     0.1000    0.0193
    40        0.3132             nan     0.1000   -0.0008
    60        0.2034             nan     0.1000   -0.0008
    80        0.1422             nan     0.1000   -0.0001
   100        0.1054             nan     0.1000    0.0008
   120        0.0825             nan     0.1000   -0.0005
   140        0.0641             nan     0.1000   -0.0002
   150        0.0579             nan     0.1000   -0.0000

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9822             nan     0.1000    0.1139
     2        1.8835             nan     0.1000    0.0966
     3        1.7971             nan     0.1000    0.0937
     4        1.7214             nan     0.1000    0.0610
     5        1.6633             nan     0.1000    0.0595
     6        1.6089             nan     0.1000    0.0480
     7        1.5637             nan     0.1000    0.0353
     8        1.5178             nan     0.1000    0.0359
     9        1.4680             nan     0.1000    0.0510
    10        1.4316             nan     0.1000    0.0336
    20        1.1128             nan     0.1000    0.0161
    40        0.8012             nan     0.1000    0.0079
    60        0.6461             nan     0.1000    0.0021
    80        0.5377             nan     0.1000   -0.0003
   100        0.4631             nan     0.1000   -0.0000
   120        0.3977             nan     0.1000    0.0004
   140        0.3504             nan     0.1000    0.0006
   150        0.3273             nan     0.1000   -0.0007

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9171             nan     0.1000    0.1954
     2        1.7626             nan     0.1000    0.1568
     3        1.6472             nan     0.1000    0.1033
     4        1.5345             nan     0.1000    0.0975
     5        1.4454             nan     0.1000    0.0787
     6        1.3704             nan     0.1000    0.0620
     7        1.2984             nan     0.1000    0.0609
     8        1.2147             nan     0.1000    0.0639
     9        1.1537             nan     0.1000    0.0474
    10        1.0950             nan     0.1000    0.0528
    20        0.7590             nan     0.1000    0.0164
    40        0.4866             nan     0.1000   -0.0004
    60        0.3482             nan     0.1000    0.0051
    80        0.2632             nan     0.1000   -0.0002
   100        0.2054             nan     0.1000   -0.0020
   120        0.1638             nan     0.1000    0.0001
   140        0.1325             nan     0.1000   -0.0000
   150        0.1185             nan     0.1000   -0.0003

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.8765             nan     0.1000    0.2070
     2        1.6964             nan     0.1000    0.1708
     3        1.5454             nan     0.1000    0.1416
     4        1.4087             nan     0.1000    0.1241
     5        1.3056             nan     0.1000    0.0944
     6        1.2147             nan     0.1000    0.0823
     7        1.1277             nan     0.1000    0.0804
     8        1.0578             nan     0.1000    0.0540
     9        0.9898             nan     0.1000    0.0564
    10        0.9376             nan     0.1000    0.0398
    20        0.5827             nan     0.1000    0.0166
    40        0.3293             nan     0.1000    0.0012
    60        0.2168             nan     0.1000   -0.0006
    80        0.1542             nan     0.1000   -0.0008
   100        0.1128             nan     0.1000   -0.0010
   120        0.0833             nan     0.1000   -0.0006
   140        0.0634             nan     0.1000    0.0002
   150        0.0558             nan     0.1000   -0.0006

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9522             nan     0.1000    0.1489
     2        1.8279             nan     0.1000    0.1041
     3        1.7222             nan     0.1000    0.1136
     4        1.6456             nan     0.1000    0.0853
     5        1.5802             nan     0.1000    0.0636
     6        1.5207             nan     0.1000    0.0543
     7        1.4698             nan     0.1000    0.0470
     8        1.4326             nan     0.1000    0.0335
     9        1.3979             nan     0.1000    0.0265
    10        1.3643             nan     0.1000    0.0269
    20        1.0971             nan     0.1000    0.0149
    40        0.7854             nan     0.1000    0.0038
    60        0.6048             nan     0.1000    0.0019
    80        0.4977             nan     0.1000    0.0047
   100        0.4250             nan     0.1000   -0.0012
   120        0.3709             nan     0.1000    0.0024
   140        0.3313             nan     0.1000   -0.0001
   150        0.3131             nan     0.1000   -0.0004

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9006             nan     0.1000    0.2025
     2        1.7393             nan     0.1000    0.1496
     3        1.6103             nan     0.1000    0.1218
     4        1.4959             nan     0.1000    0.1063
     5        1.3987             nan     0.1000    0.0738
     6        1.3170             nan     0.1000    0.0626
     7        1.2421             nan     0.1000    0.0675
     8        1.1794             nan     0.1000    0.0498
     9        1.1173             nan     0.1000    0.0491
    10        1.0661             nan     0.1000    0.0384
    20        0.7277             nan     0.1000    0.0181
    40        0.4617             nan     0.1000    0.0018
    60        0.3312             nan     0.1000   -0.0009
    80        0.2481             nan     0.1000    0.0013
   100        0.1881             nan     0.1000    0.0001
   120        0.1488             nan     0.1000   -0.0010
   140        0.1216             nan     0.1000   -0.0018
   150        0.1099             nan     0.1000   -0.0010

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.8849             nan     0.1000    0.2200
     2        1.6986             nan     0.1000    0.1837
     3        1.5460             nan     0.1000    0.1446
     4        1.4270             nan     0.1000    0.1053
     5        1.3126             nan     0.1000    0.1038
     6        1.2139             nan     0.1000    0.0956
     7        1.1202             nan     0.1000    0.0721
     8        1.0467             nan     0.1000    0.0688
     9        0.9876             nan     0.1000    0.0477
    10        0.9290             nan     0.1000    0.0462
    20        0.5751             nan     0.1000    0.0147
    40        0.3162             nan     0.1000    0.0032
    60        0.2001             nan     0.1000    0.0036
    80        0.1409             nan     0.1000   -0.0008
   100        0.1010             nan     0.1000    0.0002
   120        0.0764             nan     0.1000   -0.0003
   140        0.0593             nan     0.1000   -0.0006
   150        0.0530             nan     0.1000   -0.0001

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9920             nan     0.1000    0.1997
     2        1.8472             nan     0.1000    0.1692
     3        1.7239             nan     0.1000    0.1245
     4        1.6318             nan     0.1000    0.0928
     5        1.5467             nan     0.1000    0.0807
     6        1.4877             nan     0.1000    0.0548
     7        1.4235             nan     0.1000    0.0623
     8        1.3698             nan     0.1000    0.0509
     9        1.3223             nan     0.1000    0.0323
    10        1.2755             nan     0.1000    0.0352
    20        0.9694             nan     0.1000    0.0134
    40        0.6943             nan     0.1000    0.0040
    60        0.5419             nan     0.1000    0.0053
    80        0.4423             nan     0.1000    0.0014
   100        0.3754             nan     0.1000   -0.0005
   120        0.3258             nan     0.1000   -0.0017
   140        0.2858             nan     0.1000    0.0006
   150        0.2688             nan     0.1000   -0.0006

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9499             nan     0.1000    0.2214
     2        1.7414             nan     0.1000    0.1910
     3        1.5781             nan     0.1000    0.1694
     4        1.4479             nan     0.1000    0.1324
     5        1.3454             nan     0.1000    0.0967
     6        1.2486             nan     0.1000    0.0920
     7        1.1693             nan     0.1000    0.0652
     8        1.1024             nan     0.1000    0.0533
     9        1.0548             nan     0.1000    0.0356
    10        1.0037             nan     0.1000    0.0444
    20        0.6616             nan     0.1000    0.0161
    40        0.4044             nan     0.1000    0.0035
    60        0.2844             nan     0.1000    0.0001
    80        0.2135             nan     0.1000   -0.0005
   100        0.1653             nan     0.1000   -0.0008
   120        0.1327             nan     0.1000    0.0008
   140        0.1069             nan     0.1000   -0.0003
   150        0.0971             nan     0.1000   -0.0001

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9031             nan     0.1000    0.2599
     2        1.6784             nan     0.1000    0.2022
     3        1.5041             nan     0.1000    0.1577
     4        1.3584             nan     0.1000    0.1281
     5        1.2308             nan     0.1000    0.1054
     6        1.1248             nan     0.1000    0.1075
     7        1.0292             nan     0.1000    0.0836
     8        0.9551             nan     0.1000    0.0699
     9        0.8930             nan     0.1000    0.0553
    10        0.8278             nan     0.1000    0.0575
    20        0.4958             nan     0.1000    0.0184
    40        0.2683             nan     0.1000    0.0057
    60        0.1687             nan     0.1000    0.0010
    80        0.1181             nan     0.1000   -0.0002
   100        0.0866             nan     0.1000   -0.0001
   120        0.0632             nan     0.1000   -0.0004
   140        0.0484             nan     0.1000   -0.0005
   150        0.0424             nan     0.1000   -0.0001

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0984             nan     0.1000    0.1938
     2        1.9421             nan     0.1000    0.1455
     3        1.8131             nan     0.1000    0.1320
     4        1.7057             nan     0.1000    0.0879
     5        1.6063             nan     0.1000    0.0766
     6        1.5334             nan     0.1000    0.0573
     7        1.4759             nan     0.1000    0.0513
     8        1.4302             nan     0.1000    0.0395
     9        1.3952             nan     0.1000    0.0330
    10        1.3504             nan     0.1000    0.0446
    20        1.0673             nan     0.1000    0.0192
    40        0.7497             nan     0.1000    0.0091
    60        0.5889             nan     0.1000    0.0032
    80        0.4844             nan     0.1000    0.0009
   100        0.4155             nan     0.1000    0.0009
   120        0.3614             nan     0.1000   -0.0008
   140        0.3216             nan     0.1000   -0.0022
   150        0.3043             nan     0.1000   -0.0001

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0444             nan     0.1000    0.2314
     2        1.8541             nan     0.1000    0.1708
     3        1.6800             nan     0.1000    0.1594
     4        1.5467             nan     0.1000    0.1210
     5        1.4283             nan     0.1000    0.1028
     6        1.3345             nan     0.1000    0.0824
     7        1.2509             nan     0.1000    0.0686
     8        1.1841             nan     0.1000    0.0529
     9        1.1248             nan     0.1000    0.0570
    10        1.0669             nan     0.1000    0.0361
    20        0.7213             nan     0.1000   -0.0008
    40        0.4472             nan     0.1000    0.0015
    60        0.3151             nan     0.1000    0.0016
    80        0.2338             nan     0.1000   -0.0016
   100        0.1823             nan     0.1000   -0.0007
   120        0.1464             nan     0.1000   -0.0001
   140        0.1180             nan     0.1000   -0.0008
   150        0.1076             nan     0.1000   -0.0001

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0174             nan     0.1000    0.2461
     2        1.7942             nan     0.1000    0.1886
     3        1.6147             nan     0.1000    0.1653
     4        1.4628             nan     0.1000    0.1342
     5        1.3348             nan     0.1000    0.1318
     6        1.2224             nan     0.1000    0.0980
     7        1.1196             nan     0.1000    0.0866
     8        1.0429             nan     0.1000    0.0733
     9        0.9600             nan     0.1000    0.0723
    10        0.8988             nan     0.1000    0.0489
    20        0.5581             nan     0.1000    0.0211
    40        0.2960             nan     0.1000   -0.0001
    60        0.1944             nan     0.1000    0.0011
    80        0.1330             nan     0.1000    0.0000
   100        0.0931             nan     0.1000   -0.0003
   120        0.0696             nan     0.1000   -0.0005
   140        0.0538             nan     0.1000   -0.0000
   150        0.0473             nan     0.1000   -0.0001

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9170             nan     0.1000    0.1684
     2        1.7843             nan     0.1000    0.1317
     3        1.6725             nan     0.1000    0.0973
     4        1.5830             nan     0.1000    0.0820
     5        1.5106             nan     0.1000    0.0647
     6        1.4525             nan     0.1000    0.0563
     7        1.3946             nan     0.1000    0.0487
     8        1.3395             nan     0.1000    0.0494
     9        1.2959             nan     0.1000    0.0404
    10        1.2552             nan     0.1000    0.0366
    20        0.9754             nan     0.1000    0.0057
    40        0.6644             nan     0.1000   -0.0014
    60        0.4944             nan     0.1000    0.0033
    80        0.4028             nan     0.1000    0.0004
   100        0.3437             nan     0.1000    0.0013
   120        0.2971             nan     0.1000   -0.0007
   140        0.2610             nan     0.1000   -0.0005
   150        0.2459             nan     0.1000   -0.0014

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.8656             nan     0.1000    0.1981
     2        1.6824             nan     0.1000    0.1502
     3        1.5383             nan     0.1000    0.1411
     4        1.4196             nan     0.1000    0.1158
     5        1.3162             nan     0.1000    0.0823
     6        1.2418             nan     0.1000    0.0634
     7        1.1662             nan     0.1000    0.0706
     8        1.0972             nan     0.1000    0.0518
     9        1.0346             nan     0.1000    0.0515
    10        0.9882             nan     0.1000    0.0324
    20        0.6623             nan     0.1000    0.0189
    40        0.3989             nan     0.1000    0.0059
    60        0.2888             nan     0.1000    0.0010
    80        0.2227             nan     0.1000   -0.0013
   100        0.1812             nan     0.1000   -0.0004
   120        0.1486             nan     0.1000   -0.0004
   140        0.1263             nan     0.1000   -0.0014
   150        0.1141             nan     0.1000    0.0003

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.8398             nan     0.1000    0.2397
     2        1.6497             nan     0.1000    0.1985
     3        1.4856             nan     0.1000    0.1437
     4        1.3599             nan     0.1000    0.1213
     5        1.2388             nan     0.1000    0.1260
     6        1.1372             nan     0.1000    0.0769
     7        1.0384             nan     0.1000    0.0864
     8        0.9659             nan     0.1000    0.0512
     9        0.9020             nan     0.1000    0.0547
    10        0.8484             nan     0.1000    0.0370
    20        0.5150             nan     0.1000    0.0103
    40        0.2812             nan     0.1000    0.0002
    60        0.1783             nan     0.1000   -0.0006
    80        0.1301             nan     0.1000   -0.0003
   100        0.0951             nan     0.1000   -0.0004
   120        0.0706             nan     0.1000   -0.0003
   140        0.0535             nan     0.1000   -0.0005
   150        0.0481             nan     0.1000   -0.0001

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.1016             nan     0.1000    0.1778
     2        1.9480             nan     0.1000    0.1314
     3        1.8255             nan     0.1000    0.1356
     4        1.7301             nan     0.1000    0.0999
     5        1.6435             nan     0.1000    0.0701
     6        1.5800             nan     0.1000    0.0633
     7        1.5317             nan     0.1000    0.0308
     8        1.4783             nan     0.1000    0.0470
     9        1.4294             nan     0.1000    0.0483
    10        1.3948             nan     0.1000    0.0231
    20        1.0877             nan     0.1000    0.0206
    40        0.7880             nan     0.1000    0.0072
    60        0.6128             nan     0.1000    0.0023
    80        0.5095             nan     0.1000   -0.0009
   100        0.4281             nan     0.1000   -0.0001
   120        0.3697             nan     0.1000    0.0010
   140        0.3224             nan     0.1000   -0.0008
   150        0.3037             nan     0.1000   -0.0014

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0406             nan     0.1000    0.2205
     2        1.8433             nan     0.1000    0.1794
     3        1.6781             nan     0.1000    0.1578
     4        1.5487             nan     0.1000    0.1291
     5        1.4405             nan     0.1000    0.0948
     6        1.3466             nan     0.1000    0.0876
     7        1.2614             nan     0.1000    0.0813
     8        1.1962             nan     0.1000    0.0642
     9        1.1480             nan     0.1000    0.0199
    10        1.0942             nan     0.1000    0.0458
    20        0.7413             nan     0.1000    0.0252
    40        0.4600             nan     0.1000    0.0053
    60        0.3232             nan     0.1000   -0.0002
    80        0.2399             nan     0.1000    0.0020
   100        0.1911             nan     0.1000   -0.0005
   120        0.1521             nan     0.1000   -0.0005
   140        0.1260             nan     0.1000   -0.0003
   150        0.1153             nan     0.1000   -0.0009

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0228             nan     0.1000    0.2370
     2        1.7904             nan     0.1000    0.2221
     3        1.6047             nan     0.1000    0.1914
     4        1.4539             nan     0.1000    0.1485
     5        1.3188             nan     0.1000    0.1182
     6        1.2100             nan     0.1000    0.0955
     7        1.1214             nan     0.1000    0.0724
     8        1.0418             nan     0.1000    0.0628
     9        0.9695             nan     0.1000    0.0613
    10        0.9161             nan     0.1000    0.0354
    20        0.5487             nan     0.1000    0.0108
    40        0.2939             nan     0.1000   -0.0009
    60        0.1827             nan     0.1000    0.0024
    80        0.1283             nan     0.1000   -0.0002
   100        0.0930             nan     0.1000    0.0001
   120        0.0708             nan     0.1000   -0.0006
   140        0.0552             nan     0.1000   -0.0008
   150        0.0480             nan     0.1000   -0.0002

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9454             nan     0.1000    0.1381
     2        1.8026             nan     0.1000    0.1483
     3        1.6811             nan     0.1000    0.0906
     4        1.5866             nan     0.1000    0.0798
     5        1.5127             nan     0.1000    0.0767
     6        1.4438             nan     0.1000    0.0650
     7        1.3886             nan     0.1000    0.0458
     8        1.3460             nan     0.1000    0.0318
     9        1.3090             nan     0.1000    0.0334
    10        1.2751             nan     0.1000    0.0256
    20        1.0324             nan     0.1000    0.0148
    40        0.7705             nan     0.1000    0.0059
    60        0.6256             nan     0.1000    0.0031
    80        0.5285             nan     0.1000    0.0029
   100        0.4594             nan     0.1000    0.0007
   120        0.3993             nan     0.1000   -0.0013
   140        0.3532             nan     0.1000    0.0012
   150        0.3330             nan     0.1000   -0.0013

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9286             nan     0.1000    0.1324
     2        1.7606             nan     0.1000    0.1608
     3        1.6095             nan     0.1000    0.1305
     4        1.4987             nan     0.1000    0.1107
     5        1.3964             nan     0.1000    0.0895
     6        1.3062             nan     0.1000    0.0764
     7        1.2332             nan     0.1000    0.0534
     8        1.1708             nan     0.1000    0.0496
     9        1.1265             nan     0.1000    0.0350
    10        1.0743             nan     0.1000    0.0406
    20        0.7692             nan     0.1000    0.0098
    40        0.5083             nan     0.1000    0.0042
    60        0.3665             nan     0.1000    0.0024
    80        0.2717             nan     0.1000    0.0002
   100        0.2137             nan     0.1000    0.0000
   120        0.1710             nan     0.1000    0.0001
   140        0.1429             nan     0.1000   -0.0005
   150        0.1319             nan     0.1000   -0.0004

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.8800             nan     0.1000    0.2153
     2        1.6967             nan     0.1000    0.1869
     3        1.5180             nan     0.1000    0.1458
     4        1.3897             nan     0.1000    0.1161
     5        1.2708             nan     0.1000    0.0879
     6        1.1817             nan     0.1000    0.0756
     7        1.0963             nan     0.1000    0.0744
     8        1.0306             nan     0.1000    0.0496
     9        0.9649             nan     0.1000    0.0563
    10        0.9115             nan     0.1000    0.0374
    20        0.5897             nan     0.1000    0.0125
    40        0.3444             nan     0.1000   -0.0012
    60        0.2260             nan     0.1000    0.0011
    80        0.1562             nan     0.1000    0.0004
   100        0.1155             nan     0.1000   -0.0005
   120        0.0865             nan     0.1000   -0.0001
   140        0.0670             nan     0.1000   -0.0009
   150        0.0605             nan     0.1000   -0.0008

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0186             nan     0.1000    0.1552
     2        1.8723             nan     0.1000    0.1033
     3        1.7684             nan     0.1000    0.0938
     4        1.6809             nan     0.1000    0.0797
     5        1.6329             nan     0.1000    0.0373
     6        1.5585             nan     0.1000    0.0722
     7        1.5017             nan     0.1000    0.0591
     8        1.4589             nan     0.1000    0.0268
     9        1.4184             nan     0.1000    0.0237
    10        1.3711             nan     0.1000    0.0412
    20        1.0823             nan     0.1000    0.0170
    40        0.7533             nan     0.1000    0.0068
    60        0.5854             nan     0.1000    0.0047
    80        0.4804             nan     0.1000    0.0013
   100        0.4136             nan     0.1000   -0.0027
   120        0.3656             nan     0.1000    0.0001
   140        0.3275             nan     0.1000   -0.0016
   150        0.3125             nan     0.1000   -0.0017

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9591             nan     0.1000    0.2214
     2        1.7833             nan     0.1000    0.1604
     3        1.6308             nan     0.1000    0.1378
     4        1.5029             nan     0.1000    0.1119
     5        1.3993             nan     0.1000    0.0868
     6        1.3227             nan     0.1000    0.0773
     7        1.2400             nan     0.1000    0.0735
     8        1.1753             nan     0.1000    0.0543
     9        1.1197             nan     0.1000    0.0437
    10        1.0748             nan     0.1000    0.0352
    20        0.7238             nan     0.1000    0.0153
    40        0.4569             nan     0.1000    0.0045
    60        0.3359             nan     0.1000    0.0015
    80        0.2683             nan     0.1000   -0.0018
   100        0.2176             nan     0.1000    0.0005
   120        0.1786             nan     0.1000   -0.0014
   140        0.1451             nan     0.1000   -0.0014
   150        0.1312             nan     0.1000   -0.0005

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9057             nan     0.1000    0.2483
     2        1.7261             nan     0.1000    0.1719
     3        1.5494             nan     0.1000    0.1663
     4        1.4022             nan     0.1000    0.1429
     5        1.2788             nan     0.1000    0.1053
     6        1.1703             nan     0.1000    0.0829
     7        1.0871             nan     0.1000    0.0783
     8        1.0171             nan     0.1000    0.0563
     9        0.9521             nan     0.1000    0.0563
    10        0.8989             nan     0.1000    0.0394
    20        0.5494             nan     0.1000    0.0175
    40        0.3036             nan     0.1000    0.0012
    60        0.1988             nan     0.1000   -0.0011
    80        0.1452             nan     0.1000   -0.0007
   100        0.1064             nan     0.1000    0.0001
   120        0.0775             nan     0.1000   -0.0004
   140        0.0609             nan     0.1000   -0.0000
   150        0.0538             nan     0.1000   -0.0006

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9560             nan     0.1000    0.1315
     2        1.8248             nan     0.1000    0.1370
     3        1.7206             nan     0.1000    0.0958
     4        1.6344             nan     0.1000    0.0860
     5        1.5740             nan     0.1000    0.0590
     6        1.5140             nan     0.1000    0.0542
     7        1.4670             nan     0.1000    0.0307
     8        1.4195             nan     0.1000    0.0462
     9        1.3799             nan     0.1000    0.0384
    10        1.3407             nan     0.1000    0.0383
    20        1.0743             nan     0.1000    0.0156
    40        0.7966             nan     0.1000    0.0067
    60        0.6271             nan     0.1000    0.0017
    80        0.5214             nan     0.1000    0.0008
   100        0.4390             nan     0.1000   -0.0008
   120        0.3799             nan     0.1000    0.0011
   140        0.3317             nan     0.1000   -0.0007
   150        0.3129             nan     0.1000    0.0015

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9151             nan     0.1000    0.2125
     2        1.7578             nan     0.1000    0.1737
     3        1.6248             nan     0.1000    0.1355
     4        1.5120             nan     0.1000    0.1078
     5        1.4112             nan     0.1000    0.0839
     6        1.3405             nan     0.1000    0.0621
     7        1.2683             nan     0.1000    0.0596
     8        1.2049             nan     0.1000    0.0455
     9        1.1529             nan     0.1000    0.0342
    10        1.1131             nan     0.1000    0.0304
    20        0.8058             nan     0.1000    0.0226
    40        0.5065             nan     0.1000    0.0042
    60        0.3685             nan     0.1000    0.0036
    80        0.2720             nan     0.1000   -0.0002
   100        0.2110             nan     0.1000    0.0003
   120        0.1661             nan     0.1000    0.0007
   140        0.1337             nan     0.1000    0.0001
   150        0.1197             nan     0.1000   -0.0002

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.8938             nan     0.1000    0.1836
     2        1.7074             nan     0.1000    0.1433
     3        1.5600             nan     0.1000    0.1307
     4        1.4156             nan     0.1000    0.1334
     5        1.3105             nan     0.1000    0.0921
     6        1.2129             nan     0.1000    0.0803
     7        1.1493             nan     0.1000    0.0461
     8        1.0827             nan     0.1000    0.0513
     9        1.0153             nan     0.1000    0.0473
    10        0.9575             nan     0.1000    0.0398
    20        0.6143             nan     0.1000    0.0105
    40        0.3272             nan     0.1000    0.0000
    60        0.2047             nan     0.1000    0.0015
    80        0.1436             nan     0.1000    0.0006
   100        0.1068             nan     0.1000   -0.0006
   120        0.0821             nan     0.1000   -0.0014
   140        0.0644             nan     0.1000    0.0001
   150        0.0570             nan     0.1000   -0.0000

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.1213             nan     0.1000    0.1694
     2        2.0036             nan     0.1000    0.1276
     3        1.8920             nan     0.1000    0.1125
     4        1.8056             nan     0.1000    0.0857
     5        1.7238             nan     0.1000    0.0551
     6        1.6660             nan     0.1000    0.0627
     7        1.6112             nan     0.1000    0.0380
     8        1.5697             nan     0.1000    0.0328
     9        1.5329             nan     0.1000    0.0242
    10        1.4851             nan     0.1000    0.0417
    20        1.1727             nan     0.1000    0.0278
    40        0.8545             nan     0.1000    0.0014
    60        0.6769             nan     0.1000   -0.0001
    80        0.5532             nan     0.1000    0.0033
   100        0.4679             nan     0.1000   -0.0011
   120        0.3975             nan     0.1000   -0.0013
   140        0.3515             nan     0.1000    0.0023
   150        0.3311             nan     0.1000    0.0001

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0712             nan     0.1000    0.2202
     2        1.9066             nan     0.1000    0.1482
     3        1.7680             nan     0.1000    0.1324
     4        1.6505             nan     0.1000    0.1184
     5        1.5489             nan     0.1000    0.0971
     6        1.4699             nan     0.1000    0.0743
     7        1.4165             nan     0.1000    0.0419
     8        1.3441             nan     0.1000    0.0582
     9        1.2769             nan     0.1000    0.0514
    10        1.2103             nan     0.1000    0.0506
    20        0.8604             nan     0.1000    0.0218
    40        0.5283             nan     0.1000    0.0056
    60        0.3601             nan     0.1000    0.0005
    80        0.2709             nan     0.1000   -0.0025
   100        0.2065             nan     0.1000    0.0003
   120        0.1681             nan     0.1000   -0.0013
   140        0.1398             nan     0.1000   -0.0010
   150        0.1262             nan     0.1000    0.0002

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        2.0545             nan     0.1000    0.2238
     2        1.8487             nan     0.1000    0.1879
     3        1.6960             nan     0.1000    0.1118
     4        1.5510             nan     0.1000    0.1386
     5        1.4315             nan     0.1000    0.0988
     6        1.3279             nan     0.1000    0.0762
     7        1.2314             nan     0.1000    0.0716
     8        1.1663             nan     0.1000    0.0449
     9        1.0729             nan     0.1000    0.0857
    10        1.0035             nan     0.1000    0.0613
    20        0.6215             nan     0.1000    0.0191
    40        0.3444             nan     0.1000    0.0007
    60        0.2204             nan     0.1000    0.0018
    80        0.1529             nan     0.1000   -0.0015
   100        0.1106             nan     0.1000    0.0003
   120        0.0824             nan     0.1000   -0.0005
   140        0.0624             nan     0.1000   -0.0001
   150        0.0538             nan     0.1000   -0.0002

Iter   TrainDeviance   ValidDeviance   StepSize   Improve
     1        1.9657             nan     0.1000    0.2041
     2        1.7771             nan     0.1000    0.1501
     3        1.6222             nan     0.1000    0.1324
     4        1.4955             nan     0.1000    0.1144
     5        1.3775             nan     0.1000    0.1143
     6        1.2824             nan     0.1000    0.0833
     7        1.2099             nan     0.1000    0.0566
     8        1.1390             nan     0.1000    0.0606
     9        1.0741             nan     0.1000    0.0564
    10        1.0164             nan     0.1000    0.0434
    20        0.6853             nan     0.1000    0.0098
    40        0.4313             nan     0.1000   -0.0016
    60        0.3026             nan     0.1000   -0.0018
    80        0.2324             nan     0.1000   -0.0028
   100        0.1784             nan     0.1000   -0.0008
   120        0.1424             nan     0.1000   -0.0020
   140        0.1160             nan     0.1000   -0.0006
   150        0.1046             nan     0.1000   -0.0013
```
Table of boost results
========================================================


```r
predictGbm = round(predict(modelFitGbm, pmlsummary2))
table(predictGbm, pmlsummary2$numclass)
```

```
          
predictGbm  1  2  3  4  5
         1 96  1  0  0  0
         2 13 74  2  0  0
         3  0  4 68  7  0
         4  0  0  0 62 16
         5  0  0  0  0 63
```
Plot boost results
========================================================

![plot of chunk unnamed-chunk-7](MLproject-figure/unnamed-chunk-7-1.png) 

Predict with random forest
========================================================


```r
modelFitRF <- train(pmlsummary2$numclass ~ .,method="rf",data=pmlsummary2)
```
Table of random forest results
========================================================


```r
predictRF = round(predict(modelFitRF, pmlsummary2))
table(predictRF, pmlsummary2$numclass)
```

```
         
predictRF  1  2  3  4  5
        1 98  0  0  0  0
        2 11 77  0  0  0
        3  0  2 70  3  0
        4  0  0  0 66 17
        5  0  0  0  0 62
```
Plot random forest results
========================================================

![plot of chunk unnamed-chunk-10](MLproject-figure/unnamed-chunk-10-1.png) 
Test data
========================================================

- At this point I load up the test data  
- Test data does not have summary rows, and summary columns are NA
- So this model fails on test data.


