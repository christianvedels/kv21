# Code KV21

This repository contains all code used in analysing and making predictions for KV21.

# Structure

## 01: Valgdatabase.dst.dk
Code prefaced by 01 is used to clean data from valgdatabase.dst.dk
Data from valgdatabase.dst.dk is not included in repository since it is not explicitly stated that it can be redistributed. 

For replication it should be noted that the script reads the data form a relative path on my computer at ../Valgdatabase with subfolders "EV04", "EV09", ... "FV01", etc. Each of these folders contain all data available for download at valgadatabasen.

In the future I might reimplemnt this with an API to valgdatabsen instead (when it exists).

After formatting and cleaning the code saves a 3 frames  
- 'valgdata': Votes and info from all elections from 2001 and forward  
- 'personlige_stemmer': Individual personal votes*
- 'kandidater': Info about all candidates*

*These do not exist in valgdatabase.dst.dk for some reason. 

## 02: Descriptive statistics


# Contents
**Valgdatabase_meta.csv**: Contains metadata used for cleaning valgdatabase data