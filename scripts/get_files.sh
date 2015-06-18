#!/bin/bash

BASE_URL=http://download.geonames.org/export/dump
mkdir -p ../data
wget $BASE_URL/admin1CodesASCII.txt -P ../data
wget $BASE_URL/admin2Codes.txt -P ../data
wget $BASE_URL/countryInfo.txt -P ../data
wget $BASE_URL/timeZones.txt -P ../data
wget $BASE_URL/cities1000.zip -P ../data
cd ../data && unzip cities1000.zip