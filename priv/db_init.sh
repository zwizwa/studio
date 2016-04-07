#!/bin/bash
cd $(dirname $0)
sqlite3 db.sqlite3 <schema.sql
./db_import.sh midiport.csv
./db_import.sh midiclock.csv
