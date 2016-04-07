#!/bin/bash
cd $(dirname $0)
sqlite3 db.sqlite3 <schema.sql
./db_import.sh midiports.csv
