#!/bin/bash
cat<<EOF | sqlite3 db.sqlite3
.separator ','
.import $1 $(basename $1 .csv)
EOF
