#!/bin/bash
MYDIRECTORY="/home/zaiming/work/xintiandi/project/reports/gift/"

# R CMD BATCH ${MYDIRECTORY}/monthly_excel_report.R /dev/tty 
Rscript ${MYDIRECTORY}/gift_report.R
