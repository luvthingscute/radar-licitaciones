@echo off
cd /d "%~dp0"
"C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe" run_app.R > data\run_dashboard.out.log 2> data\run_dashboard.err.log
