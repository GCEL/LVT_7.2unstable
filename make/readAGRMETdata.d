readAGRMETdata.o readAGRMETdata.d : readAGRMETdata.F90
readAGRMETdata.o : LVT_coreMod.o
readAGRMETdata.o : LVT_logMod.o
readAGRMETdata.o : LVT_timeMgrMod.o
readAGRMETdata.o : LVT_histDataMod.o
readAGRMETdata.o : AGRMET_dataMod.o
