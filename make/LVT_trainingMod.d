LVT_trainingMod.o LVT_trainingMod.d : LVT_trainingMod.F90
LVT_trainingMod.o : LVT_trainingAlg_pluginMod.o
LVT_trainingMod.o : LVT_histDataMod.o
LVT_trainingMod.o : LVT_coreMod.o
LVT_trainingMod.o : LVT_logMod.o
LVT_trainingMod.o : map_utils.o
