echo off
set arg1=%1
del *.o
del *.gbc
..\rgbasm -H -o visualizer.o visualizer.asm
..\rgblink -o visualizer.gbc -m visualizer.map -n visualizer.sym visualizer.o
..\rgbfix -Cv -i GBVP -t "GBvisualizer" -m 25 visualizer.gbc