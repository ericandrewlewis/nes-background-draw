ca65 main.asm \
  -g \
  -o main.o

ld65 -o main.nes \
  -C linker.cfg \
  main.o \
  -m main.map.txt \
  -Ln main.labels.txt \
  --dbgfile main.nes.dbg