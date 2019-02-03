ca65 example.s \
  -g \
  -o example.o

ld65 -o example.nes \
  -C example.cfg \
  example.o \
  -m example.map.txt \
  -Ln example.labels.txt \
