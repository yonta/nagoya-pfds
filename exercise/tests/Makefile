SMLSHARP = smlsharp
all: main
main: ../chp9/ex9.11.o chp9/test-ex9.11.o ../chp9/ex9.12.o chp9/test-ex9.12.o \
 ../chp9/ex9.13.o chp9/test-ex9.13.o main.o ../chp9/heap.sig \
 ../chp9/ex9.11.smi chp9/test-ex9.11.smi ../chp9/ex9.12.smi \
 chp9/test-ex9.12.smi ../chp9/random_access_list.sig ../chp9/ex9.13.smi \
 chp9/test-ex9.13.smi main.smi
	$(SMLSHARP) $(LDFLAGS) -o $@ main.smi $(LIBS)
../chp9/ex9.11.o: ../chp9/ex9.11.sml ../chp9/heap.sig ../chp9/ex9.11.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c ../chp9/ex9.11.sml
chp9/test-ex9.11.o: chp9/test-ex9.11.sml ../chp9/heap.sig ../chp9/ex9.11.smi \
 chp9/test-ex9.11.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c chp9/test-ex9.11.sml
../chp9/ex9.12.o: ../chp9/ex9.12.sml ../chp9/ex9.12.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c ../chp9/ex9.12.sml
chp9/test-ex9.12.o: chp9/test-ex9.12.sml ../chp9/ex9.12.smi \
 chp9/test-ex9.12.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c chp9/test-ex9.12.sml
../chp9/ex9.13.o: ../chp9/ex9.13.sml ../chp9/random_access_list.sig \
 ../chp9/ex9.13.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c ../chp9/ex9.13.sml
chp9/test-ex9.13.o: chp9/test-ex9.13.sml ../chp9/random_access_list.sig \
 ../chp9/ex9.13.smi chp9/test-ex9.13.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c chp9/test-ex9.13.sml
main.o: main.sml ../chp9/heap.sig ../chp9/ex9.11.smi chp9/test-ex9.11.smi \
 ../chp9/ex9.12.smi chp9/test-ex9.12.smi ../chp9/random_access_list.sig \
 ../chp9/ex9.13.smi chp9/test-ex9.13.smi main.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c main.sml
