SMLSHARP = smlsharp
all: testRunner
testRunner: ../../chp9/ex9.11.o test-ex9.11.o ../../chp9/heap.sig \
 ../../chp9/ex9.11.smi test-ex9.11.smi
	$(SMLSHARP) $(LDFLAGS) -o $@ test-ex9.11.smi $(LIBS)
../../chp9/ex9.11.o: ../../chp9/ex9.11.sml ../../chp9/heap.sig \
 ../../chp9/ex9.11.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c ../../chp9/ex9.11.sml
test-ex9.11.o: test-ex9.11.sml ../../chp9/heap.sig ../../chp9/ex9.11.smi \
 test-ex9.11.smi
	$(SMLSHARP) $(SMLFLAGS) -o $@ -c test-ex9.11.sml
