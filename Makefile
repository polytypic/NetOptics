NetOptics.dll: NetOptics.fsi NetOptics.fs
	fsharpc --target:library --out:$@ $^

Test.exe: NetOptics.dll Test.fs
	fsharpc --target:exe --out:$@ $(patsubst %,--reference:%,$(filter %.dll,$^)) $(filter %.fs %.fsi,$^)

.PHONY: run
test: Test.exe
	mono Test.exe

.PHONY: clean
clean:
	rm -f NetOptics.dll Test.exe
