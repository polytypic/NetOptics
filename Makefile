.PHONY: test
test: bin/NetOptics.Test.exe
	mono $^

.PHONY: clean
clean:
	rm -rf bin

bin/NetOptics.dll: NetOptics.Optic.fsi NetOptics.Optic.fs NetOptics.fs
	fsharpc --target:library --out:$@ $(patsubst %,--reference:%,$(filter %.dll,$^)) $(filter %.fs %.fsi,$^)

bin/NetAtom.dll: System.Reactive.dll bin/NetOptics.dll NetAtom.fs NetAtom.Atom.fsi NetAtom.Atom.fs
	fsharpc --target:library --out:$@ $(patsubst %,--reference:%,$(filter %.dll,$^)) $(filter %.fs %.fsi,$^)

bin/NetOptics.Test.exe: bin/NetOptics.dll NetOptics.Test.fs
	fsharpc --target:exe --out:$@ $(patsubst %,--reference:%,$(filter %.dll,$^)) $(filter %.fs %.fsi,$^)
