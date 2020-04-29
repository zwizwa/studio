.PHONY: all clean
all:
	cargo build --release
	if [ ! -z "$$ELF" ] ; then ln -f target/release/studio_rs "$$ELF" ; fi

clean:
	rm -rf target *~ *.elf *.dasm *.readelf

