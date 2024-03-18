X16EMU=d:\Retro\X16\x16Emu\x16emu
BASE=d:\Retro\Atari\DEV\Mad-Pascal\base

x16:
	mads resources/om_iso_manual.asm -o:assets/om_iso_manual.fnt
	mads resources/om_iso_field.asm -o:assets/om_iso_field.fnt
	mp -t x16 oldmansion.pas
	mads oldmansion.a65 -x -i:$(BASE) -o:oldmansion.prg
	$(X16EMU) -prg oldmansion.prg -run -debug

