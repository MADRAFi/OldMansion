# Old Mansion

A game for [Commander X16](https://www.commanderx16.com/) computer.

## Introduction

In 1987, polish computer magazine Bajtek published a listing of a computer game called "Stary Dom" ("Old House") authored by Wojciech Zientara. Simple text game written in basic that everyone could type in and play on 8 bit Atari computer. Rumor says game was inspired by another game publish earier for ZX Spectrum in some magazine.

## Remake

In 2019 bocianu has converted the game into MAD-Pascal. Game has been enhanced with title screen, graphics and music. Besides polish version now it was also avaiable in english.
Game was still intended for 8bit Atari computers.

In 2024 I have decided to make a conversion to Commander X16. In order to do that I had to prepare and update MAD-Pascal so it could build code for Commande X16.

Old Mansion does not utilize all of capabilities of X16 but it is fun to play.

![old mansion animation](resources/oldmansion.gif)

## Required tools

Tools required to compile the game.

- [MAD-Pascal](https://github.com/MADRAFi/Mad-Pascal/tree/x16)
- [MADS](https://github.com/tebe6502/Mad-Assembler)

## Utilities

Software used to produce resources for the game.

- [Gimp plugin](https://github.com/jestin/gimp-vera-tileset-plugin)
- [Furnace](https://tildearrow.org/furnace/)
- [PixelFontEdit](http://www.min.at/pixelfont)

## Compile

In order to compile you need to have MAD-Pascal with x16 target support.
**Please edit makefile to point to the path for MAD-Pascal base dir and X16 Emulator**

`

make x16

`

## Credits

| Year | Author                                   |
| ---- | ---------------------------------------- |
| 1987 | Wojciech Zientara                        |
| 2019 | Wojciech Bociański                       |
| 2024 | Code: Rafał Czemko  Music: Brandon Blume |
