Disassembly work of the "Insight Enterprises Z80 single board computer".

As presented by Tech Time Traveller in the video [Insight Enterprises Z80 Prototype (?) Single Board Computer](https://www.youtube.com/watch?v=_z1kBb-Zwpg)

The file `ie-rom.commented.asm` contains the disassembly of the EPROM along with comments.
Only a very small part has been disassembled so far.

If you want to contribute please submit a pull request or write an issue.

WHAT HAS BEEN FOUND SO FAR
==========================

1) at start the EPROM is copied in RAM at $E000 and then excuted from there;
   It does that by a trick I do not full understand (see the code).

2) I/O port 34h seems to contain a sort of memory configuration switches
   There are 4 memory configurations, the video RAM may be $2000 or $4000 depending
   on the memory configuration





