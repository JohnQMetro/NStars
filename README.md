# NStars

This is a program to build and maintain a list of nearby stars. For each system it can store much information, and can estimate some stellar parameters from parallax and photometry.

It uses Free Pascal and Lazarus, you can get Lazarus and Free Pascal here: https://www.lazarus-ide.org/

Two 'third party' libraries are used.
The first is the Synapse TCP/IP library, I use the trunk version found here: https://sourceforge.net/p/synalist/code/HEAD/tree/trunk/
The second is W. Ehrhardt's DAMath, math functions that are convenient and more accurate than what is included with Free Pascal. You can find it here: http://www.wolfgang-ehrhardt.de/misc_en.html#damath

In addition to the program sources, I've also included mainlist.nsl2, which is my current Nearby Star List, in the format used by the NStars program.

Also, you might have to edit themes.pas in LCL to avoid ugly black rims on some tabs (see https://bugs.freepascal.org/view.php?id=25468).