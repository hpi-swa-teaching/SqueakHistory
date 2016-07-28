# Squeak History Project  [![Build Status](https://travis-ci.org/HPI-SWA-Teaching/SWT16-Project-01.svg?branch=master)](https://travis-ci.org/HPI-SWA-Teaching/SWT16-Project-01)[![Coverage Status](https://coveralls.io/repos/github/HPI-SWA-Teaching/SWT16-Project-01/badge.svg?branch=master)](https://coveralls.io/github/HPI-SWA-Teaching/SWT16-Project-01?branch=master)
=========================


Squeak History is a tool for Squeak programmers;
It is a platform to parse and analyze all past versions of Squeak. 

The "versions" button is replaced and shows when the history is loaded ALL past versions.
You can for example list all Top-Contributors.
##Warnings!!
* Once you loaded the project, the code of the versions button is overwritten.
* If a full archive is loaded the image can't be saved and opened again. Use
`clearInstance`before closing the image.

## Installation and Initialization
Just Drag and Drop the .sar file and click "install sar file"
###Submodule
Initialize and update Submodule
``` shell
git submodule init
git submodule update
```
###UpdateInstance
On workspace execute
``` smalltalk
History updateInstance.
```

##Documentation
###Usage
The versions button is replaced. Once you use the versions button, all versions from the loaded `History` are displayed.
```
Tools > Browser > versions
```

###Clean-Up
Execute on Workspace:
```smalltalk
History clearInstance
```
###Interface

## Credits
*  [Felix Thiel](https://github.com/iLoach)
*  [Robert Beilich](https://github.com/RobertBeilich)
*  [Julius Kunze](https://github.com/JuliusKunze)
*  [Jonas Pohlmann](https://github.com/GittiHab)
*  [Jonathan Janetzki](https://github.com/jjanetzki)
