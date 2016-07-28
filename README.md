# Squeak History Project  [![Build Status](https://travis-ci.org/HPI-SWA-Teaching/SWT16-Project-01.svg?branch=master)](https://travis-ci.org/HPI-SWA-Teaching/SWT16-Project-01)[![Coverage Status](https://coveralls.io/repos/github/HPI-SWA-Teaching/SWT16-Project-01/badge.svg?branch=master)](https://coveralls.io/github/HPI-SWA-Teaching/SWT16-Project-01?branch=master)
=========================


Squeak History is a tool for Squeak programmers;
It is a platform to parse and analyze all past versions of Squeak. 

The "versions" button is replaced and shows all changes loaded from the Archive for the specified method.
Furthermore it's possible to analyse the complete Archive.
##Warnings!!
* Once you loaded the project, the code of the versions button is overwritten.
* If a full archive is loaded the image can't be saved and opened again. Use
`clearInstance`before closing the image.

## Installation and Initialization
Just Drag and Drop the .sar file and click "install sar file"
###Submodule
After cloning the Project the Archive-Submodule has to be Initialized and updated:
``` shell
git submodule init
git submodule update
```
###UpdateInstance
On workspace execute:
``` smalltalk
History updateInstance.
```

##Documentation
###Usage
####Versions Button
The versions button is replaced. Once you use the versions button, all versions from the loaded `History` are displayed.
```
Tools > Browser > versions
```
####Top-Contributers
You can display the people with the most changes on the Transcript. With the full `History` loaded is can last about one hour:
```smalltalk
History instance writeContributorsToTranscript
```
###Clean-Up
Execute on Workspace:
```smalltalk
History clearInstance
```
###Interface
For an example implementation consult the tests in the project.
Changes can be accessed via the method:
```smalltalk
History instance changesForClass: <className> forMethod:#<methodName>
```
Changes can be shown manually in the changes browser via:
```smalltalk
HistoryChangesBrowser showChanges: <changes>
```

###Presentation
More information (e. g. about the changes file format) can be found in [our presentation](https://docs.google.com/presentation/d/1QWh2Hi8F1zkmSdmuBmypmDYzYWWFsMUxKecAOb7HFPo/edit?usp=sharing). Explanations are in the comments below the slides.


## Credits
*  [Felix Thiel](https://github.com/iLoach)
*  [Robert Beilich](https://github.com/RobertBeilich)
*  [Julius Kunze](https://github.com/JuliusKunze)
*  [Jonas Pohlmann](https://github.com/PoJo93)
*  [Jonathan Janetzki](https://github.com/jjanetzki)
