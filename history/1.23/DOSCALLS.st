DLL subclass: #DOSCALLS
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'DLL callout'!


DOSCALLS class
	instanceVariableNames: ''!

Smalltalk at: #DosCalls put: Dictionary new!

!DOSCALLS class methodsFor: 'public'!
FunctionDictionary
^DosCalls
!

moduleName
^'DOSCALLS'
! !

DOSCALLS load!
DosCalls at: #DosBeep put: (CFunction
	function: 286 dll: DOSCALLS
	parameterTypes: #(uint uint) returnType: #uint)!
DosCalls at: #DosCopy put: (CFunction
	function: 258 dll: DOSCALLS
	parameterTypes: #(string string uint) returnType: #uint)!

