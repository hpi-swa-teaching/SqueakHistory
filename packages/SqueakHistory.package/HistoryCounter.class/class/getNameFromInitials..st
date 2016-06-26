as yet unclassified
getNameFromInitials: anInitial
	"translates initials to names"

	| ndic |
	ndic := Dictionary new.
	ndic at: 'ar' put: 'Andreas Raab'.
	ndic at: 'di' put: 'Dan Ingalls'.
	ndic at: 'tk' put: 'Ted Kaehler'.
	ndic at: 'ward' put: 'Ward Cunningham'.
	ndic at: 'ikp' put: 'Ian Piumarta'.
	ndic at: 'jj' put: 'Jamie Jones'.
	ndic at: 'to' put: 'Tomohiro Oda'.
	ndic at: 'wod' put: 'Bill Dargel'.
	ndic at: 'SqR' put: 'AndresValloud'.
	ndic at: 'tao' put: 'Tim Olson'.
	ndic at: 'mjg' put: 'Mark Guzdial'.
	ndic at: 'ack' put: 'Alan Kay'.
	ndic at: 'hmm' put: 'Hans-Martin Mosner'.
	ndic at: 'sw' put: 'Scott Wallace'.
	ndic at: 'sn' put: 'NikolaySuslov'.
	ndic at: 'jhm' put: 'John Maloney'.
	ndic at: 'stp' put: 'Stephen Travis Pope'.
	ndic at: 'jm' put: 'John Maloney'.
	ndic at: 'go' put: 'Georg Gollmann'. 
	
	(ndic includesKey: anInitial) 
		ifTrue: [^ndic at: anInitial]
		ifFalse: [^anInitial].
	