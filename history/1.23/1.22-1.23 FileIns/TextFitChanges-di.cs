'From Squeak 1.23 of October 4, 1997 on 5 October 1997 at 4:38:43 pm'!

!Paragraph methodsFor: 'accessing' stamp: 'di 10/5/97 15:33'!
clippingRectangle: clipRect 
	clippingRectangle _ clipRect! !

!Paragraph methodsFor: 'accessing'!
replaceFrom: start to: stop with: aText displaying: displayBoolean
	"Replace the receiver's text starting at position start, stopping at stop, by 
	the characters in aText. It is expected that most requirements for 
	modifications to the receiver will call this code. Certainly all cut's or 
	paste's." 

	| compositionScanner obsoleteLines obsoleteLastLine firstLineIndex lastLineIndex
	startLine stopLine replacementRange visibleRectangle startIndex newLine done
	newStop obsoleteY newY moveRectangle |

	text replaceFrom: start to: stop with: aText.		"Update the text."
	lastLine = 0 ifTrue:
		["if lines have never been set up, measure them and display
		all the lines falling in the visibleRectangle"
		self composeAll.
		displayBoolean ifTrue: [^ self displayLines: (1 to: lastLine)]].

	"save -- things get pretty mashed as we go along"
	obsoleteLines _ lines copy.
	obsoleteLastLine _ lastLine.

	"find the starting and stopping lines"
	firstLineIndex _ startLine _ self lineIndexOfCharacterIndex: start.
	stopLine _ self lineIndexOfCharacterIndex: stop.

	"how many characters being inserted or deleted
		-- negative if aText size is < characterInterval size."
	replacementRange _ aText size - (stop - start + 1).
	"Give ourselves plenty of elbow room."
	compositionRectangle _ compositionRectangle withHeight: (textStyle lineGrid * 9999).
	"build a boundingBox of the actual screen space in question -- we'll need it later"
	visibleRectangle _ (clippingRectangle intersect: compositionRectangle)
							intersect: destinationForm boundingBox.
	compositionScanner _ CompositionScanner new in: self.		"Initialize a scanner."

	"If the starting line is not also the first line, then measuring must commence from line preceding the one in which characterInterval start appears.  For example, deleting a line with only a carriage return may move characters following the deleted portion of text into the line preceding the deleted line."
	startIndex _ (lines at: firstLineIndex) first.
	startLine > 1
		ifTrue: 	[newLine _ compositionScanner composeLine: startLine - 1
						fromCharacterIndex: (lines at: startLine - 1) first
						inParagraph: self.
				(lines at: startLine - 1) = newLine
					ifFalse:	["start in line preceding the one with the starting character"
							startLine _ startLine - 1.
							self lineAt: startLine put: newLine.
							startIndex _ newLine last + 1]].
	startIndex > text size ifTrue:
		["nil lines after a deletion -- remeasure last line below"
		self trimLinesTo: (firstLineIndex - 1 max: 0).
		text size = 0 ifTrue:
			["entire text deleted -- clear visibleRectangle and return."
			displayBoolean ifTrue: [destinationForm fill: visibleRectangle rule: rule fillColor: self backgroundColor].
			self updateCompositionHeight.
			^self]].

	"Now we really get to it."
	done _ false.
	lastLineIndex _ stopLine.
	[done or: [startIndex > text size]]
		whileFalse: 
		[self lineAt: firstLineIndex put:
			(newLine _ compositionScanner composeLine: firstLineIndex
							fromCharacterIndex: startIndex inParagraph: self).
		[(lastLineIndex > obsoleteLastLine
			or: ["no more old lines to compare with?"
				newLine last <
					(newStop _ (obsoleteLines at: lastLineIndex) last + replacementRange)])
			  	or: [done]]
			whileFalse: 
			[newStop = newLine last
				ifTrue:	["got the match"
						"get source and dest y's for moving the unchanged lines"
						obsoleteY _ self topAtLineIndex: lastLineIndex + 1
									using: obsoleteLines and: obsoleteLastLine.
						newY _ self topAtLineIndex: firstLineIndex + 1.
						stopLine _ firstLineIndex.
						done _ true.
							"Fill in the new line vector with the old unchanged lines.
							Update their starting and stopping indices on the way."
						((lastLineIndex _ lastLineIndex + 1) to: obsoleteLastLine) do:
							[:upDatedIndex | 
							self lineAt: (firstLineIndex _ firstLineIndex + 1) 
								put: ((obsoleteLines at: upDatedIndex)
							  		slide: replacementRange)].
							"trim off obsolete lines, if any"
						self trimLinesTo: firstLineIndex]
				ifFalse:	[lastLineIndex _ lastLineIndex + 1]].
		startIndex _ newLine last + 1.
		firstLineIndex _ firstLineIndex + 1].

	"Now the lines are up to date -- Whew!!.  What remains is to move
	the 'unchanged' lines and display those which have changed."
	displayBoolean   "Not much to do if not displaying"
		ifFalse: [^ self updateCompositionHeight].
	startIndex > text size ifTrue:
		["If at the end of previous lines simply display lines from the line in
		which the first character of the replacement occured through the
		end of the paragraph."
		self updateCompositionHeight.
		self displayLines:
			(startLine to: (stopLine _ firstLineIndex min: lastLine)).
		destinationForm  "Clear out area at the bottom"
			fill: ((visibleRectangle left @ (self topAtLineIndex: lastLine + 1)
						extent: visibleRectangle extent)
					intersect: visibleRectangle)
			rule: rule fillColor: self backgroundColor]
		ifFalse:
		[newY ~= obsoleteY ifTrue:
			["Otherwise first move the unchanged lines within
			the visibleRectangle with a good old bitblt."
			moveRectangle _
				visibleRectangle left @ (obsoleteY max: visibleRectangle top)
					corner: visibleRectangle corner.
			destinationForm copyBits: moveRectangle from: destinationForm
				at: moveRectangle origin + (0 @ (newY-obsoleteY))
				clippingBox: visibleRectangle
				rule: Form over fillColor: nil].

		"Then display the altered lines."
		self displayLines: (startLine to: stopLine).

		newY < obsoleteY
			ifTrue:
			[(self topAtLineIndex: obsoleteLastLine+1 using: obsoleteLines and: obsoleteLastLine) > visibleRectangle bottom
				ifTrue:
				["A deletion may have 'pulled' previously undisplayed lines
				into the visibleRectangle.  If so, display them."
				self displayLines:
					((self lineIndexOfTop: visibleRectangle bottom - (obsoleteY - newY))
						to: (self lineIndexOfTop: visibleRectangle bottom))].
			"Clear out obsolete material at the bottom of the visibleRectangle."
			destinationForm
				fill: ((visibleRectangle left @ ((self bottomAtLineIndex: lastLine) + 1)
						extent: visibleRectangle extent)
					intersect: visibleRectangle)  "How about just corner: ??"
				rule: rule fillColor: self backgroundColor].

		(newY > obsoleteY and: [obsoleteY < visibleRectangle top])
			ifTrue:
				["An insertion may have 'pushed' previously undisplayed lines
				into the visibleRectangle.  If so, display them."
				self displayLines:
					((self lineIndexOfTop: visibleRectangle top)
						to: (self lineIndexOfTop: visibleRectangle top + (newY-obsoleteY)))].

		self updateCompositionHeight]! !


!TextMorph methodsFor: 'initialization' stamp: 'di 10/5/97 16:36'!
initialize
	super initialize.
	color _ Color black.
	textStyle _ TextStyle default copy.
	text _ 'Text' asText allBold.  "Default contents"
	wrapFlag _ true.
! !

!TextMorph methodsFor: 'drawing'!
drawOn: aCanvas
	| selectionRects |
	self hasFocus ifTrue:
		[aCanvas fillRectangle: bounds color: Color white.
		selectionRects _ editor selectionRects.
		(selectionRects size = 1 and: [selectionRects first width = 1])
			ifTrue: [aCanvas image: CaretForm
							at: (selectionRects first bottomLeft + CaretForm offset)]
			ifFalse: [selectionRects do:
						[:rect | aCanvas fillRectangle: rect color: SelectionColor]]].
	text size = 0
		ifTrue: [self hasFocus ifFalse: [aCanvas fillRectangle: bounds color: Color lightRed]]
		ifFalse: [aCanvas paragraph: self paragraph bounds: bounds color: color].! !

!TextMorph methodsFor: 'geometry' stamp: 'di 10/5/97 16:12'!
extent: aPoint
	self releaseEditor.
	self releaseParagraph.  "invalidate the paragraph cache"
	super extent: (aPoint max: 20@(textStyle lineGrid+2)).
	self fit! !

!TextMorph methodsFor: 'private' stamp: 'di 10/5/97 15:36'!
fit
	"Adjust bounds vertically to fit the text."
	| newExtent |
	newExtent _ wrapFlag
		ifTrue: [bounds width @ ((self paragraph height max: textStyle lineGrid) + 2)]
		ifFalse: [(self paragraph extent max: 20@textStyle lineGrid) + (0@2)].
	newExtent ~= bounds extent
		ifTrue: [super extent: newExtent.
				paragraph clippingRectangle: self bounds].
	self updateAnchors.
	self changed! !

!TextMorph methodsFor: 'private' stamp: 'di 10/5/97 16:35'!
paragraph
	"Paragraph instantiation is lazy -- create it only when needed"
	| compWidth fullWidth |
	paragraph ifNotNil: [^ paragraph].

	"...Code here to recreate the paragraph..."
	compWidth _ wrapFlag ifTrue: [bounds width] ifFalse: [999999].
	paragraph _ Paragraph basicNew.
	fullWidth _ paragraph setWithText: text style: textStyle
			compositionRectangle: (bounds topLeft extent: compWidth @ 999999)
			clippingRectangle: bounds 
			foreColor: color backColor: Color white.
	wrapFlag ifFalse:
		[paragraph compositionRectangle:
			(paragraph compositionRectangle withWidth: fullWidth)].
	self fit.
	^ paragraph! !

TextMorph removeSelector: #convertbosfceptthpeh0:bosfcepttwpe0:!
TextMorphEditor removeSelector: #controlInitialize!
