'From Squeak 1.21c of Aug 4, 1997 on 2 October 1997 at 1:21:31 am'!
"Change Set:		TextMorphMods
Date:			1 October 1997
Author:			Dan Ingalls

This is a complete rework of TextMorphs including...
InstVar paragraph is now only a cache - it can be nilled out at any time,
	especially prior to duplicating or storing on a file.
All rectangles are now in morphic coordinates.
Text selection and type-in use morphic events throughout, so that (multiple)
	remote hands will work in editing.
All changes propagate immediately out to the variables text and textStyle.
Lazy paragraph means that no layout gets wasted on default initial
	contents when a textMorph is being created with other contents.
Bounds properly track growth and shrinkage due to editing, and
	command halos track this properly.
Alignment can now be changed from a menu.
"
TextMorphEditor removeSelector: #processRedButton!

Object subclass: #KeyboardBuffer
	instanceVariableNames: 'event eventUsed '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
Morph subclass: #TextMorph
	instanceVariableNames: 'textStyle text wrapFlag paragraph editor '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Basic'!
ParagraphEditor subclass: #TextMorphEditor
	instanceVariableNames: 'morph oldInterval pivotBlock '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!HandMorph methodsFor: 'event dispatching' stamp: 'di 9/29/97 14:24'!
keyboardFocus 
	^ keyboardFocus! !

!HandMorph methodsFor: 'event dispatching' stamp: 'di 9/29/97 13:03'!
newKeyboardFocus: aMorphOrNil
	"Make the given morph the new keyboard focus, canceling the previous keyboard focus if any. If the argument is nil, the current keyboard focus is cancelled."
	| oldFocus |
	oldFocus _ keyboardFocus.
	keyboardFocus _ aMorphOrNil.
	oldFocus ifNotNil: [oldFocus == aMorphOrNil ifFalse: [oldFocus keyboardFocusChange: false]].
	aMorphOrNil ifNotNil: [aMorphOrNil keyboardFocusChange: true].! !

!KeyboardBuffer methodsFor: 'all' stamp: 'di 9/30/97 19:53'!
commandKeyPressed
	^ event commandKeyPressed! !

!KeyboardBuffer methodsFor: 'all' stamp: 'di 9/30/97 19:54'!
controlKeyPressed
	^ event controlKeyPressed! !

!KeyboardBuffer methodsFor: 'all' stamp: 'di 9/29/97 12:34'!
flushKeyboard
	eventUsed ifFalse: [^ eventUsed _ true].! !

!KeyboardBuffer methodsFor: 'all' stamp: 'di 9/29/97 12:34'!
keyboard
	eventUsed ifFalse: [eventUsed _ true.  ^ event keyCharacter].
	^ nil! !

!KeyboardBuffer methodsFor: 'all' stamp: 'di 9/29/97 12:34'!
keyboardPeek
	eventUsed ifFalse: [^ event keyCharacter].
	^ nil! !

!KeyboardBuffer methodsFor: 'all' stamp: 'di 9/29/97 12:34'!
keyboardPressed
	^ eventUsed not! !

!KeyboardBuffer methodsFor: 'all' stamp: 'di 9/30/97 19:54'!
leftShiftDown
	^ event shiftPressed! !


!ParagraphEditor methodsFor: 'menu messages' stamp: 'di 9/30/97 10:23'!
changeAlignment
	| aList reply  |
	aList _ #(leftFlush centered justified rightFlush).
	reply _ (SelectionMenu labelList: aList selections: aList) startUp.
	reply ~~ nil ifTrue:
		[paragraph perform: reply.
		paragraph composeAll.
		self recomputeSelection.
		Display fill: paragraph clippingRectangle 
			fillColor: view backgroundColor.	"very brute force"
		self display.
		"paragraph changed"].
	^ true! !


!TextMorph methodsFor: 'initialization' stamp: 'di 9/30/97 09:25'!
initialize
	super initialize.
	color _ Color black.
	textStyle _ TextStyle default copy.
	wrapFlag _ true.
! !

!TextMorph methodsFor: 'initialization' stamp: 'di 9/29/97 11:48'!
text: t textStyle: s
	"Private -- for use only in morphic duplication"
	text _ t.
	textStyle _ s! !

!TextMorph methodsFor: 'accessing' stamp: 'di 9/30/97 15:48'!
contents: stringOrText
	^ self contentsAsIs: stringOrText! !

!TextMorph methodsFor: 'accessing' stamp: 'di 9/30/97 15:48'!
contentsAsIs: stringOrText
	"Accept new text contents with line breaks only as in the text.
	Fit my width and height to the result."
	wrapFlag _ false.
	self newContents: stringOrText! !

!TextMorph methodsFor: 'accessing' stamp: 'di 9/30/97 09:51'!
contentsWrapped: stringOrText
	"Accept new text contents.  Lay it out, wrapping within my current width.
	Then fit my height to the result."
	wrapFlag _ true.
	self newContents: stringOrText! !

!TextMorph methodsFor: 'accessing' stamp: 'di 9/29/97 11:47'!
copyRecordingIn: dict
	"Overridden to copy deeper text structure."
	^ (super copyRecordingIn: dict)
		text: text copy textStyle: textStyle copy! !

!TextMorph methodsFor: 'accessing' stamp: 'di 9/30/97 15:37'!
newContents: stringOrText
	"Accept new text contents."
	| newText |
	newText _ stringOrText asText.
	text = newText ifTrue: [^ self].  "No substantive change"
	text _ newText.
	self releaseParagraph.  "update the paragraph cache"
	self paragraph.  "re-instantiate to set bounds"! !

!TextMorph methodsFor: 'alignment' stamp: 'di 9/29/97 13:21'!
centered 
	textStyle centered.
	self changed! !

!TextMorph methodsFor: 'alignment' stamp: 'di 9/29/97 13:22'!
justified 
	textStyle justified.
	self changed! !

!TextMorph methodsFor: 'alignment' stamp: 'di 9/29/97 13:21'!
leftFlush 
	textStyle leftFlush.
	self changed! !

!TextMorph methodsFor: 'alignment' stamp: 'di 9/29/97 13:22'!
rightFlush 
	textStyle rightFlush.
	self changed! !

!TextMorph methodsFor: 'drawing'!
drawOn: aCanvas
	self hasFocus ifTrue:
		[aCanvas fillRectangle: bounds color: Color white.
		editor selectionRects do:
			[:rect | aCanvas fillRectangle: rect color: self selectionColor]].
	aCanvas paragraph: self paragraph bounds: bounds color: color.! !

!TextMorph methodsFor: 'drawing' stamp: 'di 9/30/97 10:40'!
selectionColor

	^ Color lightRed
! !

!TextMorph methodsFor: 'editing' stamp: 'di 9/30/97 10:26'!
acceptContents
	"The message is sent when the user hits enter or Cmd-S. Accept the current contents and end editing. This default implementation does nothing."
	self updateFromParagraph! !

!TextMorph methodsFor: 'editing' stamp: 'di 9/30/97 10:27'!
chooseAlignment
	self installEditor changeAlignment.
	self updateFromParagraph! !

!TextMorph methodsFor: 'editing' stamp: 'di 9/30/97 10:27'!
chooseEmphasis
	self installEditor changeEmphasis.
	self updateFromParagraph! !

!TextMorph methodsFor: 'editing' stamp: 'di 9/30/97 10:28'!
chooseFont
	self installEditor offerFontMenu.
	self updateFromParagraph! !

!TextMorph methodsFor: 'editing' stamp: 'di 9/30/97 10:28'!
chooseStyle
	self installEditor changeStyle.
	self updateFromParagraph! !

!TextMorph methodsFor: 'editing' stamp: 'di 9/29/97 12:34'!
handlesMouseDown: evt

	^ self uncoveredAt: evt cursorPoint! !

!TextMorph methodsFor: 'editing' stamp: 'di 9/29/97 11:46'!
hasFocus
	^ editor ~~ nil! !

!TextMorph methodsFor: 'editing' stamp: 'di 9/29/97 11:58'!
keyboardFocusChange: aBoolean
	aBoolean
		ifTrue: ["A hand is wanting to send us characters..."
				self hasFocus ifFalse: [self installEditor.
										self changed]]
		ifFalse: ["A hand has clicked elsewhere...".
				((self world hands collect: [:h | h keyboardFocus]) includes: self)
					ifFalse: [self releaseEditor.
							self changed]].
! !

!TextMorph methodsFor: 'editing'!
keyStroke: evt
	"Handle a keystroke event."
	self installEditor.
	editor sensor: (KeyboardBuffer new startingEvent: evt).  "Make a version that takes an event"
	editor readKeyboard.
	self updateFromParagraph! !

!TextMorph methodsFor: 'editing' stamp: 'di 9/29/97 12:39'!
mouseDown: evt
	"Make this TextMorph be the keyboard input focus, if it isn't already, and repond to the text selection gesture."

	evt hand newKeyboardFocus: self.  "This will install editor if nil"
	editor mouseDown: evt.
	self changed! !

!TextMorph methodsFor: 'editing' stamp: 'di 9/29/97 12:34'!
mouseMove: evt
	evt redButtonPressed
		ifTrue: [editor mouseMove: evt.
				self changed].
	! !

!TextMorph methodsFor: 'editing' stamp: 'di 9/29/97 12:34'!
mouseUp: evt
	editor mouseUp: evt.
	self changed! !

!TextMorph methodsFor: 'anchors' stamp: 'di 9/29/97 18:09'!
anchor: aMorph
	"Set an anchor for this morph in the current text selection"
	| anchor index |
	editor ifNil: [^ self halt].  "No can do"
	anchor _ TextMorphAnchor new
			anchoredObject: aMorph
			location: (self paragraph characterBlockForIndex: editor selectionInterval first) topLeft
						- self position.
	index _ editor selectionInterval first.
	self removeAllAnchorsTo: aMorph.
	text addAttribute: anchor from: index to: index! !

!TextMorph methodsFor: 'anchors' stamp: 'di 9/29/97 18:16'!
updateAnchors
	| anchors ranges |
	anchors _ OrderedCollection new.
	text runsDoStartStopAndAttributes:
		[:start :stop :attributes |
		attributes do: [:att | (att isMemberOf: TextMorphAnchor) ifTrue: [anchors add: att]]].
	anchors isEmpty ifTrue: [^ self].
	anchors do:
		[:a |
		ranges _ text findAttribute: a.
		"Update anchor location"
		a newLocation: (self paragraph characterBlockForIndex: ranges first first) topLeft
							- self position.
		ranges size > 1 ifTrue:
			[ranges allButFirst do:  "Remove any other fragmentary references"
				[:range | text removeAttribute: a from: range first to: range last]]].
	self layoutChanged! !

!TextMorph methodsFor: 'geometry' stamp: 'di 9/30/97 15:36'!
extent: aPoint
	self releaseEditor.
	self releaseParagraph.  "invalidate the paragraph cache"
	super extent: (aPoint max: 20@20).
	self fit! !

!TextMorph methodsFor: 'geometry' stamp: 'di 9/29/97 12:55'!
privateMoveBy: delta
	self releaseEditor.
	super privateMoveBy: delta.
! !

!TextMorph methodsFor: 'geometry' stamp: 'di 9/30/97 15:36'!
privateOwner: aMorph
	self releaseParagraph.  "invalidate the paragraph cache"
	super privateOwner: aMorph! !

!TextMorph methodsFor: 'menu' stamp: 'di 9/30/97 10:15'!
addCustomMenuItems: aCustomMenu hand: aHandMorph
	(submorphs isEmpty and: [editor ~~nil]) ifFalse:
		[aCustomMenu add: 'set anchor' action: #anchorMorph:].
	aCustomMenu add: 'remove all anchors' action: #removeAllAnchors.

	aCustomMenu add: 'set font...' action: #chooseFont.
	aCustomMenu add: 'set style...' action: #chooseStyle.
	aCustomMenu add: 'set alignment...' action: #chooseAlignment.
! !

!TextMorph methodsFor: 'private' stamp: 'di 9/30/97 15:25'!
fit
	"Adjust bounds vertically to fit the text."
	| height extent |
	wrapFlag ifTrue:
		[height _ self paragraph height.
		height ~= bounds height ifTrue: [super extent: bounds width @ height]]
		ifFalse:
		[extent _ self paragraph extent.
		extent ~= bounds extent ifTrue: [super extent: extent]].
	self updateAnchors.
	self changed! !

!TextMorph methodsFor: 'private'!
installEditor
	"Install an editor for my paragraph.  This constitutes 'hasFocus'."
	| editView |
	editor ifNotNil: [^ self].
	editor _ TextMorphEditor new morph: self.
	editView _ DisplayTextView new model: self paragraph controller: editor.
	editView window: self bounds viewport: self bounds.
	editor changeParagraph: self paragraph.
	^ editor! !

!TextMorph methodsFor: 'private'!
paragraph
	"Paragraph instantiation is lazy -- create it only when needed"
	| compWidth fullWidth |
	paragraph ifNotNil: [^ paragraph].
	text ifNil: [text _ 'Text' asText allBold].  "Default contents"

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

!TextMorph methodsFor: 'private' stamp: 'di 10/1/97 19:48'!
prepareToBeSaved
	self releaseEditor; releaseParagraph! !

!TextMorph methodsFor: 'private'!
releaseEditor
	"Release the editor for my paragraph.  This morph no longer 'hasFocus'."
	editor ifNotNil:
		[self paragraph release.
		editor _ nil].
self releaseParagraph "THIS IS A TEST!!"! !

!TextMorph methodsFor: 'private' stamp: 'di 9/30/97 15:35'!
releaseParagraph
	"Paragraph instantiation is lazy -- it will be created only when needed"
	paragraph ifNotNil:
		[paragraph release.
		paragraph _ nil]! !

!TextMorph methodsFor: 'private' stamp: 'di 10/2/97 01:20'!
updateFromParagraph
	paragraph ifNil: [^ self].
	wrapFlag ifNil: [wrapFlag _ true].
	text _ paragraph text.
	textStyle _ paragraph textStyle.
	self fit! !

!TextMorph methodsFor: 'object fileIn' stamp: 'di 10/1/97 19:47'!
convertbosfceptthpeh0: varDict bosfcepttwpe0: smartRefStrm
	"These variables are automatically stored into the new instance ('textStyle' 'text' 'paragraph' 'editor' ).
	This method is for additional changes. Use statements like (foo _ varDict at: 'foo')."

	"Be sure to to fill in ('wrapFlag' ) and deal with the information in ('hasFocus' 'hideSelection' )"
	wrapFlag _ true.
	editor _ nil.
	self updateFromParagraph; releaseParagraph.! !


!TextMorphEditor methodsFor: 'all'!
mouseDown: evt 
	"An attempt to break up the old processRedButton code into threee phases"
	| clickPoint |
	oldInterval _ startBlock stringIndex to: stopBlock stringIndex - 1.
	clickPoint _ evt cursorPoint.
	(paragraph clickAt: clickPoint for: nil) ifTrue: [^ self].
	self closeTypeIn.  "probably not necess"
	sensor leftShiftDown
		ifFalse:
			[stopBlock _ startBlock _ pivotBlock _
				paragraph characterBlockAtPoint: clickPoint]
		ifTrue:
			[(self characterBlockAtPoint: clickPoint) <= startBlock
			ifTrue: [stopBlock _ startBlock.
					pivotBlock _ stopBlock]
			ifFalse: [startBlock _  stopBlock.
					pivotBlock _ startBlock]].
! !

!TextMorphEditor methodsFor: 'all'!
mouseMove: evt 
	"An attempt to break up the old processRedButton code into threee phases"
	| dragBlock |
	dragBlock _ paragraph characterBlockAtPoint: (evt cursorPoint).
	dragBlock > pivotBlock
		ifTrue: [stopBlock _ dragBlock.  startBlock _ pivotBlock]
		ifFalse: [startBlock _ dragBlock.  stopBlock _ pivotBlock]! !

!TextMorphEditor methodsFor: 'all' stamp: 'di 9/29/97 12:34'!
mouseUp: evt
	"An attempt to break up the old processRedButton code into threee phases"
	(startBlock = stopBlock 
		and: [oldInterval = (startBlock stringIndex to: startBlock stringIndex-1)])
		ifTrue: [self selectWord].
	self setEmphasisHere.
	(self isDisjointFrom: oldInterval) ifTrue:
		[otherInterval _ oldInterval]! !

!TextMorphEditor methodsFor: 'all'!
scrollBy: ignore
	"Ignore scroll requests."
self halt.! !

!TextMorphEditor methodsFor: 'all'!
select
	"Ignore selection redraw requests."! !

!TextMorphEditor methodsFor: 'all' stamp: 'di 9/29/97 12:34'!
selectionRects
	"Return an array of rectangles comprising the selection"
	^ paragraph selectionRectsFrom: startBlock to: stopBlock! !

!TextMorphEditor methodsFor: 'all'!
updateMarker
	"Ignore scrollbar redraw requests."
! !

!TextMorphEditor methodsFor: 'all' stamp: 'di 10/1/97 17:00'!
zapSelectionWith: aText
	"**overridden to inhibit old-style display"
	| start stop |
	self deselect.
	start _ startBlock stringIndex.
	stop _ stopBlock stringIndex.
	(start = stop and: [aText size = 0]) ifFalse:
		[paragraph replaceFrom: start to: stop - 1
			with: aText displaying: false.  "** was true in super"
		self computeIntervalFrom: start to: start + aText size - 1.
		UndoInterval _ otherInterval _ self selectionInterval]! !


ParagraphEditor removeSelector: #selectionRects!
TextMorph removeSelector: #contentsClipped:!
TextMorph removeSelector: #screenRect!
TextMorph removeSelector: #text:textStyle:paragraph:!
TextMorphEditor removeSelector: #processRedButton!
TextMorphEditor removeSelector: #controlTerminate!
TextMorphEditor removeSelector: #clearStartAndStopBlocks!
TextMorph allSubInstancesDo: [:m | m updateFromParagraph; releaseParagraph]!

