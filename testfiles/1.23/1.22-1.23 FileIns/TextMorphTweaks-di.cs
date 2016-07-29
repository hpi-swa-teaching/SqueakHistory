'From Squeak 1.21c of Aug 4, 1997 on 2 October 1997 at 11:54:58 am'!
"Change Set:		TextMorphTweaks
Date:			2 October 1997
Author:			Dan Ingalls

Further TextMorph tweaks providing...
Takes out a 'self halt' left by accident.
Green selection for clarity.
Null selection displayed as a caret.
Anchored objects properly handled in duplication.
Elimination of spurious mvc display in some edits.
"!

Morph subclass: #TextMorph
	instanceVariableNames: 'textStyle text wrapFlag paragraph editor '
	classVariableNames: 'CaretForm SelectionColor '
	poolDictionaries: ''
	category: 'Morphic-Basic'!

!Paragraph methodsFor: 'selecting' stamp: 'di 10/2/97 09:25'!
selectionRectsFrom: characterBlock1 to: characterBlock2 
	"Return an array of rectangles representing the area between the two character blocks given as arguments."
	| visibleRectangle initialRectangle interiorRectangle finalRectangle lineNo baseline |
	characterBlock1 = characterBlock2 ifTrue:
		[lineNo _ self lineIndexOfCharacterIndex: characterBlock1 stringIndex.
		baseline _ lineNo = 0 ifTrue: [textStyle baseline]
							ifFalse: [(lines at: lineNo) baseline].
		^ Array with: (characterBlock1 topLeft extent: 1 @ baseline)].
	visibleRectangle _ clippingRectangle intersect: compositionRectangle.
	characterBlock1 top = characterBlock2 top
		ifTrue: [characterBlock1 left < characterBlock2 left
					ifTrue: 
						[initialRectangle _ 
							(characterBlock1 topLeft corner: characterBlock2 bottomLeft)
								intersect: visibleRectangle]
					ifFalse: 
						[initialRectangle _ 
							(characterBlock2 topLeft corner: characterBlock1 bottomLeft)
								intersect: visibleRectangle]]
		ifFalse: [characterBlock1 top < characterBlock2 top
					ifTrue: 
						[initialRectangle _ 
							(characterBlock1 topLeft 
								corner: visibleRectangle right @ characterBlock1 bottom)
								intersect: visibleRectangle.
						characterBlock1 bottom = characterBlock2 top
							ifTrue: 
								[finalRectangle _ 
									(visibleRectangle left @ characterBlock2 top 
										corner: characterBlock2 bottomLeft)
										intersect: visibleRectangle]
							ifFalse: 
								[interiorRectangle _ 
									(visibleRectangle left @ characterBlock1 bottom
										corner: visibleRectangle right 
														@ characterBlock2 top)
										intersect: visibleRectangle.
								finalRectangle _ 
									(visibleRectangle left @ characterBlock2 top 
										corner: characterBlock2 bottomLeft)
										intersect: visibleRectangle]]
				ifFalse: 
					[initialRectangle _ 
						(visibleRectangle left @ characterBlock1 top 
							corner: characterBlock1 bottomLeft)
							intersect: visibleRectangle.
					characterBlock1 top = characterBlock2 bottom
						ifTrue: 
							[finalRectangle _ 
								(characterBlock2 topLeft 
									corner: visibleRectangle right 
												@ characterBlock2 bottom)
									intersect: visibleRectangle]
						ifFalse: 
							[interiorRectangle _ 
								(visibleRectangle left @ characterBlock2 bottom 
									corner: visibleRectangle right @ characterBlock1 top)
									intersect: visibleRectangle.
							finalRectangle _ 
								(characterBlock2 topLeft 
									corner: visibleRectangle right 
												@ characterBlock2 bottom)
									intersect: visibleRectangle]]].
	^ (Array with: initialRectangle with: interiorRectangle with: finalRectangle)
			select: [:rect | rect notNil]! !


!ParagraphEditor methodsFor: 'menu messages' stamp: 'di 10/2/97 11:36'!
changeAlignment
	| aList reply  |
	aList _ #(leftFlush centered justified rightFlush).
	reply _ (SelectionMenu labelList: aList selections: aList) startUp.
	reply ~~ nil ifTrue:
		[paragraph perform: reply.
		paragraph composeAll.
		self recomputeSelection.
		self mvcRedisplay].
	^ true! !

!ParagraphEditor methodsFor: 'menu messages' stamp: 'di 10/2/97 11:39'!
changeEmphasis
	| aList reply  |
	aList _ #(plain bold italic narrow underlined struckOut).
	reply _ (SelectionMenu labelList: aList selections: aList) startUp.
	reply ~~ nil ifTrue:
		[self setEmphasis: reply.
		paragraph composeAll.
		self recomputeSelection.
		self mvcRedisplay].
	^ true! !

!ParagraphEditor methodsFor: 'menu messages' stamp: 'di 10/2/97 11:41'!
changeStyle
	"Let user change styles for the current text pane  
	 Moved from experimentalCommand to its own method  "

	| aList reply style |
	aList _ (TextConstants select: [:thang | thang isKindOf: TextStyle])
			keys asOrderedCollection.
	reply _ (SelectionMenu labelList: aList selections: aList) startUp.
	reply ~~ nil ifTrue:
		[style _ TextConstants at: reply ifAbsent: [self beep. ^ true].
		paragraph textStyle: style copy.
		paragraph composeAll.
		self recomputeSelection.
		self mvcRedisplay].
	^ true! !

!ParagraphEditor methodsFor: 'menu messages' stamp: 'di 10/2/97 11:34'!
mvcRedisplay
	"Overridable by subclasses that do their own display"
	Display fill: paragraph clippingRectangle 
			fillColor: view backgroundColor.	"very brute force"
	self display! !


!TextMorph methodsFor: 'drawing' stamp: 'di 10/2/97 10:30'!
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
	aCanvas paragraph: self paragraph bounds: bounds color: color.! !

!TextMorph methodsFor: 'drawing' stamp: 'di 10/2/97 09:13'!
selectionColor
	^ (Color r: 0.4 g: 1.0 b: 0)
! !

!TextMorph methodsFor: 'editing' stamp: 'di 10/2/97 10:32'!
mouseDown: evt
	"Make this TextMorph be the keyboard input focus, if it isn't already, and repond to the text selection gesture."

	evt hand newKeyboardFocus: self.  "This will install editor if nil"
	evt hand showTemporaryCursor: Cursor blank.
	editor mouseDown: evt.
	self changed! !

!TextMorph methodsFor: 'editing' stamp: 'di 10/2/97 10:32'!
mouseUp: evt
	editor mouseUp: evt.
	evt hand showTemporaryCursor: nil.
	self changed! !

!TextMorph methodsFor: 'private' stamp: 'di 10/2/97 11:17'!
installEditor
	"Install an editor for my paragraph.  This constitutes 'hasFocus'."
	| editView |
	editor ifNotNil: [^ editor].
	editor _ TextMorphEditor new morph: self.
	editView _ DisplayTextView new model: self paragraph controller: editor.
	editView window: self bounds viewport: self bounds.
	editor changeParagraph: self paragraph.
	^ editor! !

!TextMorph methodsFor: 'private' stamp: 'di 10/2/97 10:42'!
text: t textStyle: s
	"Private -- for use only in morphic duplication"
	text _ t.
	textStyle _ s! !

!TextMorph methodsFor: 'private' stamp: 'di 10/2/97 11:14'!
updateReferencesUsing: refDict
	| anchors range new |
	super updateReferencesUsing: refDict.
	"Update any anchors in the text of a newly copied morph"
	anchors _ IdentityDictionary new.
	text runsDoStartStopAndAttributes:
		[:start :stop :attributes |
		attributes do: [:att | (att isMemberOf: TextMorphAnchor)
							ifTrue: [anchors at: att put: (start to: stop)]]].
	anchors isEmpty ifTrue: [^ self].
	anchors keysDo:
		[:old |  range _ anchors at: old.
		text removeAttribute: old from: range first to: range last.
		new _ TextMorphAnchor new anchoredObject: (refDict at: old anchoredObject)
								location: old location.
		text addAttribute: new from: range first to: range last].
	self layoutChanged "for good measure"! !


!TextMorph class methodsFor: 'as yet unclassified' stamp: 'di 10/2/97 10:30'!
initialize	"TextMorph initialize"
	"Initialize constants shared by classes associated with text display."

	SelectionColor _ Color r: 0.4 g: 1.0 b: 0.
	CaretForm _ (ColorForm extent: 16@5
					fromArray: #(2r001100e26 2r001100e26 2r011110e26 2r111111e26 2r110011e26)
					offset: -2@0)
					colors: (Array with: Color transparent with: SelectionColor)! !


!TextMorphAnchor methodsFor: 'all' stamp: 'di 10/2/97 11:13'!
location
	^ location! !


!TextMorphEditor methodsFor: 'all' stamp: 'di 10/2/97 11:38'!
mvcRedisplay
	"Ignore mvcRedisplay requests."! !

!TextMorphEditor methodsFor: 'all' stamp: 'di 10/2/97 09:08'!
scrollBy: ignore 
	"Ignore scroll requests."! !


TextMorph initialize!
