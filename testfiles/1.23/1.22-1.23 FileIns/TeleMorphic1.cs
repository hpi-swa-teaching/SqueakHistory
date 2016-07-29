'From Squeak 1.22 of September 21, 1997 on 26 September 1997 at 1:10:07 pm'!
"Change Set:		TeleMorphic
Date:			26 September 1997
Author:			Dan Ingalls, John Malone

Numerous changes to support multi-user applications in Morphic.
Major bodies of code include Hans-Martin Mosner's MenuMorph, 
which is required for operation from remote hands.  Also a new
color chooser morph, and changes to Polygon, Curve and TextMorph."!

Object subclass: #Morph
	instanceVariableNames: 'bounds owner submorphs fullBounds color eventHandler '
	classVariableNames: 'EmptyArray '
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
SketchMorph subclass: #ColorPickerMorph
	instanceVariableNames: 'selectedColor sourceHand deleteOnMouseUp selector target '
	classVariableNames: 'ColorChart FeedbackBox TransparentBox '
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
Morph subclass: #HandMorph
	instanceVariableNames: 'eventSubscribers keyboardFocus mouseDownMorph mouseOverMorphs clickClient clickState firstClickEvent firstClickTime userInitials lastEvent eventTransform argument lastMenuSelection lastMetaMenuItem targetOffset damageRecorder cacheCanvas cachedCanvasHasHoles temporaryCursor grid gridOn remoteConnections lastEventTransmitted lastWorldExtent '
	classVariableNames: 'BlankCursor ColorChart DoubleClickTime NormalCursor RemoteConnections UseHardwareCursor '
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
StringMorph subclass: #MenuItemMorph
	instanceVariableNames: 'action flags subMenu '
	classVariableNames: 'SubMenuMarker '
	poolDictionaries: ''
	category: 'Morphic-Menus'!
Morph subclass: #MenuLineMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!
LayoutMorph subclass: #MenuMorph
	instanceVariableNames: 'lastSelection receiver popUpOwner '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!
SketchMorph subclass: #MultiuserTinyPaint
	instanceVariableNames: 'drawState '
	classVariableNames: 'LastMouseIndex PenColorIndex PenIndex PenSizeIndex '
	poolDictionaries: ''
	category: 'Morphic-TinyPaint'!
BorderedMorph subclass: #Polygon
	instanceVariableNames: 'vertices closed filledForm quickFill arrows arrowForms handles midpoints '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Basic'!
HandMorph subclass: #RemoteHandMorph
	instanceVariableNames: 'remoteWorldExtent socket waitingForConnection receiveBuffer '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!
TextMorphEditor removeSelector: #processRedButton!
ParagraphEditor subclass: #TextMorphEditor
	instanceVariableNames: 'morph oldInterval pivotBlock '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
SketchMorph subclass: #TinyPaint
	instanceVariableNames: 'brush brushSize brushColor lastMouse '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
Morph subclass: #WorldMorph
	instanceVariableNames: 'hands viewBox canvas damageRecorder stepList lastStepTime model eToyHolder '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!Object methodsFor: 'menus'!
dispatchAsMenuActionTo: anObject
	"Don't know what to do..."! !

!Object methodsFor: 'menus'!
performMenuAction: anObject
	^anObject dispatchAsMenuActionTo: self! !

!Object methodsFor: 'menus'!
performMenuAction: anObject with: argument
	^anObject dispatchAsMenuActionTo: self with: argument! !


!BlockContext methodsFor: 'menus'!
dispatchAsMenuActionTo: anObject
	^self numArgs = 0
		ifTrue: [self value]
		ifFalse: [self value: anObject]! !

!BlockContext methodsFor: 'menus'!
dispatchAsMenuActionTo: anObject with: argument
	^self numArgs = 0
		ifTrue: [self value]
		ifFalse: [self numArgs = 1
					ifTrue: [self value: anObject]
					ifFalse: [self value: anObject value: argument]]! !

!BlockContext methodsFor: 'menus'!
performMenuAction: anObject
	self value: anObject! !


!Color class methodsFor: 'other' stamp: 'jm 9/25/97 13:52'!
colorPaletteForDepth: depth extent: paletteExtent
	"Returns a form of the given size showing a color palette for the given depth."
	"(Color colorPaletteForDepth: Display depth extent: 720@100) display"

	| c p f nSteps rect w h q |
	f _ Form extent: paletteExtent depth: depth.
	f fill: f boundingBox fillColor: Color white.
	nSteps _ depth>8 ifTrue: [12] ifFalse: [6].
	w _ paletteExtent x // (nSteps*nSteps).
	h _ paletteExtent y - 20 // nSteps.
	0 to: nSteps-1 do: [:r |
		0 to: nSteps-1 do: [:g |
			0 to: nSteps-1 do: [:b |
				c _ Color r: r g: g b: b range: nSteps-1.
				rect _ ((r*nSteps*w) + (b*w)) @ (g*h) extent: w@(h+1).
				f fill: rect fillColor: c].
			].
		].
	q _ Quadrangle origin: paletteExtent - (50@19) corner: paletteExtent.
	q displayOn: f.
	('Trans.' asParagraph asForm) displayOn: f at: q origin + (9@0) rule: Form paint.

	w _ ((paletteExtent x - q width - 130) // 64) max: 1.
	p _ paletteExtent x - q width - (64 * w) - 1 @ (paletteExtent y - 19).
	0 to: 63 do:
		[ :v | c _ Color r: v g: v b: v range: 63.
		f fill: ((v*w)@0 + p extent: (w+1)@19) fillColor: c].
	^ f! !


!ColorPickerMorph class reorganize!
('all' initialize)
!


!ColorPickerMorph class methodsFor: 'all' stamp: 'jm 9/25/97 14:18'!
initialize
	"ColorPickerMorph initialize"

	| chartExtent |
	chartExtent _ 216@56.
	ColorChart _ Color colorPaletteForDepth: 8 extent: chartExtent.
	TransparentBox _ Rectangle origin: chartExtent - (50@19) extent: 50@19.
	FeedbackBox _ 0@37 extent: 101@19.
! !


!KeyboardBuffer methodsFor: 'all' stamp: 'di 9/25/97 11:40'!
flushKeyboard
	eventUsed ifFalse: [^ eventUsed _ true].! !

!KeyboardBuffer methodsFor: 'all' stamp: 'di 9/25/97 11:41'!
keyboard
	eventUsed ifFalse: [eventUsed _ true.  ^ event keyCharacter].
	^ nil! !

!KeyboardBuffer methodsFor: 'all' stamp: 'di 9/25/97 11:41'!
keyboardPeek
	eventUsed ifFalse: [^ event keyCharacter].
	^ nil! !

!KeyboardBuffer methodsFor: 'all' stamp: 'di 9/25/97 11:41'!
keyboardPressed
	^ eventUsed not! !


!MenuItemMorph class methodsFor: 'class initialization'!
initialize
	"MenuItemMorph initialize"
	SubMenuMarker := Form
			extent: 5@9
			fromArray: #( 2147483648 3221225472 3758096384 4026531840 4160749568 4026531840 3758096384 3221225472 2147483648)
			offset: 0@0! !


!Morph methodsFor: 'geometry' stamp: 'di 9/25/97 20:45'!
align: aPoint1 with: aPoint2
	"Translate by aPoint2 - aPoint1."

	^ self position: self position + (aPoint2 - aPoint1)! !


!BorderedMorph methodsFor: 'menu' stamp: 'jm 9/25/97 16:55'!
changeBorderColor: evt

	evt hand changeColorTarget: self selector: #borderColor:.
! !

!BorderedMorph methodsFor: 'menu' stamp: 'di 9/25/97 19:53'!
changeBorderWidth: event
	| menu |
	menu _ MenuMorph new.
	0 to: 5 do: [:w | menu add: w printString action: w].
	menu receiver: [:w | self borderWidth: w].
	menu popUpOwner: event hand.
	menu popUpAt: event hand position in: event hand world for: event hand! !


!ColorPickerMorph reorganize!
('initialization' initialize)
('accessing' deleteOnMouseUp deleteOnMouseUp: selectedColor selector selector: sourceHand sourceHand: target target:)
('event handling' handlesMouseDown: mouseDown: mouseUp:)
('stepping' step stepTime)
('private' pickColorAt: updateColor:feedbackColor:)
!


!ColorPickerMorph methodsFor: 'initialization' stamp: 'jm 9/25/97 16:29'!
initialize

	super initialize.
	self form: ColorChart deepCopy.
	selectedColor _ Color white.
	sourceHand _ nil.
	deleteOnMouseUp _ true.
	selector _ nil.
	target _ nil.
! !

!ColorPickerMorph methodsFor: 'accessing' stamp: 'jm 9/25/97 16:20'!
deleteOnMouseUp

	^ deleteOnMouseUp
! !

!ColorPickerMorph methodsFor: 'accessing' stamp: 'jm 9/25/97 16:18'!
deleteOnMouseUp: aBoolean

	deleteOnMouseUp _ aBoolean.
! !

!ColorPickerMorph methodsFor: 'accessing' stamp: 'jm 9/25/97 16:19'!
selectedColor

	^ selectedColor
! !

!ColorPickerMorph methodsFor: 'accessing' stamp: 'jm 9/25/97 16:20'!
selector

	^ selector
! !

!ColorPickerMorph methodsFor: 'accessing' stamp: 'jm 9/25/97 16:19'!
selector: aSymbol

	selector _ aSymbol.
! !

!ColorPickerMorph methodsFor: 'accessing' stamp: 'jm 9/25/97 16:20'!
sourceHand

	^ sourceHand
! !

!ColorPickerMorph methodsFor: 'accessing' stamp: 'jm 9/25/97 16:19'!
sourceHand: aHand

	sourceHand _ aHand.
! !

!ColorPickerMorph methodsFor: 'accessing' stamp: 'jm 9/25/97 16:20'!
target

	^ target
! !

!ColorPickerMorph methodsFor: 'accessing' stamp: 'jm 9/25/97 16:19'!
target: anObject

	target _ anObject.
! !

!ColorPickerMorph methodsFor: 'event handling' stamp: 'jm 9/25/97 15:35'!
handlesMouseDown: evt

	^ true
! !

!ColorPickerMorph methodsFor: 'event handling' stamp: 'jm 9/25/97 16:12'!
mouseDown: evt

	sourceHand _ evt hand.
	self startStepping.
! !

!ColorPickerMorph methodsFor: 'event handling' stamp: 'jm 9/25/97 16:28'!
mouseUp: evt

	self stopStepping.
	sourceHand _ nil.
	deleteOnMouseUp ifTrue: [self delete].
! !

!ColorPickerMorph methodsFor: 'stepping' stamp: 'jm 9/25/97 16:05'!
step

	sourceHand ifNotNil:
		[self pickColorAt: sourceHand position].
! !

!ColorPickerMorph methodsFor: 'stepping' stamp: 'jm 9/25/97 16:08'!
stepTime

	^ 50
! !

!ColorPickerMorph methodsFor: 'private' stamp: 'jm 9/25/97 16:03'!
pickColorAt: aPoint

	| worldBox globalP c |
	(FeedbackBox containsPoint: aPoint - self topLeft) ifTrue: [^ self].  "do nothing"

	"pick up color, either inside or outside this world"
	worldBox _ self world viewBox.
	globalP _ aPoint + worldBox topLeft.  "get point in screen coordinates"
	(worldBox containsPoint: globalP)
		ifTrue: [c _ self world colorAt: aPoint belowMorph: Morph new]
		ifFalse: [c _ Display colorAt: globalP].

	"check for transparent color and update using appropriate feedback color"
	(TransparentBox containsPoint: aPoint - self topLeft)
		ifTrue: [self updateColor: Color transparent feedbackColor: Color white]
		ifFalse: [self updateColor: c feedbackColor: c].
! !

!ColorPickerMorph methodsFor: 'private' stamp: 'jm 9/25/97 18:04'!
updateColor: aColor feedbackColor: feedbackColor
	"Set my selected color to the given color if it is different. Give user feedback. Inform the target of the change if the target and selector are not nil." 

	selectedColor = aColor ifTrue: [^ self].  "do nothing if color doesn't change"

	originalForm fill: FeedbackBox fillColor: feedbackColor.
	self form: originalForm.
	selectedColor _ aColor.
	((target ~~ nil) and: [selector ~~ nil]) ifTrue: [
		selector numArgs = 2
			ifTrue: [target perform: selector with: aColor with: sourceHand]
			ifFalse: [target perform: selector with: aColor]].
! !


!HandMorph methodsFor: 'initialization' stamp: 'jm 9/24/97 13:40'!
initialize
	super initialize.
	self initForEvents.
	keyboardFocus _ nil.
	mouseOverMorphs _ OrderedCollection new.
	bounds _ 0@0 extent: Cursor normal extent.
	userInitials _ ''.
	damageRecorder _ DamageRecorder new.
	grid _ 4@4.
	gridOn _ false.
	remoteConnections _ OrderedCollection new.
	lastEventTransmitted _ MorphicEvent new.

! !

!HandMorph methodsFor: 'event dispatching' stamp: 'di 9/24/97 10:44'!
handleMouseUp: evt
	"Dispatch a mouseUp event."
	| oldFocus |
	clickState ~~ #idle ifTrue: [self checkForDoubleClick: evt].

	"drop morphs being carried, if any"
	mouseDownMorph = nil ifTrue: [^ self dropMorphsEvent: evt].

	"ensure that at least one mouseMove: is reported for each mouse focus transaction:"
	mouseDownMorph mouseMove: (self transformEvent: (evt copy setType: #mouseMove)).

	oldFocus := mouseDownMorph.	"make sure that focus becomes nil."
	mouseDownMorph _ nil.  "mouse focus transaction ends when mouse goes up"
	oldFocus mouseUp: (self transformEvent: evt).
! !

!HandMorph methodsFor: 'event dispatching' stamp: 'di 9/25/97 20:03'!
mouseDownRecipient: aMorph
	"Install a new recipient for mousedown events, namely stillDown and up,
	and, in the process, unsubscribe any prior recipient"

	mouseDownMorph _ aMorph! !

!HandMorph methodsFor: 'event dispatching' stamp: 'di 9/25/97 14:34'!
newKeyboardFocus: aMorphOrNil
	"Make the given morph the new keyboard focus, canceling the previous keyboard focus if any. If the argument is nil, the current keyboard focus is cancelled."

	keyboardFocus == aMorphOrNil ifTrue: [^ self].
	keyboardFocus ifNotNil: [keyboardFocus keyboardFocusChange: false].
	keyboardFocus _ aMorphOrNil.
	aMorphOrNil ifNotNil: [aMorphOrNil keyboardFocusChange: true].
! !

!HandMorph methodsFor: 'event dispatching' stamp: 'di 9/24/97 11:37'!
newMouseFocus: aMorphOrNil
	"Old protocol for compatibility with HMM's MorphicMenu"

	mouseDownMorph _ aMorphOrNil! !

!HandMorph methodsFor: 'event dispatching' stamp: 'jm 9/25/97 10:36'!
processEvents
	"Process user input events from the local input devices."

	| griddedPoint evt currentExtent |
	griddedPoint _ Sensor cursorPoint - owner viewBox topLeft.
	gridOn ifTrue: [griddedPoint _ griddedPoint grid: grid].

	evt _ MorphicEvent new
		setMousePoint: griddedPoint
		buttons: Sensor primMouseButtons
		lastEvent: lastEvent
		hand: self.

	remoteConnections size > 0 ifTrue: [
		currentExtent _ self worldBounds extent.
		lastWorldExtent ~= currentExtent ifTrue: [
			self transmitEvent: (MorphicEvent newWorldExtent: currentExtent).
			lastWorldExtent _ currentExtent].
		self transmitEvent: evt].

	(evt yellowButtonPressed and:
	 [lastEvent yellowButtonPressed not]) ifTrue: [
		lastEvent _ evt.
		^ self invokeMetaMenu: evt].

	(evt blueButtonPressed and:
	 [lastEvent blueButtonPressed not]) ifTrue: [
		lastEvent _ evt.
		^ self specialGesture: evt].

	lastEvent _ evt.
	self handleEvent: evt.

	Sensor keyboardPressed ifTrue: [
		evt _ MorphicEvent new
			setKeyValue: Sensor keyboard asciiValue
			mousePoint: griddedPoint
			buttons: Sensor primMouseButtons
			hand: self.
		lastEvent _ evt.
		self handleEvent: evt.
		remoteConnections size > 0 ifTrue: [self transmitEvent: evt]].
! !

!HandMorph methodsFor: 'meta menu' stamp: 'di 9/24/97 12:09'!
buildMorphMenuFor: argMorph
	"Build the morph menu. This menu has two sections. The first section contains commands that are interpreted by the hand; the second contains commands provided by the target morph. The variable lastMetaMenuItem determines the boundary between the sections."
	| menu |
	argument _ argMorph.
	menu _ MenuMorph new.

	menu add: 'grab' action: #grabMorph.
	menu add: 'delete' action: #dismissMorph.
	menu add: 'go behind' action: #goBehind.
	menu add: 'duplicate' action: #duplicateMorph.
	((self world rootMorphsAt: targetOffset) size > 1)
		ifTrue: [menu add: 'embed' action: #embedMorph].
	(argMorph isKindOf: SketchMorph)  ifFalse: [
		menu add: 'resize' action: #resizeMorph.
		menu add: 'fill color' action: #changeColor].
	(argMorph morphsAt: targetOffset) size > 1 ifTrue: [
		menu add: 'submorphs...' action: #operateOnSubmorph:].
	menu addLine.
	menu add: 'inspect' action: #inspectMorph.
	menu add: 'browse' action: #browseMorphClass.
	menu add: 'make own subclass' action: #subclassMorph.
	menu addLine.
	menu add: 'sensitize' action: #sensitizeMorph.
	menu add: 'name me' action: #nameMorph.
	(argMorph isKindOf: MorphicModel) ifTrue: [
		menu add: 'save morph as prototype' action: #saveAsPrototype.
		(argMorph ~~ self world modelOrNil) ifTrue: [
			 menu add: 'become this world''s model' action: #beThisWorldsModel]].
	menu add: 'save morph in file' action: #saveMorphInFile.
	menu addLine.
	lastMetaMenuItem _ menu selections size.
	argMorph addCustomMenuItems: menu hand: self.
	^ menu
! !

!HandMorph methodsFor: 'meta menu' stamp: 'jm 9/26/97 10:16'!
buildWorldMenu
	"Build the meta menu for the world."
	| menu |
	menu _ MenuMorph new.
	Project current isTopProject ifFalse:
		[menu add: 'exit this world' action: #exitWorld.
		menu addLine].
	menu add: 'new morph' action: #newMorph.
	menu add: 'new drawing' action: #makeNewDrawing.
	menu add: 'read morph(s) from file' action: #readMorphFile.
	menu addLine.
	menu add: 'run all' action: #startRunningAll.
	menu add: 'stop all' action: #stopRunningAll.
	menu addLine.
	menu add: 'change background color' action: #changeBackgroundColor.
	menu add: 'inspect world' action: #inspectWorld.
	menu addLine.
		menu add: 'save world in file' action: #saveWorldInFile.
	menu addLine.
	menu add: 'add slot to model' action: #newVariable.
	menu add: 'write init method for model' action: #writeInitMethodForModel.
	menu add: 'grab model for this world' action: #grabModel.
	gridOn
		ifTrue: [menu add: 'turn gridding off' action: #setGridding]
		ifFalse: [menu add: 'turn gridding on' action: #setGridding].
	menu addLine.
	menu add: 'local host address' action: #reportLocalAddress.
	menu add: 'connect remote user' action: #connectRemoteUser.
	menu add: 'disconnect remote user' action: #disconnectRemoteUser.
	menu add: 'disconnect all remote users' action: #disconnectAllRemoteUsers.
	lastMetaMenuItem _ menu selections size.
	^ menu
! !

!HandMorph methodsFor: 'meta menu' stamp: 'jm 9/25/97 16:55'!
changeBackgroundColor

	self changeColorTarget: self world selector: #color:.
! !

!HandMorph methodsFor: 'meta menu' stamp: 'jm 9/25/97 16:55'!
changeColor

	self changeColorTarget: argument selector: #color:.
! !

!HandMorph methodsFor: 'meta menu' stamp: 'di 9/25/97 21:15'!
changeColorTarget: aMorph selector: aSymbol

	| m points b |
	m _ ColorPickerMorph new
		sourceHand: self;
		target: aMorph;
		selector: aSymbol.
	points _ #(topCenter rightCenter bottomCenter leftCenter). "possible anchors"
	1 to: 4 do:
		[:i |  "Try the four obvious anchor points"
		b _ m bounds align: (m bounds perform: (points at: i))
					with: (aMorph bounds perform: (points atWrap: i+2)).
		(self worldBounds containsRect: b) ifTrue:
			["Yes it fits"
			m position: b topLeft.
			^ self world acceptDroppingMorph: m event: MorphicEvent new]].

	"when all else fails..."
	m position: 20@20.
	self world acceptDroppingMorph: m event: MorphicEvent new.
! !

!HandMorph methodsFor: 'meta menu' stamp: 'di 9/24/97 20:48'!
invokeMetaMenu: evt
"Invoke the meta menu. If the hand is over the background, the world menu is presented. If it is over a morph, a menu of operations for that morph is presented. Each menu entry contains a string to be presented in the menu and a selector. If the selector takes an argument, the mouse-down event that invoked the menu is passed as an argument. This lets the command know which hand invoked it in  order to do things like attaching the result of the command to that hand."

"Shortcut: If the shift key is pressed, the user is given a chance to select a submorph on which to operate."
	| menu caption receiver saveTargetOffset |
	"if carrying morphs, just drop them"
	self hasSubmorphs ifTrue: [^ self dropMorphsEvent: evt].

	targetOffset _ saveTargetOffset _ self position.
	argument _ self argumentOrNil.
	argument == nil
		ifTrue: [caption _ 'World'.
				menu _ self buildWorldMenu]
		ifFalse: [evt shiftPressed ifTrue:
					["Shift key shortcut to submorphs"
					argument _ self chooseTargetSubmorphOf: argument.
					argument ifNil: [^ self]].  "user abort"
				menu _ self buildMorphMenuFor: argument.
				caption _ argument class name].
	menu addTitle: caption.
	menu lastSelectionAction: lastMenuSelection.
	menu popUpOwner: self.
	menu receiver: [:item |
		targetOffset _ saveTargetOffset.
		menu delete.
		owner displayWorld.
		item ifNotNil: [
			lastMenuSelection _ item.
			receiver := (menu selections indexOf: item) <= lastMetaMenuItem
				ifTrue: [self]
				ifFalse: [self argumentOrNil].
			Cursor normal showWhile: [receiver performMenuAction: item with: evt]]].
	menu popUpAt: evt cursorPoint in: owner for: self.
	self newMouseFocus: menu items first! !

!HandMorph methodsFor: 'meta menu' stamp: 'di 9/24/97 20:48'!
newMorph

	| morphClassList menu categories subMenu |
	menu _ MenuMorph new.
	menu addTitle: 'Select Morph Class'.
	morphClassList _ Morph withAllSubclasses asSortedCollection:
		[:m1 :m2 | m1 class name < m2 class name].
	morphClassList remove: WorldMorph;
			remove: HandMorph;
			remove: MorphicModel;
			remove: RemoteHandMorph.
	morphClassList := morphClassList select:
			[:c | (c inheritsFrom: MorphicModel) not or:
						["Only include Models that have been saved"
						c includesSelector: #initMorph]].
	categories := (morphClassList collect: [:each | each category]) asSet asSortedCollection.
	categories do: [:cat |
		subMenu := MenuMorph new.
		subMenu receiver: [:cls | self newMorphOfClass: cls].
		morphClassList do: [:each |
			each category = cat ifTrue: [subMenu add: each name action: each]].
		menu add: cat subMenu: subMenu].
	menu popUpOwner: self.
	menu popUpAt: self position in: owner for: self! !

!HandMorph methodsFor: 'meta menu'!
newMorphOfClass: morphClass
	"taken out of the old method #newMorph"
	| m |
	m _ morphClass new.
	m installModelIn: owner.  "A chance to install model pointers"
	self attachMorph: m.
	owner startSteppingSubmorphsOf: m! !

!HandMorph methodsFor: 'remote morphic' stamp: 'jm 9/26/97 11:32'!
cleanupDeadConnections
	"Clean up any remote connections that have been disconnected or become invalid."

	| liveConnections sock |
	liveConnections _ OrderedCollection new.
	remoteConnections do: [:pair |
		sock _ pair first.
		sock isUnconnectedOrInvalid
			ifTrue: [pair first destroy]
			ifFalse: [liveConnections add: pair]].
	remoteConnections _ liveConnections.
! !

!HandMorph methodsFor: 'remote morphic' stamp: 'jm 9/26/97 11:00'!
connectRemoteUser
	"Prompt for the initials to be used to identify the cursor of a remote user, then create a cursor for that user and wait for a connection."

	| initials addr h |
	initials _ FillInTheBlank request: 'Enter initials for remote user''s cursor?'.
	initials isEmpty ifTrue: [^ self].  "abort"
	addr _ NetNameResolver promptUserForHostAddress.
	addr = 0 ifTrue: [^ self].  "abort"

	Socket ensureNetworkConnected.
	h _ RemoteHandMorph new userInitials: initials.
	self world addHand: h.
	h startListening.
	self startTransmittingEventsTo: addr.
! !

!HandMorph methodsFor: 'remote morphic' stamp: 'jm 9/26/97 10:30'!
disconnectAllRemoteUsers
	"Disconnect all remote hands and stop transmitting events."

	| addr |
	self world hands do: [:h |
		(h isKindOf: RemoteHandMorph) ifTrue: [
			addr _ h remoteHostAddress.
			addr = 0 ifFalse: [self stopTransmittingEventsTo: addr].
			h withdrawFromWorld]].

	remoteConnections do: [:pair | pair first closeAndDestroy: 5].
	remoteConnections _ OrderedCollection new.
! !

!HandMorph methodsFor: 'remote morphic' stamp: 'jm 9/26/97 11:11'!
disconnectRemoteUser
	"Prompt for the initials of the remote user, then remove the remote hand with those initials, breaking its connection."

	"select hand to remove"
	| initials handToRemove addr |
	initials _ FillInTheBlank request: 'Enter initials for remote user''s cursor?'.
	initials isEmpty ifTrue: [^ self].  "abort"
	handToRemove _ nil.
	self world hands do: [:h |
		h userInitials = initials ifTrue: [handToRemove _ h]].
	handToRemove ifNil: [^ self].  "no hand with those initials"

	addr _ handToRemove remoteHostAddress.
	addr = 0 ifFalse: [self stopTransmittingEventsTo: addr].
	handToRemove withdrawFromWorld.
! !

!HandMorph methodsFor: 'remote morphic' stamp: 'jm 9/26/97 07:47'!
reportLocalAddress
	"Report the local host address of this computer."

	| addrString m s |
	Socket initializeNetwork.
	addrString _ NetNameResolver localAddressString.
	m _ RectangleMorph new
		color: (Color r: 0.6 g: 0.8 b: 0.6);
		extent: 118@36;
		borderWidth: 1.
	s _ StringMorph contents: 'Local Host Address:'.
	s position: m position + (5@4).
	m addMorph: s.
	s _ StringMorph contents: addrString.
	s position: m position + (5@19).
	m addMorph: s.
	self attachMorph: m.
! !

!HandMorph methodsFor: 'remote morphic' stamp: 'jm 9/26/97 11:05'!
startTransmittingEventsTo: addr
	"Attempt to broadcast events from this hand to a remote hand on the host with the given address. This method just creates the new socket and initiates a connection; it does not wait for the other end to answer."

	| sock |
	remoteConnections do: [:pair |
		sock _ pair first.
		(sock isConnected and: [sock remoteAddress = addr])
			ifTrue: [^ self]].  "don't connect if already connected to the given address"
	Transcript
		show: 'Connecting to remote WorldMorph at ';
		show: (NetNameResolver stringFromAddress: addr), ' ...'; cr.
	sock _ SimpleClientSocket new.
	sock connectTo: addr port: 54323.
	remoteConnections add: (Array with: sock with: #opening).
! !

!HandMorph methodsFor: 'remote morphic' stamp: 'jm 9/26/97 11:31'!
stopTransmittingEventsTo: addr
	"Stop broadcasting events from this world's cursor to a remote cursor on the host with the given address. This method issues a 'close' but does not destroy the socket; it will be destroyed when the other end reads the last data and closes the connection."

	| sock |
	remoteConnections do: [:pair |
		sock _ pair first.
		(sock isUnconnectedOrInvalid not and: [sock remoteAddress = addr]) ifTrue: [
			sock close.
			pair at: 2 put: #closing]].
! !

!HandMorph methodsFor: 'remote morphic' stamp: 'jm 9/26/97 11:33'!
transmitEvent: aMorphicEvent
	"Transmit the given event to all remote connections."

	| evtString sock firstEvt |
	lastEventTransmitted = aMorphicEvent ifTrue: [^ self].
	evtString _ aMorphicEvent storeString, (String with: Character cr).
	self cleanupDeadConnections.
	remoteConnections do: [:pair |
		sock _ pair first.
		sock isConnected
			ifTrue: [
				(pair at: 2) = #opening ifTrue: [
					"connection established; send worldExtent as first event"
					firstEvt _ MorphicEvent newWorldExtent: self worldBounds extent.
					sock sendData: firstEvt storeString, (String with: Character cr).
					Transcript
						show: 'Connection established with remote WorldMorph at ';
						show: (NetNameResolver stringFromAddress: sock remoteAddress); cr.
					pair at: 2 put: #connected].
				sock sendData: evtString]
			ifFalse: [
				(pair at: 2) = #connected ifTrue: [
					"other end has closed; close our end"
					Transcript
						show: 'Closing connection with remote WorldMorph at ';
						show: (NetNameResolver stringFromAddress: sock remoteAddress); cr.
					sock close.
					pair at: 2 put: #closing]]].

	lastEventTransmitted _ aMorphicEvent.
! !

!HandMorph methodsFor: 'remote morphic' stamp: 'jm 9/26/97 10:35'!
worldBounds

	^ self world bounds
! !


!MenuItemMorph methodsFor: 'flag accessing'!
isEnabled
	^(flags bitAnd: 1) = 0! !

!MenuItemMorph methodsFor: 'flag accessing'!
isEnabled: aBoolean
	self isEnabled = aBoolean ifTrue: [^self].
	flags := flags bitXor: 1.
	self color: (aBoolean ifTrue: [Color black] ifFalse: [Color gray])! !

!MenuItemMorph methodsFor: 'flag accessing'!
isInTransition
	^(flags bitAnd: 2) > 0! !

!MenuItemMorph methodsFor: 'flag accessing' stamp: 'di 9/24/97 20:48'!
isInTransition: aBoolean for: hand
	self isInTransition = aBoolean ifTrue: [^self].
	flags := flags bitXor: 2.
	self changed.
	self showSubMenuFor: hand! !

!MenuItemMorph methodsFor: 'accessing'!
action
	^action! !

!MenuItemMorph methodsFor: 'accessing'!
action: aSymbol
	action := aSymbol! !

!MenuItemMorph methodsFor: 'accessing'!
subMenu
	^subMenu! !

!MenuItemMorph methodsFor: 'accessing'!
subMenu: aMenuMorph
	subMenu := aMenuMorph.
	self changed! !

!MenuItemMorph methodsFor: 'drawing'!
drawOn: aCanvas
	(self isInTransition and: [self isEnabled]) ifTrue: [aCanvas fillRectangle: self bounds color: owner color darker].
	super drawOn: aCanvas.
	subMenu == nil ifFalse: [aCanvas image: SubMenuMarker at: (self bounds right - 8 @ (self bounds top + self bounds bottom - SubMenuMarker height + 1 // 2))]! !

!MenuItemMorph methodsFor: 'initialize-release'!
initialize
	color _ Color black.
	owner _ nil.
	submorphs _ EmptyArray.
	font _ nil.
	hasFocus _ false.
	contents := ''.
	bounds := 0@0 extent: 10@10.
	flags := 0! !

!MenuItemMorph methodsFor: 'events' stamp: 'jm 9/24/97 14:24'!
handlesMouseDown: evt

	^ true
! !

!MenuItemMorph methodsFor: 'events' stamp: 'di 9/24/97 20:49'!
mouseDown: evt
	"Handle a mouse down event. Menu items get activated when the mouse is over them."

	self isInMenu ifFalse: [^super mouseDown: evt].
	evt shiftPressed ifTrue: ["enable label editing" ^ super mouseDown: evt].
	evt hand newMouseFocus: self.
	self isInTransition: true for: evt hand! !

!MenuItemMorph methodsFor: 'events' stamp: 'di 9/24/97 20:51'!
mouseMove: evt
	| m |
	m _ evt hand recipientForMouseDown: evt.
	self isInTransition: m == self for: evt hand.
	m == self ifTrue: [^self].
	(((m isKindOf: MenuItemMorph) and: [m isInMenu]) and: [
		m owner == self owner or: [
		m owner == subMenu or: [
		m owner hasSubMenu: owner]]])
		ifTrue: [m owner == subMenu ifFalse: [self hideSubMenuFor: evt hand].
				(m owner == self owner or: [m subMenu == owner or: [m owner == subMenu]])
					ifFalse: [owner delete].
				evt hand newMouseFocus: m]! !

!MenuItemMorph methodsFor: 'events' stamp: 'di 9/24/97 20:52'!
mouseUp: evt
	"Handle a mouse up event. Menu items get activated when the mouse is over them."

	self isInTransition: false for: evt hand.
	self isInMenu ifTrue: [
		owner deleteIfPopUp.
		subMenu == nil
			ifFalse: [self hideSubMenuFor: evt hand]
			ifTrue: [(self bounds containsPoint: evt cursorPoint)
					ifTrue: [owner itemWasSelected: self]]]! !

!MenuItemMorph methodsFor: 'layout'!
hResizing
	^#spaceFill! !

!MenuItemMorph methodsFor: 'layout'!
isLayoutMorph
	^true! !

!MenuItemMorph methodsFor: 'layout' stamp: 'di 9/24/97 11:05'!
layoutInWidth: w height: h
	| scanner |
	scanner _ QuickPrint newOn: Display box: Display boundingBox font: font.
	self extent: ((scanner stringWidth: contents) @ (scanner lineHeight) max: w@h)! !

!MenuItemMorph methodsFor: 'layout'!
minHeight
	^self extent y! !

!MenuItemMorph methodsFor: 'layout'!
minWidth
	| scanner |
	scanner _ QuickPrint newOn: Display box: Display boundingBox font: font.
	^(scanner stringWidth: contents) + (subMenu == nil ifTrue: [0] ifFalse: [10])! !

!MenuItemMorph methodsFor: 'layout'!
vResizing
	^#shrinkWrap! !

!MenuItemMorph methodsFor: 'private' stamp: 'di 9/24/97 20:51'!
hideSubMenuFor: hand
	subMenu == nil ifFalse: [subMenu delete].
	self isInTransition: false for: hand! !

!MenuItemMorph methodsFor: 'private'!
isInMenu
	^owner isKindOf: MenuMorph! !

!MenuItemMorph methodsFor: 'private' stamp: 'di 9/24/97 20:48'!
showSubMenuFor: hand
	subMenu == nil ifFalse: [
		subMenu delete; popUpOwner: self.
		subMenu popUpAt: self bounds topRight in: self world for: hand]! !


!MenuLineMorph methodsFor: 'drawing'!
drawOn: aCanvas
	aCanvas fillRectangle: (bounds topLeft corner: bounds rightCenter) color: owner color darker.
	aCanvas fillRectangle: (bounds leftCenter corner: bounds bottomRight) color: owner color lighter! !

!MenuLineMorph methodsFor: 'layout'!
hResizing
	^#spaceFill! !

!MenuLineMorph methodsFor: 'layout'!
isLayoutMorph
	^true! !

!MenuLineMorph methodsFor: 'layout' stamp: 'di 9/24/97 11:06'!
layoutInWidth: w height: h
	self extent: w@h! !

!MenuLineMorph methodsFor: 'layout'!
minHeight
	^2! !

!MenuLineMorph methodsFor: 'layout'!
minWidth
	^10! !

!MenuLineMorph methodsFor: 'layout'!
vResizing
	^#shrinkWrap! !


!MenuMorph methodsFor: 'initialize-release' stamp: 'di 9/24/97 10:54'!
initialize
	super initialize.
	self setColor: (Color r: 0.8 g: 0.8 b: 0.8) borderWidth: 2 borderColor: #raised.
	inset := 3.
	orientation _ #vertical.
	hResizing _ #shrinkWrap.
	vResizing _ #shrinkWrap.
! !

!MenuMorph methodsFor: 'geometry'!
minHeightWhenEmpty

	^ 10
! !

!MenuMorph methodsFor: 'geometry'!
minWidthWhenEmpty

	^ 20
! !

!MenuMorph methodsFor: 'geometry' stamp: 'di 9/24/97 20:48'!
popUpAt: aPoint in: world for: hand
	| selectedItem delta |
	selectedItem := self items detect: [:each | each == lastSelection] ifNone: [].
	selectedItem == nil
		ifTrue: [self position: aPoint]
		ifFalse: [self position: aPoint - selectedItem position + self position].
	delta := self bounds amountToTranslateWithin: hand worldBounds.
	delta = (0@0) ifFalse: [self position: self position + delta].
	world addMorphFront: self.
	self changed! !

!MenuMorph methodsFor: 'geometry'!
positionSelectionAt: aPoint
	| selectedItem |
	selectedItem := self items detect: [:each | each == lastSelection] ifNone: [].
	selectedItem == nil
		ifTrue: [self position: aPoint]
		ifFalse: [self position: aPoint - selectedItem position + self position]! !

!MenuMorph methodsFor: 'adding'!
add: aString action: anAction
	| item |
	item := MenuItemMorph new.
	item contents: aString; action: anAction.
	self addMorphBack: item! !

!MenuMorph methodsFor: 'adding'!
add: aString subMenu: aMenuMorph
	| item |
	item := MenuItemMorph new.
	item contents: aString; subMenu: aMenuMorph.
	self addMorphBack: item! !

!MenuMorph methodsFor: 'adding'!
addLine
	self addMorphBack: MenuLineMorph new! !

!MenuMorph methodsFor: 'adding' stamp: 'di 9/24/97 10:53'!
addTitle: aString
	| title |
	title := LayoutMorph new setColor: (Color r: 0.5 g: 1 b: 0.75) borderWidth: 1 borderColor: #inset.
	title vResizing: #shrinkWrap.
	title orientation: #vertical.
	title centering: #center.
	title addMorph: (StringMorph new contents: aString).
	self addMorphFront: title! !

!MenuMorph methodsFor: 'accessing'!
hasSubMenu: aMenuMorph
	| sub |
	self items do: [:each |
		sub := each subMenu.
		sub == nil ifFalse: [
			sub == aMenuMorph ifTrue: [^true].
			(sub hasSubMenu: aMenuMorph) ifTrue: [^true]]].
	^false! !

!MenuMorph methodsFor: 'accessing'!
isPopUp
	^popUpOwner notNil! !

!MenuMorph methodsFor: 'accessing'!
items
	^submorphs select: [:each | each isKindOf: MenuItemMorph]! !

!MenuMorph methodsFor: 'accessing'!
lastSelectionAction
	^lastSelection == nil ifTrue: [lastSelection action]! !

!MenuMorph methodsFor: 'accessing'!
lastSelectionAction: aSymbol
	lastSelection := self items detect: [:each | each action == aSymbol] ifNone: []! !

!MenuMorph methodsFor: 'accessing'!
popUpOwner
	^popUpOwner! !

!MenuMorph methodsFor: 'accessing'!
popUpOwner: aMenuItemMorph
	popUpOwner := aMenuItemMorph! !

!MenuMorph methodsFor: 'accessing'!
receiver
	^receiver! !

!MenuMorph methodsFor: 'accessing'!
receiver: anObject
	receiver := anObject! !

!MenuMorph methodsFor: 'accessing'!
selections
	^self items collect: [:each | each action]! !

!MenuMorph methodsFor: 'menu'!
addCustomMenuItems: aCustomMenu hand: aHandMorph

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu addLine.
	aCustomMenu add: 'add item...' action: #addItem.
	aCustomMenu add: 'add title...' action: #addTitle.
	aCustomMenu add: 'add line' action: #addLine.
	(self canDetachSubMenu: aHandMorph)
		ifTrue: [aCustomMenu add: 'detach submenu' action: #detachSubMenu:]! !

!MenuMorph methodsFor: 'menu'!
addItem
	| string action |
	string := FillInTheBlank request: 'Label for new item?'.
	string = '' ifTrue: [^self].
	action := FillInTheBlank request: 'Action?'.
	action := action = '' ifFalse: [action first = $[
			ifTrue: [Object readFromString: action]
			ifFalse: [action asSymbol]].
	self add: string action: action! !

!MenuMorph methodsFor: 'menu'!
addTitle
	| string |
	string := FillInTheBlank request: 'Title for menu?'.
	string = '' ifTrue: [^self].
	self addTitle: string! !

!MenuMorph methodsFor: 'menu'!
beSubMenu: evt
	| rootMorphs targetRoot targetMorph |
	rootMorphs _ evt hand world rootMorphsAt: evt hand targetOffset.
	rootMorphs size < 2 ifTrue: [^ self].
	targetRoot _ rootMorphs at: 2.
	targetMorph _ evt hand chooseTargetSubmorphOf: targetRoot.
	targetMorph ifNotNil: [
		targetMorph subMenu: self.
		self delete.
		targetMorph changed].
! !

!MenuMorph methodsFor: 'menu'!
canDetachSubMenu: hand
	| possibleTargets item |
	possibleTargets := hand argumentOrNil morphsAt: hand targetOffset.
	item := possibleTargets detect: [:each | each isKindOf: MenuItemMorph] ifNone: [].
	item == nil ifTrue: [^false].
	^item subMenu notNil! !

!MenuMorph methodsFor: 'menu'!
deleteIfPopUp
	self isPopUp ifTrue: [
		self delete.
		(popUpOwner isKindOf: MenuItemMorph) ifTrue: [
			popUpOwner owner deleteIfPopUp]].
	! !

!MenuMorph methodsFor: 'menu'!
detachSubMenu: evt
	| possibleTargets item |
	possibleTargets := evt hand argumentOrNil morphsAt: evt hand targetOffset.
	item := possibleTargets detect: [:each | each isKindOf: MenuItemMorph] ifNone: [].
	item == nil ifTrue: [^self].
	item subMenu == nil ifFalse: [
		evt hand attachMorph: item subMenu.
		item subMenu popUpOwner: nil.
		item subMenu: nil]! !

!MenuMorph methodsFor: 'menu'!
itemWasSelected: aMenuItem
	lastSelection := aMenuItem.
	receiver performMenuAction: aMenuItem action! !

!MenuMorph methodsFor: 'dropping/grabbing'!
acceptDroppingMorph: aMorph event: evt
	"Allow the user to add submorphs just by dropping them on this morph."

	| item |
	(aMorph isKindOf: MenuMorph) ifTrue: [
			item := (self morphsAt: evt cursorPoint)
						detect: [:each | each isKindOf: MenuItemMorph]
						ifNone: []].
	item == nil ifTrue: [^super acceptDroppingMorph: aMorph event: evt].
	item subMenu: aMorph.
	aMorph delete! !


!MorphicEvent reorganize!
('initialization' initialize)
('accessing' buttons hand type)
('classification' isKeystroke isMouse isMouseDown isMouseMove isMouseUp)
('equality' = hash)
('mouse' anyButtonPressed blueButtonPressed cursorPoint redButtonPressed targetPoint transformedBy: yellowButtonPressed)
('keyboard' commandKeyPressed controlKeyPressed keyCharacter keyValue optionKeyPressed shiftPressed)
('printing' printOn: storeOn:)
('private' setCursorPoint: setHand: setKeyValue:mousePoint:buttons:hand: setMousePoint:buttons:lastEvent:hand: setType: setType:cursorPoint:buttons:keyValue:)
!


!MorphicEvent methodsFor: 'equality' stamp: 'jm 9/24/97 13:05'!
= aMorphicEvent

	(aMorphicEvent isKindOf: self class) ifFalse: [^ false].
	type = aMorphicEvent type ifFalse: [^ false].
	cursorPoint = aMorphicEvent cursorPoint ifFalse: [^ false].
	buttons = aMorphicEvent buttons ifFalse: [^ false].
	keyValue = aMorphicEvent keyValue ifFalse: [^ false].
	^ true
! !

!MorphicEvent methodsFor: 'equality' stamp: 'jm 9/24/97 13:06'!
hash

	^ cursorPoint hash + buttons hash + keyValue hash
! !

!MorphicEvent methodsFor: 'printing' stamp: 'jm 9/24/97 12:23'!
storeOn: aStream

	aStream nextPutAll: type.
	aStream space.
	cursorPoint x storeOn: aStream.
	aStream space.
	cursorPoint y storeOn: aStream.
	aStream space.
	buttons storeOn: aStream.
	aStream space.
	keyValue storeOn: aStream.
! !

!MorphicEvent methodsFor: 'private' stamp: 'jm 9/24/97 12:51'!
setHand: aHandMorph
	"Set the hand that originated this event."

	sourceHand _ aHandMorph.
! !

!MorphicEvent methodsFor: 'private'!
setMousePoint: aPoint buttons: anInteger lastEvent: lastEvent hand: hand

	cursorPoint _ aPoint.
	buttons _ anInteger.
	keyValue _ 0.
	sourceHand _ hand.

	self anyButtonPressed ifTrue: [
		lastEvent anyButtonPressed
			ifTrue: [type _ #mouseMove]
			ifFalse: [type _ #mouseDown].
	] ifFalse: [
		lastEvent anyButtonPressed
			ifTrue: [type _ #mouseUp]
			ifFalse: [type _ #mouseMove]].

! !

!MorphicEvent methodsFor: 'private' stamp: 'jm 9/24/97 12:25'!
setType: t cursorPoint: p buttons: b keyValue: k

	type _ t.
	cursorPoint _ p.
	buttons _ b.
	keyValue _ k.
	sourceHand _ nil.
! !


!MorphicEvent class methodsFor: 'instance creation' stamp: 'jm 9/25/97 09:29'!
newWorldExtent: aPoint
	"Answer an event that records a WorldMorph window size change."

	^ self basicNew setType: #worldExtent
		cursorPoint: aPoint
		buttons: 0
		keyValue: 0
! !

!MorphicEvent class methodsFor: 'instance creation' stamp: 'jm 9/24/97 13:18'!
readFrom: aStream
	"Read a MorphicEvent from the given stream."

	| s type x y buttons keyValue |
	s _ WriteStream on: ''.
	[aStream peek isLetter] whileTrue: [s nextPut: aStream next].
	type _ s contents asSymbol.
	aStream skip: 1.

	x _ Integer readFrom: aStream.
	aStream skip: 1.
	y _ Integer readFrom: aStream.
	aStream skip: 1.

	buttons _ Integer readFrom: aStream.
	aStream skip: 1.

	keyValue _ Integer readFrom: aStream.

	^ self basicNew setType: type
		cursorPoint: x@y
		buttons: buttons
		keyValue: keyValue
! !


MultiuserTinyPaint comment:
'A very simple paint program that handles multiple users (hands).
Each user has their own brush size and color.
'!

!MultiuserTinyPaint reorganize!
('initialization' initialize)
('events' handlesMouseDown: mouseDown: mouseMove:)
('menu' addCustomMenuItems:hand: brushColor:hand: clear fill: setPenColor: setPenSize:)
('private' createDrawStateFor:)
!


!MultiuserTinyPaint methodsFor: 'initialization' stamp: 'jm 9/25/97 21:08'!
initialize

	super initialize.
	color _ Color veryVeryLightGray.
	drawState _ IdentityDictionary new.
	self clear.
! !

!MultiuserTinyPaint methodsFor: 'events'!
handlesMouseDown: evt

	^ true
! !

!MultiuserTinyPaint methodsFor: 'events' stamp: 'jm 9/25/97 21:25'!
mouseDown: evt

	| state |
	(drawState includesKey: evt hand) ifFalse: [self createDrawStateFor: evt hand].
	state _ drawState at: evt hand.
	state at: LastMouseIndex put: evt cursorPoint.
! !

!MultiuserTinyPaint methodsFor: 'events' stamp: 'jm 9/25/97 21:29'!
mouseMove: evt

	| state lastP p pen |
	state _ drawState at: evt hand ifAbsent: [^ self].
	lastP _ state at: LastMouseIndex.
	p _ evt cursorPoint.
	p = lastP ifTrue: [^ self].

	pen _ state at: PenIndex.
	pen drawFrom: lastP - bounds origin to: p - bounds origin.
	self invalidRect: (
		((lastP min: p) - pen sourceForm extent) corner:
		((lastP max: p) + pen sourceForm extent)).
	state at: LastMouseIndex put: p.
! !

!MultiuserTinyPaint methodsFor: 'menu' stamp: 'jm 9/25/97 21:30'!
addCustomMenuItems: aCustomMenu hand: aHandMorph

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu add: 'clear' action: #clear.
	aCustomMenu add: 'pen color' action: #setPenColor:.
	aCustomMenu add: 'pen size' action: #setPenSize:.
	aCustomMenu add: 'fill' action: #fill:.
! !

!MultiuserTinyPaint methodsFor: 'menu' stamp: 'jm 9/25/97 21:33'!
brushColor: aColor hand: hand

	| state |
	(drawState includesKey: hand) ifFalse: [self createDrawStateFor: hand].
	state _ drawState at: hand.
	(state at: PenIndex) color: aColor.
	state at: PenColorIndex put: aColor.
! !

!MultiuserTinyPaint methodsFor: 'menu' stamp: 'jm 9/25/97 21:17'!
clear

	| newPen |
	self form: ((Form extent: 400@300 depth: 8) fillColor: color).
	drawState do: [:state |
		newPen _ Pen newOnForm: originalForm.
		newPen roundNib: (state at: PenSizeIndex).
		newPen color: (state at: PenColorIndex).
		state at: PenIndex put: newPen].
! !

!MultiuserTinyPaint methodsFor: 'menu' stamp: 'jm 9/25/97 21:32'!
fill: evt

	| state fillPt |
	(drawState includesKey: evt hand) ifFalse: [self createDrawStateFor: evt hand].
	state _ drawState at: evt hand.

	Cursor blank show.
	Cursor crossHair showWhile:
		[fillPt _ Sensor waitButton - self world viewBox origin - self position].
	originalForm shapeFill: (state at: PenColorIndex) interiorPoint: fillPt.
	self changed.
! !

!MultiuserTinyPaint methodsFor: 'menu' stamp: 'jm 9/25/97 21:11'!
setPenColor: evt

	evt hand changeColorTarget: self selector: #brushColor:hand:.
! !

!MultiuserTinyPaint methodsFor: 'menu' stamp: 'jm 9/25/97 21:21'!
setPenSize: evt

	| menu sizes nibSize state |
	menu _ CustomMenu new.
	sizes _ (0 to: 5), (6 to: 12 by: 2), (15 to: 40 by: 5).
	sizes do: [:w | menu add: w printString action: w].
	nibSize _ menu startUp.
	nibSize ifNotNil: [
		(drawState includesKey: evt hand) ifFalse: [self createDrawStateFor: evt hand].
		state _ drawState at: evt hand.
		state at: PenSizeIndex put: nibSize.
		(state at: PenIndex) roundNib: nibSize].
! !

!MultiuserTinyPaint methodsFor: 'private' stamp: 'jm 9/25/97 21:35'!
createDrawStateFor: aHand

	| pen state |
	pen _ Pen newOnForm: originalForm.
	state _ Array new: 4.
	state at: PenIndex put: pen.
	state at: PenSizeIndex put: 3.
	state at: PenColorIndex put: Color red.
	state at: LastMouseIndex put: nil.
	drawState at: aHand put: state.
! !


!MultiuserTinyPaint class methodsFor: 'class initialization' stamp: 'jm 9/25/97 21:16'!
initialize
	"MultiuserTinyPaint initialize"

	"indices into the state array for a given hand"
	PenIndex _ 1.
	PenSizeIndex _ 2.
	PenColorIndex _ 3.
	LastMouseIndex _ 4.
! !


!NetNameResolver class methodsFor: 'lookups' stamp: 'jm 9/24/97 10:38'!
promptUserForHostAddressDefault: defaultName
	"Ask the user for a host name and return its address. If the default name is the empty string, use the last host name as the default."
	"NetNameResolver promptUserForHostAddressDefault: ''"

	| default hostName serverAddr |
	Socket initializeNetwork.
	defaultName isEmpty
		ifTrue: [default _ DefaultHostName]
		ifFalse: [default _ defaultName].
	hostName _ FillInTheBlank
		request: 'Host name or address?'
		initialAnswer: default.
	hostName isEmpty ifTrue: [^ 0].
	serverAddr _ NetNameResolver addressForName: hostName timeout: 15.
	serverAddr = nil ifTrue: [self error: 'Could not find the address for ', hostName].
	hostName size > 0 ifTrue: [DefaultHostName _ hostName].
	^ serverAddr
! !


!Polygon methodsFor: 'initialization' stamp: 'jm 8/2/97 14:21'!
initialize
	super initialize.
	vertices _ Array with: 20@20 with: 40@30 with: 20@40.
	color _ Color orange.
	borderWidth _ 2.
	borderColor _ Color magenta.
	closed _ true.
	quickFill _ true.
	arrows _ #none.
	self computeBounds.
! !

!Polygon methodsFor: 'initialization' stamp: 'di 9/26/97 10:05'!
installModelIn: aWorld
	aWorld isWorldMorph ifTrue: [self addHandles]! !

!Polygon methodsFor: 'initialization' stamp: 'di 9/26/97 09:03'!
vertices: verts color: c borderWidth: bw borderColor: bc
	super initialize.
	vertices _ verts.
	color _ c.
	borderWidth _ bw.
	borderColor _ bc.
	closed _ true.
	quickFill _ true.
	arrows _ #none.
	self computeBounds.
! !

!Polygon methodsFor: 'access' stamp: 'sw 9/14/97 18:22'!
vertices
	^ vertices! !

!Polygon methodsFor: 'geometry' stamp: 'tk 9/8/97 10:41'!
containsPoint: aPoint
	(super containsPoint: aPoint) ifFalse: [^ false].
	closed
	ifTrue: [filledForm colors: (Array with: Color white with: Color
black).
		^ (filledForm pixelValueAt: aPoint - bounds topLeft) = 1]
	ifFalse: [
		self lineSegmentsDo:
			[:p1 :p2 | (aPoint onLineFrom: p1 to: p2) ifTrue:
[^ true]].
		arrowForms ifNotNil: [arrowForms do:
			[:f | (f pixelValueAt: aPoint - f offset) > 0
ifTrue: [^ true]]].
		^ false]! !

!Polygon methodsFor: 'geometry' stamp: 'sw 9/14/97 18:22'!
flipHAroundX: centerX
	"Flip me horizontally around the center.  If centerX is nil, compute my center of gravity."

	| cent |
	cent _ centerX 
		ifNil: [bounds center x
			"cent _ 0.
			vertices do: [:each | cent _ cent + each x].
			cent asFloat / vertices size"]		"average is the center"
		ifNotNil: [centerX].
	self setVertices: (vertices collect: [:vv |
			(vv x - cent) * -1 + cent @ vv y]) reversed.! !

!Polygon methodsFor: 'geometry' stamp: 'sw 9/14/97 18:22'!
flipVAroundY: centerY
	"Flip me vertically around the center.  If centerY is nil, compute my center of gravity."

	| cent |
	cent _ centerY 
		ifNil: [bounds center y
			"cent _ 0.
			vertices do: [:each | cent _ cent + each y].
			cent asFloat / vertices size"]		"average is the center"
		ifNotNil: [centerY].
	self setVertices: (vertices collect: [:vv |
			vv x @ ((vv y - cent) * -1 + cent)]) reversed.! !

!Polygon methodsFor: 'geometry' stamp: 'sw 9/14/97 18:22'!
inset: amt
	"Only works if I am made of rectangles (every segment of me is horizontal or vertical).  Inset each vertex by amt.  Uses containsPoint."

	| delta four cnt offset |
	delta _ amt asPoint.
	four _ {delta.  -1@1 * delta.  -1@-1 * delta.  1@-1 * delta}.
	self setVertices: (vertices collect: [:vv | 
		cnt _ 0.
		offset _ four detectSum: [:del | 
			(self containsPoint: del+vv) ifTrue: [cnt _ cnt + 1. del] ifFalse: [0@0]].
		cnt = 2 ifTrue: [offset _ offset // 2].
		vv + offset]).! !

!Polygon methodsFor: 'geometry' stamp: 'sw 9/14/97 18:22'!
merge: aPolygon
	"Expand myself to enclose the other polygon.  (Later merge overlapping or disjoint in a smart way.)  For now, the two polygons must share at least two vertices.  Shared vertices must come one after the other in each polygon.  Polygons must not overlap."

	| shared mv vv hv xx |
	shared _ vertices select: [:mine | 
		(aPolygon vertices includes: mine)].
	shared size < 2 ifTrue: [^ nil].	"not sharing a segment"
	mv _ vertices asOrderedCollection.
	[shared includes: mv first] whileFalse: ["rotate them"
		vv _ mv removeFirst.
		mv addLast: vv].
	hv _ aPolygon vertices asOrderedCollection.
	[mv first = hv first] whileFalse: ["rotate him until same shared vertex is first"
		vv _ hv removeFirst.
		hv addLast: vv].
	[shared size > 2] whileTrue: [
		shared _ shared asOrderedCollection.
		(self mergeDropThird: mv in: hv from: shared) ifNil: [^ nil]].
		"works by side effect on the lists"
	(mv at: 2) = hv last ifTrue: [mv removeFirst; removeFirst.
		^ self setVertices: (hv, mv) asArray].
	(hv at: 2) = mv last ifTrue: [hv removeFirst; removeFirst.
		^ self setVertices: (mv, hv) asArray].
	(mv at: 2) = (hv at: 2) ifTrue: [hv removeFirst.  mv remove: (mv at: 2).
		xx _ mv removeFirst.
		^ self setVertices: (hv, (Array with: xx), mv reversed) asArray].
	mv last = hv last ifTrue: [mv removeLast.  hv removeFirst.
		^ self setVertices: (mv, hv reversed) asArray].
	^ nil
! !

!Polygon methodsFor: 'geometry' stamp: 'sw 9/14/97 18:22'!
mergeDropThird: mv in: hv from: shared
	"We are merging two polygons.  In this case, they have at least three identical shared vertices.  Make sure they are sequential in each, and drop the middle one from vertex lists mv, hv, and shared.  First vertices on lists are identical already."

	"know (mv first = hv first)"
	| mdrop vv |
	(shared includes: (mv at: mv size - 2)) 
		ifTrue: [(shared includes: (mv last)) ifTrue: [mdrop _ mv last]]
		ifFalse: [(shared includes: (mv last)) ifTrue: [
			(shared includes: (mv at: 2)) ifTrue: [mdrop _ mv first]]].
	(shared includes: (mv at: 3)) ifTrue: [
		(shared includes: (mv at: 2)) ifTrue: [mdrop _ mv at: 2]].
	mdrop ifNil: [^ nil].
	mv remove: mdrop.
	hv remove: mdrop.
	shared remove: mdrop.
	[shared includes: mv first] whileFalse: ["rotate them"
		vv _ mv removeFirst.
		mv addLast: vv].
	[mv first = hv first] whileFalse: ["rotate him until same shared vertex is first"
		vv _ hv removeFirst.
		hv addLast: vv].
! !

!Polygon methodsFor: 'geometry' stamp: 'sw 9/14/97 18:22'!
rotate: degrees around: centerPt
	"Rotate me around the center.  If center is nil, use the center of my bounds.  Rotation is clockwise on the screen."

	| cent |
	cent _ centerPt 
		ifNil: [bounds center]	"approx the center"
		ifNotNil: [centerPt].
	degrees \\ 90 = 0 ifTrue: ["make these cases exact"
		degrees \\ 360 = 90 ifTrue: ["right"
			^ self setVertices: (vertices collect: [:vv |
				(vv - cent) y * -1 @ ((vv - cent) x) + cent])].
		degrees \\ 360 = 180 ifTrue: [
			^ self setVertices: (vertices collect: [:vv |
				(vv - cent) negated + cent])].
		degrees \\ 360 = 270 ifTrue: ["left"
			^ self setVertices: (vertices collect: [:vv |
				(vv - cent) y @ ((vv - cent) x * -1) + cent])].
		degrees \\ 360 = 0 ifTrue: [^ self].
		].
	self setVertices: (vertices collect: [:vv |
			(Point r: (vv - cent) r degrees: (vv - cent) degrees + degrees) + cent]).! !

!Polygon methodsFor: 'editing' stamp: 'di 9/26/97 09:58'!
dragVertex: evt fromHandle: handle vertIndex: ix
	| p |
	p _ evt cursorPoint.
	vertices at: ix put: p.
	handle position: p + (borderWidth//2) - (handle extent//2).
	self computeBounds! !

!Polygon methodsFor: 'editing' stamp: 'di 9/26/97 09:54'!
dropVertex: evt fromHandle: handle vertIndex: ix
	| p |
	p _ vertices at: ix.
	(((vertices atWrap: ix-1) dist: p) < 3 or:
		[((vertices atWrap: ix+1) dist: p) < 3])
		ifTrue: ["Drag a vertex onto its neighbor means delete"
				self setVertices: (vertices copyReplaceFrom: ix to: ix with: Array new)]! !

!Polygon methodsFor: 'editing' stamp: 'di 9/26/97 09:17'!
newVertex: evt fromHandle: handle afterVert: ix
	"Insert a new vertex and fix everything up!!
	Install the drag-handle of the new vertex as recipient of further mouse events."
	| pt |
	pt _ evt cursorPoint.
	self setVertices: (vertices copyReplaceFrom: ix+1 to: ix with: (Array with: pt)).
	evt hand mouseDownRecipient: (handles at: ix+1*2-1)! !

!Polygon methodsFor: 'menu' stamp: 'tk 7/28/97 23:04'!
addCustomMenuItems: aCustomMenu hand: aHandMorph
	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	handles == nil ifTrue: [aCustomMenu add: 'show handles' action: #addHandles]
		ifFalse: [aCustomMenu add: 'hide handles' action: #removeHandles].
	closed ifTrue: [aCustomMenu add: 'open polygon' action: #makeOpen]
		ifFalse:
		[aCustomMenu add: 'close polygon' action: #makeClosed.
		arrows == #none ifFalse: [aCustomMenu add: '---' action: #makeNoArrows].
		arrows == #forward ifFalse: [aCustomMenu add: '-->' action: #makeForwardArrow].
		arrows == #back ifFalse: [aCustomMenu add: '<--' action: #makeBackArrow].
		arrows == #both ifFalse: [aCustomMenu add: '<-->' action: #makeBothArrows]]! !

!Polygon methodsFor: 'menu' stamp: 'di 9/26/97 09:10'!
addHandles
	| handle newVert tri |
	self removeHandles.
	handles _ OrderedCollection new.
	tri _ Array with: 0@-4 with: 4@3 with: -3@3.
	vertices withIndexDo:
		[:vertPt :vertIndex |
		handle _ EllipseMorph newBounds: (Rectangle center: vertPt + (borderWidth//2) extent: 8@8)
				color: Color yellow.
		handle on: #mouseStillDown send: #dragVertex:fromHandle:vertIndex:
				to: self withValue: vertIndex.
		handle on: #mouseUp send: #dropVertex:fromHandle:vertIndex:
				to: self withValue: vertIndex.
		self addMorph: handle.
		handles addLast: handle.
		(closed or: [vertIndex < vertices size]) ifTrue:
			["newVert _ (RectangleMorph newBounds: (Rectangle center: vertPt + (vertices atWrap: vertIndex+1) // 2 extent: 7@7)
					color: Color green) borderWidth: 1."
			newVert _ Polygon
					vertices: (tri collect: [:p | p + (vertPt + (vertices atWrap: vertIndex+1) // 2)])
					color: Color green borderWidth: 1 borderColor: Color black.
			newVert on: #mouseDown send: #newVertex:fromHandle:afterVert:
					to: self withValue: vertIndex.
			self addMorph: newVert.
			handles addLast: newVert]].
	self changed! !

!Polygon methodsFor: 'menu' stamp: 'tk 9/2/97 16:04'!
removeHandles
	"tk 9/2/97 allow it to be called twice (when nil already)"

	handles ifNotNil: [
		handles do: [:h | h delete].
		handles _ nil].! !

!Polygon methodsFor: 'private' stamp: 'di 9/26/97 09:57'!
computeBounds
	self changed.
	bounds _ self curveBounds.
	self computeFill.
	self computeArrows.
	handles ifNotNil: [self updateHandles].
	self layoutChanged.
	self changed! !

!Polygon methodsFor: 'private' stamp: 'di 9/26/97 09:42'!
updateHandles
	| newVert |
	vertices withIndexDo:
		[:vertPt :vertIndex |
		(closed or: [vertIndex < vertices size]) ifTrue:
			[newVert _ handles at: vertIndex*2.
			newVert position: (vertPt + (vertices atWrap: vertIndex+1)
								- newVert extent) // 2 + (2@0)]].! !


!Curve methodsFor: 'private' stamp: 'di 9/26/97 09:53'!
addHandles
	super addHandles.
	self updateHandles! !

!Curve methodsFor: 'private' stamp: 'di 9/26/97 10:01'!
updateHandles
	| midPts nextVertIx tweens newVert |
	midPts _ OrderedCollection new.
	nextVertIx _ 2.
	tweens _ OrderedCollection new.
	self lineSegmentsDo:
		[:p1 :p2 |
		tweens addLast: p2.
		p2 = (vertices atWrap: nextVertIx) ifTrue:
			["Found endPoint."
			midPts addLast: (tweens at: tweens size // 2)
						+ (tweens at: tweens size + 1 // 2) // 2.
			tweens _ OrderedCollection new.
			nextVertIx _ nextVertIx + 1]].
	midPts withIndexDo:
		[:midPt :vertIndex |
		(closed or: [vertIndex < vertices size]) ifTrue:
			[newVert _ handles at: vertIndex*2.
			newVert position: midPt - (newVert extent // 2) + (2@0)]].! !


!Polygon class reorganize!
('instance creation' vertices:color:borderWidth:borderColor:)
!


!Polygon class methodsFor: 'instance creation' stamp: 'di 9/26/97 09:06'!
vertices: verts color: c borderWidth: bw borderColor: bc
	^ self basicNew vertices: verts color: c borderWidth: bw borderColor: bc! !


!RemoteHandMorph reorganize!
('initialization' initialize)
('meta menu' connectRemoteUser disconnectAllRemoteUsers disconnectRemoteUser)
('connections' remoteHostAddress startListening stopListening)
('other' drawOn: processEvents withdrawFromWorld worldBounds)
('private' appendNewDataToReceiveBuffer getNextRemoteEvent receiveData)
!


!RemoteHandMorph methodsFor: 'initialization' stamp: 'jm 9/25/97 09:30'!
initialize

	super initialize.
	remoteWorldExtent _ 100@100.  "initial guess"
	socket _ nil.
	waitingForConnection _ false.
	receiveBuffer _ ''.
! !

!RemoteHandMorph methodsFor: 'meta menu' stamp: 'jm 9/26/97 09:45'!
connectRemoteUser
	"This menu command does nothing when invoked by a RemoteHandMorph."
! !

!RemoteHandMorph methodsFor: 'meta menu' stamp: 'jm 9/26/97 10:16'!
disconnectAllRemoteUsers
	"This menu command does nothing when invoked by a RemoteHandMorph."
! !

!RemoteHandMorph methodsFor: 'meta menu' stamp: 'jm 9/26/97 09:45'!
disconnectRemoteUser
	"This menu command does nothing when invoked by a RemoteHandMorph."
! !

!RemoteHandMorph methodsFor: 'connections' stamp: 'jm 9/26/97 11:31'!
remoteHostAddress
	"Return the address of the remote host or zero if not connected."

	(socket ~~ nil and: [socket isUnconnectedOrInvalid not])
		ifTrue: [^ socket remoteAddress]
		ifFalse: [^ 0].
! !

!RemoteHandMorph methodsFor: 'connections' stamp: 'jm 9/26/97 11:03'!
startListening
	"Create a socket and start listening for a connection."

	self stopListening.
	Transcript show: 'My address is ', NetNameResolver localAddressString; cr.
	Transcript show: 'Remote hand ', userInitials, ' waiting for a connection...'; cr.
	socket _ Socket new.
	socket listenOn: 54323.
	waitingForConnection _ true.
! !

!RemoteHandMorph methodsFor: 'other' stamp: 'jm 9/26/97 10:22'!
drawOn: aCanvas
	"For remote cursors, always draw the hand itself (i.e., the cursor)."

	super drawOn: aCanvas.
	aCanvas image: NormalCursor at: self position.
! !

!RemoteHandMorph methodsFor: 'other' stamp: 'jm 9/25/97 09:31'!
processEvents
	"Process user input events from the remote input devices."

	| evt |
	evt _ self getNextRemoteEvent.
	[evt ~~ nil] whileTrue: [
		evt type == #worldExtent ifTrue: [
			remoteWorldExtent _ evt cursorPoint.
			^ self].

		(evt yellowButtonPressed and:
		 [lastEvent yellowButtonPressed not]) ifTrue: [
			lastEvent _ evt.
			^ self invokeMetaMenu: evt].

		self handleEvent: evt.
		lastEvent _ evt.
		evt _ self getNextRemoteEvent].
! !

!RemoteHandMorph methodsFor: 'other' stamp: 'jm 9/26/97 09:42'!
withdrawFromWorld
	"Close the socket, if any, and remove this hand from the world."

	self stopListening.
	Transcript show: 'Remote hand ', userInitials, ' closed'; cr.
	owner ifNotNil: [owner removeHand: self].
! !

!RemoteHandMorph methodsFor: 'other' stamp: 'jm 9/25/97 09:33'!
worldBounds

	^ 0@0 extent: remoteWorldExtent
! !

!RemoteHandMorph methodsFor: 'private' stamp: 'jm 9/24/97 11:58'!
appendNewDataToReceiveBuffer
	"Append all available raw data to my receive buffer. Assume that my socket is not nil."

	| newData tempBuf bytesRead |
	socket dataAvailable ifTrue: [
		"get all the data currently available"
		newData _ WriteStream on: (String new: receiveBuffer size + 1000).
		newData nextPutAll: receiveBuffer.
		tempBuf _ String new: 1000.
		[socket dataAvailable] whileTrue: [
			bytesRead _ socket receiveDataInto: tempBuf.
			1 to: bytesRead do: [:i | newData nextPut: (tempBuf at: i)]].
		receiveBuffer _ newData contents].
! !

!RemoteHandMorph methodsFor: 'private' stamp: 'jm 9/26/97 10:23'!
getNextRemoteEvent
	"Return the next remote event, or nil if the receive buffer does not contain a full event record. An event record is the storeString for a MorphicEvent terminated by a <CR> character."

	| i s |
	self receiveData.
	receiveBuffer isEmpty ifTrue: [^ nil].

	i _ receiveBuffer indexOf: Character cr ifAbsent: [^ nil].
	s _ receiveBuffer copyFrom: 1 to: i - 1.
	receiveBuffer _ receiveBuffer copyFrom: i + 1 to: receiveBuffer size.
	^ (MorphicEvent readFromString: s) setHand: self
! !

!RemoteHandMorph methodsFor: 'private' stamp: 'jm 9/26/97 09:40'!
receiveData
	"Check my connection status and withdraw from the world if the connection has been broken. Append any data that has arrived to receiveBuffer. "

	socket ifNotNil: [
		socket isConnected
			ifTrue: [  "connected"
				waitingForConnection ifTrue: [
					Transcript show: 'Remote hand ', userInitials, ' connected'; cr.
					waitingForConnection _ false].
				self appendNewDataToReceiveBuffer]
			ifFalse: [  "not connected"
				waitingForConnection ifFalse: [
					"connection was established, then broken"
					self withdrawFromWorld.
					receiveBuffer _ '']]].
! !


!Socket methodsFor: 'queries' stamp: 'jm 9/26/97 11:31'!
isUnconnectedOrInvalid
	"Return true if this socket is completely disconnected or is invalid."

	| status |
	socketHandle ifNil: [^ true].
	status _ self primSocketConnectionStatus: socketHandle.
	^ (status = Unconnected) | (status = InvalidSocket)
! !

!Socket methodsFor: 'queries' stamp: 'jm 9/26/97 08:41'!
isValid
	"Return true if this socket contains a valid, non-nil socket handle."

	| status |
	socketHandle ifNil: [^ false].
	status _ self primSocketConnectionStatus: socketHandle.
	^ status ~= InvalidSocket
! !

!Socket methodsFor: 'connection open/close' stamp: 'jm 9/25/97 22:01'!
closeAndDestroy
	"First, try to close this connection gracefully. If the close attempt fails or times out, abort the connection. In either case, destroy the socket. Do nothing if the socket has already been destroyed (i.e., if its socketHandle is nil)."

	self closeAndDestroy: 20.

! !

!Socket methodsFor: 'connection open/close' stamp: 'jm 9/25/97 22:01'!
closeAndDestroy: timeoutSeconds
	"First, try to close this connection gracefully. If the close attempt fails or times out, abort the connection. In either case, destroy the socket. Do nothing if the socket has already been destroyed (i.e., if its socketHandle is nil)."

	socketHandle = nil
		ifFalse: [
			self close.  "close this end"
			(self waitForDisconnectionUntil: (Socket deadlineSecs: timeoutSeconds))
				ifFalse: [
					"if the other end doesn't close soon, just abort the connection"
					self primSocketAbortConnection: socketHandle].
			self destroy].
! !


!Socket class methodsFor: 'network initialization' stamp: 'jm 9/25/97 21:58'!
ensureNetworkConnected
	"Try to ensure that an intermittent network connection, such as a dialup or ISDN line, is actually connected. This is necessary to make sure a server is visible in order to accept an incoming connection."
	"Socket ensureNetworkConnected"

	NetNameResolver initializeNetwork.
	Utilities
		informUser: 'Contacting domain name server...'
		during: [
			NetNameResolver
				addressForName: 'bogusNameToForceDNSToBeConsulted.org'
				timeout: 30].
! !

!Socket class methodsFor: 'tests' stamp: 'jm 9/26/97 11:31'!
loopbackTest
	"Send data from one socket to another on the local machine. Tests most of the socket primitives."
	"Socket loopbackTest"

	| sock1 sock2 bytesToSend sendBuf receiveBuf done bytesSent bytesReceived t extraBytes |
	Transcript cr; show: 'starting client/server test'; cr.

	Transcript show: '---------- Connecting ----------'; cr.
	Socket initializeNetwork.
	sock1 _ Socket new.
	sock2 _ Socket new.
	sock1 listenOn: 54321.
	sock2 connectTo: (NetNameResolver localHostAddress) port: 54321.
	sock1 waitForConnectionUntil: self standardDeadline.
	sock2 waitForConnectionUntil: self standardDeadline.
	(sock1 isConnected) ifFalse: [self error: 'sock1 not connected'].
	(sock2 isConnected) ifFalse: [self error: 'sock2 not connected'].
	Transcript show: 'connection established'; cr.

	bytesToSend _ 5000000.
	sendBuf _ String new: 4000 withAll: $x.
	receiveBuf _ String new: 50000.
	done _ false.
	bytesSent _ bytesReceived _ 0.
	t _ Time millisecondsToRun: [
		[done] whileFalse: [
			(sock1 sendDone and: [bytesSent < bytesToSend]) ifTrue: [
				bytesSent _ bytesSent + (sock1 sendSomeData: sendBuf)].
			sock2 dataAvailable ifTrue: [
				bytesReceived _ bytesReceived +
					(sock2 receiveDataInto: receiveBuf)].
			done _ (bytesSent >= bytesToSend) and: [bytesReceived = bytesSent]]].

	Transcript show: 'closing connection'; cr.
	sock1 waitForSendDoneUntil: self standardDeadline.
	sock1 close.
	sock2 waitForDisconnectionUntil: self standardDeadline.
	extraBytes _ sock2 discardReceivedData.
	extraBytes > 0 ifTrue: [
		Transcript show: ' *** received ', extraBytes size printString, ' extra bytes ***'; cr.
	].
	sock2 close.
	sock1 waitForDisconnectionUntil: self standardDeadline.
	(sock1 isUnconnectedOrInvalid) ifFalse: [self error: 'sock1 not closed'].
	(sock2 isUnconnectedOrInvalid) ifFalse: [self error: 'sock2 not closed'].
	Transcript show: '---------- Connection Closed ----------'; cr.

	sock1 destroy.
	sock2 destroy.
	Transcript show: 'client/server test done; time = ', t printString; cr.
	Transcript show: ((bytesToSend asFloat / t) roundTo: 0.01) printString, ' kBytes/sec'; cr.
	Transcript endEntry.
! !


!Symbol methodsFor: 'menus'!
dispatchAsMenuActionTo: receiver
	^receiver perform: self! !

!Symbol methodsFor: 'menus'!
dispatchAsMenuActionTo: receiver with: argument
	^self numArgs = 0
		ifTrue: [receiver perform: self]
		ifFalse: [receiver perform: self with: argument]! !


!TextMorph methodsFor: 'drawing' stamp: 'di 9/25/97 10:57'!
drawOn: aCanvas
	| offset |
	hasFocus ifTrue: [aCanvas fillRectangle: self bounds color: Color white].
	editor ifNotNil:
		[offset _ self world viewBox topLeft.
		editor selectionRects do:
			[:rect | aCanvas fillRectangle: (rect translateBy: offset negated) color: self selectionColor]].
	aCanvas paragraph: paragraph bounds: bounds color: color.! !

!TextMorph methodsFor: 'editing' stamp: 'di 9/25/97 12:02'!
handlesMouseDown: evt

	^ self uncoveredAt: evt cursorPoint! !

!TextMorph methodsFor: 'editing' stamp: 'di 9/25/97 08:34'!
keyStroke: evt
	"Handle a keystroke event."
	editor sensor: (KeyboardBuffer new startingEvent: evt).
	editor readKeyboard.
	paragraph composeAll.
	self fit.
	self updateAnchors.
	self changed.
! !

!TextMorph methodsFor: 'editing' stamp: 'di 9/25/97 14:29'!
mouseDown: evt
	"Make this TextMorph be the keyboard input focus, if it isn't already, and repond to the text selection gesture."

	editor ifNil: [self installEditor].
	hasFocus ifFalse: [evt hand newKeyboardFocus: self].
	editor mouseDown: evt.
	self changed.
! !

!TextMorph methodsFor: 'editing' stamp: 'di 9/25/97 14:30'!
mouseMove: evt
	evt redButtonPressed
		ifTrue: [editor mouseMove: evt.
				self changed].
	! !

!TextMorph methodsFor: 'editing' stamp: 'di 9/25/97 14:30'!
mouseUp: evt
	editor mouseUp: evt.
	self changed! !


!TextMorphEditor methodsFor: 'all' stamp: 'di 9/25/97 09:57'!
cursorPointFrom: evt
	^ evt cursorPoint + morph world viewBox topLeft! !

!TextMorphEditor methodsFor: 'all' stamp: 'di 9/25/97 14:32'!
mouseDown: evt 
	"An attempt to break up the old processRedButton code into threee phases"
	| clickPoint |
	oldInterval _ startBlock stringIndex to: stopBlock stringIndex - 1.
	clickPoint _ self cursorPointFrom: evt.
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

!TextMorphEditor methodsFor: 'all' stamp: 'di 9/25/97 11:30'!
mouseMove: evt 
	"An attempt to break up the old processRedButton code into threee phases"
	| dragBlock |
	dragBlock _ paragraph characterBlockAtPoint: (self cursorPointFrom: evt).
	dragBlock > pivotBlock
		ifTrue: [stopBlock _ dragBlock.  startBlock _ pivotBlock]
		ifFalse: [startBlock _ dragBlock.  stopBlock _ pivotBlock]! !

!TextMorphEditor methodsFor: 'all' stamp: 'di 9/25/97 11:21'!
mouseUp: evt
	"An attempt to break up the old processRedButton code into threee phases"
	(startBlock = stopBlock 
		and: [oldInterval = (startBlock stringIndex to: startBlock stringIndex-1)])
		ifTrue: [self selectWord].
	self setEmphasisHere.
	(self isDisjointFrom: oldInterval) ifTrue:
		[otherInterval _ oldInterval]! !

!TextMorphEditor methodsFor: 'all' stamp: 'di 9/25/97 11:23'!
selectionRects
	"Return an array of rectangles comprising the selection"
	^ paragraph selectionRectsFrom: startBlock to: stopBlock! !


!TinyPaint methodsFor: 'initialization' stamp: 'jm 9/25/97 17:01'!
initialize

	super initialize.
	color _ Color veryVeryLightGray.
	brushColor _ Color red.
	brushSize _ 3.
	self clear.
! !

!TinyPaint methodsFor: 'events'!
handlesMouseDown: evt

	^ true
! !

!TinyPaint methodsFor: 'events'!
mouseDown: evt

	lastMouse _ evt cursorPoint.
! !

!TinyPaint methodsFor: 'events'!
mouseMove: evt

	| p |
	p _ evt cursorPoint.
	p = lastMouse ifTrue: [^ self].
	brush drawFrom: lastMouse - bounds origin to: p - bounds origin.
	self invalidRect: (
		((lastMouse min: p) - brush sourceForm extent) corner:
		((lastMouse max: p) + brush sourceForm extent)).
	lastMouse _ p.
! !

!TinyPaint methodsFor: 'menu'!
addCustomMenuItems: aCustomMenu hand: aHandMorph

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu add: 'clear' action: #clear.
	aCustomMenu add: 'pen color' action: #setPenColor:.
	aCustomMenu add: 'pen size' action: #setPenSize.
	aCustomMenu add: 'fill' action: #fill.
! !

!TinyPaint methodsFor: 'menu' stamp: 'jm 9/25/97 16:53'!
brushColor: aColor

	brushColor _ aColor.
	brush color: aColor.
! !

!TinyPaint methodsFor: 'menu' stamp: 'jm 9/25/97 17:02'!
clear

	self form: ((Form extent: 400@300 depth: 8) fillColor: color).
	brush _ Pen newOnForm: originalForm.
	brush roundNib: brushSize.
	brush color: brushColor.
! !

!TinyPaint methodsFor: 'menu'!
fill

	| fillPt |
	Cursor blank show.
	Cursor crossHair showWhile:
		[fillPt _ Sensor waitButton - self world viewBox origin - self position].
	originalForm shapeFill: brushColor interiorPoint: fillPt.
	self changed.
! !

!TinyPaint methodsFor: 'menu' stamp: 'jm 9/25/97 16:55'!
setPenColor: evt

	evt hand changeColorTarget: self selector: #brushColor:.
! !

!TinyPaint methodsFor: 'menu' stamp: 'jm 9/25/97 17:01'!
setPenSize

	| menu sizes nibSize |
	menu _ CustomMenu new.
	sizes _ (0 to: 5), (6 to: 12 by: 2), (15 to: 40 by: 5).
	sizes do: [:w | menu add: w printString action: w].
	nibSize _ menu startUp.
	nibSize ifNotNil: [
		brushSize _ nibSize.
		brush roundNib: nibSize].
! !


!WorldMorph methodsFor: 'hands' stamp: 'jm 9/26/97 10:32'!
removeHand: aHandMorph
	"Remove the given hand from the list of hands for this world."

	(hands includes: aHandMorph) ifTrue: [
		aHandMorph dropMorphsEvent: MorphicEvent new.
		hands _ hands copyWithout: aHandMorph].
! !

!WorldMorph methodsFor: 'interaction loop' stamp: 'jm 9/25/97 09:46'!
doOneCycleInBackground
	"Do one cycle of the interactive loop. This method is called repeatedly when this world is not the active window but is running in the background."

	self runStepMethods.
	self processEventsInBackground.
	self displayWorld.
! !

!WorldMorph methodsFor: 'interaction loop' stamp: 'jm 9/25/97 09:45'!
processEventsInBackground
	"Process user input events, but only for remote hands. Used when this world is not the active window, but is running in background."

	hands do:
		[:h | (h isKindOf: RemoteHandMorph) ifTrue: [h processEvents]].
! !

!WorldMorph methodsFor: 'interaction loop' stamp: 'jm 9/25/97 12:04'!
startBackgroundProcess
	"Start a process to update this world in the background. Return the process created."

	| p |
	p _ [[true] whileTrue: [
		self doOneCycleInBackground.
		(Delay forMilliseconds: 20) wait]] newProcess.
	p resume.
	^ p
! !


ColorPickerMorph initialize!
MenuItemMorph initialize!
HandMorph removeSelector: #gridPoint!
HandMorph removeSelector: #stopListening!
MultiuserTinyPaint initialize!
ParagraphEditor removeSelector: #selectionRects!
Polygon removeSelector: #chooseNewVertices:!
RemoteHandMorph removeSelector: #updateFromSocket!
RemoteHandMorph removeSelector: #readDataFromSocket:!
Socket removeSelector: #isUnconnected!
TextMorphEditor removeSelector: #processRedButton!
View removeSelector: #grid:!
WorldMorph removeSelector: #addRemoteHandInitials:!
