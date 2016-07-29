'From Squeak 1.22 of September 21, 1997 on 30 September 1997 at 3:13:51 pm'!
StringMorph subclass: #MenuItemMorph
	instanceVariableNames: 'isInTransition isEnabled subMenu isSelected target selector arguments '
	classVariableNames: 'SubMenuMarker '
	poolDictionaries: ''
	category: 'Morphic-Menus'!
LayoutMorph subclass: #MenuMorph
	instanceVariableNames: 'defaultTarget lastSelection stayUp originalEvent popUpOwner '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!
MorphicModel subclass: #MorphicModel2
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Models'!
MorphicModel subclass: #MorphicModel3
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Models'!
SketchMorph subclass: #ColorPickerMorph
	instanceVariableNames: 'selectedColor sourceHand deleteOnMouseUp updateContinuously selector target '
	classVariableNames: 'ColorChart FeedbackBox TransparentBox '
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!BackgroundMorph methodsFor: 'all' stamp: 'jm 9/28/97 14:44'!
addCustomMenuItems: aCustomMenu hand: aHandMorph
	running
		ifTrue: [aCustomMenu add: 'stop' action: #stopRunning]
		ifFalse: [aCustomMenu add: 'start' action: #startRunning].
! !


!BorderedMorph methodsFor: 'menu' stamp: 'jm 9/28/97 14:36'!
addCustomMenuItems: aCustomMenu hand: aHandMorph
	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu add: 'border color...' action: #changeBorderColor:.
	self doesBevels ifTrue:
		[borderColor == #raised ifFalse: [aCustomMenu add: 'raised bevel' action: #borderRaised].
		borderColor == #inset ifFalse: [aCustomMenu add: 'inset bevel' action: #borderInset]].
	aCustomMenu add: 'border width...' action: #changeBorderWidth:.
! !

!BorderedMorph methodsFor: 'menu' stamp: 'jm 9/29/97 10:33'!
changeBorderWidth: evt

	| menu |
	menu _ MenuMorph new.
	menu addStayUpItem.
	0 to: 5 do: [:w |
		menu add: w printString
			target: self
			selector: #borderWidth:
			argument: w].
	menu popUpAt: evt hand position event: evt.
! !


!BookMorph methodsFor: 'menu' stamp: 'jm 9/28/97 14:36'!
addCustomMenuItems: aCustomMenu hand: aHandMorph

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu add: 'previous page' action: #previousPage.
	aCustomMenu add: 'next page' action: #nextPage.
	aCustomMenu add: 'insert a page' action: #insertPage.
	aCustomMenu add: 'delete this page' action: #deletePage.
	aCustomMenu add: 'page controls' action: #pageControls:.
	saveBlock ifNotNil: [aCustomMenu add: 'save' action: #save].
! !


!CodeBrowser methodsFor: 'system pane' stamp: 'jm 9/28/97 19:19'!
systemPaneMenuButtonPressed: event

	| menu |
	menu _ MenuMorph new defaultTarget: self.
	menu addTitle: 'system category'.
	"**these two belong in class pane**"
	menu add: 'select class...' action: #selectClass.
	menu add: 'select recent...' action: #selectRecentClass.
	menu add: 'browse all classes' action: #browseAllClasses.
	menu add: 'spawn selection' action: #spawnSystemCategory.
	menu addLine.
	menu add: 'fileOut' action: #fileOutSystemCategory.
	menu addLine.
	menu add: 'reorganize' action: #editSystemOrganization.
	menu add: 'add selection...' action: #addSystemCategory.
	menu add: 'rename selection...' action: #renameSystemCategory.
	menu add: 'remove selection' action: #removeSystemCategory.
	event hand invokeMenu: menu event: event.
! !


!GraphMorph methodsFor: 'script support' stamp: 'jm 9/28/97 14:35'!
addCustomMenuItems: aCustomMenu hand: aHandMorph

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu add: 'read file' action: #readDataFromFile.
! !


!HandMorph methodsFor: 'meta menu' stamp: 'jm 9/29/97 10:33'!
buildMorphHandleMenuFor: argMorph
	"Build the morph menu for the given morph's menu handle. This menu has two sections. The first section contains commands that are interpreted by the hand; the second contains commands provided by the target morph."

	| menu |
	argument _ argMorph.
	menu _ MenuMorph new defaultTarget: self.
	menu addStayUpItem.

	menu add: 'delete' action: #dismissMorph.
	menu add: 'go behind' action: #goBehind.
	(argMorph isKindOf: SketchMorph) ifFalse: [
		menu add: 'fill color' action: #changeColor].
	menu addLine.
	menu add: 'inspect' action: #inspectMorph.
	menu add: 'browse' action: #browseMorphClass.
	menu add: 'make own subclass' action: #subclassMorph.
	menu addLine.
	menu add: 'sensitize' action: #sensitizeMorph.
	menu add: 'name me' action: #nameMorph.
	menu add: 'save morph in file' action: #saveMorphInFile.
	menu addLine.
	menu defaultTarget: argMorph.
	argMorph addCustomMenuItems: menu hand: self.
	^ menu
! !

!HandMorph methodsFor: 'meta menu' stamp: 'jm 9/29/97 10:33'!
buildMorphMenuFor: argMorph
	"Build the morph menu. This menu has two sections. The first section contains commands that are interpreted by the hand; the second contains commands provided by the target morph. The variable lastMetaMenuItem determines the boundary between the sections."
	| menu |
	argument _ argMorph.
	menu _ MenuMorph new defaultTarget: self.
	menu addStayUpItem.

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
		menu add: 'submorphs...'
			target: self
			selector: #selectSubmorphToOperateOn:sending:event:
			argumentList: (Array with: argMorph with: #operateOnSubmorph:event:)].
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
	lastMetaMenuItem _ menu items size.
	menu defaultTarget: argMorph.
	argMorph addCustomMenuItems: menu hand: self.
	^ menu
! !

!HandMorph methodsFor: 'meta menu' stamp: 'jm 9/29/97 08:40'!
buildWorldMenu
	"Build the meta menu for the world."
	| menu |
	menu _ MenuMorph new defaultTarget: self.
	menu addStayUpItem.
	Project current isTopProject ifFalse:
		[menu add: 'exit this world' action: #exitWorld.
		menu addLine].
	menu add: 'new morph' action: #newMorph.
	menu add: 'new drawing' action: #makeNewDrawing.
	menu add: 'read morph(s) from file' action: #readMorphFile.
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
	^ menu
! !

!HandMorph methodsFor: 'meta menu' stamp: 'jm 9/27/97 06:59'!
changeBackgroundColor

	| colorPicker |
	colorPicker _ self changeColorTarget: self world selector: #color:.
	colorPicker updateContinuously: false.
! !

!HandMorph methodsFor: 'meta menu' stamp: 'jm 9/27/97 06:59'!
changeColorTarget: aMorph selector: aSymbol

	| m points b |
	m _ ColorPickerMorph new
		sourceHand: self;
		target: aMorph;
		selector: aSymbol.
	points _ #(topCenter rightCenter bottomCenter leftCenter).  "possible anchors"
	1 to: 4 do: [:i |  "Try the four obvious anchor points"
		b _ m bounds
				align: (m bounds perform: (points at: i))
				with: (aMorph bounds perform: (points atWrap: i + 2)).
		(self worldBounds containsRect: b) ifTrue: [  "Yes, it fits"
			m position: b topLeft.
			self world addMorphFront: m.
			m changed.
			^ self]].

	"when all else fails..."
	m position: 20@20.
	self world addMorphFront: m.
	m changed.
	^ m
! !

!HandMorph methodsFor: 'meta menu' stamp: 'jm 9/29/97 07:50'!
invokeMenu: aMenu event: evt
	"Invoke the given menu."

	aMenu popUpAt: evt cursorPoint event: evt.
! !

!HandMorph methodsFor: 'meta menu' stamp: 'jm 9/28/97 19:25'!
invokeMetaMenu: evt
	"Invoke the meta menu. If the hand is over the background, the world menu is presented. If it is over a morph, a menu of operations for that morph is presented. Each menu entry contains a string to be presented in the menu and a selector. If the selector takes an argument, the mouse-down event that invoked the menu is passed as an argument. This lets the command know which hand invoked it in  order to do things like attaching the result of the command to that hand."
	"Shortcut: If the shift key is pressed, the user is given a chance to select a submorph on which to operate."

	| menu |
	"if carrying morphs, just drop them"
	self hasSubmorphs ifTrue: [^ self dropMorphsEvent: evt].

	targetOffset _ self position.
	argument _ self argumentOrNil.
	argument == nil
		ifTrue: [
			menu _ self buildWorldMenu.
			menu addTitle: 'World']
		ifFalse: [
			menu _ self buildMorphMenuFor: argument.
			menu addTitle: argument class name].
	self invokeMenu: menu event: evt.
! !

!HandMorph methodsFor: 'meta menu' stamp: 'jm 9/28/97 19:36'!
makeNewDrawing

	| rect m aPaintWindow |
	rect _ self world paintArea.	"Let it tell us"
	m _ self drawingClass new form: (Form extent: rect extent depth: self world canvas depth).
	m bounds: rect.
	aPaintWindow _ SketchEditorMorph new initializeFor: m inWorld: self world.
	aPaintWindow afterNewPicDo: [:aForm :aRect |
		owner fullRepaintNeeded.
		m form: aForm.
		m position: aRect origin.
		"m rotationDegrees: 0.		is default of form:"
		self world addMorphFront: m].
	self world addMorphFront: aPaintWindow.
	Cursor normal showWhile: [
		aPaintWindow deliverPainting: 
			(aPaintWindow getPaintingStartingWith: nil at: nil)].
! !

!HandMorph methodsFor: 'meta menu' stamp: 'jm 9/29/97 10:34'!
newMorph

	| morphClassList menu categories subMenu |
	menu _ MenuMorph new.
	menu addStayUpItem.
	menu addTitle: 'Select Morph Class'.
	morphClassList _ Morph withAllSubclasses asSortedCollection:
		[:m1 :m2 | m1 class name < m2 class name].
	morphClassList remove: WorldMorph;
		remove: HandMorph;
		remove: MorphicModel;
		remove: RemoteHandMorph.
	morphClassList _ morphClassList select:
		[:c | (c inheritsFrom: MorphicModel) not or:
			  ["Only include Models that have been saved"
			   c includesSelector: #initMorph]].
	categories _ (morphClassList collect: [:each | each category]) asSet asSortedCollection.
	categories do: [:cat |
		subMenu _ MenuMorph new.
		morphClassList do: [:each |
			each category = cat ifTrue: [
				subMenu add: each name
					target: self
					selector: #newMorphOfClass:
					argument: each]].
		menu add: cat subMenu: subMenu].

	menu popUpAt: self position forHand: self.
! !

!HandMorph methodsFor: 'meta menu' stamp: 'jm 9/28/97 19:30'!
operateOnSubmorph: aMorph event: evt
	"Invoke the morph menu for the given submorph."

	| menu |
	menu _ self buildMorphMenuFor: aMorph.
	menu addTitle: aMorph class name.
	self invokeMenu: menu event: evt.
! !

!HandMorph methodsFor: 'meta menu' stamp: 'jm 9/29/97 07:49'!
selectSubmorphToOperateOn: rootMorph sending: aSymbol event: evt
	"Let the user select a submorph of the given root morph. When selected, the given selector is sent with the selected submorph as an argument."

	| possibleTargets menu |
	possibleTargets _ rootMorph morphsAt: targetOffset.
	possibleTargets size = 1 ifTrue: [^ self perform: aSymbol with: possibleTargets first].
	menu _ MenuMorph new.
	possibleTargets do: [:m |
		menu add: (self submorphNameFor: m)
			target: self
			selector: aSymbol
			argumentList: (Array with: m with: evt)].
	menu popUpAt: self position event: evt.
! !

!HandMorph methodsFor: 'special gestures' stamp: 'jm 9/28/97 19:14'!
specialGesture: evt
	"Blue mouse button (cmd-mouse on the Macintosh) gestures that allow a morph to be grabbed or duplicated without recourse to the meta menu."
	"Summary:
		Cmd-mouse			grab morph (for picking up buttons, etc.)
		Cmd-shift-mouse		duplicate morph"

	"if carrying morphs, just drop them"
	self hasSubmorphs ifTrue: [^ self dropMorphsEvent: evt].

	targetOffset _ self position.
	(argument _ self argumentOrNil) ifNil: [^ self].
	evt shiftPressed
		ifTrue: [self duplicateMorph]
		ifFalse: [self grabMorph].
! !


!ImageMorph methodsFor: 'all' stamp: 'jm 9/28/97 17:21'!
addCustomMenuItems: aCustomMenu hand: aHandMorph
	aCustomMenu add: 'read from file' action: #readFromFile.
	aCustomMenu add: 'grab from screen' action: #grabFromScreen.
! !


!MenuItemMorph reorganize!
('initialization' initialize)
('accessing' arguments arguments: isEnabled isEnabled: selector selector: subMenu subMenu: target target:)
('drawing' drawOn:)
('events' handlesMouseDown: mouseDown: mouseMove: mouseUp:)
('layout' hResizing isLayoutMorph layoutInWidth:height: minHeight minWidth vResizing)
('private' deletePopupBackToCommonMenuWith: deselectForNewMorph: deselectItem hideSubmenu isInMenu isSelected: selectFromHand:)
!


!MenuItemMorph methodsFor: 'initialization' stamp: 'jm 9/29/97 11:01'!
initialize

	super initialize.
	bounds _ 0@0 extent: 10@10.
	color _ Color black.
	font _ nil.
	contents _ ''.
	hasFocus _ false.
	isEnabled _ true.
	subMenu _ nil.
	isSelected _ false.
	target _ nil.
	selector _ nil.
	arguments _ nil.
! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jm 9/28/97 13:07'!
arguments

	^ arguments
! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jm 9/28/97 13:08'!
arguments: aCollection

	arguments _ aCollection.
! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jm 9/28/97 12:40'!
isEnabled

	^ isEnabled
! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jm 9/28/97 12:40'!
isEnabled: aBoolean

	isEnabled = aBoolean ifTrue: [^ self].
	isEnabled _ aBoolean.
	self color: (aBoolean ifTrue: [Color black] ifFalse: [Color gray]).
! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jm 9/28/97 12:51'!
selector

	^ selector
! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jm 9/28/97 12:51'!
selector: aSymbol

	selector _ aSymbol.
! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jm 9/28/97 12:24'!
subMenu

	^ subMenu
! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jm 9/28/97 12:24'!
subMenu: aMenuMorph

	subMenu _ aMenuMorph.
	self changed.
! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jm 9/28/97 13:18'!
target

	^ target! !

!MenuItemMorph methodsFor: 'accessing' stamp: 'jm 9/28/97 13:18'!
target: anObject

	target _ anObject.
! !

!MenuItemMorph methodsFor: 'drawing' stamp: 'jm 9/29/97 11:01'!
drawOn: aCanvas

	(isSelected & isEnabled) ifTrue: [
		aCanvas fillRectangle: self bounds color: owner color darker].
	super drawOn: aCanvas.
	subMenu == nil ifFalse: [
		aCanvas
			image: SubMenuMarker
			at: (self right - 8 @ ((self top + self bottom - SubMenuMarker height) // 2))].
! !

!MenuItemMorph methodsFor: 'events' stamp: 'jm 9/29/97 15:10'!
mouseDown: evt
	"Handle a mouse down event. Menu items get activated when the mouse is over them."

	self isInMenu ifFalse: [^ super mouseDown: evt].
	evt shiftPressed ifTrue: [^ super mouseDown: evt].  "enable label editing" 
	self selectFromHand: evt hand.
! !

!MenuItemMorph methodsFor: 'events' stamp: 'jm 9/29/97 17:36'!
mouseMove: evt

	| m |
	m _ evt hand recipientForMouseDown: evt.
	m == self
		ifTrue: [isSelected ifFalse: [self isSelected: true]]
		ifFalse: [
			self deselectForNewMorph: m.
			((m isKindOf: MenuItemMorph) and: [m isInMenu]) ifTrue: [
				m selectFromHand: evt hand]].
"xxx
	m == self ifFalse: [
		((m isKindOf: MenuItemMorph) and: [m isInMenu]) ifTrue: [
			m owner == subMenu
				ifFalse: [self isSelected: false hand: evt hand].
			m isSelected: true hand: evt hand.
			evt hand newMouseFocus: m]].
xxx"
"xxx
	m == self ifTrue: [^ self].
	((m isKindOf: MenuItemMorph) and: [m isInMenu]) ifTrue: [
		m isSelected: true hand: evt hand.
		menu _ m owner.
		(menu == self owner or: [
		 menu == subMenu or: [
		 menu hasSubMenu: owner]]) ifTrue: [
			menu == subMenu ifFalse: [self hideSubmenu].
			(menu == self owner or: [m subMenu == owner or: [menu == subMenu]])
				ifFalse: [owner delete].
			evt hand newMouseFocus: m]].
xxx"
! !

!MenuItemMorph methodsFor: 'events' stamp: 'jm 9/29/97 13:06'!
mouseUp: evt
	"Handle a mouse up event. Menu items get activated when the mouse is over them."

	| mouseInMe w |
	self deselectItem.
	mouseInMe _ self bounds containsPoint: evt cursorPoint.
	self isInMenu ifTrue: [
		(mouseInMe and: [self selector = #toggleStayUp:]) ifFalse: [
			w _ owner world.
			owner deleteIfPopUp].
		subMenu ifNil: [
			mouseInMe ifTrue: [
				w ifNotNil: [w displayWorld].
				owner invokeItem: self]]].
! !

!MenuItemMorph methodsFor: 'layout' stamp: 'jm 9/28/97 12:34'!
hResizing

	^ #spaceFill
! !

!MenuItemMorph methodsFor: 'layout' stamp: 'jm 9/28/97 12:34'!
isLayoutMorph

	^ true
! !

!MenuItemMorph methodsFor: 'layout' stamp: 'jm 9/28/97 12:34'!
layoutInWidth: w height: h

	| scanner |
	scanner _ QuickPrint newOn: Display box: Display boundingBox font: font.
	self extent: ((scanner stringWidth: contents) @ (scanner lineHeight) max: w@h).
! !

!MenuItemMorph methodsFor: 'layout' stamp: 'jm 9/28/97 12:35'!
minHeight

	^ self extent y
! !

!MenuItemMorph methodsFor: 'layout' stamp: 'jm 9/28/97 12:35'!
minWidth

	| scanner |
	scanner _ QuickPrint newOn: Display box: Display boundingBox font: font.
	^ (scanner stringWidth: contents) + (subMenu == nil ifTrue: [0] ifFalse: [10])
! !

!MenuItemMorph methodsFor: 'layout' stamp: 'jm 9/28/97 12:35'!
vResizing

	^ #shrinkWrap
! !

!MenuItemMorph methodsFor: 'private' stamp: 'jm 9/29/97 18:27'!
deletePopupBackToCommonMenuWith: menuOrMenuItem

	| m menuToKeepUp owningItem |
	(menuOrMenuItem isKindOf: MenuMorph)
		ifTrue: [m _ menuOrMenuItem]
		ifFalse: [
			(menuOrMenuItem isKindOf: MenuItemMorph)
				ifTrue: [m _ menuOrMenuItem owner]
				ifFalse: [^ self]].

	menuToKeepUp _ IdentitySet new.
	[m isKindOf: MenuMorph] whileTrue: [
		menuToKeepUp add: m.
		owningItem _ m popUpOwner.
		(owningItem isKindOf: MenuItemMorph)
			ifTrue: [m _ owningItem owner]
			ifFalse: [m _ nil]].

	m _ self owner.
	[m isKindOf: MenuMorph] whileTrue: [
		(menuToKeepUp includes: m) ifTrue: [^ self].
		m stayUp ifFalse: [m delete].
		(m popUpOwner isKindOf: MenuItemMorph) ifTrue: [m popUpOwner isSelected: false].
		owningItem _ m popUpOwner.
		(owningItem isKindOf: MenuItemMorph)
			ifTrue: [m _ owningItem owner]
			ifFalse: [m _ nil]].
! !

!MenuItemMorph methodsFor: 'private' stamp: 'jm 9/29/97 18:11'!
deselectForNewMorph: aMorph

	aMorph == owner ifTrue: [^ self].   "in my menu but not over any item"
	(aMorph == subMenu or: [aMorph owner == subMenu])
		ifTrue: [^ self].  "selecting my submenu or an item in it, leave me selected"

	isSelected _ false.
	self changed.
	subMenu ifNotNil: [subMenu stayUp ifFalse: [subMenu delete]].

	self deletePopupBackToCommonMenuWith: aMorph.
	aMorph owner ~= self owner ifFalse: [
		self deletePopupBackToCommonMenuWith: aMorph].
! !

!MenuItemMorph methodsFor: 'private' stamp: 'jm 9/29/97 15:48'!
deselectItem

	| item |
	isSelected _ false.
	self changed.
	subMenu ifNotNil: [subMenu deleteIfPopUp].
	(owner isKindOf: MenuMorph) ifTrue: [
		item _ owner popUpOwner.
		(item isKindOf: MenuItemMorph) ifTrue: [item deselectItem]].
! !

!MenuItemMorph methodsFor: 'private' stamp: 'jm 9/29/97 12:04'!
hideSubmenu

	subMenu ifNotNil: [subMenu deleteIfPopUp].
	(owner isKindOf: MenuMorph) ifTrue: [owner deleteIfPopUp].
! !

!MenuItemMorph methodsFor: 'private' stamp: 'jm 9/28/97 12:35'!
isInMenu

	^ owner isKindOf: MenuMorph
! !

!MenuItemMorph methodsFor: 'private' stamp: 'jm 9/29/97 13:30'!
isSelected: aBoolean

	isSelected _ aBoolean.
	self changed.
! !

!MenuItemMorph methodsFor: 'private' stamp: 'jm 9/29/97 18:32'!
selectFromHand: aHand

	isSelected _ true.
	self changed.
	aHand newMouseFocus: self.
	subMenu ifNotNil: [
		subMenu delete.
		subMenu popUpAt: self bounds topRight forHand2: aHand.
		subMenu popUpOwner: self].
! !


!MenuItemMorph class methodsFor: 'class initialization' stamp: 'jm 9/29/97 16:42'!
initialize
	"MenuItemMorph initialize"

	| f |
	f _ Form
		extent: 5@9
		fromArray: #(2147483648 3221225472 3758096384 4026531840 4160749568 4026531840 3758096384 3221225472 2147483648)
		offset: 0@0.
	SubMenuMarker _ ColorForm transparentFrom: f.
! !


!MenuLineMorph methodsFor: 'drawing' stamp: 'jm 9/28/97 12:36'!
drawOn: aCanvas

	aCanvas
		fillRectangle: (bounds topLeft corner: bounds rightCenter)
		color: owner color darker.
	aCanvas
		fillRectangle: (bounds leftCenter corner: bounds bottomRight)
		color: owner color lighter.
! !

!MenuLineMorph methodsFor: 'layout' stamp: 'jm 9/28/97 12:36'!
hResizing

	^ #spaceFill
! !

!MenuLineMorph methodsFor: 'layout' stamp: 'jm 9/28/97 12:37'!
isLayoutMorph

	^ true
! !

!MenuLineMorph methodsFor: 'layout' stamp: 'jm 9/28/97 12:37'!
layoutInWidth: w height: h

	self extent: w@h.
! !

!MenuLineMorph methodsFor: 'layout' stamp: 'jm 9/28/97 12:37'!
minHeight

	^ 2
! !

!MenuLineMorph methodsFor: 'layout' stamp: 'jm 9/28/97 12:37'!
minWidth

	^ 10
! !

!MenuLineMorph methodsFor: 'layout' stamp: 'jm 9/28/97 12:37'!
vResizing

	^ #shrinkWrap
! !


!MenuMorph reorganize!
('initialization' initialize)
('accessing' hasSubMenu: items lastSelection lastSelection: popUpOwner popUpOwner: stayUp stayUp:)
('construction' add:action: add:subMenu: add:target:selector: add:target:selector:argument: add:target:selector:argumentList: addLine addStayUpItem addTitle: defaultTarget:)
('menu' addCustomMenuItems:hand: addItem addTitle canDetachSubMenu: detachSubMenu: removeItem: setTarget: toggleStayUp:)
('layout' minHeightWhenEmpty minWidthWhenEmpty)
('control' deleteIfPopUp invokeItem: popUpAt:event: popUpAt:forHand2: popUpAt:forHand:)
!


!MenuMorph methodsFor: 'initialization' stamp: 'jm 9/29/97 07:15'!
initialize

	super initialize.
	self setColor: (Color r: 0.8 g: 0.8 b: 0.8) borderWidth: 2 borderColor: #raised.
	inset _ 3.
	orientation _ #vertical.
	hResizing _ #shrinkWrap.
	vResizing _ #shrinkWrap.
	defaultTarget _ nil.
	lastSelection _ nil.
	stayUp _ false.
	originalEvent _ nil.
	popUpOwner _ nil.
! !

!MenuMorph methodsFor: 'accessing' stamp: 'jm 9/28/97 13:41'!
hasSubMenu: aMenuMorph

	| sub |
	self items do: [:each |
		sub _ each subMenu.
		sub ifNotNil: [
			sub == aMenuMorph ifTrue: [^ true].
			(sub hasSubMenu: aMenuMorph) ifTrue: [^ true]]].
	^ false
! !

!MenuMorph methodsFor: 'accessing' stamp: 'jm 9/28/97 13:02'!
items

	^ submorphs select: [:m | m isKindOf: MenuItemMorph]
! !

!MenuMorph methodsFor: 'accessing' stamp: 'jm 9/28/97 17:35'!
lastSelection
	"Return the label of the last selected item or nil."

	lastSelection == nil
		ifTrue: [^ lastSelection selector]
		ifFalse: [^ nil].
! !

!MenuMorph methodsFor: 'accessing' stamp: 'jm 9/29/97 07:17'!
lastSelection: aString
	"Set the last selection so that it is selected by default when this menu first pops up."

	lastSelection _ self items
		detect: [:each | each selector == aString] ifNone: [nil].
! !

!MenuMorph methodsFor: 'accessing' stamp: 'jm 9/28/97 13:42'!
popUpOwner

	^ popUpOwner
! !

!MenuMorph methodsFor: 'accessing' stamp: 'jm 9/28/97 13:42'!
popUpOwner: aMenuItemMorph

	popUpOwner _ aMenuItemMorph.
! !

!MenuMorph methodsFor: 'accessing' stamp: 'jm 9/29/97 07:21'!
stayUp

	^ stayUp
! !

!MenuMorph methodsFor: 'accessing' stamp: 'jm 9/29/97 07:21'!
stayUp: aBoolean

	stayUp _ aBoolean.
! !

!MenuMorph methodsFor: 'construction' stamp: 'jm 9/28/97 17:28'!
add: aString action: aSymbol
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the default target object."
	"Details: Note that the menu item added captures the default target object at the time the item is added; the default target can later be changed before added additional items without affecting the targets of previously added entries. The model is that each entry is like a button that knows everything it needs to perform its action."

	self add: aString
		target: defaultTarget
		selector: aSymbol
		argumentList: EmptyArray.
! !

!MenuMorph methodsFor: 'construction' stamp: 'jm 9/28/97 14:04'!
add: aString subMenu: aMenuMorph
	"Append the given submenu with the given label."

	| item |
	item _ MenuItemMorph new.
	item contents: aString;
		subMenu: aMenuMorph.
	self addMorphBack: item.
! !

!MenuMorph methodsFor: 'construction' stamp: 'jm 9/28/97 17:24'!
add: aString target: anObject selector: aSymbol
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object."

	self add: aString
		target: anObject
		selector: aSymbol
		argumentList: EmptyArray.
! !

!MenuMorph methodsFor: 'construction' stamp: 'jm 9/28/97 14:05'!
add: aString target: target selector: aSymbol argument: arg
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given argument."

	self add: aString
		target: target
		selector: aSymbol
		argumentList: (Array with: arg)
! !

!MenuMorph methodsFor: 'construction' stamp: 'jm 9/28/97 14:05'!
add: aString target: target selector: aSymbol argumentList: argList
	"Append a menu item with the given label. If the item is selected, it will send the given selector to the target object with the given arguments. If the selector takes one more argument than the number of arguments in the given list, then the triggering event is supplied as as the last argument."

	| item |
	item _ MenuItemMorph new
		contents: aString;
		target: target;
		selector: aSymbol;
		arguments: argList asArray.
	self addMorphBack: item.
! !

!MenuMorph methodsFor: 'construction' stamp: 'jm 9/29/97 08:01'!
addLine
	"Append a divider line to this menu. Suppress duplicate lines."

	(self lastSubmorph isKindOf: MenuLineMorph)
		ifFalse: [self addMorphBack: MenuLineMorph new].
! !

!MenuMorph methodsFor: 'construction' stamp: 'jm 9/30/97 15:10'!
addStayUpItem
	"Append a menu item that can be used to toggle this menu's persistent."

true ifTrue: [^ self].
	self add: 'stay up'
		target: self
		selector: #toggleStayUp:
		argumentList: EmptyArray.
! !

!MenuMorph methodsFor: 'construction' stamp: 'jm 9/29/97 07:19'!
addTitle: aString
	"Add a title line at the top of this menu."

	| title |
	title _ LayoutMorph new setColor: (Color r: 0.5 g: 1 b: 0.75) borderWidth: 1 borderColor: #inset.
	title vResizing: #shrinkWrap.
	title orientation: #vertical.
	title centering: #center.
	title addMorph: (StringMorph new contents: aString).
	self addMorphFront: title.
! !

!MenuMorph methodsFor: 'construction' stamp: 'jm 9/28/97 17:18'!
defaultTarget: anObject
	"Set the default target for adding menu items."

	defaultTarget _ anObject.
! !

!MenuMorph methodsFor: 'menu' stamp: 'jm 9/29/97 08:12'!
addCustomMenuItems: aCustomMenu hand: aHandMorph

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu addLine.
	aCustomMenu add: 'add title...' action: #addTitle.
	aCustomMenu add: 'set target...' action: #setTarget:.
	defaultTarget ifNotNil: [
		aCustomMenu add: 'add item...' action: #addItem].
	aCustomMenu add: 'remove item' action: #removeItem:.
	aCustomMenu add: 'add line' action: #addLine.
	(self canDetachSubMenu: aHandMorph)
		ifTrue: [aCustomMenu add: 'detach submenu' action: #detachSubMenu:].
! !

!MenuMorph methodsFor: 'menu' stamp: 'jm 9/29/97 07:58'!
addItem

	| string sel |
	string _ FillInTheBlank request: 'Label for new item?'.
	string isEmpty ifTrue: [^ self].
	sel _ FillInTheBlank request: 'Selector?'.
	sel isEmpty ifFalse: [sel _ sel asSymbol].
	self add: string action: sel.
! !

!MenuMorph methodsFor: 'menu' stamp: 'jm 9/28/97 13:33'!
addTitle

	| string |
	string _ FillInTheBlank request: 'Title for this menu?'.
	string isEmpty ifTrue: [^ self].
	self addTitle: string.
! !

!MenuMorph methodsFor: 'menu' stamp: 'jm 9/28/97 13:38'!
canDetachSubMenu: hand

	| possibleTargets item |
	possibleTargets _ hand argumentOrNil morphsAt: hand targetOffset.
	item _ possibleTargets
		detect: [:each | each isKindOf: MenuItemMorph]
		ifNone: [^ false].
	^ item subMenu notNil
! !

!MenuMorph methodsFor: 'menu' stamp: 'jm 9/29/97 08:20'!
detachSubMenu: evt

	| possibleTargets item subMenu |
	possibleTargets _ evt hand argumentOrNil morphsAt: evt hand targetOffset.
	item _ possibleTargets detect: [:each | each isKindOf: MenuItemMorph] ifNone: [^ self].
	subMenu _ item subMenu.
	subMenu ifNotNil: [
		item subMenu: nil.
		item delete.
		subMenu stayUp: true.
		subMenu popUpOwner: nil.
		subMenu addTitle: item contents.
		evt hand attachMorph: subMenu].
! !

!MenuMorph methodsFor: 'menu' stamp: 'jm 9/29/97 08:07'!
removeItem: evt

	| possibleTargets item |
	possibleTargets _ evt hand argumentOrNil morphsAt: evt hand targetOffset.
	item _ possibleTargets
		detect: [:each |
					(each isKindOf: MenuItemMorph) or:
					 [each isKindOf: MenuLineMorph]]
		ifNone: [^ self].
	item delete.
! !

!MenuMorph methodsFor: 'menu' stamp: 'jm 9/29/97 08:24'!
setTarget: evt
	"Set the default target object to be used for add item commands, and re-target all existing items to the new target or the the invoking hand."

	| rootMorphs old |
	rootMorphs _ self world rootMorphsAt: evt hand targetOffset.
	rootMorphs size > 1
		ifTrue: [defaultTarget _ rootMorphs at: 2]
		ifFalse: [^ self].
	"re-target all existing items"
	self items do: [:item |
		old _ item target.
		old isHandMorph
			ifTrue: [item target: evt hand. evt hand setArgument: defaultTarget]
			ifFalse: [item target: defaultTarget]].
! !

!MenuMorph methodsFor: 'menu' stamp: 'jm 9/29/97 16:27'!
toggleStayUp: evt
	"Toggle my 'stayUp' flag and adjust the menu item to reflect its new state."

	stayUp _ stayUp not.
	self items do: [:item |
		item selector = #toggleStayUp: ifTrue: [
			stayUp
				ifTrue: [item contents: 'dismiss this menu']
				ifFalse: [item contents: 'stay up']]].
	stayUp ifFalse: [self delete].
! !

!MenuMorph methodsFor: 'control' stamp: 'jm 9/29/97 13:31'!
deleteIfPopUp
	"Remove this menu from the screen if stayUp is not true. If it is a submenu, also remove its owning menu."

	stayUp ifFalse: [self delete].
	(popUpOwner notNil and: [popUpOwner isKindOf: MenuItemMorph]) ifTrue: [
		popUpOwner isSelected: false.
		(popUpOwner owner isKindOf: MenuMorph)
			ifTrue: [popUpOwner owner deleteIfPopUp]].
! !

!MenuMorph methodsFor: 'control' stamp: 'jm 9/29/97 07:53'!
invokeItem: aMenuItem
	"Perform the action associated with the given menu item."

	| sel target args selArgCount |
	aMenuItem isEnabled ifFalse: [^ self].
	lastSelection _ aMenuItem.
	"to do: report lastSelection"
	sel _ aMenuItem selector.
	target _ aMenuItem target.
	args _ aMenuItem arguments.
	selArgCount _ sel numArgs.
	Cursor normal showWhile: [  "show cursor in case item opens a new MVC window"
		selArgCount = 0
			ifTrue: [target perform: sel]
			ifFalse: [
				selArgCount = args size
					ifTrue: [target perform: sel withArguments: args]
					ifFalse: [target perform: sel withArguments: (args copyWith: originalEvent)]]].
! !

!MenuMorph methodsFor: 'control' stamp: 'jm 9/29/97 07:52'!
popUpAt: aPoint event: evt
	"Present this menu at the given point in response to the given event."

	originalEvent _ evt.
	self popUpAt: aPoint forHand: evt hand.
! !

!MenuMorph methodsFor: 'control' stamp: 'jm 9/29/97 18:32'!
popUpAt: aPoint forHand2: hand
	"Present this menu at the given point under control of the given hand."

	| selectedItem delta |
	popUpOwner _ hand.
	selectedItem _ self items detect: [:each | each == lastSelection] ifNone: [self items first].
	self position: aPoint - selectedItem position + self position.
	delta _ self bounds amountToTranslateWithin: hand worldBounds.
	delta = (0@0) ifFalse: [self position: self position + delta].
	hand world addMorphFront: self.
	self changed.
! !

!MenuMorph methodsFor: 'control' stamp: 'jm 9/29/97 17:17'!
popUpAt: aPoint forHand: hand
	"Present this menu at the given point under control of the given hand."

	| selectedItem delta |
	popUpOwner _ hand.
	selectedItem _ self items detect: [:each | each == lastSelection] ifNone: [self items first].
	self position: aPoint - selectedItem position + self position.
	delta _ self bounds amountToTranslateWithin: hand worldBounds.
	delta = (0@0) ifFalse: [self position: self position + delta].
	hand world addMorphFront: self.
	hand newMouseFocus: selectedItem.
	self changed.
! !


!MenuMorph class reorganize!
('example' example)
!


!MenuMorph class methodsFor: 'example' stamp: 'jm 9/29/97 10:36'!
example
	"MenuMorph example"

	| menu |
	menu _ MenuMorph new.
	menu addStayUpItem.
	menu add: 'apples' action: #apples.
	menu add: 'oranges' action: #oranges.
	menu addLine.
	menu addLine.  "extra lines ignored"
	menu add: 'peaches' action: #peaches.
	menu addLine.
	menu add: 'pears' action: #pears.
	menu addLine.
	^ menu
! !


!MouseOverHandlesMorph methodsFor: 'private' stamp: 'jm 9/28/97 18:27'!
doMenu: evt
	"Ask hand to put up the menu for my owner."

	| menu |
	self removeHandles.
	self world doOneCycle.
	menu _ evt hand buildMorphHandleMenuFor: owner.
	menu addTitle: owner class name.
	evt hand invokeMenu: menu event: evt.
! !


!PaintBox methodsFor: 'mouse events' stamp: 'jm 9/28/97 19:35'!
backgroundMouseUp: evt morph: m
	"Edit this method to add mouse-driven behavior."

	| field pic rect ss |
	(m containsPoint: evt cursorPoint)
		ifFalse: [(m findA: StringMorph) color: self buttonUpColor.
			m borderColor: Color gray.
			m borderWidth: 1.
			^ self].
	(ss _ self world findA: SketchEditorMorph) ifNotNil: [ss save].	"save old drawing"
	"field _ self world findA: PlayfieldMorph."
	field ifNil: [
		(m findA: StringMorph) color: self buttonUpColor.
		m borderColor: Color gray.
		m borderWidth: 1.
		^ self].
	pic _ field backgroundSketch.
	pic ifNotNil: [pic editDrawingInWorld: self world]		"need to resubmit it?"
		ifNil: [rect _ self world paintArea.	"Let it tell us"
			pic _ self world hands first drawingClass new form: 
				(Form extent: rect extent depth: Display depth).
			pic bounds: rect.
			"self world addMorphBack: pic.  done below"
			pic _ field backgroundSketch: pic.	"returns a different guy"
			pic ifNotNil: [pic editDrawingInWorld: self world]].! !


!SketchMorph methodsFor: 'menu' stamp: 'jm 9/28/97 19:36'!
editDrawingInWorld: w

	| oldRotation aPaintWindow startForm where |
	w displayWorld.
	aPaintWindow _ SketchEditorMorph new initializeFor: self.
	oldRotation _ rotationDegrees.
	self rotationDegrees: 0.
	where _ self position extent: self form extent.
	startForm _ self form deepCopy.
	self rotationDegrees: oldRotation.	"while drawing is still rotated.  Cancel leaves it right"
	aPaintWindow afterNewPicDo: [:aForm :aRect |
		self form: aForm.
		self position: aRect origin.
		self rotationDegrees: oldRotation.
		owner changed].
	w addMorphFront: aPaintWindow.
	aPaintWindow changed.
	Cursor normal showWhile: [
		aPaintWindow deliverPainting: 
			(aPaintWindow getPaintingStartingWith: startForm
				at: where)].
! !


!ColorPickerMorph methodsFor: 'initialization' stamp: 'jm 9/27/97 06:56'!
initialize

	super initialize.
	self form: ColorChart deepCopy.
	selectedColor _ Color white.
	sourceHand _ nil.
	deleteOnMouseUp _ true.
	updateContinuously _ true.
	selector _ nil.
	target _ nil.
! !

!ColorPickerMorph methodsFor: 'accessing' stamp: 'jm 9/27/97 06:45'!
updateContinuously

	^ updateContinuously
! !

!ColorPickerMorph methodsFor: 'accessing' stamp: 'jm 9/27/97 06:44'!
updateContinuously: aBoolean

	updateContinuously _ aBoolean.
! !

!ColorPickerMorph methodsFor: 'event handling' stamp: 'jm 9/27/97 06:55'!
mouseUp: evt

	self stopStepping.
	sourceHand _ nil.
	deleteOnMouseUp ifTrue: [self delete].
	self updateTargetColor.
! !

!ColorPickerMorph methodsFor: 'menu' stamp: 'jm 9/28/97 14:36'!
addCustomMenuItems: aCustomMenu hand: aHandMorph

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	deleteOnMouseUp
		ifTrue: [aCustomMenu add: 'stay up' action: #toggleDeleteOnMouseUp]
		ifFalse: [aCustomMenu add: 'do not stay up' action: #toggleDeleteOnMouseUp].
	updateContinuously
		ifTrue: [aCustomMenu add: 'update only at end' action: #toggleUpdateContinuously]
		ifFalse: [aCustomMenu add: 'update continuously' action: #toggleUpdateContinuously].
! !

!ColorPickerMorph methodsFor: 'menu' stamp: 'jm 9/27/97 06:53'!
toggleDeleteOnMouseUp

	deleteOnMouseUp _ deleteOnMouseUp not.
! !

!ColorPickerMorph methodsFor: 'menu' stamp: 'jm 9/27/97 06:54'!
toggleUpdateContinuously

	updateContinuously _ updateContinuously not.
! !

!ColorPickerMorph methodsFor: 'private' stamp: 'jm 9/27/97 06:47'!
updateColor: aColor feedbackColor: feedbackColor
	"Set my selected color to the given color if it is different. Give user feedback. Inform the target of the change if the target and selector are not nil." 

	selectedColor = aColor ifTrue: [^ self].  "do nothing if color doesn't change"

	originalForm fill: FeedbackBox fillColor: feedbackColor.
	self form: originalForm.
	selectedColor _ aColor.
	updateContinuously ifTrue: [self updateTargetColor].
! !

!ColorPickerMorph methodsFor: 'private' stamp: 'jm 9/27/97 06:47'!
updateTargetColor

	((target ~~ nil) and: [selector ~~ nil]) ifTrue: [
		selector numArgs = 2
			ifTrue: [target perform: selector with: selectedColor with: sourceHand]
			ifFalse: [target perform: selector with: selectedColor]].
! !


!MultiuserTinyPaint methodsFor: 'menu' stamp: 'jm 9/29/97 07:49'!
setPenSize: evt

	| menu sizes |
	menu _ MenuMorph new.
	sizes _ (0 to: 5), (6 to: 12 by: 2), (15 to: 40 by: 5).
	sizes do: [:w |
		menu add: w printString
			target: self
			selector: #penSize:hand:
			argumentList: (Array with: w with: evt hand)].

	menu popUpAt: evt hand position event: evt.
! !


!TextMorph methodsFor: 'anchors' stamp: 'jm 9/28/97 19:01'!
anchorMorph: evt

	| root |
	root _ evt hand argumentOrNil.
	root ifNil: [^ self].
	evt hand selectSubmorphToOperateOn: self selector: #anchor:.
! !


Object removeSelector: #performMenuAction:!
Object removeSelector: #performMenuAction:with:!
Object removeSelector: #dispatchAsMenuActionTo:!
BlockContext removeSelector: #performMenuAction:!
BlockContext removeSelector: #dispatchAsMenuActionTo:!
HandMorph removeSelector: #operateOnSubmorph:!
HandMorph removeSelector: #chooseTarget!
HandMorph removeSelector: #startRunningAll!
HandMorph removeSelector: #stopRunningAll!
HandMorph removeSelector: #saveEToyInFile!
HandMorph removeSelector: #invokeMenu:caption:event:!
HandMorph removeSelector: #stepAll!
HandMorph removeSelector: #saveWorldTest!
MenuItemMorph removeSelector: #hideSubMenuFor:!
MenuItemMorph removeSelector: #isInTransition:for:!
MenuItemMorph removeSelector: #action:!
MenuItemMorph removeSelector: #isInTransition!
MenuItemMorph removeSelector: #action!
MenuItemMorph removeSelector: #showSubMenuFor:!
MenuItemMorph initialize!
MenuMorph removeSelector: #receiver:!
MenuMorph removeSelector: #itemWasSelected:!
MenuMorph removeSelector: #popUpAt:in:for:!
MenuMorph removeSelector: #beSubMenu:!
MenuMorph removeSelector: #positionSelectionAt:!
MenuMorph removeSelector: #isPopUp!
MenuMorph removeSelector: #lastSelectionAction!
MenuMorph removeSelector: #selections!
MenuMorph removeSelector: #receiver!
MenuMorph removeSelector: #lastSelectionAction:!
MenuMorph removeSelector: #acceptDroppingMorph:event:!
Symbol removeSelector: #dispatchAsMenuActionTo:!
