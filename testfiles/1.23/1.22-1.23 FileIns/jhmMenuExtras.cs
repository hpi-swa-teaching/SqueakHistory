'From Squeak 1.22 of September 21, 1997 on 4 October 1997 at 8:47:15 am'!
'From Squeak 1.22 of September 21, 1997 on 3 October 1997 at 3:05:58 pm'!
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
!MenuItemMorph reorganize!
('initialization' initialize)
('accessing' arguments arguments: isEnabled isEnabled: selector selector: subMenu subMenu: target target:)
('drawing' drawOn:)
('events' handlesMouseDown: mouseDown: mouseMove: mouseUp:)
('layout' hResizing isLayoutMorph layoutInWidth:height: minHeight minWidth vResizing)
('private' deletePopupBackToCommonMenuWith: deselectForNewMorph: deselectItem hideSubmenu isInMenu isSelected: selectFromHand:)
!
!MenuMorph reorganize!
('initialization' initialize)
('accessing' hasSubMenu: items lastSelection lastSelection: popUpOwner popUpOwner: stayUp stayUp:)
('construction' add:action: add:subMenu: add:target:selector: add:target:selector:argument: add:target:selector:argumentList: addLine addStayUpItem addTitle: defaultTarget:)
('menu' addCustomMenuItems:hand: addItem addTitle canDetachSubMenu: detachSubMenu: removeItem: setTarget: toggleStayUp:)
('layout' minHeightWhenEmpty minWidthWhenEmpty)
('control' deleteIfPopUp invokeItem: popUpAt:event: popUpAt:forHand2: popUpAt:forHand:)
!
!MenuMorph methodsFor: 'construction'!
addStayUpItem
	"Append a menu item that can be used to toggle this menu's persistent."

	self add: 'stay up'
		target: self
		selector: #toggleStayUp:
		argumentList: EmptyArray.
! !
!MenuMorph class reorganize!
('example' example)
!
Object removeSelector: #performMenuAction:!
Object removeSelector: #performMenuAction:with:!
Object removeSelector: #dispatchAsMenuActionTo:!
BlockContext removeSelector: #performMenuAction:!
BlockContext removeSelector: #dispatchAsMenuActionTo:!
HandMorph removeSelector: #operateOnSubmorph:!
HandMorph removeSelector: #chooseTarget!
HandMorph removeSelector: #invokeMenu:caption:event:!
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
