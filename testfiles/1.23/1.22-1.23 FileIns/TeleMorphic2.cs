'From Squeak 1.22 of September 21, 1997 on 26 September 1997 at 3:44:40 pm'!

!HandMorph methodsFor: 'remote morphic' stamp: 'jm 9/26/97 15:37'!
cleanupDeadConnections
	"Clean up any remote connections that have been disconnected or become invalid."

	| liveConnections sock |
	liveConnections _ OrderedCollection new.
	remoteConnections do: [:triple |
		sock _ triple first.
		sock isUnconnectedOrInvalid
			ifTrue: [
				(triple at: 2) = #opening
					ifTrue: [
Transcript show: 'trying connection again...'; cr.
						sock destroy.
						sock _ Socket new.
						sock connectTo: (triple at: 3) port: 54323.
						triple at: 1 put: sock.
						liveConnections add: triple]  "try again"
					ifFalse: [triple first destroy]]
			ifFalse: [liveConnections add: triple]].
	remoteConnections _ liveConnections.
! !

!HandMorph methodsFor: 'remote morphic' stamp: 'jm 9/26/97 15:00'!
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
	h changed.
	h startListening.
	self startTransmittingEventsTo: addr.
! !

!HandMorph methodsFor: 'remote morphic' stamp: 'jm 9/26/97 14:34'!
disconnectAllRemoteUsers
	"Disconnect all remote hands and stop transmitting events."

	| addr |
	self world hands do: [:h |
		(h isKindOf: RemoteHandMorph) ifTrue: [
			addr _ h remoteHostAddress.
			addr = 0 ifFalse: [self stopTransmittingEventsTo: addr].
			h withdrawFromWorld]].

	remoteConnections do: [:triple | triple first closeAndDestroy: 5].
	remoteConnections _ OrderedCollection new.
! !

!HandMorph methodsFor: 'remote morphic' stamp: 'jm 9/26/97 14:35'!
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
	remoteConnections add: (Array with: sock with: #opening with: addr).
! !

!HandMorph methodsFor: 'remote morphic' stamp: 'jm 9/26/97 14:35'!
stopTransmittingEventsTo: addr
	"Stop broadcasting events from this world's cursor to a remote cursor on the host with the given address. This method issues a 'close' but does not destroy the socket; it will be destroyed when the other end reads the last data and closes the connection."

	| sock |
	remoteConnections do: [:triple |
		sock _ triple first.
		(sock isUnconnectedOrInvalid not and: [sock remoteAddress = addr]) ifTrue: [
			sock close.
			triple at: 2 put: #closing]].
! !

!HandMorph methodsFor: 'remote morphic' stamp: 'jm 9/26/97 14:41'!
transmitEvent: aMorphicEvent
	"Transmit the given event to all remote connections."

	| evtString sock status firstEvt |
	lastEventTransmitted = aMorphicEvent ifTrue: [^ self].
	evtString _ aMorphicEvent storeString, (String with: Character cr).
	self cleanupDeadConnections.
	remoteConnections do: [:triple |
		sock _ triple first.
		status _ triple at: 2.
		sock isConnected
			ifTrue: [
				status = #opening ifTrue: [
					"connection established; send worldExtent as first event"
					firstEvt _ MorphicEvent newWorldExtent: self worldBounds extent.
					sock sendData: firstEvt storeString, (String with: Character cr).
					Transcript
						show: 'Connection established with remote WorldMorph at ';
						show: (NetNameResolver stringFromAddress: sock remoteAddress); cr.
					triple at: 2 put: #connected].
				sock sendData: evtString]
			ifFalse: [
				status = #connected ifTrue: [
					"other end has closed; close our end"
					Transcript
						show: 'Closing connection with remote WorldMorph at ';
						show: (NetNameResolver stringFromAddress: sock remoteAddress); cr.
					sock close.
					triple at: 2 put: #closing]]].

	lastEventTransmitted _ aMorphicEvent.
! !


!MultiuserTinyPaint methodsFor: 'menu' stamp: 'jm 9/26/97 14:50'!
addCustomMenuItems: aCustomMenu hand: aHandMorph

	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu add: 'clear' action: #clear.
	aCustomMenu add: 'pen color' action: #setPenColor:.
	aCustomMenu add: 'pen size' action: #setPenSize:.
"	aCustomMenu add: 'fill' action: #fill:."
! !

!MultiuserTinyPaint methodsFor: 'menu' stamp: 'jm 9/26/97 14:47'!
penSize: anInteger hand: hand

	| state |
	(drawState includesKey: hand) ifFalse: [self createDrawStateFor: hand].
	state _ drawState at: hand.
	state at: PenSizeIndex put: anInteger.
	(state at: PenIndex) roundNib: anInteger.
! !

!MultiuserTinyPaint methodsFor: 'menu' stamp: 'jm 9/26/97 14:45'!
setPenSize: evt

	| menu sizes |
	menu _ MenuMorph new.
	sizes _ (0 to: 5), (6 to: 12 by: 2), (15 to: 40 by: 5).
	sizes do: [:w | menu add: w printString action: w].
	menu receiver: [:w | self penSize: w hand: evt hand].
	menu popUpOwner: evt hand.
	menu popUpAt: evt hand position in: evt hand world for: evt hand.

"	nibSize ifNotNil: [
		(drawState includesKey: evt hand) ifFalse: [self createDrawStateFor: evt hand].
		state _ drawState at: evt hand.
		state at: PenSizeIndex put: nibSize.
		(state at: PenIndex) roundNib: nibSize]."
! !


!WorldMorph methodsFor: 'hands' stamp: 'jm 9/26/97 15:00'!
addHand: aHandMorph
	"Add the given hand to the list of hands for this world."

	hands _ hands copyWith: aHandMorph.
	aHandMorph privateOwner: self.
! !



