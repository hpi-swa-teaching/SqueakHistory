Object subclass: #NetNameResolver
	instanceVariableNames: ''
	classVariableNames: 'DefaultHostName ResolverBusy ResolverError ResolverReady ResolverSemaphore ResolverUninitialized '
	poolDictionaries: ''
	category: 'System-Network'!
Object subclass: #Socket
	instanceVariableNames: 'semaphore socketHandle '
	classVariableNames: 'Connected InvalidSocket OtherEndClosed ThisEndClosed Unconnected WaitingForConnection '
	poolDictionaries: ''
	category: 'System-Network'!
Socket subclass: #SimpleClientSocket
	instanceVariableNames: ''
	classVariableNames: 'CR CrLf LF '
	poolDictionaries: ''
	category: 'System-Network'!
SimpleClientSocket subclass: #HTTPSocket
	instanceVariableNames: 'headerTokens '
	classVariableNames: 'HTTPBlabEmail HTTPPort HTTPProxy ParamDelimiters '
	poolDictionaries: ''
	category: 'System-Network'!


