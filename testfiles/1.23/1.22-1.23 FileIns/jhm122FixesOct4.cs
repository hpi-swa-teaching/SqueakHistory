'From Squeak 1.23 of October 4, 1997 on 4 October 1997 at 6:39:23 pm'!
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
Object subclass: #SoundRecorder
	instanceVariableNames: 'stereo samplingRate recordedBuffers recordProcess bufferAvailableSema paused meteringBuffer meterLevel soundPlaying currentBuffer nextIndex '
	classVariableNames: 'CanRecordWhilePlaying '
	poolDictionaries: ''
	category: 'System-Sound'!

!AbstractSound methodsFor: 'copying' stamp: 'jm 10/4/97 16:39'!
copy
	"A sound should copy all of the state needed to play itself. Thus, two copies of a sound can be played at the same time. These semantics require a recursive copy down to the level of immutable data. For example, a SampledSound need not copy its sample buffer."

	self subclassResponsibility.
! !


!Form class methodsFor: 'instance creation' stamp: 'jm 10/4/97 13:32'!
fromBMPFileNamed: fileName
	"Read a Form from a Windows bitmap (BMP-format) file with the given name."
	"Form fromBMPFileNamed: 'test.bmp'"

	| f bfType bfSize bfReserved bfOffBits biSize biWidth biHeight
	  biPlanes biBitCount biCompression form pixelLine pixIndex rgb rowBytes line |
	f _ (FileStream oldFileNamed: fileName) binary.
	bfType _ f nextLitteEndianNumber: 2.
	bfSize _ f nextLitteEndianNumber: 4.
	bfReserved _ f nextLitteEndianNumber: 4.
	bfOffBits _ f nextLitteEndianNumber: 4.
	biSize _ f nextLitteEndianNumber: 4.
	biWidth _ f nextLitteEndianNumber: 4.
	biHeight _ f nextLitteEndianNumber: 4.
	biPlanes _ f nextLitteEndianNumber: 2.
	biBitCount _ f nextLitteEndianNumber: 2.
	biCompression _ f nextLitteEndianNumber: 4.
	f nextLitteEndianNumber: 4.  "biSizeImage"
	f nextLitteEndianNumber: 4.  "biXPelsPerMeter"
	f nextLitteEndianNumber: 4.  "biYPelsPerMeter"
	f nextLitteEndianNumber: 4.  "biClrUsed"
	f nextLitteEndianNumber: 4.  "biClrImportant"

	((bfType = 19778) & (bfReserved = 0) & (biPlanes = 1) &
	 (biSize = 40) & (bfSize <= f size))
		ifFalse: [self error: 'Bad BMP file header'].
	biCompression = 0
		ifFalse: [self error: 'Can currently only read uncompressed BMP files'].

	f position: bfOffBits.  "Skip past any color map to the image data"
	form _ Form extent: biWidth@biHeight
				depth: (biBitCount = 24 ifTrue: [32] ifFalse: [biBitCount]).
	rowBytes _ (biBitCount * biWidth + 31 // 32) * 4.
	line _ Form extent: biWidth@1 depth: form depth.
	1 to: biHeight do: [:i |
		biBitCount = 24
		ifTrue: [pixelLine _ f next: rowBytes.
				pixIndex _ 1.
				1 to: biWidth do: [:j |
					rgb _ (pixelLine at: pixIndex) +
						   ((pixelLine at: pixIndex + 1) bitShift: 8) +
						   ((pixelLine at: pixIndex + 2) bitShift: 16).
					line bits at: j put: rgb.
					pixIndex _ pixIndex + 3]]
		ifFalse: [line bits copyFromByteArray: (f next: rowBytes)].
		form copy: line boundingBox from: line to: 0@(biHeight-i) rule: Form over].

	f close.
	^ form! !


!InterimSoundMorph methodsFor: 'all' stamp: 'jm 10/4/97 16:35'!
mouseDown: evt

	(graphic containsPoint: evt cursorPoint)
		ifTrue: [sound copy play]
		ifFalse: [super mouseDown: evt].
! !


!MixedSound methodsFor: 'copying' stamp: 'jm 10/4/97 16:52'!
copy
	"Copy my component sounds."

	^ self clone copySounds
! !

!MixedSound methodsFor: 'copying' stamp: 'jm 10/4/97 16:53'!
copySounds
	"Private!! Support for copying. Copy my component sounds and settings array."

	sounds _ (sounds collect: [:s | s copy]).
	panSettings _ panSettings copy.
! !


!PluckedSound methodsFor: 'copying' stamp: 'jm 10/4/97 16:46'!
copy

	^ self clone copyRing
! !

!PluckedSound methodsFor: 'copying' stamp: 'jm 10/4/97 16:47'!
copyRing
	"Private!! Support for copying"

	ring _ ring copy.
! !


!RepeatingSound methodsFor: 'copying' stamp: 'jm 10/4/97 16:48'!
copy
	"Copy my component sound."

	^ self clone copySound
! !

!RepeatingSound methodsFor: 'copying' stamp: 'jm 10/4/97 16:48'!
copySound
	"Private!! Support for copying."

	sound _ sound copy.
! !


!RestSound methodsFor: 'copying' stamp: 'jm 10/4/97 16:49'!
copy

	^ self clone
! !


!RWBinaryOrTextStream methodsFor: 'all' stamp: 'jm 10/4/97 15:58'!
contentsOfEntireFile
	"For compatibility with file streams."

	^ self contents! !


!SampledSound methodsFor: 'playing' stamp: 'jm 10/4/97 16:28'!
mixSampleCount: n into: aSoundBuffer startingAt: startIndex pan: pan
	"Mix the given number of samples with the samples already in the given buffer starting at the given index. Assume that the buffer size is at least (index + count) - 1. The pan parameter determines the left-right balance of the sound, where 0 is left only, 1000 is right only, and 500 is centered."

	| lastIndex i thisSample channelIndex sample sampleIndex |
	<primitive: 179>
	self var: #aSoundBuffer declareC: 'short int *aSoundBuffer'.
	self var: #samples declareC: 'short int *samples'.

	lastIndex _ (startIndex + n) - 1.
	i _ startIndex.
	sampleIndex _ indexTimes1000 // 1000.
	[(sampleIndex <= samplesSize) and: [i <= lastIndex]] whileTrue: [
		thisSample _ samples at: sampleIndex.
		pan > 0 ifTrue: [
			channelIndex _ 2 * i.
			sample _ (aSoundBuffer at: channelIndex) + ((thisSample * pan) // 1000).
			sample >  32767 ifTrue: [ sample _  32767 ].  "clipping!!"
			sample < -32767 ifTrue: [ sample _ -32767 ].  "clipping!!"
			aSoundBuffer at: channelIndex put: sample.
		].
		pan < 1000 ifTrue: [
			channelIndex _ (2 * i) - 1.
			sample _ (aSoundBuffer at: channelIndex) + ((thisSample * (1000 - pan)) // 1000).
			sample >  32767 ifTrue: [ sample _  32767 ].  "clipping!!"
			sample < -32767 ifTrue: [ sample _ -32767 ].  "clipping!!"
			aSoundBuffer at: channelIndex put: sample.
		].
		indexTimes1000 _ indexTimes1000 + incrementTimes1000.
		sampleIndex _ indexTimes1000 // 1000.
		i _ i + 1].
	count _ count - n.
! !

!SampledSound methodsFor: 'copying' stamp: 'jm 10/4/97 16:51'!
copy

	^ self clone
! !


!SampleStreamer methodsFor: 'all' stamp: 'jm 10/4/97 16:49'!
copy

	^ self clone
! !


!SequentialSound methodsFor: 'copying' stamp: 'jm 10/4/97 16:59'!
copy
	"Copy my component sounds."

	^ self clone copySounds! !

!SequentialSound methodsFor: 'copying' stamp: 'jm 10/4/97 16:58'!
copySounds
	"Private!! Support for copying. Copy my component sounds."

	sounds _ (sounds collect: [:s | s copy]).
! !


!HTTPSocket methodsFor: 'all' stamp: 'jm 9/26/97 17:27'!
redirect
	"See if the header has a 'Location: url CrLf' in it.  If so, return the new URL of this page.  tk 6/24/97 18:03"

	| this |
	1 to: headerTokens size do: [:ii | 
		this _ headerTokens at: ii.
		(this first asLowercase = $l and: [this asLowercase = 'location:']) ifTrue: [
			^ (headerTokens at: ii+1)]].
	^ nil	"not found"
! !


!Socket class methodsFor: 'tests' stamp: 'jm 10/4/97 15:50'!
loopbackTest
	"Send data from one socket to another on the local machine. Tests most of the socket primitives."
	"Socket loopbackTest"

	| sock1 sock2 bytesToSend sendBuf receiveBuf done bytesSent bytesReceived t extraBytes |
	Transcript cr; show: 'starting loopback test'; cr.

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
	Transcript show: 'loopback test done; time = ', t printString; cr.
	Transcript show: ((bytesToSend asFloat / t) roundTo: 0.01) printString, ' kBytes/sec'; cr.
	Transcript endEntry.
! !


!SimpleClientSocket class methodsFor: 'simple HTTP example' stamp: 'jm 10/4/97 16:02'!
httpTestHost: hostName port: port url: url
	"This test fetches a URL from the given host and port."
	"SimpleClientSocket httpTestHost: 'www.exploratorium.edu' port: 80 url: '/'"
	"Tests URL fetch through a local HTTP proxie server:
		(SimpleClientSocket
			httpTestHost: '127.0.0.1'
			port: 8080
			url: 'HTTP://www.exploratorium.edu/index.html')"

	| hostAddr s result buf bytes totalBytes t |
	Transcript cr; show: 'starting http test'; cr.
	Socket initializeNetwork.
	hostAddr _ NetNameResolver addressForName: hostName timeout: 10.
	hostAddr = nil ifTrue: [^ self inform: 'Could not find an address for ', hostName].

	s _ SimpleClientSocket new.
	Transcript show: '---------- Connecting ----------'; cr.
	s connectTo: hostAddr port: port.
	s waitForConnectionUntil: "self standardDeadline" (Socket deadlineSecs: 4).
	(s isConnected) ifFalse: [
		s destroy.
		^ self inform: 'could not connect'].
	Transcript show: 'connection open; waiting for data'; cr.

	s sendCommand: 'GET ', url, ' HTTP/1.0'.
	s sendCommand: 'User-Agent: Squeak 1.19'.
	s sendCommand: 'ACCEPT: text/html'.	"always accept plain text"
	s sendCommand: 'ACCEPT: application/octet-stream'.	"also accept binary"
	s sendCommand: ''.  "blank line"

	result _ WriteStream on: (String new: 10000).
	buf _ String new: 10000.
	totalBytes _ 0.
	t _ Time millisecondsToRun: [
		[s isConnected] whileTrue: [
			s waitForDataUntil: (Socket deadlineSecs: 5).
			bytes _ s receiveDataInto: buf.
			1 to: bytes do: [:i | result nextPut: (buf at: i)].
			totalBytes _ totalBytes + bytes.
			Transcript show: totalBytes printString, ' bytes received'; cr]].

	s destroy.
	Transcript show: '---------- Connection Closed ----------'; cr; endEntry.
	Transcript show: 'http test done; ', totalBytes printString, ' bytes read in '.
	Transcript show: ((t / 1000.0) roundTo: 0.01) printString, ' seconds'; cr.
	Transcript show: ((totalBytes asFloat / t) roundTo: 0.01) printString, ' kBytes/sec'; cr.
	Transcript endEntry.
	StringHolderView
		open: (StringHolder new contents: (result contents))
		label: 'HTTP Test Result: URL Contents'.
! !


!HTTPSocket class methodsFor: 'examples' stamp: 'jm 10/4/97 15:57'!
httpGet: url
	"Return the exact contents of a web page or other web object. The parsed header is saved.  Use a proxy server if one has been registered.  tk 7/23/97 17:21"
	"	HTTPSocket httpShowPage: 'http://www.altavista.digital.com/index.html'	 "
	"	HTTPSocket httpShowPage: 'www.webPage.com/~kaehler2/ab.html'	 "
	"	HTTPSocket httpShowPage: 'www.exploratorium.edu/index.html'	 "
	"	HTTPSocket httpShowPage: 'www.apple.com/default.html'	 "
	"	HTTPSocket httpShowPage: 'www.altavista.digital.com/'	 "
	"	HTTPSocket httpShowPage: 'jumbo/tedk/ab.html'	 "

	^ self httpGet: url accept: 'application/octet-stream'
! !


!SoundPlayer class methodsFor: 'player process' stamp: 'jm 10/4/97 16:19'!
oldStylePlayLoop
	"This version of the play loop is used if the VM does not yet support the new sound playing that signals a semaphore when a sound buffer becomes available."

	| samples |
	[true] whileTrue: [
		[(samples _ self primSoundAvailableBytes // 4) > 100]
			whileFalse: [(Delay forMilliseconds: 1) wait].

		samples _ samples min: Buffer stereoSampleCount.
		PlayerSemaphore critical: [
			ActiveSounds _ ActiveSounds select: [:snd | snd samplesRemaining > 0].
			ActiveSounds do: [:snd |
				snd ~~ SoundJustStarted ifTrue: [
					snd playSampleCount: samples into: Buffer startingAt: 1 stereo: Stereo]].
			self primSoundPlaySamples: samples from: Buffer startingAt: 1.
			Buffer primFill: 0.
			SoundJustStarted _ nil]].
! !

!SoundPlayer class methodsFor: 'player process' stamp: 'jm 10/4/97 16:19'!
playLoop
	"The sound player process loop."

	| samples |
	[true] whileTrue: [
		[(samples _ self primSoundAvailableBytes // 4) > 100]
			whileFalse: [ReadyForBuffer wait].

		samples _ samples min: Buffer stereoSampleCount.
		PlayerSemaphore critical: [
			ActiveSounds _ ActiveSounds select: [:snd | snd samplesRemaining > 0].
			ActiveSounds do: [:snd |
				snd ~~ SoundJustStarted ifTrue: [
					snd playSampleCount: samples into: Buffer startingAt: 1 stereo: Stereo]].
			self primSoundPlaySamples: samples from: Buffer startingAt: 1.
			Buffer primFill: 0.
			SoundJustStarted _ nil]].
! !

!SoundPlayer class methodsFor: 'private' stamp: 'jm 10/4/97 16:24'!
startPlayingImmediately: aSound
	"Private!! Start playing the given sound as soon as possible by mixing it into the sound output buffers of the underlying sound driver."

	| dontInsertSamples totalSamples buf n leftover src rest |
	dontInsertSamples _ true.  "temporary, until insert samples primitive is ported"
	dontInsertSamples ifTrue: [
		ActiveSounds add: aSound.
		^ self].

	"first, fill a double-size buffer with samples"
	totalSamples _ Buffer stereoSampleCount * 2.  "two buffer's worth"
	buf _ SoundBuffer newStereoSampleCount: totalSamples.
	aSound playSampleCount: totalSamples into: buf startingAt: 1 stereo: Stereo.

	PlayerSemaphore critical: [
		"insert as many samples as possible into the sound driver's buffers"
		n _ self primSoundInsertSamples: totalSamples
			from: buf
			samplesOfLeadTime: 1024.
		leftover _ totalSamples - n.

		"copy the remainder of buf into Buffer"
		"Note: the following loop iterates over 16-bit words, not two-word stereo slices"
		"assert: 0 < leftover <= Buffer stereoSampleCount"
		src _ 2 * n.
		1 to: 2 * leftover do:
			[:dst | Buffer at: dst put: (buf at: (src _ src + 1))].

		"generate enough additional samples to finish filling Buffer"
		rest _ Buffer stereoSampleCount - leftover.
		aSound playSampleCount: rest into: Buffer startingAt: leftover + 1 stereo: Stereo.

		"record the fact that this sound has already been played into Buffer so that  we don't process it again this time around"
		SoundJustStarted _ aSound.

		ActiveSounds add: aSound].
! !


!SoundRecorder methodsFor: 'accessing' stamp: 'jm 10/4/97 17:55'!
recordedSound
	"Return the sound that was recorded."

	| snd |
	stereo ifTrue: [^ self condensedStereoSound].
	snd _ SequentialSound new.
	recordedBuffers do: [:buf |
		snd add: (SampledSound new setSamples: buf samplingRate: samplingRate)].
	^ snd
! !

!SoundRecorder methodsFor: 'recording controls' stamp: 'jm 10/4/97 16:09'!
pause
	"Go into pause mode. The record level continues to be updated, but no sound is recorded."

	paused _ true.
	((currentBuffer ~~ nil) and: [nextIndex > 1])
		ifTrue: [
			recordedBuffers addLast: (currentBuffer copyFrom: 1 to: nextIndex - 1).
			self allocateBuffer].

	soundPlaying ifNotNil: [
		soundPlaying pause.
		soundPlaying _ nil].

	CanRecordWhilePlaying ifFalse: [self stopRecording].
! !

!SoundRecorder methodsFor: 'recording controls' stamp: 'jm 10/4/97 16:09'!
resumeRecording
	"Continue recording from the point at which it was last paused."

	CanRecordWhilePlaying ifFalse: [self startRecording].
	paused _ false.
! !

!SoundRecorder methodsFor: 'recording controls' stamp: 'jm 10/4/97 16:08'!
startRecording
	"Turn of the sound input driver and start the recording process. Initially, recording is paused."

	| semaIndex |
	CanRecordWhilePlaying ifFalse: [SoundPlayer shutDown].
	recordProcess ifNotNil: [self stopRecording].
	paused _ true.
	meteringBuffer _ SoundBuffer newMonoSampleCount: 1024.
	meterLevel _ 0.
	self allocateBuffer.
	bufferAvailableSema _ Semaphore new.
	semaIndex _ Smalltalk registerExternalObject: bufferAvailableSema.
	self primStartRecordingDesiredSampleRate: (SoundPlayer samplingRate)
		stereo: stereo
		semaIndex: semaIndex.
	samplingRate _ self primGetActualRecordingSampleRate.
	recordProcess _ [self recordLoop] newProcess.
	recordProcess priority: Processor userInterruptPriority.
	recordProcess resume.
! !

!SoundRecorder methodsFor: 'private' stamp: 'jm 10/4/97 17:54'!
condensedStereoSound
	"Decompose my buffers into left and right channels and return a mixed sound consisting of the those two channels. This may be take a while, since the data must be copied into new buffers."

	| sz leftBuf rightBuf leftI rightI left |
	sz _ recordedBuffers inject: 0 into: [:tot :buff | tot + buff size].
	leftBuf _ SoundBuffer newMonoSampleCount: (sz + 1) // 2.
	rightBuf _ SoundBuffer newMonoSampleCount: (sz + 1) // 2.
	leftI _ rightI _ 1.
	left _ true.
	recordedBuffers do: [:b |
		1 to: b size do: [:j |
			left
				ifTrue: [leftBuf at: leftI put: (b at: j). leftI _ leftI + 1. left _ false]
				ifFalse: [rightBuf at: rightI put: (b at: j). rightI _ rightI + 1. left _ true]]].
	^ MixedSound new
		add: (SampledSound new setSamples: leftBuf samplingRate: samplingRate) pan: 0;
		add: (SampledSound new setSamples: rightBuf samplingRate: samplingRate) pan: 1000
! !


!SoundRecorder class reorganize!
('class initialization' initialize)
('instance creation' new)
!


!SoundRecorder class methodsFor: 'class initialization' stamp: 'jm 10/4/97 16:13'!
initialize
	"SoundRecorder initialize"
	"Details: Some computers cannot record and playback sound at the same time. If CanRecordWhilePlaying is false, then the SoundRecorder alternates between recording and playing. If it is true, sounds can be playing during recording."

	CanRecordWhilePlaying _ false.
! !


!WaveTableSound reorganize!
('initialization' setPitch:dur:loudness:)
('accessing' decayRate decayRate: loudness loudness: pitch pitch:)
('sound generation' doControl mixSampleCount:into:startingAt:pan: reset samplesRemaining)
('copying' copy)
!


!WaveTableSound methodsFor: 'copying' stamp: 'jm 10/4/97 16:45'!
copy

	^ self clone
! !


SoundRecorder initialize!
