'From Squeak 1.23 of October 4, 1997 on 4 October 1997 at 12:04:54 pm'!

!Bitmap methodsFor: 'filing' stamp: 'di 10/4/97 12:00'!
readCompressedFrom: strm
	"Decompress a run-coded stream into this bitmap:
		[0 means end of runs]
		[n = 1..127] [(n+3) copies of next byte]
		[n = 128..191] [(n-127) next bytes as is]
		[n = 192..255] [(n-190) copies of next 4 bytes]"
	| n byte out outBuff bytes |
	out _ WriteStream on: (outBuff _ ByteArray new: self size*4).
	[(n _ strm next) > 0] whileTrue:
		[(n between: 1 and: 127) ifTrue:
			[byte _ strm next.
			1 to: n+3 do: [:i | out nextPut: byte]].
		(n between: 128 and: 191) ifTrue:
			[1 to: n-127 do: [:i | out nextPut: strm next]].
		(n between: 192 and: 255) ifTrue:
			[bytes _ (1 to: 4) collect: [:i | strm next].
			1 to: n-190 do: [:i | bytes do: [:b | out nextPut: b]]]].
	out position = outBuff size ifFalse: [self error: 'Decompression size error'].
	"Copy the final byteArray into self"
	self copyFromByteArray: outBuff.
"
Integerity check:
 | f r |
r _ Rectangle fromUser.
f _ Form fromDisplay: r.
f bits: (Bitmap decompressFromByteArray: f bits compressToByteArray).
f bits size = f bitsSize ifFalse: [self halt].
f displayAt: r topLeft

Total Integerity check:
Form allInstances do: [:f |
f bits = (Bitmap decompressFromByteArray: f bits compressToByteArray)
	ifFalse: [self halt]]
"! !

!Bitmap methodsFor: 'accessing' stamp: 'di 10/4/97 11:56'!
copyFromByteArray: byteArray
	"This method should work with either byte orderings"
	| long |
	(self size * 4) = byteArray size ifFalse: [self halt].
	1 to: byteArray size by: 4 do:
		[:i | long _ Integer
				byte1: (byteArray at: i+3)
				byte2: (byteArray at: i+2)
				byte3: (byteArray at: i+1)
				byte4: (byteArray at: i).
		self at: i+3//4 put: long]! !

!Bitmap methodsFor: 'accessing' stamp: 'di 10/4/97 11:59'!
copyToByteArray: byteArray
	"This method should work with either byte orderings"
	| long |
	(self size * 4) = byteArray size ifFalse: [self halt].
	1 to: byteArray size by: 4 do:
		[:i | long _ self at: i+3//4.
		byteArray at: i+3 put: (long digitAt: 1).
		byteArray at: i+2 put: (long digitAt: 2).
		byteArray at: i+1 put: (long digitAt: 3).
		byteArray at: i put: (long digitAt: 4)]! !


!GIFReadWriter methodsFor: 'private-decoding' stamp: 'di 10/4/97 12:02'!
readBitData
	"using modified Lempel-Ziv Welch algorithm."

	| outCodes outCount bitMask initCodeSize code curCode oldCode inCode finChar i bytes f |
	self readWord.	"skip Image Left"
	self readWord.	"skip Image Top"
	width _ self readWord.
	height _ self readWord.
	interlace _ (self next bitAnd: 16r40) ~= 0.
	"I ignore the possible existence of a local color map."
	pass _ 0.
	xpos _ 0.
	ypos _ 0.
	rowByteSize _ ((width + 3) // 4) * 4.
	remainBitCount _ 0.
	bufByte _ 0.
	bufStream _ ReadStream on: ByteArray new.

	outCodes _ ByteArray new: 1025.
	outCount _ 0.
	bitMask _ (1 bitShift: bitsPerPixel) - 1.
	prefixTable _ Array new: 4096.
	suffixTable _ Array new: 4096.

	initCodeSize _ self next.
	self setParameters: initCodeSize.
	bitsPerPixel > 8 ifTrue: [^self error: 'never heard of a GIF that deep'].
	bytes _ ByteArray new: rowByteSize * height.
	[(code _ self readCode) = eoiCode] whileFalse:
		[code = clearCode
			ifTrue:
				[self setParameters: initCodeSize.
				curCode _ oldCode _ code _ self readCode.
				finChar _ curCode bitAnd: bitMask.
				"Horrible hack to avoid running off the end of the bitmap.  Seems to cure problem reading some gifs!!? tk 6/24/97 20:16"
				xpos = 0 ifTrue: [
						ypos < height ifTrue: [
							bytes at: (ypos * rowByteSize) + xpos + 1 put: finChar]]
					ifFalse: [bytes at: (ypos * rowByteSize) + xpos + 1 put: finChar].
				self updatePixelPosition]
			ifFalse:
				[curCode _ inCode _ code.
				curCode >= freeCode ifTrue:
					[curCode _ oldCode.
					outCodes at: (outCount _ outCount + 1) put: finChar].
				[curCode > bitMask] whileTrue:
					[outCount > 1024
						ifTrue: [^self error: 'corrupt GIF file (OutCount)'].
					outCodes at: (outCount _ outCount + 1)
						put: (suffixTable at: curCode + 1).
					curCode _ prefixTable at: curCode + 1].
				finChar _ curCode bitAnd: bitMask.
				outCodes at: (outCount _ outCount + 1) put: finChar.
				i _ outCount.
				[i > 0] whileTrue:
					["self writePixel: (outCodes at: i) to: bits"
					bytes at: (ypos * rowByteSize) + xpos + 1 put: (outCodes at: i).
					self updatePixelPosition.
					i _ i - 1].
				outCount _ 0.
				prefixTable at: freeCode + 1 put: oldCode.
				suffixTable at: freeCode + 1 put: finChar.
				oldCode _ inCode.
				freeCode _ freeCode + 1.
				self checkCodeSize]].
	prefixTable _ suffixTable _ nil.

	f _ ColorForm extent: width@height depth: 8.
	f bits copyFromByteArray: bytes.
	^ f
! !


Bitmap removeSelector: #isBigEndian!
