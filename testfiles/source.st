SystemOrganization addCategory: #'Collections-Abstract'!
SystemOrganization addCategory: #'Collections-Arrayed'!
SystemOrganization addCategory: #'Collections-Sequenceable'!
SystemOrganization addCategory: #'Collections-SkipLists'!
SystemOrganization addCategory: #'Collections-Stack'!
SystemOrganization addCategory: #'Collections-Streams'!
SystemOrganization addCategory: #'Collections-Strings'!
SystemOrganization addCategory: #'Collections-Support'!
SystemOrganization addCategory: #'Collections-Text'!
SystemOrganization addCategory: #'Collections-Unordered'!
SystemOrganization addCategory: #'Collections-Weak'!

Magnitude subclass: #Character
	instanceVariableNames: 'value'
	classVariableNames: 'ClassificationTable CharacterTable UppercaseBit LowercaseBit LetterBits'
	poolDictionaries: ''
	category: 'Collections-Strings'!

!Character commentStamp: 'ar 4/9/2005 22:35' prior: 0!
I represent a character by storing its associated Unicode. The first 256 characters are created uniquely, so that all instances of latin1 characters ($R, for example) are identical.

	The code point is based on Unicode.  Since Unicode is 21-bit wide character set, we have several bits available for other information.  As the Unicode Standard  states, a Unicode code point doesn't carry the language information.  This is going to be a problem with the languages so called CJK (Chinese, Japanese, Korean.  Or often CJKV including Vietnamese).  Since the characters of those languages are unified and given the same code point, it is impossible to display a bare Unicode code point in an inspector or such tools.  To utilize the extra available bits, we use them for identifying the languages.  Since the old implementation uses the bits to identify the character encoding, the bits are sometimes called "encoding tag" or neutrally "leading char", but the bits rigidly denotes the concept of languages.

	The other languages can have the language tag if you like.  This will help to break the large default font (font set) into separately loadable chunk of fonts.  However, it is open to the each native speakers and writers to decide how to define the character equality, since the same Unicode code point may have different language tag thus simple #= comparison may return false.

I represent a character by storing its associated ASCII code (extended to 256 codes). My instances are created uniquely, so that all instances of a character ($R, for example) are identical.!

!Character class methodsFor: 'instance creation' stamp: 'ar 4/9/2005 22:36'!
allByteCharacters
	"Answer all the characters that can be encoded in a byte"
	^ (0 to: 255) collect: [:v | Character value: v]

	
! !

!Character class methodsFor: 'instance creation' stamp: 'ar 4/9/2005 22:37'!
allCharacters
	"This name is obsolete since only the characters that will fit in a byte can be queried"
	^self allByteCharacters
	
! !

!Character class methodsFor: 'constants' stamp: 'rhi 9/8/2000 14:57'!
alphabet
	"($a to: $z) as: String"

	^ 'abcdefghijklmnopqrstuvwxyz' copy! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'NS 7/11/2000 09:20'!
arrowDown
	^ self value: 31! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'NS 7/11/2000 09:20'!
arrowLeft
	^ self value: 28! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'NS 7/11/2000 09:20'!
arrowRight
	^ self value: 29! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'NS 7/11/2000 09:20'!
arrowUp
	^ self value: 30! !

!Character class methodsFor: 'accessing untypeable characters' stamp: ''!
backspace
	"Answer the Character representing a backspace."

	^self value: 8! !

!Character class methodsFor: 'constants' stamp: ''!
characterTable
	"Answer the class variable in which unique Characters are stored."

	^CharacterTable! !

!Character class methodsFor: 'instance creation' stamp: 'RAH 4/25/2000 19:49'!
codePoint: integer 
	"Return a character whose encoding value is integer."
	#Fundmntl.
	(0 > integer or: [255 < integer])
		ifTrue: [self error: 'parameter out of range 0..255'].
	^ CharacterTable at: integer + 1! !

!Character class methodsFor: 'private' stamp: 'lr 11/21/2005 17:24'!
constantNameFor: aCharacter
	^ self constantNames
		detect: [ :each | (self perform: each) = aCharacter ]
		ifNone: [ nil ].! !

!Character class methodsFor: 'private' stamp: 'lr 11/21/2005 17:33'!
constantNames
	^ #( backspace cr delete escape lf newPage space tab ).! !

!Character class methodsFor: 'accessing untypeable characters' stamp: ''!
cr
	"Answer the Character representing a carriage return."

	^self value: 13! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'NS 7/11/2000 09:19'!
delete
	^ self value: 127! !

!Character class methodsFor: 'instance creation' stamp: ''!
digitValue: x 
	"Answer the Character whose digit value is x. For example, answer $9 for 
	x=9, $0 for x=0, $A for x=10, $Z for x=35."

	| index |
	index := x asInteger.
	^CharacterTable at: 
		(index < 10
			ifTrue: [48 + index]
			ifFalse: [55 + index])
		+ 1! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'NS 7/11/2000 09:21'!
end
	^ self value: 4! !

!Character class methodsFor: 'accessing untypeable characters' stamp: ''!
enter
	"Answer the Character representing enter."

	^self value: 3! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'ls 9/2/1999 08:06'!
escape
	"Answer the ASCII ESC character"

	^self value: 27! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'sma 3/15/2000 22:33'!
euro
	"The Euro currency sign, that E with two dashes. The key code is a wild guess"

	^ Character value: 219! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'NS 7/11/2000 09:21'!
home
	^ self value: 1! !

!Character class methodsFor: 'class initialization' stamp: 'yo 10/4/2003 16:03'!
initialize
	"Create the table of unique Characters."
"	self initializeClassificationTable"! !

!Character class methodsFor: 'class initialization' stamp: 'dgd 8/24/2003 15:10'!
initializeClassificationTable
	"
	Initialize the classification table. The classification table is a
	compact encoding of upper and lower cases of characters with

		- bits 0-7: The lower case value of this character.
		- bits 8-15: The upper case value of this character.
		- bit 16: lowercase bit (e.g., isLowercase == true)
		- bit 17: uppercase bit (e.g., isUppercase == true)

	"
	| ch1 ch2 |

	LowercaseBit := 1 bitShift: 16.
	UppercaseBit := 1 bitShift: 17.

	"Initialize the letter bits (e.g., isLetter == true)"
	LetterBits := LowercaseBit bitOr: UppercaseBit.

	ClassificationTable := Array new: 256.
	"Initialize the defaults (neither lower nor upper case)"
	0 to: 255 do:[:i|
		ClassificationTable at: i+1 put: (i bitShift: 8) + i.
	].

	"Initialize character pairs (upper-lower case)"
	#(
		"Basic roman"
		($A $a) 	($B $b) 	($C $c) 	($D $d) 
		($E $e) 	($F $f) 	($G $g) 	($H $h) 
		($I $i) 		($J $j) 		($K $k) 	($L $l) 
		($M $m)	($N $n)	($O $o)	($P $p) 
		($Q $q) 	($R $r) 	($S $s) 	($T $t) 
		($U $u)	($V $v)	($W $w)	($X $x)
		($Y $y)	($Z $z)
		"International"
		($Ä $ä)	($Å $å)	($Ç $ç)	($É $é)
		($Ñ $ñ)	($Ö $ö)	($Ü $ü)	($À $à)
		($Ã $ã)	($Õ $õ)	($Œ $œ)	($Æ $æ)
		"International - Spanish"
		($Á $á)	($Í $í)		($Ó $ó)	($Ú $ú)
		"International - PLEASE CHECK"
		($È $è)	($Ì $ì)		($Ò $ò)	($Ù $ù)
		($Ë $ë)	($Ï $ï)
		($Â $â)	($Ê $ê)	($Î $î)	($Ô $ô)	($Û $û)
	) do:[:pair|
		ch1 := pair first asciiValue.
		ch2 := pair last asciiValue.
		ClassificationTable at: ch1+1 put: (ch1 bitShift: 8) + ch2 + UppercaseBit.
		ClassificationTable at: ch2+1 put: (ch1 bitShift: 8) + ch2 + LowercaseBit.
	].

	"Initialize a few others for which we only have lower case versions."
	#($ß $Ø $ø $ÿ) do:[:char|
		ch1 := char asciiValue.
		ClassificationTable at: ch1+1 put: (ch1 bitShift: 8) + ch1 + LowercaseBit.
	].
! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'NS 7/11/2000 09:19'!
insert
	^ self value: 5! !

!Character class methodsFor: 'instance creation' stamp: 'ar 4/9/2005 22:24'!
leadingChar: leadChar code: code

	code >= 16r400000 ifTrue: [
		self error: 'code is out of range'.
	].
	leadChar >= 256 ifTrue: [
		self error: 'lead is out of range'.
	].

	^self value: (leadChar bitShift: 22) + code.! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'ls 9/8/1998 22:15'!
lf
	"Answer the Character representing a linefeed."

	^self value: 10! !

!Character class methodsFor: 'accessing untypeable characters' stamp: ''!
linefeed
	"Answer the Character representing a linefeed."

	^self value: 10! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'wiz 4/9/2006 20:30'!
nbsp
	"non-breakable space. Latin1 encoding common usage."

	^ Character value: 160! !

!Character class methodsFor: 'instance creation' stamp: ''!
new
	"Creating new characters is not allowed."

	self error: 'cannot create new characters'! !

!Character class methodsFor: 'accessing untypeable characters' stamp: ''!
newPage
	"Answer the Character representing a form feed."

	^self value: 12! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'NS 7/11/2000 09:20'!
pageDown
	^ self value: 12! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'NS 7/11/2000 09:21'!
pageUp
	^ self value: 11! !

!Character class methodsFor: 'instance creation' stamp: ''!
separators
	^ #(32 "space"
		13 "cr"
		9 "tab"
		10 "line feed"
		12 "form feed")
		collect: [:v | Character value: v]

	
! !

!Character class methodsFor: 'accessing untypeable characters' stamp: ''!
space
	"Answer the Character representing a space."

	^self value: 32! !

!Character class methodsFor: 'accessing untypeable characters' stamp: ''!
tab
	"Answer the Character representing a tab."

	^self value: 9! !

!Character class methodsFor: 'instance creation' stamp: 'ar 4/9/2005 22:19'!
value: anInteger 
	"Answer the Character whose value is anInteger."

	anInteger > 255 ifTrue: [^self basicNew setValue: anInteger].
	^ CharacterTable at: anInteger + 1.
! !

!Character methodsFor: 'comparing' stamp: ''!
< aCharacter 
	"Answer true if the receiver's value < aCharacter's value."

	^self asciiValue < aCharacter asciiValue! !

!Character methodsFor: 'comparing' stamp: 'ar 4/9/2005 21:48'!
= aCharacter 
	"Primitive. Answer true if the receiver and the argument are the same
	object (have the same object pointer) and false otherwise. Optional. See
	Object documentation whatIsAPrimitive."

	^ self == aCharacter or:[
		aCharacter isCharacter and: [self asciiValue = aCharacter asciiValue]]! !

!Character methodsFor: 'comparing' stamp: ''!
> aCharacter 
	"Answer true if the receiver's value > aCharacter's value."

	^self asciiValue > aCharacter asciiValue! !

!Character methodsFor: 'converting' stamp: ''!
asCharacter
	"Answer the receiver itself."

	^self! !

!Character methodsFor: 'converting' stamp: 'ls 9/5/1998 01:18'!
asIRCLowercase
	"convert to lowercase, using IRC's rules"

	self == $[ ifTrue: [ ^ ${ ].
	self == $] ifTrue: [ ^ $} ].
	self == $\ ifTrue: [ ^ $| ].

	^self asLowercase! !

!Character methodsFor: 'converting' stamp: ''!
asInteger
	"Answer the value of the receiver."

	^value! !

!Character methodsFor: 'converting' stamp: 'yo 8/16/2004 11:35'!
asLowercase
	"If the receiver is uppercase, answer its matching lowercase Character."
	"A tentative implementation.  Eventually this should consult the Unicode table."

	| v |
	v := self charCode.
	(((8r101 <= v and: [v <= 8r132]) or: [16rC0 <= v and: [v <= 16rD6]]) or: [16rD8 <= v and: [v <= 16rDE]])
		ifTrue: [^ Character value: value + 8r40]
		ifFalse: [^ self]! !

!Character methodsFor: 'converting' stamp: 'sma 3/11/2000 17:21'!
asString
	^ String with: self! !

!Character methodsFor: 'converting' stamp: 'raa 5/26/2001 09:54'!
asSymbol 
	"Answer a Symbol consisting of the receiver as the only element."

	^Symbol internCharacter: self! !

!Character methodsFor: 'converting' stamp: 'tk 9/4/2000 12:05'!
asText
	^ self asString asText! !

!Character methodsFor: 'converting' stamp: 'ar 4/9/2005 21:51'!
asUnicode

	| table charset v |
	self leadingChar = 0 ifTrue: [^ value].
	charset := EncodedCharSet charsetAt: self leadingChar.
	charset isCharset ifFalse: [^ self charCode].
	table := charset ucsTable.
	table isNil ifTrue: [^ 16rFFFD].

	v := table at: self charCode + 1.
	v = -1 ifTrue: [^ 16rFFFD].

	^ v.
! !

!Character methodsFor: 'converting' stamp: 'ar 4/9/2005 22:25'!
asUnicodeChar
	"@@@ FIXME: Make this use asUnicode and move it to its lonely sender @@@"
	| table charset v |
	self leadingChar = 0 ifTrue: [^ value].
	charset := EncodedCharSet charsetAt: self leadingChar.
	charset isCharset ifFalse: [^ self].
	table := charset ucsTable.
	table isNil ifTrue: [^ Character value: 16rFFFD].

	v := table at: self charCode + 1.
	v = -1 ifTrue: [^ Character value: 16rFFFD].

	^ Character leadingChar: charset unicodeLeadingChar code: v.! !

!Character methodsFor: 'converting' stamp: 'yo 8/16/2004 11:34'!
asUppercase
	"If the receiver is lowercase, answer its matching uppercase Character."
	"A tentative implementation.  Eventually this should consult the Unicode table."	

	| v |
	v := self charCode.
	(((8r141 <= v and: [v <= 8r172]) or: [16rE0 <= v and: [v <= 16rF6]]) or: [16rF8 <= v and: [v <= 16rFE]])
		ifTrue: [^ Character value: value - 8r40]
		ifFalse: [^ self]
! !

!Character methodsFor: 'accessing' stamp: ''!
asciiValue
	"Answer the value of the receiver that represents its ascii encoding."

	^value! !

!Character methodsFor: 'converting' stamp: 'yo 8/11/2003 21:18'!
basicSqueakToIso
	| asciiValue |

	value < 128 ifTrue: [^ self].
	value > 255 ifTrue: [^ self].
	asciiValue := #(196 197 199 201 209 214 220 225 224 226 228 227 229 231 233 232 234 235 237 236 238 239 241 243 242 244 246 245 250 249 251 252 134 176 162 163 167 149 182 223 174 169 153 180 168 128 198 216 129 177 138 141 165 181 142 143 144 154 157 170 186 158 230 248 191 161 172 166 131 173 178 171 187 133 160 192 195 213 140 156 150 151 147 148 145 146 247 179 253 159 185 164 139 155 188 189 135 183 130 132 137 194 202 193 203 200 205 206 207 204 211 212 190 210 218 219 217 208 136 152 175 215 221 222 184 240 254 255 256 ) at: self asciiValue - 127.
	^ Character value: asciiValue.
! !

!Character methodsFor: 'testing' stamp: 'yo 8/5/2003 16:57'!
canBeGlobalVarInitial

	^ (EncodedCharSet charsetAt: self leadingChar) canBeGlobalVarInitial: self.
! !

!Character methodsFor: 'testing' stamp: 'yo 8/5/2003 16:58'!
canBeNonGlobalVarInitial

	^ (EncodedCharSet charsetAt: self leadingChar) canBeNonGlobalVarInitial: self.
! !

!Character methodsFor: 'accessing' stamp: 'yo 12/29/2002 10:11'!
charCode

	^ (value bitAnd: 16r3FFFFF).
! !

!Character methodsFor: 'copying' stamp: 'tk 12/9/2000 11:46'!
clone
	"Answer with the receiver, because Characters are unique."! !

!Character methodsFor: 'accessing' stamp: 'RAH 4/25/2000 19:49'!
codePoint
	"Return the encoding value of the receiver."
	#Fundmntl.
	^ self asciiValue! !

!Character methodsFor: 'object fileIn' stamp: 'tk 1/17/2000 11:27'!
comeFullyUpOnReload: smartRefStream
	"Use existing an Character.  Don't use the new copy."

	^ self class value: value! !

!Character methodsFor: 'copying' stamp: ''!
copy
	"Answer with the receiver because Characters are unique."! !

!Character methodsFor: 'copying' stamp: ''!
deepCopy
	"Answer with the receiver because Characters are unique."! !

!Character methodsFor: 'accessing' stamp: 'yo 12/1/2003 19:30'!
digitValue
	"Answer 0-9 if the receiver is $0-$9, 10-35 if it is $A-$Z, and < 0 
	otherwise. This is used to parse literal numbers of radix 2-36."

	^ (EncodedCharSet charsetAt: self leadingChar) digitValue: self.
! !

!Character methodsFor: 'comparing' stamp: ''!
hash
	"Hash is reimplemented because = is implemented."

	^value! !

!Character methodsFor: 'printing' stamp: 'ar 4/9/2005 21:53'!
hex
	^value hex! !

!Character methodsFor: 'testing' stamp: ''!
isAlphaNumeric
	"Answer whether the receiver is a letter or a digit."

	^self isLetter or: [self isDigit]! !

!Character methodsFor: 'testing' stamp: 'yo 8/28/2002 13:42'!
isCharacter

	^ true.
! !

!Character methodsFor: 'testing' stamp: 'yo 8/5/2003 16:43'!
isDigit

	^ (EncodedCharSet charsetAt: self leadingChar) isDigit: self.
! !

!Character methodsFor: 'testing' stamp: 'yo 8/5/2003 16:43'!
isLetter

	^ (EncodedCharSet charsetAt: self leadingChar) isLetter: self.
! !

!Character methodsFor: 'printing' stamp: ''!
isLiteral

	^true! !

!Character methodsFor: 'testing' stamp: 'yo 8/5/2003 16:43'!
isLowercase

	^ (EncodedCharSet charsetAt: self leadingChar) isLowercase: self.
! !

!Character methodsFor: 'testing' stamp: 'yo 8/27/2002 15:18'!
isOctetCharacter

	^ value < 256.
! !

!Character methodsFor: 'testing' stamp: 'yo 7/29/2005 15:21'!
isSafeForHTTP
	"whether a character is 'safe', or needs to be escaped when used, eg, in a URL"
	"[GG]  See http://www.faqs.org/rfcs/rfc1738.html. ~ is unsafe and has been removed"
	^ self charCode < 128
		and: [self isAlphaNumeric
				or: ['.-_' includes: (Character value: self charCode)]]! !

!Character methodsFor: 'testing' stamp: ''!
isSeparator
	"Answer whether the receiver is one of the separator characters--space, 
	cr, tab, line feed, or form feed."

	value = 32 ifTrue: [^true].	"space"
	value = 13 ifTrue: [^true].	"cr"
	value = 9 ifTrue: [^true].	"tab"
	value = 10 ifTrue: [^true].	"line feed"
	value = 12 ifTrue: [^true].	"form feed"
	^false! !

!Character methodsFor: 'testing' stamp: 'di 4/3/1999 00:38'!
isSpecial
	"Answer whether the receiver is one of the special characters"

	^'+-/\*~<>=@,%|&?!!' includes: self! !

!Character methodsFor: 'testing' stamp: 'ar 4/12/2005 14:09'!
isTraditionalDomestic
	"Yoshiki's note about #isUnicode says:
		[This method] is for the backward compatibility when we had domestic
		traditional encodings for CJK languages.  To support loading the
		projects in traditional domestic encodings (From Nihongo4), and load
		some changesets.  Once we decided to get rid of classes like JISX0208
		from the EncodedCharSet table, the need for isUnicode will not be
		necessary.
	I (Andreas) decided to change the name from isUnicode to #isTraditionalDomestic
	since I found isUnicode to be horribly confusing (how could the character *not*
	be Unicode after all?). But still, we should remove this method in due time."
	^ ((EncodedCharSet charsetAt: self leadingChar) isKindOf: LanguageEnvironment class) not! !

!Character methodsFor: 'testing' stamp: 'yo 8/5/2003 16:43'!
isUppercase

	^ (EncodedCharSet charsetAt: self leadingChar) isUppercase: self.
! !

!Character methodsFor: 'testing' stamp: ''!
isVowel
	"Answer whether the receiver is one of the vowels, AEIOU, in upper or 
	lower case."

	^'AEIOU' includes: self asUppercase! !

!Character methodsFor: 'converting' stamp: 'ar 4/9/2005 22:15'!
isoToSqueak 
	^self "no longer needed"! !

!Character methodsFor: 'accessing' stamp: 'yo 12/29/2002 10:14'!
leadingChar

	^ (value bitAnd: (16r3FC00000)) bitShift: -22.
! !

!Character methodsFor: 'converting' stamp: 'ar 4/10/2005 16:05'!
macToSqueak
	"Convert the receiver from MacRoman to Squeak encoding"
	| asciiValue |
	value < 128 ifTrue: [^ self].
	value > 255 ifTrue: [^ self].
	asciiValue := #(196 197 199 201 209 214 220 225 224 226 228 227 229 231 233 232 234 235 237 236 238 239 241 243 242 244 246 245 250 249 251 252 134 176 162 163 167 149 182 223 174 169 153 180 168 128 198 216 129 177 138 141 165 181 142 143 144 154 157 170 186 158 230 248 191 161 172 166 131 173 178 171 187 133 160 192 195 213 140 156 150 151 147 148 145 146 247 179 255 159 185 164 139 155 188 189 135 183 130 132 137 194 202 193 203 200 205 206 207 204 211 212 190 210 218 219 217 208 136 152 175 215 221 222 184 240 253 254 ) at: self asciiValue - 127.
	^ Character value: asciiValue.
! !

!Character methodsFor: 'object fileIn' stamp: 'tk 2/16/2001 14:52'!
objectForDataStream: refStrm
	"I am being collected for inclusion in a segment.  Do not include Characters!!  Let them be in outPointers."

	refStrm insideASegment
		ifFalse: ["Normal use" ^ self]
		ifTrue: ["recording objects to go into an ImageSegment"			
			"remove it from references.  Do not trace."
			refStrm references removeKey: self ifAbsent: [].
			^ nil]
! !

!Character methodsFor: 'printing' stamp: 'lr 11/21/2005 17:40'!
printOn: aStream
	| name |
	value > 32
		ifTrue: [ aStream nextPut: $$; nextPut: self ]
		ifFalse: [
			name := self class constantNameFor: self.
			name notNil
				ifTrue: [ aStream nextPutAll: self class name; space; nextPutAll: name ]
				ifFalse: [ aStream nextPutAll: self class name; nextPutAll: ' value: '; print: value ] ].! !

!Character methodsFor: 'printing' stamp: 'MPW 1/1/1901 22:04'!
printOnStream: aStream

	aStream print:'$', (String with:self).! !

!Character methodsFor: 'comparing' stamp: 'md 8/2/2005 18:21'!
sameAs: aCharacter 
	"Answer whether the receiver is equal to aCharacter, ignoring case"
	^ (self asLowercase = aCharacter asLowercase)	! !

!Character methodsFor: 'private' stamp: 'ar 4/9/2005 22:18'!
setValue: newValue
	value ifNotNil:[^self error:'Characters are immutable'].
	value := newValue.! !

!Character methodsFor: 'converting' stamp: 'ar 4/9/2005 22:16'!
squeakToIso
	^self "no longer needed"! !

!Character methodsFor: 'converting' stamp: 'ar 4/10/2005 16:05'!
squeakToMac
	"Convert the receiver from Squeak to MacRoman encoding."
	value < 128 ifTrue: [^ self].
	value > 255 ifTrue: [^ self].
	^ Character value: (#(
		173 176 226 196 227 201 160 224 246 228 178 220 206 179 182 183	"80-8F"
		184 212 213 210 211 165 208 209 247 170 185 221 207 186 189 217	"90-9F"
		202 193 162 163 219 180 195 164 172 169 187 199 194 197 168 248	"A0-AF"
		161 177 198 215 171 181 166 225 252 218 188 200 222 223 240 192 	"B0-BF"
		203 231 229 204 128 129 174 130 233 131 230 232 237 234 235 236 	"C0-CF"
		245 132 241 238 239 205 133 249 175 244 242 243 134 250 251 167	"D0-DF"
		136 135 137 139 138 140 190 141 143 142 144 145 147 146 148 149	"E0-EF"
		253 150 152 151 153 155 154 214 191 157 156 158 159 254 255 216	"F0-FF"
	) at: value - 127)
! !

!Character methodsFor: 'printing' stamp: 'ar 4/9/2005 22:30'!
storeBinaryOn: aStream
	"Store the receiver on a binary (file) stream"
	value < 256 
		ifTrue:[aStream basicNextPut: self]
		ifFalse:[Stream nextInt32Put: value].! !

!Character methodsFor: 'printing' stamp: 'lr 1/3/2007 19:30'!
storeOn: aStream
	"Common character literals are preceded by '$', however special need to be encoded differently: for some this might be done by using one of the shortcut constructor methods for the rest we have to create them by ascii-value."

	| name |
	(value between: 33 and: 255)
		ifTrue: [ aStream nextPut: $$; nextPut: self ]
		ifFalse: [
			name := self class constantNameFor: self.
			name notNil
				ifTrue: [ aStream nextPutAll: self class name; space; nextPutAll: name ]
				ifFalse: [
					aStream 
						nextPut: $(; nextPutAll: self class name; 
						nextPutAll: ' value: '; print: value; nextPut: $) ] ].! !

!Character methodsFor: 'converting' stamp: ''!
to: other
	"Answer with a collection in ascii order -- $a to: $z"
	^ (self asciiValue to: other asciiValue) collect:
				[:ascii | Character value: ascii]! !

!Character methodsFor: 'testing' stamp: ''!
tokenish
	"Answer whether the receiver is a valid token-character--letter, digit, or 
	colon."

	^self isLetter or: [self isDigit or: [self = $:]]! !

!Character methodsFor: 'copying' stamp: 'tk 1/7/1999 16:50'!
veryDeepCopyWith: deepCopier
	"Return self.  I can't be copied."! !

Magnitude subclass: #LookupKey
	instanceVariableNames: 'key'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Support'!

!LookupKey commentStamp: '<historical>' prior: 0!
I represent a key for looking up entries in a data structure. Subclasses of me, such as Association, typically represent dictionary entries.!

LookupKey subclass: #Association
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Support'!

!Association commentStamp: '<historical>' prior: 0!
I represent a pair of associated objects--a key and a value. My instances can serve as entries in a dictionary.!

!Association class methodsFor: 'instance creation' stamp: ''!
key: newKey value: newValue
	"Answer an instance of me with the arguments as the key and value of 
	the association."

	^(super key: newKey) value: newValue! !

!Association methodsFor: 'comparing' stamp: 'md 1/27/2004 17:27'!
= anAssociation

	^ super = anAssociation and: [value = anAssociation value]! !

!Association methodsFor: 'filter streaming' stamp: 'MPW 1/1/1901 20:53'!
byteEncode: aStream
	aStream writeAssocation:self.! !

!Association methodsFor: 'comparing' stamp: 'md 1/27/2004 17:28'!
hash
	"Hash is reimplemented because = is implemented."
	
	^key hash bitXor: value hash.! !

!Association methodsFor: 'self evaluating' stamp: 'sd 7/31/2005 21:44'!
isSelfEvaluating
	^ self class == Association! !

!Association methodsFor: 'testing' stamp: 'ar 8/14/2001 23:06'!
isSpecialWriteBinding
	"Return true if this variable binding is write protected, e.g., should not be accessed primitively but rather by sending #value: messages"
	^false! !

!Association methodsFor: 'testing' stamp: 'ar 8/14/2001 22:39'!
isVariableBinding
	"Return true if I represent a literal variable binding"
	^true! !

!Association methodsFor: 'accessing' stamp: ''!
key: aKey value: anObject 
	"Store the arguments as the variables of the receiver."

	key := aKey.
	value := anObject! !

!Association methodsFor: 'objects from disk' stamp: 'tk 10/3/2000 13:03'!
objectForDataStream: refStrm
	| dp |
	"I am about to be written on an object file.  If I am a known global, write a proxy that will hook up with the same resource in the destination system."

	^ (Smalltalk associationAt: key ifAbsent: [nil]) == self 
		ifTrue: [dp := DiskProxy global: #Smalltalk selector: #associationOrUndeclaredAt: 
							args: (Array with: key).
			refStrm replace: self with: dp.
			dp]
		ifFalse: [self]! !

!Association methodsFor: 'printing' stamp: ''!
printOn: aStream

	super printOn: aStream.
	aStream nextPutAll: '->'.
	value printOn: aStream! !

!Association methodsFor: 'printing' stamp: 'MPW 1/4/1901 08:31'!
propertyListOn: aStream
	aStream write:key; print:'='; write:value.
! !

!Association methodsFor: 'printing' stamp: ''!
storeOn: aStream
	"Store in the format (key->value)"
	aStream nextPut: $(.
	key storeOn: aStream.
	aStream nextPutAll: '->'.
	value storeOn: aStream.
	aStream nextPut: $)! !

!Association methodsFor: 'accessing' stamp: ''!
value
	"Answer the value of the receiver."

	^value! !

!Association methodsFor: 'accessing' stamp: ''!
value: anObject 
	"Store the argument, anObject, as the value of the receiver."

	value := anObject! !

Association subclass: #SkipListNode
	instanceVariableNames: 'pointers object'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-SkipLists'!

!SkipListNode commentStamp: 'sac 1/23/2002 19:39' prior: 0!
This is a skiplist node. It can be treated as an association that allows a 'next' operator to go to the next node in the skiplist.

!

!SkipListNode class methodsFor: 'instance creation' stamp: 'sac 1/22/2002 17:53'!
key: key value: value level: maxLevel 
        ^ (super key: key value: value) initialize: maxLevel! !

!SkipListNode class methodsFor: 'instance creation' stamp: 'LC 6/17/2001 09:16'!
new: maxLevel
	^ super new initialize: maxLevel! !

!SkipListNode class methodsFor: 'instance creation' stamp: 'sac 1/22/2002 17:57'!
on: element level: maxLevel 
        ^ self key: element value: element level: maxLevel
! !

!SkipListNode class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 12:44'!
tailOfLevel: n
	^ self on: nil level: n! !

!SkipListNode methodsFor: 'accessing' stamp: 'LC 6/17/2001 12:55'!
atForward: i put: node
	^ pointers at: i put: node! !

!SkipListNode methodsFor: 'accessing' stamp: 'LC 6/18/2001 13:34'!
forward: i 
	^ pointers at: i! !

!SkipListNode methodsFor: 'initialization' stamp: 'LC 6/17/2001 11:54'!
initialize: maxLevel
	pointers := Array new: maxLevel! !

!SkipListNode methodsFor: 'accessing' stamp: 'LC 6/18/2001 12:21'!
level
	^ pointers size! !

!SkipListNode methodsFor: 'accessing' stamp: 'LC 6/18/2001 19:20'!
next
	^ pointers first! !

!SkipListNode methodsFor: 'accessing' stamp: 'sac 1/22/2002 17:22'!
printOn: aStream
        | first |
        aStream
                nextPut: $[.
        super printOn: aStream.
        aStream
                nextPutAll: ']-->('.
        first := true.
        pointers do: [:node |
                first ifTrue: [first := false] ifFalse: [aStream space].
                node ifNil: [aStream nextPutAll: '*'] 
                ifNotNil: [node printOn: aStream]].
        aStream nextPut: $)
! !

Association subclass: #WeakKeyAssociation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Support'!

!WeakKeyAssociation commentStamp: '<historical>' prior: 0!
I am an association holding only weakly on my key.!

!WeakKeyAssociation methodsFor: 'comparing' stamp: 'ar 3/21/98 15:45'!
< aLookupKey 
	"Refer to the comment in Magnitude|<."

	^self key < aLookupKey key! !

!WeakKeyAssociation methodsFor: 'comparing' stamp: 'ar 3/21/98 15:46'!
= aLookupKey

	self species = aLookupKey species
		ifTrue: [^self key = aLookupKey key]
		ifFalse: [^false]! !

!WeakKeyAssociation methodsFor: 'comparing' stamp: 'ar 3/21/98 15:46'!
hash
	"Hash is reimplemented because = is implemented."

	^self key hash! !

!WeakKeyAssociation methodsFor: 'comparing' stamp: 'ar 3/21/98 15:46'!
hashMappedBy: map
	"Answer what my hash would be if oops changed according to map."

	^self key hashMappedBy: map! !

!WeakKeyAssociation methodsFor: 'comparing' stamp: 'ar 3/21/98 15:47'!
identityHashMappedBy: map
	"Answer what my hash would be if oops changed according to map."

	^ self key identityHashMappedBy: map! !

!WeakKeyAssociation methodsFor: 'accessing' stamp: 'ar 3/21/98 15:54'!
key
	^key isNil
		ifTrue:[nil]
		ifFalse:[key at: 1]! !

!WeakKeyAssociation methodsFor: 'accessing' stamp: 'ar 3/21/98 15:45'!
key: aKey
	key := WeakArray with: aKey! !

!WeakKeyAssociation methodsFor: 'accessing' stamp: 'raok 11/29/2002 14:49'!
key: aKey value: anObject
	key := WeakArray with: aKey.
	value := anObject.! !

!WeakKeyAssociation methodsFor: 'printing' stamp: 'ar 3/21/98 15:53'!
printOn: aStream
	self key printOn: aStream.
	aStream nextPutAll: '->'.
	self value printOn: aStream! !

!WeakKeyAssociation methodsFor: 'printing' stamp: 'ar 3/21/98 15:53'!
storeOn: aStream
	aStream 
		nextPut: $(;
		nextPutAll: self class name;
		nextPutAll:' key: '.
	self key storeOn: aStream.
	aStream nextPutAll: ' value: '.
	self value storeOn: aStream.
	aStream nextPut: $)! !

!LookupKey class methodsFor: 'instance creation' stamp: 'md 6/29/2005 16:34'!
key: aKey 
	"Answer an instance of me with the argument as the lookup up."

	^self basicNew key: aKey! !

!LookupKey methodsFor: 'comparing' stamp: ''!
< aLookupKey 
	"Refer to the comment in Magnitude|<."

	^key < aLookupKey key! !

!LookupKey methodsFor: 'comparing' stamp: ''!
= aLookupKey

	self species = aLookupKey species
		ifTrue: [^key = aLookupKey key]
		ifFalse: [^false]! !

!LookupKey methodsFor: 'bindings' stamp: 'ar 8/16/2001 11:59'!
beBindingOfType: aClass announcing: aBool
	"Make the receiver a global binding of the given type"
	| old new |
	(Smalltalk associationAt: self key) == self
		ifFalse:[^self error:'Not a global variable binding'].
	self class == aClass ifTrue:[^self].
	old := self.
	new := aClass key: self key value: self value.
	old become: new.
	"NOTE: Now self == read-only (e.g., the new binding)"
	^self recompileBindingsAnnouncing: aBool! !

!LookupKey methodsFor: 'bindings' stamp: 'ar 8/16/2001 11:50'!
beReadOnlyBinding
	"Make the receiver (a global read-write binding) be a read-only binding"
	^self beReadOnlyBindingAnnouncing: true! !

!LookupKey methodsFor: 'bindings' stamp: 'ar 8/16/2001 11:50'!
beReadOnlyBindingAnnouncing: aBool
	"Make the receiver (a global read-write binding) be a read-only binding"
	^self beBindingOfType: ReadOnlyVariableBinding announcing: aBool! !

!LookupKey methodsFor: 'bindings' stamp: 'ar 8/16/2001 11:50'!
beReadWriteBinding
	"Make the receiver (a global read-only binding) be a read-write binding"
	^self beReadWriteBindingAnnouncing: true! !

!LookupKey methodsFor: 'bindings' stamp: 'ar 8/16/2001 11:51'!
beReadWriteBindingAnnouncing: aBool
	"Make the receiver (a global read-write binding) be a read-write binding"
	^self beBindingOfType: Association announcing: aBool! !

!LookupKey methodsFor: 'accessing' stamp: 'ajh 9/12/2002 12:04'!
canAssign

	^ true! !

!LookupKey methodsFor: 'comparing' stamp: ''!
hash
	"Hash is reimplemented because = is implemented."

	^key hash! !

!LookupKey methodsFor: 'comparing' stamp: ''!
hashMappedBy: map
	"Answer what my hash would be if oops changed according to map."

	^key hashMappedBy: map! !

!LookupKey methodsFor: 'comparing' stamp: 'di 9/27/97 20:45'!
identityHashMappedBy: map
	"Answer what my hash would be if oops changed according to map."

	^ key identityHashMappedBy: map! !

!LookupKey methodsFor: 'testing' stamp: 'ar 8/14/2001 22:39'!
isVariableBinding
	"Return true if I represent a literal variable binding"
	^true! !

!LookupKey methodsFor: 'accessing' stamp: ''!
key
	"Answer the lookup key of the receiver."

	^key! !

!LookupKey methodsFor: 'accessing' stamp: ''!
key: anObject 
	"Store the argument, anObject, as the lookup key of the receiver."

	key := anObject! !

!LookupKey methodsFor: 'accessing' stamp: 'ajh 3/24/2003 21:14'!
name

	^ self key isString
		ifTrue: [self key]
		ifFalse: [self key printString]! !

!LookupKey methodsFor: 'printing' stamp: ''!
printOn: aStream

	key printOn: aStream! !

!LookupKey methodsFor: 'bindings' stamp: 'dvf 8/23/2003 11:50'!
recompileBindingsAnnouncing: aBool 
	"Make the receiver (a global read-write binding) be a read-only binding"

	aBool 
		ifTrue: 
			[Utilities informUserDuring: 
					[:bar | 
					(self systemNavigation allCallsOn: self) do: 
							[:mref | 
							bar value: 'Recompiling ' , mref asStringOrText.
							mref actualClass recompile: mref methodSymbol]]]
		ifFalse: 
			[(self systemNavigation allCallsOn: self) 
				do: [:mref | mref actualClass recompile: mref methodSymbol]]! !

!LookupKey methodsFor: 'filter streaming' stamp: 'MPW 1/1/1901 02:35'!
writeOnFilterStream: aStream

	aStream write:key.! !

LookupKey subclass: #ReadOnlyVariableBinding
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Support'!

!ReadOnlyVariableBinding class methodsFor: 'instance creation' stamp: 'ar 8/14/2001 23:11'!
key: key value: aValue
	^self new privateSetKey: key value: aValue! !

!ReadOnlyVariableBinding methodsFor: 'accessing' stamp: 'ajh 9/12/2002 12:06'!
canAssign

	^ false! !

!ReadOnlyVariableBinding methodsFor: 'testing' stamp: 'ar 8/14/2001 23:08'!
isSpecialWriteBinding
	"Return true if this variable binding is write protected, e.g., should not be accessed primitively but rather by sending #value: messages"
	^true! !

!ReadOnlyVariableBinding methodsFor: 'private' stamp: 'ar 8/14/2001 23:11'!
privateSetKey: aKey value: aValue
	key := aKey.
	value := aValue! !

!ReadOnlyVariableBinding methodsFor: 'accessing' stamp: 'ar 8/14/2001 23:09'!
value
	^value! !

!ReadOnlyVariableBinding methodsFor: 'accessing' stamp: 'ar 8/17/2001 18:03'!
value: aValue
	(AttemptToWriteReadOnlyGlobal signal: 'Cannot store into read-only bindings') == true ifTrue:[
		value := aValue.
	].! !

LookupKey weakSubclass: #WeakValueAssociation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Support'!

!WeakValueAssociation commentStamp: '<historical>' prior: 0!
I am a lookup key (acting like an association but) holding only weakly on my value.!

!WeakValueAssociation class methodsFor: 'instance creation' stamp: 'r++ 5/27/2000 18:07'!
key: anObject value: bObject
	^ self new key: anObject value: bObject! !

!WeakValueAssociation class methodsFor: 'as yet unclassified' stamp: 'r++ 5/27/2000 18:12'!
new
	^ self new: 1! !

!WeakValueAssociation methodsFor: 'accessing' stamp: 'r++ 5/27/2000 18:11'!
key: aKey value: anObject 
	"Store the arguments as the variables of the receiver."

	key := aKey.
	self value: anObject! !

!WeakValueAssociation methodsFor: 'accessing' stamp: 'r++ 5/27/2000 18:08'!
value
	^self at: 1! !

!WeakValueAssociation methodsFor: 'accessing' stamp: 'r++ 5/27/2000 18:08'!
value: anObject 
	"Store the argument, anObject, as the value of the receiver."

	self at: 1 put: anObject! !

Object subclass: #Collection
	instanceVariableNames: ''
	classVariableNames: 'MutexForPicking RandomForPicking'
	poolDictionaries: ''
	category: 'Collections-Abstract'!

!Collection commentStamp: '<historical>' prior: 0!
I am the abstract superclass of all classes that represent a group of elements.!

Collection subclass: #Bag
	instanceVariableNames: 'contents'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Unordered'!

!Bag commentStamp: '<historical>' prior: 0!
I represent an unordered collection of possibly duplicate elements.
	
I store these elements in a dictionary, tallying up occurrences of equal objects. Because I store an occurrence only once, my clients should beware that objects they store will not necessarily be retrieved such that == is true. If the client cares, a subclass of me should be created.!

!Bag class methodsFor: 'instance creation' stamp: 'nk 3/17/2001 09:52'!
contentsClass
	^Dictionary! !

!Bag class methodsFor: 'instance creation' stamp: 'sma 5/12/2000 13:31'!
new
	^ self new: 4! !

!Bag class methodsFor: 'instance creation' stamp: 'nk 3/17/2001 09:52'!
new: nElements
	^ super new setContents: (self contentsClass new: nElements)! !

!Bag class methodsFor: 'instance creation' stamp: 'sma 5/12/2000 17:17'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."

	^ self withAll: aCollection

"Examples:
	Bag newFrom: {1. 2. 3. 3}
	{1. 2. 3. 3} as: Bag
"! !

!Bag methodsFor: 'comparing' stamp: 'md 10/17/2004 16:09'!
= aBag
	"Two bags are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same size.
	 (c) each element occurs the same number of times in both of them"

	(aBag isKindOf: Bag) ifFalse: [^false].
	self size = aBag size ifFalse: [^false].
	contents associationsDo: [:assoc|
		(aBag occurrencesOf: assoc key) = assoc value
			ifFalse: [^false]].
	^true

! !

!Bag methodsFor: 'adding' stamp: 'sma 5/12/2000 17:18'!
add: newObject 
	"Include newObject as one of the receiver's elements. Answer newObject."

	^ self add: newObject withOccurrences: 1! !

!Bag methodsFor: 'adding' stamp: 'sma 5/12/2000 17:20'!
add: newObject withOccurrences: anInteger 
	"Add newObject anInteger times to the receiver. Answer newObject."

	contents at: newObject put: (contents at: newObject ifAbsent: [0]) + anInteger.
	^ newObject! !

!Bag methodsFor: 'converting' stamp: 'sma 5/12/2000 14:34'!
asBag
	^ self! !

!Bag methodsFor: 'converting' stamp: 'sma 5/12/2000 14:30'!
asSet
	"Answer a set with the elements of the receiver."

	^ contents keys! !

!Bag methodsFor: 'accessing' stamp: 'sma 5/12/2000 17:23'!
at: index 
	self errorNotKeyed! !

!Bag methodsFor: 'accessing' stamp: 'sma 5/12/2000 17:23'!
at: index put: anObject 
	self errorNotKeyed! !

!Bag methodsFor: 'copying' stamp: 'sma 5/12/2000 14:53'!
copy
	^ self shallowCopy setContents: contents copy! !

!Bag methodsFor: 'accessing' stamp: 'tao 1/5/2000 18:25'!
cumulativeCounts
	"Answer with a collection of cumulative percents covered by elements so far."
	| s n |
	s := self size / 100.0. n := 0.
	^ self sortedCounts asArray collect:
		[:a | n := n + a key. (n / s roundTo: 0.1) -> a value]! !

!Bag methodsFor: 'enumerating' stamp: ''!
do: aBlock 
	"Refer to the comment in Collection|do:."

	contents associationsDo: [:assoc | assoc value timesRepeat: [aBlock value: assoc key]]! !

!Bag methodsFor: 'testing' stamp: ''!
includes: anObject 
	"Refer to the comment in Collection|includes:."

	^contents includesKey: anObject! !

!Bag methodsFor: 'testing' stamp: ''!
occurrencesOf: anObject 
	"Refer to the comment in Collection|occurrencesOf:."

	(self includes: anObject)
		ifTrue: [^contents at: anObject]
		ifFalse: [^0]! !

!Bag methodsFor: 'removing' stamp: 'sma 5/12/2000 14:32'!
remove: oldObject ifAbsent: exceptionBlock 
	"Refer to the comment in Collection|remove:ifAbsent:."

	| count |
	count := contents at: oldObject ifAbsent: [^ exceptionBlock value].
	count = 1
		ifTrue: [contents removeKey: oldObject]
		ifFalse: [contents at: oldObject put: count - 1].
	^ oldObject! !

!Bag methodsFor: 'private' stamp: 'sma 5/12/2000 14:49'!
setContents: aDictionary
	contents := aDictionary! !

!Bag methodsFor: 'accessing' stamp: 'sma 5/12/2000 11:35'!
size
	"Answer how many elements the receiver contains."

	| tally |
	tally := 0.
	contents do: [:each | tally := tally + each].
	^ tally! !

!Bag methodsFor: 'accessing' stamp: 'sma 6/15/2000 17:00'!
sortedCounts
	"Answer with a collection of counts with elements, sorted by decreasing
	count."

	| counts |
	counts := SortedCollection sortBlock: [:x :y | x >= y].
	contents associationsDo:
		[:assn |
		counts add: (Association key: assn value value: assn key)].
	^ counts! !

!Bag methodsFor: 'accessing' stamp: ''!
sortedElements
	"Answer with a collection of elements with counts, sorted by element."

	| elements |
	elements := SortedCollection new.
	contents associationsDo: [:assn | elements add: assn].
	^elements! !

!Bag methodsFor: 'accessing' stamp: 'md 1/20/2006 15:58'!
valuesAndCounts

	^ contents! !

Bag subclass: #IdentityBag
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Unordered'!

!IdentityBag commentStamp: '<historical>' prior: 0!
Like a Bag, except that items are compared with #== instead of #= .

See the comment of IdentitySet for more information.
!

!IdentityBag class methodsFor: 'instance creation' stamp: 'nk 3/17/2001 09:53'!
contentsClass
	^IdentityDictionary! !

Collection subclass: #CharacterSet
	instanceVariableNames: 'map'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Support'!

!CharacterSet commentStamp: '<historical>' prior: 0!
A set of characters.  Lookups for inclusion are very fast.!

!CharacterSet class methodsFor: 'instance creation' stamp: 'ls 8/17/1998 20:42'!
allCharacters
	"return a set containing all characters"

	| set |
	set := self empty.
	0 to: 255 do: [ :ascii | set add: (Character value: ascii) ].
	^set! !

!CharacterSet class methodsFor: 'instance creation' stamp: 'nk 8/3/2004 06:54'!
empty
 	"return an empty set of characters"
	^self new! !

!CharacterSet class methodsFor: 'instance creation' stamp: 'ls 1/3/1999 12:52'!
newFrom: aCollection
	| newCollection |
	newCollection := self new.
	newCollection addAll: aCollection.
	^newCollection! !

!CharacterSet class methodsFor: 'instance creation' stamp: 'ls 8/18/1998 00:40'!
nonSeparators
	"return a set containing everything but the whitespace characters"

	^self separators complement! !

!CharacterSet class methodsFor: 'instance creation' stamp: 'ls 8/18/1998 00:40'!
separators
	"return a set containing just the whitespace characters"

	| set |
	set := self empty.
	set addAll: Character separators.
	^set! !

!CharacterSet methodsFor: 'comparison' stamp: 'tk 7/5/2001 21:58'!
= anObject
	^self species == anObject species and: [
		self byteArrayMap = anObject byteArrayMap ]! !

!CharacterSet methodsFor: 'collection ops' stamp: 'nice 5/9/2006 23:20'!
add: aCharacter
	"I automatically become a WideCharacterSet if you add a wide character to myself"
	
	aCharacter asciiValue >= 256
		ifTrue: [| wide |
			wide := WideCharacterSet new.
			wide addAll: self.
			wide add: aCharacter.
			self become: wide.
			^aCharacter].
	map at: aCharacter asciiValue + 1 put: 1.
	^aCharacter! !

!CharacterSet methodsFor: 'private' stamp: 'ls 8/17/1998 20:35'!
byteArrayMap
	"return a ByteArray mapping each ascii value to a 1 if that ascii value is in the set, and a 0 if it isn't.  Intended for use by primitives only"
	^map! !

!CharacterSet methodsFor: 'conversion' stamp: 'nice 3/23/2007 02:28'!
byteComplement
	"return a character set containing precisely the single byte characters the receiver does not"
	
	| set |
	set _ CharacterSet allCharacters.
	self do: [ :c | set remove: c ].
	^set! !

!CharacterSet methodsFor: 'conversion' stamp: 'nice 3/23/2007 02:29'!
complement
	"return a character set containing precisely the characters the receiver does not"
	
	^CharacterSetComplement of: self! !

!CharacterSet methodsFor: 'collection ops' stamp: 'ar 4/9/2005 22:37'!
do: aBlock
	"evaluate aBlock with each character in the set"

	Character allByteCharacters do: [ :c |
		(self includes: c) ifTrue: [ aBlock value: c ] ]
! !

!CharacterSet methodsFor: 'testing' stamp: 'nice 5/9/2006 23:23'!
hasWideCharacters
	^false! !

!CharacterSet methodsFor: 'comparison' stamp: 'ls 8/17/1998 20:46'!
hash
	^self byteArrayMap hash! !

!CharacterSet methodsFor: 'collection ops' stamp: 'nice 5/9/2006 23:09'!
includes: aCharacter
	aCharacter asciiValue >= 256
		ifTrue: ["Guard against wide characters"
			^false].
	^(map at: aCharacter asciiValue + 1) > 0! !

!CharacterSet methodsFor: 'private' stamp: 'ls 8/17/1998 20:30'!
initialize
	map := ByteArray new: 256 withAll: 0.! !

!CharacterSet methodsFor: 'collection ops' stamp: 'nice 5/9/2006 23:20'!
remove: aCharacter
	aCharacter asciiValue >= 256
		ifFalse: ["Guard against wide characters"
			map at: aCharacter asciiValue + 1 put: 0].
	^aCharacter! !

!CharacterSet methodsFor: 'collection ops' stamp: 'nice 5/9/2006 23:02'!
size
	^map sum! !

!CharacterSet methodsFor: 'comparison' stamp: 'tk 7/5/2001 21:57'!
species
	^CharacterSet! !

!CharacterSet methodsFor: 'private' stamp: 'nice 5/9/2006 23:22'!
wideCharacterMap
	"used for comparing with WideCharacterSet"
	
	| wide |
	wide := WideCharacterSet new.
	wide addAll: self.
	^wide wideCharacterMap! !

Collection subclass: #CharacterSetComplement
	instanceVariableNames: 'absent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Support'!

!CharacterSetComplement commentStamp: 'nice 3/23/2007 02:27' prior: 0!
CharacterSetComplement is a space efficient implementation of (CharacterSet complement) taking care of WideCharacter (code > 255)

instance variables:
	absent <CharacterSet> contains character that are not in the set (i.e. my complement)!

!CharacterSetComplement class methodsFor: 'instance creation' stamp: 'nice 3/23/2007 02:25'!
of: aCharacterSet
	"answer the complement of aCharacterSet"
	
	^ super new complement: aCharacterSet! !

!CharacterSetComplement methodsFor: 'comparing' stamp: 'nice 3/23/2007 02:19'!
= anObject
	"Implementation note: we do not test if equal to a WideCharacterSet,
	because it is unlikely that WideCharacterSet is as complete as self"
	
	^self class == anObject class and: [
		absent = anObject complement ]! !

!CharacterSetComplement methodsFor: 'collection ops' stamp: 'nice 3/23/2007 02:06'!
add: aCharacter
	"a character is present if not absent"
	
	(absent includes: aCharacter) ifTrue: [absent remove: aCharacter].
	^aCharacter! !

!CharacterSetComplement methodsFor: 'private' stamp: 'nice 3/23/2007 02:21'!
byteArrayMap
	"return a ByteArray mapping each ascii value to a 1 if that ascii value is in the set, and a 0 if it isn't.  Intended for use by primitives only"

	^absent byteArrayMap collect: [:i | 1 - i]! !

!CharacterSetComplement methodsFor: 'converting' stamp: 'nice 3/23/2007 02:08'!
complement
	"return a character set containing precisely the characters the receiver does not"
	
	^absent copy! !

!CharacterSetComplement methodsFor: 'initialize-release' stamp: 'nice 3/23/2007 02:25'!
complement: aCharacterSet
	"initialize with the complement"
	
	absent := aCharacterSet! !

!CharacterSetComplement methodsFor: 'collection ops' stamp: 'nice 3/23/2007 02:11'!
do: aBlock
	"evaluate aBlock with each character in the set.
	don't do it, there are too many..."

	self shouldNotImplement! !

!CharacterSetComplement methodsFor: 'testing' stamp: 'nice 3/23/2007 02:12'!
hasWideCharacters
	"This is a guess that absent is not holding each and every possible wideCharacter..."
	
	^true! !

!CharacterSetComplement methodsFor: 'comparing' stamp: 'nice 3/23/2007 02:16'!
hash
	^absent hash bitXOr: self class hash! !

!CharacterSetComplement methodsFor: 'collection ops' stamp: 'nice 3/23/2007 02:07'!
includes: aCharacter
	^(absent includes: aCharacter) not! !

!CharacterSetComplement methodsFor: 'collection ops' stamp: 'nice 3/23/2007 02:15'!
reject: aBlock
	"Implementation note: rejecting present is selecting absent"
	
	^(absent select: aBlock) complement! !

!CharacterSetComplement methodsFor: 'collection ops' stamp: 'nice 3/23/2007 02:06'!
remove: aCharacter
	"This means a character is absent from myself"
	
	^absent add: aCharacter! !

!CharacterSetComplement methodsFor: 'collection ops' stamp: 'nice 3/23/2007 02:15'!
select: aBlock
	"Implementation note: selecting present is rejecting absent"
	
	^(absent reject: aBlock) complement! !

!CharacterSetComplement methodsFor: 'collection ops' stamp: 'nice 3/23/2007 02:10'!
size
	"Is this 2**32-absent size ?"
	
	^self shouldNotImplement! !

!Collection class methodsFor: 'private' stamp: 'lr 11/4/2003 12:07'!
initialize
	"Set up a Random number generator to be used by atRandom when the 
	user does not feel like creating his own Random generator."

	RandomForPicking := Random new.
	MutexForPicking := Semaphore forMutualExclusion! !

!Collection class methodsFor: 'private' stamp: 'lr 11/4/2003 12:08'!
mutexForPicking
	^ MutexForPicking! !

!Collection class methodsFor: 'instance creation' stamp: 'apb 10/15/2000 22:05'!
ofSize: n
	"Create a new collection of size n with nil as its elements.
	This method exists because OrderedCollection new: n creates an
	empty collection,  not one of size n."
	^ self new: n! !

!Collection class methodsFor: 'private' stamp: 'sma 5/12/2000 12:31'!
randomForPicking
	^ RandomForPicking! !

!Collection class methodsFor: 'instance creation' stamp: 'sma 5/6/2000 19:58'!
with: anObject 
	"Answer an instance of me containing anObject."

	^ self new
		add: anObject;
		yourself! !

!Collection class methodsFor: 'instance creation' stamp: 'sma 5/6/2000 20:01'!
with: firstObject with: secondObject 
	"Answer an instance of me containing the two arguments as elements."

	^ self new
		add: firstObject;
		add: secondObject;
		yourself! !

!Collection class methodsFor: 'instance creation' stamp: 'sma 5/6/2000 20:03'!
with: firstObject with: secondObject with: thirdObject 
	"Answer an instance of me containing the three arguments as elements."

	^ self new
		add: firstObject;
		add: secondObject;
		add: thirdObject;
		yourself! !

!Collection class methodsFor: 'instance creation' stamp: 'sma 5/6/2000 20:06'!
with: firstObject with: secondObject with: thirdObject with: fourthObject 
	"Answer an instance of me, containing the four arguments as the elements."

	^ self new
		add: firstObject;
		add: secondObject;
		add: thirdObject;
		add: fourthObject;
		yourself! !

!Collection class methodsFor: 'instance creation' stamp: 'sma 5/6/2000 20:06'!
with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject
	"Answer an instance of me, containing the five arguments as the elements."

	^ self new
		add: firstObject;
		add: secondObject;
		add: thirdObject;
		add: fourthObject;
		add: fifthObject;
		yourself! !

!Collection class methodsFor: 'instance creation' stamp: 'sma 5/6/2000 20:06'!
with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject with: sixthObject
	"Answer an instance of me, containing the six arguments as the elements."

	^ self new
		add: firstObject;
		add: secondObject;
		add: thirdObject;
		add: fourthObject;
		add: fifthObject;
		add: sixthObject;
		yourself! !

!Collection class methodsFor: 'instance creation' stamp: 'sma 5/6/2000 20:07'!
withAll: aCollection
	"Create a new collection containing all the elements from aCollection."

	^ (self new: aCollection size)
		addAll: aCollection;
		yourself! !

!Collection methodsFor: 'arithmetic' stamp: 'di 11/6/1998 13:53'!
* arg

	^ arg adaptToCollection: self andSend: #*! !

!Collection methodsFor: 'arithmetic' stamp: 'di 11/6/1998 13:53'!
+ arg

	^ arg adaptToCollection: self andSend: #+! !

!Collection methodsFor: 'copying' stamp: 'al 12/12/2003 14:31'!
, aCollection
	^self copy addAll: aCollection; yourself! !

!Collection methodsFor: 'arithmetic' stamp: 'di 11/6/1998 13:53'!
- arg

	^ arg adaptToCollection: self andSend: #-! !

!Collection methodsFor: 'arithmetic' stamp: 'di 11/6/1998 13:53'!
/ arg

	^ arg adaptToCollection: self andSend: #/! !

!Collection methodsFor: 'arithmetic' stamp: 'di 11/6/1998 13:54'!
// arg

	^ arg adaptToCollection: self andSend: #//! !

!Collection methodsFor: 'arithmetic' stamp: 'di 11/6/1998 13:54'!
\\ arg

	^ arg adaptToCollection: self andSend: #\\! !

!Collection methodsFor: 'math functions' stamp: 'TAG 11/6/1998 15:51'!
abs
	"Absolute value of all elements in the collection"
	^ self collect: [:a | a abs]! !

!Collection methodsFor: 'adapting' stamp: 'di 11/6/1998 13:34'!
adaptToCollection: rcvr andSend: selector
	"If I am involved in arithmetic with another Collection, return a Collection of
	the results of each element combined with the scalar in that expression."

	rcvr isSequenceable & self isSequenceable ifFalse:
		[self error: 'Only sequenceable collections may be combined arithmetically'].
	^ rcvr with: self collect:
		[:rcvrElement :myElement | rcvrElement perform: selector with: myElement]! !

!Collection methodsFor: 'adapting' stamp: 'mk 10/27/2003 21:48'!
adaptToComplex: rcvr andSend: selector
	"If I am involved in arithmetic with a scalar, return a Collection of
	the results of each element combined with the scalar in that expression."

	^ self collect: [:element | rcvr perform: selector with: element]! !

!Collection methodsFor: 'adapting' stamp: 'di 11/9/1998 12:16'!
adaptToNumber: rcvr andSend: selector
	"If I am involved in arithmetic with a scalar, return a Collection of
	the results of each element combined with the scalar in that expression."

	^ self collect: [:element | rcvr perform: selector with: element]! !

!Collection methodsFor: 'adapting' stamp: 'di 11/6/1998 13:37'!
adaptToPoint: rcvr andSend: selector
	"If I am involved in arithmetic with a scalar, return a Collection of
	the results of each element combined with the scalar in that expression."

	^ self collect: [:element | rcvr perform: selector with: element]! !

!Collection methodsFor: 'adapting' stamp: 'di 11/6/1998 13:37'!
adaptToString: rcvr andSend: selector
	"If I am involved in arithmetic with a String, convert it to a Number."
	^ rcvr asNumber perform: selector with: self! !

!Collection methodsFor: 'adding' stamp: ''!
add: newObject 
	"Include newObject as one of the receiver's elements. Answer newObject. 
	ArrayedCollections cannot respond to this message."

	self subclassResponsibility! !

!Collection methodsFor: 'adding' stamp: 'sma 5/12/2000 17:21'!
add: newObject withOccurrences: anInteger
	"Add newObject anInteger times to the receiver. Answer newObject."

	anInteger timesRepeat: [self add: newObject].
	^ newObject! !

!Collection methodsFor: 'adding' stamp: 'sma 5/12/2000 17:26'!
addAll: aCollection 
	"Include all the elements of aCollection as the receiver's elements. Answer 
	aCollection. Actually, any object responding to #do: can be used as argument."

	aCollection do: [:each | self add: each].
	^ aCollection! !

!Collection methodsFor: 'adding' stamp: 'sma 5/12/2000 17:23'!
addIfNotPresent: anObject
	"Include anObject as one of the receiver's elements, but only if there
	is no such element already. Anwser anObject."

	(self includes: anObject) ifFalse: [self add: anObject].
	^ anObject! !

!Collection methodsFor: 'enumerating' stamp: 'sma 4/30/2000 11:17'!
allSatisfy: aBlock
	"Evaluate aBlock with the elements of the receiver.
	If aBlock returns false for any element return false.
	Otherwise return true."

	self do: [:each | (aBlock value: each) ifFalse: [^ false]].
	^ true! !

!Collection methodsFor: 'accessing' stamp: 'sma 5/12/2000 11:33'!
anyOne
	"Answer a representative sample of the receiver. This method can
	be helpful when needing to preinfer the nature of the contents of 
	semi-homogeneous collections."

	self emptyCheck.
	self do: [:each | ^ each]! !

!Collection methodsFor: 'enumerating' stamp: 'sma 4/30/2000 11:17'!
anySatisfy: aBlock
	"Evaluate aBlock with the elements of the receiver.
	If aBlock returns true for any element return true.
	Otherwise return false."

	self do: [:each | (aBlock value: each) ifTrue: [^ true]].
	^ false! !

!Collection methodsFor: 'math functions' stamp: 'raok 10/22/2002 00:20'!
arcCos
	^self collect: [:each | each arcCos]! !

!Collection methodsFor: 'math functions' stamp: 'raok 10/22/2002 00:20'!
arcSin
	^self collect: [:each | each arcSin]! !

!Collection methodsFor: 'math functions' stamp: 'raok 10/22/2002 00:20'!
arcTan
	^self collect: [:each | each arcTan]! !

!Collection methodsFor: 'converting' stamp: 'sma 5/6/2000 20:22'!
asArray
	"Answer an Array whose elements are the elements of the receiver.
	Implementation note: Cannot use ''Array withAll: self'' as that only
	works for SequenceableCollections which support the replacement 
	primitive."

	| array index |
	array := Array new: self size.
	index := 0.
	self do: [:each | array at: (index := index + 1) put: each].
	^ array! !

!Collection methodsFor: 'converting' stamp: 'sma 5/6/2000 20:10'!
asBag
	"Answer a Bag whose elements are the elements of the receiver."

	^ Bag withAll: self! !

!Collection methodsFor: 'converting' stamp: 'sma 5/6/2000 20:22'!
asByteArray
	"Answer a ByteArray whose elements are the elements of the receiver.
	Implementation note: Cannot use ''ByteArray withAll: self'' as that only
	works for SequenceableCollections which support the replacement 
	primitive."

	| array index |
	array := ByteArray new: self size.
	index := 0.
	self do: [:each | array at: (index := index + 1) put: each].
	^ array! !

!Collection methodsFor: 'converting' stamp: 'sma 5/6/2000 20:26'!
asCharacterSet
	"Answer a CharacterSet whose elements are the unique elements of the receiver.
	The reciever should only contain characters."

	^ CharacterSet newFrom: self! !

!Collection methodsFor: 'printing' stamp: 'gk 1/14/2005 08:19'!
asCommaString
	"Return collection printed as 'a, b, c' "

	^String streamContents: [:s | self asStringOn: s delimiter: ', ']
		! !

!Collection methodsFor: 'printing' stamp: 'gk 1/14/2005 08:20'!
asCommaStringAnd
	"Return collection printed as 'a, b and c' "

	^String streamContents: [:s | self asStringOn: s delimiter: ', ' last: ' and ']
		! !

!Collection methodsFor: 'converting' stamp: 'ar 9/22/2000 10:12'!
asIdentitySet
	^(IdentitySet new: self size) addAll: self; yourself! !

!Collection methodsFor: 'converting' stamp: 'LC 6/18/2001 20:30'!
asIdentitySkipList
	"Answer a IdentitySkipList whose elements are the elements of the 
	receiver. The sort order is the default less than or equal."

	^ self as: IdentitySkipList! !

!Collection methodsFor: 'converting' stamp: 'sma 5/12/2000 17:43'!
asOrderedCollection
	"Answer an OrderedCollection whose elements are the elements of the
	receiver. The order in which elements are added depends on the order
	in which the receiver enumerates its elements. In the case of unordered
	collections, the ordering is not necessarily the same for multiple 
	requests for the conversion."

	^ self as: OrderedCollection! !

!Collection methodsFor: 'converting' stamp: 'sma 5/6/2000 20:29'!
asSet
	"Answer a Set whose elements are the unique elements of the receiver."

	^ Set withAll: self! !

!Collection methodsFor: 'converting' stamp: 'LC 6/18/2001 18:47'!
asSkipList
	"Answer a SkipList whose elements are the elements of the 
	receiver. The sort order is the default less than or equal."

	^ self as: SkipList! !

!Collection methodsFor: 'converting' stamp: 'LC 6/18/2001 18:46'!
asSkipList: aSortBlock 
	"Answer a SkipList whose elements are the elements of the 
	receiver. The sort order is defined by the argument, aSortBlock."

	| skipList |
	skipList := SortedCollection new: self size.
	skipList sortBlock: aSortBlock.
	skipList addAll: self.
	^ skipList! !

!Collection methodsFor: 'converting' stamp: ''!
asSortedArray
	"Return a copy of the receiver in sorted order, as an Array.  6/10/96 sw"

	^ self asSortedCollection asArray! !

!Collection methodsFor: 'converting' stamp: 'sma 5/12/2000 17:44'!
asSortedCollection
	"Answer a SortedCollection whose elements are the elements of the 
	receiver. The sort order is the default less than or equal."

	^ self as: SortedCollection! !

!Collection methodsFor: 'converting' stamp: 'sma 5/12/2000 17:46'!
asSortedCollection: aSortBlock 
	"Answer a SortedCollection whose elements are the elements of the 
	receiver. The sort order is defined by the argument, aSortBlock."

	| aSortedCollection |
	aSortedCollection := SortedCollection new: self size.
	aSortedCollection sortBlock: aSortBlock.
	aSortedCollection addAll: self.
	^ aSortedCollection! !

!Collection methodsFor: 'printing' stamp: 'gk 1/14/2005 08:27'!
asStringOn: aStream delimiter: delimString
	"Print elements on a stream separated
	with a delimiter String like: 'a, b, c'
	Uses #asString instead of #print:."

	self do: [:elem | aStream nextPutAll: elem asString]
		separatedBy: [aStream nextPutAll: delimString]! !

!Collection methodsFor: 'printing' stamp: 'gk 1/14/2005 08:27'!
asStringOn: aStream delimiter: delimString last: lastDelimString
	"Print elements on a stream separated
	with a delimiter between all the elements and with
	a special one before the last like: 'a, b and c'.
	Uses #asString instead of #print:

	Note: Feel free to improve the code to detect the last element."

	| n sz |
	n := 1.
	sz := self size.
	self do: [:elem |
		n := n + 1.
		aStream nextPutAll: elem asString]
	separatedBy: [
		aStream nextPutAll: (n = sz ifTrue: [lastDelimString] ifFalse: [delimString])]! !

!Collection methodsFor: 'enumerating' stamp: ''!
associationsDo: aBlock
	"Evaluate aBlock for each of the receiver's elements (key/value 
	associations).  If any non-association is within, the error is not caught now,
	but later, when a key or value message is sent to it."

	self do: aBlock! !

!Collection methodsFor: 'accessing' stamp: 'sd 11/4/2003 22:05'!
atRandom
	"Answer a random element of the receiver.  Uses a shared random 
	number generator owned by class Collection.  If you use this a lot, 
	define your own instance of Random and use #atRandom:.  Causes 
	an error if self has no elements."

	^ self class mutexForPicking critical: [
		self atRandom: self class randomForPicking ]

"Examples:
	#('one' 'or' 'the' 'other') atRandom
	(1 to: 10) atRandom
	'Just pick one of these letters at random' atRandom
	#(3 7 4 9 21) asSet atRandom		(just to show it also works for Sets)
"! !

!Collection methodsFor: 'math functions' stamp: 'TAG 11/6/1998 15:57'!
average
	^ self sum / self size! !

!Collection methodsFor: 'accessing' stamp: 'sma 5/12/2000 11:41'!
capacity
	"Answer the current capacity of the receiver."

	^ self size! !

!Collection methodsFor: 'math functions' stamp: 'TAG 11/6/1998 15:51'!
ceiling
	^ self collect: [:a | a ceiling]! !

!Collection methodsFor: 'enumerating' stamp: 'sma 5/12/2000 11:45'!
collect: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into a collection like the receiver. Answer  
	the new collection."

	| newCollection |
	newCollection := self species new.
	self do: [:each | newCollection add: (aBlock value: each)].
	^ newCollection! !

!Collection methodsFor: 'enumerating' stamp: 'dgd 9/13/2004 23:42'!
collect: collectBlock thenDo: doBlock 
	"Utility method to improve readability."
	^ (self collect: collectBlock) do: doBlock! !

!Collection methodsFor: 'enumerating' stamp: 'sma 5/12/2000 11:51'!
collect: collectBlock thenSelect: selectBlock
	"Utility method to improve readability."

	^ (self collect: collectBlock) select: selectBlock! !

!Collection methodsFor: 'testing' stamp: ''!
contains: aBlock
	"VW compatibility"
	^self anySatisfy: aBlock! !

!Collection methodsFor: 'filter streaming' stamp: 'sma 5/12/2000 12:07'!
contents
	^ self! !

!Collection methodsFor: 'copying' stamp: 'sma 5/12/2000 14:41'!
copyWith: newElement
	"Answer a new collection with newElement added (as last
	element if sequenceable)."

	^ self copy
		add: newElement;
		yourself! !

!Collection methodsFor: 'copying' stamp: 'ar 2/11/2001 01:55'!
copyWithDependent: newElement
	"Answer a new collection with newElement added (as last
	element if sequenceable)."
	^self copyWith: newElement! !

!Collection methodsFor: 'copying' stamp: 'sma 5/12/2000 14:43'!
copyWithout: oldElement 
	"Answer a copy of the receiver that does not contain any
	elements equal to oldElement."

	^ self reject: [:each | each = oldElement]

"Examples:
	'fred the bear' copyWithout: $e
	#(2 3 4 5 5 6) copyWithout: 5
"! !

!Collection methodsFor: 'copying' stamp: 'sma 5/12/2000 18:08'!
copyWithoutAll: aCollection
	"Answer a copy of the receiver that does not contain any elements 
	equal to those in aCollection."

	^ self reject: [:each | aCollection includes: each]! !

!Collection methodsFor: 'math functions' stamp: 'raok 10/22/2002 00:20'!
cos
	^self collect: [:each | each cos]! !

!Collection methodsFor: 'enumerating' stamp: 'sma 5/12/2000 11:52'!
count: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Answer the number of elements that answered true."

	| sum |
	sum := 0.
	self do: [:each | (aBlock value: each) ifTrue: [sum := sum + 1]].
	^ sum! !

!Collection methodsFor: 'math functions' stamp: 'raok 10/22/2002 00:20'!
degreeCos
	^self collect: [:each | each degreeCos]! !

!Collection methodsFor: 'math functions' stamp: 'raok 10/22/2002 00:21'!
degreeSin
	^self collect: [:each | each degreeSin]! !

!Collection methodsFor: 'enumerating' stamp: 'sma 5/12/2000 11:20'!
detect: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the first element for which aBlock evaluates to true."

	^ self detect: aBlock ifNone: [self errorNotFound: aBlock]! !

!Collection methodsFor: 'enumerating' stamp: 'sma 5/12/2000 11:52'!
detect: aBlock ifNone: exceptionBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Answer the first element for which aBlock evaluates to true. If none  
	evaluate to true, then evaluate the argument, exceptionBlock."

	self do: [:each | (aBlock value: each) ifTrue: [^ each]].
	^ exceptionBlock value! !

!Collection methodsFor: 'enumerating' stamp: ''!
detectMax: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the element for which aBlock evaluates to the highest magnitude.
	If collection empty, return nil.  This method might also be called elect:."

	| maxElement maxValue val |
	self do: [:each | 
		maxValue == nil
			ifFalse: [
				(val := aBlock value: each) > maxValue ifTrue: [
					maxElement := each.
					maxValue := val]]
			ifTrue: ["first element"
				maxElement := each.
				maxValue := aBlock value: each].
				"Note that there is no way to get the first element that works 
				for all kinds of Collections.  Must test every one."].
	^ maxElement! !

!Collection methodsFor: 'enumerating' stamp: ''!
detectMin: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Answer the element for which aBlock evaluates to the lowest number.
	If collection empty, return nil."

	| minElement minValue val |
	self do: [:each | 
		minValue == nil
			ifFalse: [
				(val := aBlock value: each) < minValue ifTrue: [
					minElement := each.
					minValue := val]]
			ifTrue: ["first element"
				minElement := each.
				minValue := aBlock value: each].
				"Note that there is no way to get the first element that works 
				for all kinds of Collections.  Must test every one."].
	^ minElement! !

!Collection methodsFor: 'enumerating' stamp: ''!
detectSum: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Return the sum of the answers."
	| sum |
	sum := 0.
	self do: [:each | 
		sum := (aBlock value: each) + sum].  
	^ sum! !

!Collection methodsFor: 'enumerating' stamp: 'sma 5/12/2000 17:52'!
difference: aCollection
	"Answer the set theoretic difference of two collections."

	^ self reject: [:each | aCollection includes: each]! !

!Collection methodsFor: 'enumerating' stamp: ''!
do: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument."

	self subclassResponsibility! !

!Collection methodsFor: 'enumerating' stamp: 'md 7/22/2005 16:26'!
do: aBlock displayingProgress: aString

	aString
		displayProgressAt: Sensor cursorPoint
		from: 0 to: self size
		during:
			[:bar |
			self inject: 1 into:
				[:index :each |
				bar value: index.
				aBlock value: each.
				index + 1]]! !

!Collection methodsFor: 'enumerating' stamp: 'sma 5/12/2000 11:57'!
do: elementBlock separatedBy: separatorBlock
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	| beforeFirst | 
	beforeFirst := true.
	self do:
		[:each |
		beforeFirst
			ifTrue: [beforeFirst := false]
			ifFalse: [separatorBlock value].
		elementBlock value: each]! !

!Collection methodsFor: 'enumerating' stamp: 'sma 5/12/2000 11:59'!
do: aBlock without: anItem 
	"Enumerate all elements in the receiver. 
	Execute aBlock for those elements that are not equal to the given item"

	^ self do: [:each | anItem = each ifFalse: [aBlock value: each]]! !

!Collection methodsFor: 'private' stamp: ''!
emptyCheck

	self isEmpty ifTrue: [self errorEmptyCollection]! !

!Collection methodsFor: 'private' stamp: ''!
errorEmptyCollection

	self error: 'this collection is empty'! !

!Collection methodsFor: 'private' stamp: ''!
errorNoMatch

	self error: 'collection sizes do not match'! !

!Collection methodsFor: 'private' stamp: 'sma 5/12/2000 11:22'!
errorNotFound: anObject
	"Actually, this should raise a special Exception not just an error."

	self error: 'Object is not in the collection.'! !

!Collection methodsFor: 'private' stamp: 'yo 6/29/2004 13:14'!
errorNotKeyed

	self error: ('Instances of {1} do not respond to keyed accessing messages.' translated format: {self class name})
! !

!Collection methodsFor: 'math functions' stamp: 'raok 10/22/2002 00:21'!
exp
	^self collect: [:each | each exp]! !

!Collection methodsFor: 'filter streaming' stamp: 'sma 5/12/2000 12:08'!
flattenOnStream: aStream 
	^ aStream writeCollection: self! !

!Collection methodsFor: 'math functions' stamp: 'TAG 11/6/1998 15:51'!
floor
	^ self collect: [:a | a floor]! !

!Collection methodsFor: 'enumerating' stamp: 'dvf 6/10/2000 18:32'!
groupBy: keyBlock having: selectBlock 
	"Like in SQL operation - Split the recievers contents into collections of 
	elements for which keyBlock returns the same results, and return those 
	collections allowed by selectBlock. keyBlock should return an Integer."
	| result key |
	result := PluggableDictionary integerDictionary.
	self do: 
		[:e | 
		key := keyBlock value: e.
		(result includesKey: key)
			ifFalse: [result at: key put: OrderedCollection new].
		(result at: key)
			add: e].
	^ result := result select: selectBlock! !

!Collection methodsFor: 'comparing' stamp: 'SqR 8/3/2000 13:36'!
hash
	"Answer an integer hash value for the receiver such that,
	  -- the hash value of an unchanged object is constant over time, and
	  -- two equal objects have equal hash values"

	| hash |

	hash := self species hash.
	self size <= 10 ifTrue:
		[self do: [:elem | hash := hash bitXor: elem hash]].
	^hash bitXor: self size hash! !

!Collection methodsFor: 'testing' stamp: 'ls 3/27/2000 17:25'!
identityIncludes: anObject 
	"Answer whether anObject is one of the receiver's elements."

	self do: [:each | anObject == each ifTrue: [^true]].
	^false! !

!Collection methodsFor: 'testing' stamp: 'jf 12/1/2003 15:37'!
ifEmpty: aBlock
	"Evaluate the block if I'm empty"

	^ self isEmpty ifTrue: aBlock! !

!Collection methodsFor: 'testing' stamp: 'md 10/7/2004 14:49'!
ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise"
	" If the notEmptyBlock has an argument, eval with the receiver as its argument"

	^ self isEmpty ifTrue: emptyBlock ifFalse: [notEmptyBlock valueWithPossibleArgument: self]! !

!Collection methodsFor: 'testing' stamp: 'md 10/7/2004 15:36'!
ifEmpty: emptyBlock ifNotEmptyDo: notEmptyBlock
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise"
	"Evaluate the notEmptyBlock with the receiver as its argument"

	^ self isEmpty ifTrue: emptyBlock ifFalse: [notEmptyBlock value: self]! !

!Collection methodsFor: 'testing' stamp: 'md 10/7/2004 14:58'!
ifNotEmpty: aBlock
	"Evaluate the given block unless the receiver is empty.

      If the block has an argument, eval with the receiver as its argument,
      but it might be better to use ifNotEmptyDo: to make the code easier to
      understand"

	^self isEmpty ifFalse: [aBlock valueWithPossibleArgument: self].
! !

!Collection methodsFor: 'testing' stamp: 'md 10/7/2004 14:48'!
ifNotEmpty: notEmptyBlock ifEmpty: emptyBlock
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise
	 If the notEmptyBlock has an argument, eval with the receiver as its argument"

	^ self isEmpty ifFalse: [notEmptyBlock valueWithPossibleArgument: self] ifTrue: emptyBlock! !

!Collection methodsFor: 'testing' stamp: 'md 10/7/2004 14:28'!
ifNotEmptyDo: aBlock
	"Evaluate the given block with the receiver as its argument."

	^self isEmpty ifFalse: [aBlock value: self].
! !

!Collection methodsFor: 'testing' stamp: 'md 10/7/2004 15:36'!
ifNotEmptyDo: notEmptyBlock ifEmpty: emptyBlock
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise
	Evaluate the notEmptyBlock with the receiver as its argument"

	^ self isEmpty ifFalse: [notEmptyBlock value: self] ifTrue: emptyBlock! !

!Collection methodsFor: 'testing' stamp: 'sma 5/12/2000 14:07'!
includes: anObject 
	"Answer whether anObject is one of the receiver's elements."

	^ self anySatisfy: [:each | each = anObject]! !

!Collection methodsFor: 'testing' stamp: ''!
includesAllOf: aCollection 
	"Answer whether all the elements of aCollection are in the receiver."
	aCollection do: [:elem | (self includes: elem) ifFalse: [^ false]].
	^ true! !

!Collection methodsFor: 'testing' stamp: ''!
includesAnyOf: aCollection 
	"Answer whether any element of aCollection is one of the receiver's elements."
	aCollection do: [:elem | (self includes: elem) ifTrue: [^ true]].
	^ false! !

!Collection methodsFor: 'testing' stamp: 'nk 8/30/2004 07:49'!
includesSubstringAnywhere: testString
	"Answer whether the receiver includes, anywhere in its nested structure, a string that has testString as a substring"
	self do:
		[:element |
			(element isString)
				ifTrue:
					[(element includesSubString: testString) ifTrue: [^ true]].
			(element isCollection)
				ifTrue:
					[(element includesSubstringAnywhere: testString) ifTrue: [^ true]]].
	^ false

"#(first (second third) ((allSentMessages ('Elvis' includes:)))) includesSubstringAnywhere:  'lvi'"! !

!Collection methodsFor: 'enumerating' stamp: ''!
inject: thisValue into: binaryBlock 
	"Accumulate a running value associated with evaluating the argument, 
	binaryBlock, with the current value of the argument, thisValue, and the 
	receiver as block arguments. For instance, to sum the numeric elements 
	of a collection, aCollection inject: 0 into: [:subTotal :next | subTotal + 
	next]."

	| nextValue |
	nextValue := thisValue.
	self do: [:each | nextValue := binaryBlock value: nextValue value: each].
	^nextValue! !

!Collection methodsFor: 'enumerating' stamp: 'sma 5/12/2000 17:52'!
intersection: aCollection
	"Answer the set theoretic intersection of two collections."

	^ self select: [:each | aCollection includes: each]! !

!Collection methodsFor: 'testing' stamp: 'ar 8/17/1999 19:43'!
isCollection
	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"
	^true! !

!Collection methodsFor: 'testing' stamp: ''!
isEmpty
	"Answer whether the receiver contains any elements."

	^self size = 0! !

!Collection methodsFor: 'testing' stamp: 'bf 3/10/2000 09:29'!
isEmptyOrNil
	"Answer whether the receiver contains any elements, or is nil.  Useful in numerous situations where one wishes the same reaction to an empty collection or to nil"

	^ self isEmpty! !

!Collection methodsFor: 'testing' stamp: 'di 11/6/1998 09:16'!
isSequenceable
	^ false! !

!Collection methodsFor: 'testing' stamp: 'dgd 4/4/2004 12:14'!
isZero
	"Answer whether the receiver is zero"
	^ false! !

!Collection methodsFor: 'math functions' stamp: 'raok 10/22/2002 00:21'!
ln
	^self collect: [:each | each ln]! !

!Collection methodsFor: 'math functions' stamp: 'TAG 11/6/1998 15:52'!
log
	^ self collect: [:each | each log]! !

!Collection methodsFor: 'math functions' stamp: 'TAG 11/6/1998 15:58'!
max
	^ self inject: self anyOne into: [:max :each | max max: each]! !

!Collection methodsFor: 'math functions' stamp: 'TAG 11/6/1998 16:00'!
median
	^ self asSortedCollection median! !

!Collection methodsFor: 'math functions' stamp: 'TAG 11/6/1998 16:00'!
min
	^ self inject: self anyOne into: [:min :each | min min: each]! !

!Collection methodsFor: 'math functions' stamp: 'TAG 11/6/1998 15:52'!
negated
	"Negated value of all elements in the collection"
	^ self collect: [:a | a negated]! !

!Collection methodsFor: 'enumerating' stamp: 'gh 9/18/2001 15:59'!
noneSatisfy: aBlock
	"Evaluate aBlock with the elements of the receiver.
	If aBlock returns false for all elements return true.
	Otherwise return false"

	self do: [:item | (aBlock value: item) ifTrue: [^ false]].
	^ true! !

!Collection methodsFor: 'testing' stamp: 'sma 5/12/2000 17:49'!
notEmpty
	"Answer whether the receiver contains any elements."

	^ self isEmpty not! !

!Collection methodsFor: 'testing' stamp: ''!
occurrencesOf: anObject 
	"Answer how many of the receiver's elements are equal to anObject."

	| tally |
	tally := 0.
	self do: [:each | anObject = each ifTrue: [tally := tally + 1]].
	^tally! !

!Collection methodsFor: 'printing' stamp: 'apb 4/21/2006 09:37'!
printElementsOn: aStream
	"The original code used #skip:, but some streams do not support that,
	 and we don't really need it."

	aStream nextPut: $(.
	self do: [:element | aStream print: element] separatedBy: [aStream space].
	aStream nextPut: $)! !

!Collection methodsFor: 'printing' stamp: 'sma 6/1/2000 09:41'!
printNameOn: aStream
	super printOn: aStream! !

!Collection methodsFor: 'printing' stamp: 'sma 6/1/2000 09:41'!
printOn: aStream 
	"Append a sequence of characters that identify the receiver to aStream."

	self printNameOn: aStream.
	self printElementsOn: aStream! !

!Collection methodsFor: 'printing' stamp: 'gk 1/14/2005 08:01'!
printOn: aStream delimiter: delimString
	"Print elements on a stream separated
	with a delimiter String like: 'a, b, c' "

	self do: [:elem | aStream print: elem] separatedBy: [aStream print: delimString]
		! !

!Collection methodsFor: 'printing' stamp: 'fbs 1/14/2005 10:54'!
printOn: aStream delimiter: delimString last: lastDelimString
	"Print elements on a stream separated
	with a delimiter between all the elements and with
	a special one before the last like: 'a, b and c'

	Note: Feel free to improve the code to detect the last element."

	| n sz |
	n := 1.
	sz := self size.
	self do: [:elem |
		n := n + 1.
		aStream print: elem]
	separatedBy: [
		n = sz
			ifTrue: [aStream print: lastDelimString]
			ifFalse: [aStream print: delimString]]! !

!Collection methodsFor: 'arithmetic' stamp: 'raok 10/22/2002 00:17'!
raisedTo: arg

	^ arg adaptToCollection: self andSend: #raisedTo:! !

!Collection methodsFor: 'math functions' stamp: 'TAG 11/6/1998 16:00'!
range
	^ self max - self min! !

!Collection methodsFor: 'math functions' stamp: 'TAG 11/6/1998 15:53'!
reciprocal
	"Return the reciever full of reciprocated elements"
	^ self collect: [:a | a reciprocal]! !

!Collection methodsFor: 'enumerating' stamp: ''!
reject: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Collect into a new collection like the receiver only those elements for 
	which aBlock evaluates to false. Answer the new collection."

	^self select: [:element | (aBlock value: element) == false]! !

!Collection methodsFor: 'enumerating' stamp: 'dgd 9/13/2004 23:42'!
reject: rejectBlock thenDo: doBlock 
	"Utility method to improve readability."
	^ (self reject: rejectBlock) do: doBlock! !

!Collection methodsFor: 'removing' stamp: 'sma 5/12/2000 11:22'!
remove: oldObject 
	"Remove oldObject from the receiver's elements. Answer oldObject 
	unless no element is equal to oldObject, in which case, raise an error.
	ArrayedCollections cannot respond to this message."

	^ self remove: oldObject ifAbsent: [self errorNotFound: oldObject]! !

!Collection methodsFor: 'removing' stamp: 'sma 5/12/2000 11:14'!
remove: oldObject ifAbsent: anExceptionBlock 
	"Remove oldObject from the receiver's elements. If several of the 
	elements are equal to oldObject, only one is removed. If no element is 
	equal to oldObject, answer the result of evaluating anExceptionBlock. 
	Otherwise, answer the argument, oldObject. ArrayedCollections cannot 
	respond to this message."

	self subclassResponsibility! !

!Collection methodsFor: 'removing' stamp: 'sma 5/12/2000 11:14'!
removeAll: aCollection 
	"Remove each element of aCollection from the receiver. If successful for 
	each, answer aCollection. Otherwise create an error notification.
	ArrayedCollections cannot respond to this message."

	aCollection do: [:each | self remove: each].
	^ aCollection! !

!Collection methodsFor: 'removing' stamp: 'sma 5/12/2000 11:16'!
removeAllFoundIn: aCollection 
	"Remove each element of aCollection which is present in the receiver 
	from the receiver. Answer aCollection. No error is raised if an element
	isn't found. ArrayedCollections cannot respond to this message."

	aCollection do: [:each | self remove: each ifAbsent: []].
	^ aCollection! !

!Collection methodsFor: 'removing' stamp: 'sma 5/12/2000 11:19'!
removeAllSuchThat: aBlock 
	"Evaluate aBlock for each element and remove all that elements from
	the receiver for that aBlock evaluates to true.  Use a copy to enumerate 
	collections whose order changes when an element is removed (i.e. Sets)."

	self copy do: [:each | (aBlock value: each) ifTrue: [self remove: each]]! !

!Collection methodsFor: 'math functions' stamp: 'nk 12/30/2003 15:47'!
roundTo: quantum
	^self collect: [ :ea | ea roundTo: quantum ]! !

!Collection methodsFor: 'math functions' stamp: 'TAG 11/6/1998 15:53'!
rounded
	^ self collect: [:a | a rounded]! !

!Collection methodsFor: 'enumerating' stamp: ''!
select: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Collect into a new collection like the receiver, only those elements for 
	which aBlock evaluates to true. Answer the new collection."

	| newCollection |
	newCollection := self species new.
	self do: [:each | (aBlock value: each) ifTrue: [newCollection add: each]].
	^newCollection! !

!Collection methodsFor: 'enumerating' stamp: 'sma 5/12/2000 11:59'!
select: selectBlock thenCollect: collectBlock
	"Utility method to improve readability."

	^ (self select: selectBlock) collect: collectBlock! !

!Collection methodsFor: 'enumerating' stamp: 'dgd 9/13/2004 23:42'!
select: selectBlock thenDo: doBlock 
	"Utility method to improve readability."
	^ (self select: selectBlock) do: doBlock! !

!Collection methodsFor: 'math functions' stamp: 'raok 10/22/2002 00:23'!
sign
	^self collect: [:each | each sign]! !

!Collection methodsFor: 'math functions' stamp: 'raok 10/22/2002 00:22'!
sin
	^self collect: [:each | each sin]! !

!Collection methodsFor: 'accessing' stamp: 'sma 5/12/2000 11:34'!
size
	"Answer how many elements the receiver contains."

	| tally |
	tally := 0.
	self do: [:each | tally := tally + 1].
	^ tally! !

!Collection methodsFor: 'math functions' stamp: 'TAG 11/6/1998 15:53'!
sqrt
	^ self collect: [:each | each sqrt]! !

!Collection methodsFor: 'math functions' stamp: 'TAG 11/6/1998 15:53'!
squared
	^ self collect: [:each | each * each]! !

!Collection methodsFor: 'printing' stamp: ''!
storeOn: aStream 
	"Refer to the comment in Object|storeOn:."

	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new)'.
	noneYet := true.
	self do: 
		[:each | 
		noneYet
			ifTrue: [noneYet := false]
			ifFalse: [aStream nextPut: $;].
		aStream nextPutAll: ' add: '.
		aStream store: each].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)! !

!Collection methodsFor: 'math functions' stamp: 'TAG 11/6/1998 16:02'!
sum
	"This is implemented using a variant of the normal inject:into: pattern. 
	The reason for this is that it is not known whether we're in the normal 
	number line, i.e. whether 0 is a good initial value for the sum. 
	Consider a collection of measurement objects, 0 would be the unitless 
	value and would not be appropriate to add with the unit-ed objects."
	| sum sample |
	sample := self anyOne.
	sum := self inject: sample into: [:accum :each | accum + each].
	^ sum - sample! !

!Collection methodsFor: 'math functions' stamp: 'raok 10/22/2002 00:22'!
tan
	^self collect: [:each | each tan]! !

!Collection methodsFor: 'private' stamp: ''!
toBraceStack: itsSize 
	"Push receiver's elements onto the stack of thisContext sender.  Error if receiver does
	 not have itsSize elements or if receiver is unordered.
	 Do not call directly: this is called by {a. b} := ... constructs."

	self size ~= itsSize ifTrue:
		[self error: 'Trying to store ', self size printString,
					' values into ', itsSize printString, ' variables.'].
	thisContext sender push: itsSize fromIndexable: self! !

!Collection methodsFor: 'converting' stamp: 'hg 12/26/2001 23:53'!
topologicallySortedUsing: aSortBlock 
	"Answer a SortedCollection whose elements are the elements of the 
	receiver, but topologically sorted. The topological order is defined 
	by the argument, aSortBlock."

	| aSortedCollection |
	aSortedCollection := SortedCollection new: self size.
	aSortedCollection sortBlock: aSortBlock.
	self do: [:each | aSortedCollection addLast: each].	"avoids sorting"
	^ aSortedCollection sortTopologically
! !

!Collection methodsFor: 'math functions' stamp: 'TAG 11/6/1998 15:54'!
truncated
	^ self collect: [:a | a truncated]! !

!Collection methodsFor: 'enumerating' stamp: 'sma 5/12/2000 17:54'!
union: aCollection
	"Answer the set theoretic union of two collections."

	^ self asSet addAll: aCollection; yourself! !

!Collection methodsFor: 'filter streaming' stamp: 'sma 5/12/2000 12:07'!
write: anObject 
	^ self add: anObject! !

Collection subclass: #Matrix
	instanceVariableNames: 'nrows ncols contents'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Unordered'!

!Matrix commentStamp: '<historical>' prior: 0!
I represent a two-dimensional array, rather like Array2D.
There are three main differences between me and Array2D:
(1) Array2D inherits from ArrayedCollection, but isn't one.  A lot of things that should work
    do not work in consequence of this.
(2) Array2D uses "at: column at: row" index order, which means that nothing you write using
    it is likely to work either.  I use the almost universal "at: row at: column" order, so it is
    much easier to adapt code from other languages without going doolally.
(3) Array2D lets you specify the class of the underlying collection, I don't.

Structure:
  nrows : a non-negative integer saying how many rows there are.
  ncols : a non-negative integer saying how many columns there are.
  contents : an Array holding the elements in row-major order.  That is, for a 2x3 array
    the contents are (11 12 13 21 22 23).  Array2D uses column major order.

    You can specify the class of 'contents' when you create a new Array2D,
    but Matrix always gives you an Array.

    There is a reason for this.  In strongly typed languages like Haskell and Clean,
    'unboxed arrays' save you both space AND time.  But in Squeak, while
    WordArray and FloatArray and so on do save space, it costs time to use them.
    A LOT of time.  I've measured aFloatArray sum running nearly twice as slow as
    anArray sum.  The reason is that whenever you fetch an element from an Array,
    that's all that happens, but when you fetch an element from aFloatArray, a whole
    new Float gets allocated to hold the value.  This takes time and churns memory.
    So the paradox is that if you want fast numerical stuff, DON'T use unboxed arrays!!

    Another reason for always insisting on an Array is that letting it be something
    else would make things like #, and #,, rather more complicated.  Always using Array
    is the simplest thing that could possibly work, and it works rather well.

I was trying to patch Array2D to make more things work, but just couldn't get my head
around the subscript order.  That's why I made Matrix.

Element-wise matrix arithmetic works; you can freely mix matrices and numbers but
don't try to mix matrices and arrays (yet).
Matrix multiplication, using the symbol +* (derived from APL's +.x), works between
(Matrix or Array) +* (Matrix or Array).  Don't try to use a number as an argument of +*.
Matrix * Number and Number * Matrix work fine, so you don't need +* with numbers.

Still to come: oodles of stuff.  Gaussian elimination maybe, other stuff probably not.
!

!Matrix class methodsFor: 'instance creation' stamp: 'raok 10/23/2002 20:58'!
column: aCollection
	"Should this be called #fromColumn:?"

	^self rows: aCollection size columns: 1 contents: aCollection asArray shallowCopy! !

!Matrix class methodsFor: 'instance creation' stamp: 'raok 10/22/2002 00:09'!
diagonal: aCollection
	|r i|
	r := self zeros: aCollection size.
	i := 0.
	aCollection do: [:each | i := i+1. r at: i at: i put: each].
	^r! !

!Matrix class methodsFor: 'instance creation' stamp: 'raok 10/23/2002 20:59'!
identity: n
	|r|

	r := self zeros: n.
	1 to: n do: [:i | r at: i at: i put: 1].
	^r! !

!Matrix class methodsFor: 'instance creation' stamp: 'raok 10/22/2002 00:06'!
new: dim
	"Answer a dim*dim matrix.  Is this an abuse of #new:?  The argument is NOT a size."
	^self rows: dim columns: dim! !

!Matrix class methodsFor: 'instance creation' stamp: 'raok 11/25/2002 12:51'!
new: dim element: element
	"Answer a dim*dim matrix with all elements set to element.
	 Is this an abuse of #new:?  The argument is NOT a size."

	^self rows: dim columns: dim element: element! !

!Matrix class methodsFor: 'instance creation' stamp: 'raok 10/22/2002 19:54'!
new: dim tabulate: aBlock
	"Answer a dim*dim matrix where it at: i at: j is aBlock value: i value: j."
	^self rows: dim columns: dim tabulate: aBlock! !

!Matrix class methodsFor: 'instance creation' stamp: 'raok 11/28/2002 14:08'!
ones: n
	^self new: n element: 1
! !

!Matrix class methodsFor: 'instance creation' stamp: 'raok 10/23/2002 20:59'!
row: aCollection
	"Should this be called #fromRow:?"

	^self rows: 1 columns: aCollection size contents: aCollection asArray shallowCopy! !

!Matrix class methodsFor: 'instance creation' stamp: 'raok 10/22/2002 00:04'!
rows: rows columns: columns
	^self rows: rows columns: columns contents: (Array new: rows*columns)! !

!Matrix class methodsFor: 'private' stamp: 'raok 10/21/2002 23:06'!
rows: rows columns: columns contents: contents
	^self new rows: rows columns: columns contents: contents! !

!Matrix class methodsFor: 'instance creation' stamp: 'raok 11/28/2002 14:10'!
rows: rows columns: columns element: element
	^self rows: rows columns: columns
		contents: ((Array new: rows*columns) atAllPut: element; yourself)! !

!Matrix class methodsFor: 'instance creation' stamp: 'raok 10/22/2002 19:51'!
rows: rows columns: columns tabulate: aBlock
	"Answer a new Matrix of the given dimensions where
	 result at: i at: j     is   aBlock value: i value: j"
	|a i|

	a := Array new: rows*columns.
	i := 0.
	1 to: rows do: [:row |
		1 to: columns do: [:column |
			a at: (i := i+1) put: (aBlock value: row value: column)]].
	^self rows: rows columns: columns contents: a
! !

!Matrix class methodsFor: 'instance creation' stamp: 'raok 11/28/2002 14:09'!
zeros: n
	^self new: n element: 0! !

!Matrix methodsFor: 'arithmetic' stamp: 'raok 10/22/2002 20:01'!
+* aCollection
	"Premultiply aCollection by self.  aCollection should be an Array or Matrix.
	 The name of this method is APL's +.x squished into Smalltalk syntax."

	^aCollection preMultiplyByMatrix: self
! !

!Matrix methodsFor: 'copying' stamp: 'raok 11/22/2002 12:57'!
, aMatrix
	"Answer a new matrix having the same number of rows as the receiver and aMatrix,
	 its columns being the columns of the receiver followed by the columns of aMatrix."
	|newCont newCols anArray oldCols a b c|

	self assert: [nrows = aMatrix rowCount].
	newCont := Array new: self size + aMatrix size.
	anArray := aMatrix privateContents.
	oldCols := aMatrix columnCount.
	newCols := ncols + oldCols.
	a := b := c := 1.
	1 to: nrows do: [:r |
		newCont replaceFrom: a to: a+ncols-1 with: contents startingAt: b.
		newCont replaceFrom: a+ncols to: a+newCols-1 with: anArray startingAt: c.
		a := a + newCols.
		b := b + ncols.
		c := c + oldCols].
	^self class rows: nrows columns: newCols contents: newCont
		
! !

!Matrix methodsFor: 'copying' stamp: 'raok 11/22/2002 12:58'!
,, aMatrix
	"Answer a new matrix having the same number of columns as the receiver and aMatrix,
	 its rows being the rows of the receiver followed by the rows of aMatrix."

	self assert: [ncols = aMatrix columnCount].
	^self class rows: nrows + aMatrix rowCount columns: ncols
		contents: contents , aMatrix privateContents
! !

!Matrix methodsFor: 'comparing' stamp: 'raok 11/22/2002 12:58'!
= aMatrix
	^aMatrix class == self class and: [
	 aMatrix rowCount = nrows and: [
	 aMatrix columnCount = ncols and: [
	 aMatrix privateContents = contents]]]! !

!Matrix methodsFor: 'adding' stamp: 'raok 10/21/2002 22:53'!
add: newObject
	self shouldNotImplement! !

!Matrix methodsFor: 'accessing' stamp: 'raok 10/21/2002 22:37'!
anyOne
	^contents anyOne! !

!Matrix methodsFor: 'converting' stamp: 'raok 10/21/2002 22:57'!
asArray
	^contents shallowCopy! !

!Matrix methodsFor: 'converting' stamp: 'raok 10/21/2002 22:57'!
asBag
	^contents asBag! !

!Matrix methodsFor: 'converting' stamp: 'raok 10/21/2002 22:58'!
asByteArray
	^contents asByteArray! !

!Matrix methodsFor: 'converting' stamp: 'raok 10/21/2002 22:58'!
asCharacterSet
	^contents asCharacterSet! !

!Matrix methodsFor: 'converting' stamp: 'raok 10/21/2002 23:00'!
asFloatArray
	^contents asFloatArray! !

!Matrix methodsFor: 'converting' stamp: 'raok 10/21/2002 22:58'!
asIdentitySet
	^contents asIdentitySet! !

!Matrix methodsFor: 'converting' stamp: 'raok 10/21/2002 23:00'!
asIntegerArray
	^contents asIntegerArray! !

!Matrix methodsFor: 'converting' stamp: 'raok 10/21/2002 22:58'!
asOrderedCollection
	^contents asOrderedCollection! !

!Matrix methodsFor: 'converting' stamp: 'raok 10/21/2002 22:58'!
asSet
	^contents asSet! !

!Matrix methodsFor: 'converting' stamp: 'raok 10/21/2002 22:58'!
asSortedArray
	^contents asSortedArray! !

!Matrix methodsFor: 'converting' stamp: 'raok 10/21/2002 22:59'!
asSortedCollection
	^contents asSortedCollection! !

!Matrix methodsFor: 'converting' stamp: 'raok 10/21/2002 22:59'!
asSortedCollection: aBlock
	^contents asSortedCollection: aBlock! !

!Matrix methodsFor: 'converting' stamp: 'raok 10/21/2002 23:00'!
asWordArray
	^contents asWordArray! !

!Matrix methodsFor: 'accessing' stamp: 'raok 10/21/2002 22:37'!
at: row at: column
	^contents at: (self indexForRow: row andColumn: column)! !

!Matrix methodsFor: 'accessing' stamp: 'raok 11/28/2002 14:14'!
at: r at: c ifInvalid: v
	"If r,c is a valid index for this matrix, answer the corresponding element.
	 Otherwise, answer v."

	(r between: 1 and: nrows) ifFalse: [^v].
	(c between: 1 and: ncols) ifFalse: [^v].
	^contents at: (r-1)*ncols + c
! !

!Matrix methodsFor: 'accessing' stamp: 'raok 11/22/2002 12:37'!
at: row at: column incrementBy: value
	"Array2D>>at:at:add: was the origin of this method, but in Smalltalk add:
	 generally suggests adding an element to a collection, not doing a sum.
	 This method, and SequenceableCollection>>at:incrementBy: that supports
	 it, have been renamed to reveal their intention more clearly."

	^contents at: (self indexForRow: row andColumn: column) incrementBy: value! !

!Matrix methodsFor: 'accessing' stamp: 'raok 10/21/2002 22:40'!
at: row at: column put: value
	^contents at: (self indexForRow: row andColumn: column) put: value! !

!Matrix methodsFor: 'accessing' stamp: 'raok 10/21/2002 22:42'!
atAllPut: value
	contents atAllPut: value! !

!Matrix methodsFor: 'accessing rows/columns' stamp: 'raok 11/22/2002 12:41'!
atColumn: column
	|p|

	p := (self indexForRow: 1 andColumn: column)-ncols.
	^(1 to: nrows) collect: [:row | contents at: (p := p+ncols)]
! !

!Matrix methodsFor: 'accessing rows/columns' stamp: 'raok 11/28/2002 14:21'!
atColumn: column put: aCollection
	|p|

	aCollection size = nrows ifFalse: [self error: 'wrong column size'].
	p := (self indexForRow: 1 andColumn: column)-ncols.
	aCollection do: [:each | contents at: (p := p+ncols) put: each].
	^aCollection
! !

!Matrix methodsFor: 'accessing' stamp: 'raok 10/21/2002 22:43'!
atRandom
	^contents atRandom
! !

!Matrix methodsFor: 'accessing' stamp: 'raok 10/21/2002 22:43'!
atRandom: aGenerator
	^contents atRandom: aGenerator! !

!Matrix methodsFor: 'accessing rows/columns' stamp: 'raok 10/21/2002 23:32'!
atRow: row
	(row between: 1 and: nrows)
		ifFalse: [self error: '1st subscript out of range'].
	^contents copyFrom: (row-1)*ncols+1 to: row*ncols! !

!Matrix methodsFor: 'accessing rows/columns' stamp: 'raok 11/22/2002 12:42'!
atRow: row put: aCollection
	|p|

	aCollection size = ncols ifFalse: [self error: 'wrong row size'].
	p := (self indexForRow: row andColumn: 1)-1.
	aCollection do: [:each | contents at: (p := p+1) put: each].
	^aCollection! !

!Matrix methodsFor: 'accessing submatrices' stamp: 'raok 11/25/2002 13:09'!
atRows: rs columns: cs
	"Answer a Matrix obtained by slicing the receiver.
	 rs and cs should be sequenceable collections of positive integers."

	^self class rows: rs size columns: cs size tabulate: [:r :c |
		self at: (rs at: r) at: (cs at: c)]! !

!Matrix methodsFor: 'accessing submatrices' stamp: 'raok 11/25/2002 12:30'!
atRows: r1 to: r2 columns: c1 to: c2
	"Answer a submatrix [r1..r2][c1..c2] of the receiver."
	|rd cd|

	rd := r1 - 1.
	cd := c1 - 1.
	^self class rows: r2-rd columns: c2-cd tabulate: [:r :c| self at: r+rd at: c+cd]
! !

!Matrix methodsFor: 'accessing submatrices' stamp: 'raok 11/25/2002 13:05'!
atRows: r1 to: r2 columns: c1 to: c2 ifInvalid: element
	"Answer a submatrix [r1..r2][c1..c2] of the receiver.
	 Portions of the result outside the bounds of the original matrix
	 are filled in with element."
	|rd cd|

	rd := r1 - 1.
	cd := c1 - 1.
	^self class rows: r2-rd columns: c2-cd tabulate: [:r :c| self at: r+rd at: c+cd ifInvalid: element]
! !

!Matrix methodsFor: 'accessing submatrices' stamp: 'raok 11/25/2002 12:32'!
atRows: r1 to: r2 columns: c1 to: c2 put: aMatrix
	"Set the [r1..r2][c1..c2] submatrix of the receiver
	 from the [1..r2-r1+1][1..c2-c1+1] submatrix of aMatrix.
	 As long as aMatrix responds to at:at: and accepts arguments in the range shown,
	 we don't care if it is bigger or even if it is a Matrix at all."
	|rd cd|

	rd := r1 - 1.
	cd := c1 - 1.
	r1 to: r2 do: [:r |
		c1 to: c2 do: [:c |
			self at: r at: c put: (aMatrix at: r-rd at: c-cd)]].
	^aMatrix
! !

!Matrix methodsFor: 'enumerating' stamp: 'raok 10/21/2002 23:41'!
collect: aBlock
	"Answer a new matrix with transformed elements; transformations should be independent."

	^self class rows: nrows columns: ncols contents: (contents collect: aBlock)! !

!Matrix methodsFor: 'accessing' stamp: 'raok 10/21/2002 22:44'!
columnCount
	^ncols! !

!Matrix methodsFor: 'copying' stamp: 'raok 10/21/2002 23:07'!
copy
	^self class rows: nrows columns: ncols contents: contents copy! !

!Matrix methodsFor: 'accessing rows/columns' stamp: 'raok 10/23/2002 20:41'!
diagonal
	"Answer (1 to: (nrows min: ncols)) collect: [:i | self at: i at: i]"
	|i|

	i := ncols negated.
	^(1 to: (nrows min: ncols)) collect: [:j | contents at: (i := i + ncols + 1)]! !

!Matrix methodsFor: 'enumerating' stamp: 'raok 10/21/2002 23:42'!
difference: aCollection
	"Union is in because the result is always a Set.
	 Difference and intersection are out because the result is like the receiver,
	 and with irregular seleection that cannot be."
	self shouldNotImplement! !

!Matrix methodsFor: 'enumerating' stamp: 'raok 10/21/2002 23:40'!
do: aBlock
	"Pass elements to aBlock one at a time in row-major order."
	contents do: aBlock! !

!Matrix methodsFor: 'comparing' stamp: 'raok 11/22/2002 13:14'!
hash
	"I'm really not sure what would be a good hash function here.
	 The essential thing is that it must be compatible with #=, and
	 this satisfies that requirement."

	^contents hash! !

!Matrix methodsFor: 'testing' stamp: 'raok 10/21/2002 23:24'!
identityIncludes: anObject
	^contents identityIncludes: anObject! !

!Matrix methodsFor: 'accessing' stamp: 'raok 10/21/2002 22:48'!
identityIndexOf: anElement
	^self identityIndexOf: anElement ifAbsent: [0@0]
! !

!Matrix methodsFor: 'accessing' stamp: 'raok 10/21/2002 22:48'!
identityIndexOf: anElement ifAbsent: anExceptionBlock
	^self rowAndColumnForIndex:
		 (contents identityIndexOf: anElement ifAbsent: [^anExceptionBlock value])
! !

!Matrix methodsFor: 'testing' stamp: 'raok 10/21/2002 23:23'!
includes: anObject
	^contents includes: anObject! !

!Matrix methodsFor: 'testing' stamp: 'raok 10/21/2002 23:24'!
includesAllOf: aCollection
	^contents includesAllOf: aCollection! !

!Matrix methodsFor: 'testing' stamp: 'raok 10/21/2002 23:24'!
includesAnyOf: aCollection
	^contents includesAnyOf: aCollection! !

!Matrix methodsFor: 'private' stamp: 'raok 10/21/2002 22:40'!
indexForRow: row andColumn: column
	(row between: 1 and: nrows)
		ifFalse: [self error: '1st subscript out of range'].
	(column between: 1 and: ncols)
		ifFalse: [self error: '2nd subscript out of range'].
	^(row-1) * ncols + column! !

!Matrix methodsFor: 'accessing' stamp: 'raok 11/22/2002 13:13'!
indexOf: anElement
	"If there are integers r, c such that (self at: r at: c) = anElement,
	 answer some such r@c, otherwise answer 0@0.  This kind of perverse
	 result is provided by analogy with SequenceableCollection>>indexOf:.
	 The order in which the receiver are searched is UNSPECIFIED except
	 that it is the same as the order used by #indexOf:ifAbsent: and #readStream."

	^self indexOf: anElement ifAbsent: [0@0]
! !

!Matrix methodsFor: 'accessing' stamp: 'raok 11/22/2002 13:10'!
indexOf: anElement ifAbsent: anExceptionBlock
	"If there are integers r, c such that (self at: r at: c) = anElement,
	 answer some such r@c, otherwise answer the result of anExceptionBlock."

	^self rowAndColumnForIndex:
		 (contents indexOf: anElement ifAbsent: [^anExceptionBlock value])
! !

!Matrix methodsFor: 'enumerating' stamp: 'raok 10/23/2002 20:57'!
indicesCollect: aBlock
	|r i|

	r := Array new: nrows * ncols.
	i := 0.
	1 to: nrows do: [:row |
		1 to: ncols do: [:column |
			r at: (i := i+1) put: (aBlock value: row value: column)]].
	^self class rows: nrows columns: ncols contents: r! !

!Matrix methodsFor: 'enumerating' stamp: 'raok 10/21/2002 23:49'!
indicesDo: aBlock
	1 to: nrows do: [:row |
		1 to: ncols do: [:column |
			aBlock value: row value: column]].! !

!Matrix methodsFor: 'enumerating' stamp: 'raok 10/21/2002 23:51'!
indicesInject: start into: aBlock
	|current|

	current := start.
	1 to: nrows do: [:row |
		1 to: ncols do: [:column |
			current := aBlock value: current value: row value: column]].
	^current! !

!Matrix methodsFor: 'enumerating' stamp: 'raok 10/21/2002 23:42'!
intersection: aCollection
	"Union is in because the result is always a Set.
	 Difference and intersection are out because the result is like the receiver,
	 and with irregular seleection that cannot be."
	self shouldNotImplement! !

!Matrix methodsFor: 'testing' stamp: 'raok 11/22/2002 13:03'!
isSequenceable
	"LIE so that arithmetic on matrices will work.
	 What matters for arithmetic is not that there should be random indexing
	 but that the structure should be stable and independent of the values of
	 the elements.  #isSequenceable is simply the wrong question to ask."
	^true! !

!Matrix methodsFor: 'testing' stamp: 'raok 10/21/2002 23:25'!
occurrencesOf: anObject
	^contents occurrencesOf: anObject! !

!Matrix methodsFor: 'arithmetic' stamp: 'raok 11/28/2002 14:22'!
preMultiplyByArray: a
	"Answer a +* self where a is an Array."

	nrows = 1 ifFalse: [self error: 'dimensions do not conform'].
	^Matrix rows: a size columns: ncols tabulate: [:row :col |
		(a at: row) * (contents at: col)]
! !

!Matrix methodsFor: 'arithmetic' stamp: 'raok 10/22/2002 20:02'!
preMultiplyByMatrix: m
	"Answer m +* self where m is a Matrix."
	|s|

	nrows = m columnCount ifFalse: [self error: 'dimensions do not conform'].
	^Matrix rows: m rowCount columns: ncols tabulate: [:row :col |
		s := 0.
		1 to: nrows do: [:k | s := (m at: row at: k) * (self at: k at: col) + s].
		s]! !

!Matrix methodsFor: 'private' stamp: 'raok 11/22/2002 12:56'!
privateContents
	"Only used in #, #,, and #= so far.
	 It used to be called #contents, but that clashes with Collection>>contents."

	^contents! !

!Matrix methodsFor: 'converting' stamp: 'raok 11/22/2002 13:02'!
readStream
	"Answer a ReadStream that returns all the elements of the receiver
	 in some UNSPECIFIED order."

	^ReadStream on: contents! !

!Matrix methodsFor: 'enumerating' stamp: 'raok 10/21/2002 23:42'!
reject: aBlock
	self shouldNotImplement! !

!Matrix methodsFor: 'removing' stamp: 'raok 10/21/2002 22:54'!
remove: anObject ifAbsent: anExceptionBlock
	self shouldNotImplement! !

!Matrix methodsFor: 'accessing' stamp: 'raok 10/21/2002 22:49'!
replaceAll: oldObject with: newObject
	contents replaceAll: oldObject with: newObject! !

!Matrix methodsFor: 'private' stamp: 'raok 10/21/2002 22:47'!
rowAndColumnForIndex: index
	|t|

	t := index - 1.
	^(t // ncols + 1)@(t \\ ncols + 1)! !

!Matrix methodsFor: 'accessing' stamp: 'raok 10/21/2002 22:44'!
rowCount
	^nrows! !

!Matrix methodsFor: 'private' stamp: 'raok 10/21/2002 23:05'!
rows: rows columns: columns contents: anArray
	self assert: [rows isInteger and: [rows >= 0]].
	self assert: [columns isInteger and: [columns >= 0]].
	self assert: [rows * columns = anArray size].
	nrows := rows.
	ncols := columns.
	contents := anArray.
	^self! !

!Matrix methodsFor: 'enumerating' stamp: 'raok 10/21/2002 23:42'!
select: aBlock
	self shouldNotImplement! !

!Matrix methodsFor: 'copying' stamp: 'raok 10/21/2002 23:07'!
shallowCopy
	^self class rows: nrows columns: ncols contents: contents shallowCopy! !

!Matrix methodsFor: 'copying' stamp: 'raok 10/21/2002 23:27'!
shuffled
	^self class rows: nrows columns: ncols contents: (contents shuffled)! !

!Matrix methodsFor: 'copying' stamp: 'raok 10/21/2002 23:27'!
shuffledBy: aRandom
	^self class rows: nrows columns: ncols contents: (contents shuffledBy: aRandom)! !

!Matrix methodsFor: 'accessing' stamp: 'raok 10/21/2002 22:49'!
size
	^contents size! !

!Matrix methodsFor: 'printing' stamp: 'raok 10/21/2002 23:22'!
storeOn: aStream
	aStream nextPut: $(; nextPutAll: self class name;
		nextPutAll: ' rows: '; store: nrows;
		nextPutAll: ' columns: '; store: ncols;
		nextPutAll: ' contents: '; store: contents;
		nextPut: $)! !

!Matrix methodsFor: 'accessing' stamp: 'raok 10/21/2002 22:52'!
swap: r1 at: c1 with: r2 at: c2
	contents swap: (self indexForRow: r1 andColumn: c1)
			 with: (self indexForRow: r2 andColumn: c2)! !

!Matrix methodsFor: 'accessing rows/columns' stamp: 'raok 11/28/2002 14:21'!
swapColumn: anIndex withColumn: anotherIndex
	|a b|

	a := self indexForRow: 1 andColumn: anIndex.
	b := self indexForRow: 1 andColumn: anotherIndex.
	nrows timesRepeat: [
		contents swap: a with: b.
		a := a + ncols.
		b := b + ncols].
! !

!Matrix methodsFor: 'accessing rows/columns' stamp: 'raok 11/28/2002 14:22'!
swapRow: anIndex withRow: anotherIndex
	|a b|

	a := self indexForRow: anIndex andColumn: 1.
	b := self indexForRow: anotherIndex andColumn: 1.
	ncols timesRepeat: [
		contents swap: a with: b.
		a := a + 1.
		b := b + 1].
! !

!Matrix methodsFor: 'accessing rows/columns' stamp: 'raok 10/22/2002 00:13'!
transposed
	self assert: [nrows = ncols].
	^self indicesCollect: [:row :column | self at: column at: row]! !

!Matrix methodsFor: 'enumerating' stamp: 'raok 10/22/2002 00:15'!
with: aCollection collect: aBlock
	"aCollection must support #at:at: and be at least as large as the receiver."

	^self withIndicesCollect: [:each :row :column |
		aBlock value: each value: (aCollection at: row at: column)]
! !

!Matrix methodsFor: 'enumerating' stamp: 'raok 10/21/2002 23:53'!
with: aCollection do: aBlock
	"aCollection must support #at:at: and be at least as large as the receiver."

	self withIndicesDo: [:each :row :column |
		aBlock value: each value: (aCollection at: row at: column)].
! !

!Matrix methodsFor: 'enumerating' stamp: 'raok 10/21/2002 23:55'!
with: aCollection inject: startingValue into: aBlock
	"aCollection must support #at:at: and be at least as large as the receiver."

	^self withIndicesInject: startingValue into: [:value :each :row :column |
		aBlock value: value value: each value: (aCollection at: row at: column)]! !

!Matrix methodsFor: 'enumerating' stamp: 'raok 10/21/2002 23:52'!
withIndicesCollect: aBlock
	|i r|

	i := 0.
	r := contents shallowCopy.
	1 to: nrows do: [:row |
		1 to: ncols do: [:column |
			i := i+1.
			r at: i put: (aBlock value: (r at: i) value: row value: column)]].
	^self class rows: nrows columns: ncols contents: r
! !

!Matrix methodsFor: 'enumerating' stamp: 'raok 10/21/2002 23:52'!
withIndicesDo: aBlock
	|i|

	i := 0.
	1 to: nrows do: [:row |
		1 to: ncols do: [:column |
			aBlock value: (contents at: (i := i+1)) value: row value: column]].
! !

!Matrix methodsFor: 'enumerating' stamp: 'raok 10/21/2002 23:52'!
withIndicesInject: start into: aBlock
	|i current|

	i := 0.
	current := start.
	1 to: nrows do: [:row |
		1 to: ncols do: [:column |
			current := aBlock value: current value: (contents at: (i := i+1)) 
							  value: row value: column]].
	^current! !

Collection subclass: #SequenceableCollection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Abstract'!

!SequenceableCollection commentStamp: '<historical>' prior: 0!
I am an abstract superclass for collections that have a well-defined order associated with their elements. Thus each element is externally-named by integers referred to as indices.!

SequenceableCollection subclass: #ArrayedCollection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Abstract'!

!ArrayedCollection commentStamp: '<historical>' prior: 0!
I am an abstract collection of elements with a fixed range of integers (from 1 to n>=0) as external keys.!

ArrayedCollection variableSubclass: #Array
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

!Array commentStamp: '<historical>' prior: 0!
I present an ArrayedCollection whose elements are objects.!

!Array class methodsFor: 'brace support' stamp: 'di 11/18/1999 22:53'!
braceStream: nElements
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	^ WriteStream basicNew braceArray: (self new: nElements)
! !

!Array class methodsFor: 'brace support' stamp: 'di 11/19/1999 08:16'!
braceWith: a
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	| array |
	array := self new: 1.
	array at: 1 put: a.
	^ array! !

!Array class methodsFor: 'brace support' stamp: 'di 11/19/1999 08:15'!
braceWith: a with: b 
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	| array |
	array := self new: 2.
	array at: 1 put: a.
	array at: 2 put: b.
	^ array! !

!Array class methodsFor: 'brace support' stamp: 'di 11/19/1999 08:17'!
braceWith: a with: b with: c 
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	| array |
	array := self new: 3.
	array at: 1 put: a.
	array at: 2 put: b.
	array at: 3 put: c.
	^ array! !

!Array class methodsFor: 'brace support' stamp: 'di 11/19/1999 08:17'!
braceWith: a with: b with: c with: d
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	| array |
	array := self new: 4.
	array at: 1 put: a.
	array at: 2 put: b.
	array at: 3 put: c.
	array at: 4 put: d.
	^ array! !

!Array class methodsFor: 'brace support' stamp: 'di 11/19/1999 08:16'!
braceWithNone
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	^ self new: 0! !

!Array class methodsFor: 'instance creation' stamp: 'md 7/19/2004 12:34'!
new: sizeRequested 
	"Answer an instance of this class with the number of indexable
	variables specified by the argument, sizeRequested.
	
	This is a shortcut (direct call of primitive, no #initialize, for performance"

	<primitive: 71>  "This method runs primitively if successful"
	^ self basicNew: sizeRequested  "Exceptional conditions will be handled in basicNew:"
! !

!Array methodsFor: 'arithmetic' stamp: 'raok 10/22/2002 20:09'!
+* aCollection
	"Premultiply aCollection by self.  aCollection should be an Array or Matrix.
	 The name of this method is APL's +.x squished into Smalltalk syntax."

	^aCollection preMultiplyByArray: self
! !

!Array methodsFor: 'converting' stamp: 'sma 5/12/2000 17:32'!
asArray
	"Answer with the receiver itself."

	^ self! !

!Array methodsFor: 'accessing' stamp: 'ar 8/26/2001 22:02'!
atWrap: index 
	"Optimized to go through the primitive if possible"
	<primitive: 60>
	^ self at: index - 1 \\ self size + 1! !

!Array methodsFor: 'accessing' stamp: 'ar 8/26/2001 22:03'!
atWrap: index put: anObject
	"Optimized to go through the primitive if possible"
	<primitive: 61>
	^ self at: index - 1 \\ self size + 1 put: anObject! !

!Array methodsFor: 'filter streaming' stamp: 'MPW 1/1/1901 01:42'!
byteEncode:aStream
	aStream writeArray:self.
! !

!Array methodsFor: 'copying' stamp: 'ar 2/11/2001 01:55'!
copyWithDependent: newElement
	self size = 0 ifTrue:[^DependentsArray with: newElement].
	^self copyWith: newElement! !

!Array methodsFor: 'converting' stamp: 'tpr 11/2/2004 11:31'!
elementsExchangeIdentityWith: otherArray
	"This primitive performs a bulk mutation, causing all pointers to the elements of this array to be replaced by pointers to the corresponding elements of otherArray.  At the same time, all pointers to the elements of otherArray are replaced by pointers to the corresponding elements of this array.  The identityHashes remain with the pointers rather than with the objects so that objects in hashed structures should still be properly indexed after the mutation."

	<primitive: 128>
	otherArray class == Array ifFalse: [^ self error: 'arg must be array'].
	self size = otherArray size ifFalse: [^ self error: 'arrays must be same size'].
	(self anySatisfy: [:obj | obj class == SmallInteger]) ifTrue: [^ self error: 'can''t become SmallIntegers'].
	(otherArray anySatisfy: [:obj | obj class == SmallInteger]) ifTrue: [^ self error: 'can''t become SmallIntegers'].
	self with: otherArray do:[:a :b| a == b ifTrue:[^self error:'can''t become yourself']].

	"Must have failed because not enough space in forwarding table (see ObjectMemory-prepareForwardingTableForBecoming:with:twoWay:).  Do GC and try again only once"
	(Smalltalk bytesLeft: true) = Smalltalk primitiveGarbageCollect
		ifTrue: [^ self primitiveFailed].
	^ self elementsExchangeIdentityWith: otherArray! !

!Array methodsFor: 'converting' stamp: 'di 3/28/1999 10:23'!
elementsForwardIdentityTo: otherArray
	"This primitive performs a bulk mutation, causing all pointers to the elements of this array to be replaced by pointers to the corresponding elements of otherArray.  The identityHashes remain with the pointers rather than with the objects so that the objects in this array should still be properly indexed in any existing hashed structures after the mutation."
	<primitive: 72>
	self primitiveFailed! !

!Array methodsFor: 'converting' stamp: 'brp 9/26/2003 08:09'!
elementsForwardIdentityTo: otherArray copyHash: copyHash
	"This primitive performs a bulk mutation, causing all pointers to the elements of this array to be replaced by pointers to the corresponding elements of otherArray.  The identityHashes remain with the pointers rather than with the objects so that the objects in this array should still be properly indexed in any existing hashed structures after the mutation."
	<primitive: 249>
	self primitiveFailed! !

!Array methodsFor: 'converting' stamp: 'ar 4/10/2005 18:03'!
evalStrings
	   "Allows you to construct literal arrays.
    #(true false nil '5@6' 'Set new' '''text string''') evalStrings
    gives an array with true, false, nil, a Point, a Set, and a String
    instead of just a bunch of Symbols"
    | it |

    ^ self collect: [:each |
        it := each.
        each == #true ifTrue: [it := true].
		      each == #false ifTrue: [it := false].
        each == #nil ifTrue: [it := nil].
        (each isString and:[each isSymbol not]) ifTrue: [
			it := Compiler evaluate: each].
        each class == Array ifTrue: [it := it evalStrings].
        it]! !

!Array methodsFor: 'private' stamp: 'sma 6/3/2000 21:39'!
hasLiteral: literal
	"Answer true if literal is identical to any literal in this array, even 
	if imbedded in further array structure. This method is only intended 
	for private use by CompiledMethod hasLiteralSymbol:"

	| lit |
	1 to: self size do: 
		[:index | 
		(lit := self at: index) == literal ifTrue: [^ true].
		(lit class == Array and: [lit hasLiteral: literal]) ifTrue: [^ true]].
	^ false! !

!Array methodsFor: 'private' stamp: 'md 3/1/2006 21:09'!
hasLiteralSuchThat: testBlock
	"Answer true if testBlock returns true for any literal in this array, even if imbedded in 	further Arrays or CompiledMethods.  This method is only intended for private use by 	CompiledMethod 	hasLiteralSuchThat:"
	| lit |
	1 to: self size do: [:index |
		(testBlock value: (lit := self at: index)) ifTrue: [^ true].
		(lit hasLiteralSuchThat: testBlock) ifTrue: [^ true]].
	^ false! !

!Array methodsFor: 'private' stamp: 'md 1/20/2006 16:53'!
hasLiteralThorough: literal
	"Answer true if literal is identical to any literal in this array, even if imbedded in further array structures or closure methods"

	| lit |
	1 to: self size do: [:index |
		(lit := self at: index) == literal ifTrue: [^ true].
		(lit hasLiteralThorough: literal) ifTrue: [^ true]].
	^ false! !

!Array methodsFor: 'comparing' stamp: ''!
hashMappedBy: map
	"Answer what my hash would be if oops changed according to map."

	self size = 0 ifTrue: [^self hash].
	^(self first hashMappedBy: map) + (self last hashMappedBy: map)! !

!Array methodsFor: 'testing' stamp: 'md 7/30/2005 21:19'!
isArray
	^true! !

!Array methodsFor: 'testing' stamp: 'sma 5/12/2000 14:11'!
isLiteral
	^ self allSatisfy: [:each | each isLiteral]! !

!Array methodsFor: 'self evaluating' stamp: 'sd 7/31/2005 21:44'!
isSelfEvaluating
	^ (self allSatisfy: [:each | each isSelfEvaluating]) and: [self class == Array]! !

!Array methodsFor: 'comparing' stamp: 'ajh 2/2/2002 15:03'!
literalEqual: other

	self class == other class ifFalse: [^ false].
	self size = other size ifFalse: [^ false].
	self with: other do: [:e1 :e2 |
		(e1 literalEqual: e2) ifFalse: [^ false]].
	^ true! !

!Array methodsFor: 'file in/out' stamp: 'tk 9/28/2000 15:35'!
objectForDataStream: refStrm
	| dp |
	"I am about to be written on an object file.  If I am one of two shared global arrays, write a proxy instead."

self == (TextConstants at: #DefaultTabsArray) ifTrue: [
	dp := DiskProxy global: #TextConstants selector: #at: args: #(DefaultTabsArray).
	refStrm replace: self with: dp.
	^ dp].
self == (TextConstants at: #DefaultMarginTabsArray) ifTrue: [
	dp := DiskProxy global: #TextConstants selector: #at: args: #(DefaultMarginTabsArray).
	refStrm replace: self with: dp.
	^ dp].
^ super objectForDataStream: refStrm! !

!Array methodsFor: 'arithmetic' stamp: 'raok 10/22/2002 20:10'!
preMultiplyByArray: a
	"Answer a+*self where a is an Array.  Arrays are always understood as column vectors,
	 so an n element Array is an n*1 Array.  This multiplication is legal iff self size = 1."

	self size = 1 ifFalse: [self error: 'dimensions do not conform'].
	^a * self first! !

!Array methodsFor: 'arithmetic' stamp: 'raok 10/22/2002 20:08'!
preMultiplyByMatrix: m
	"Answer m+*self where m is a Matrix."
	|s|

	m columnCount = self size ifFalse: [self error: 'dimensions do not conform'].
	^(1 to: m rowCount) collect: [:row |
		s := 0.
		1 to: self size do: [:k | s := (m at: row at: k) * (self at: k) + s].
		s]! !

!Array methodsFor: 'self evaluating' stamp: 'sd 7/31/2005 21:44'!
printAsLiteralFormOn: aStream
	aStream nextPut: $#.
	self printElementsOn: aStream
! !

!Array methodsFor: 'self evaluating' stamp: 'sd 7/31/2005 21:44'!
printAsSelfEvaluatingFormOn: aStream

	aStream nextPut: ${.
	self do: [:el | aStream print: el] separatedBy: [ aStream nextPutAll: ' . '].
	aStream nextPut: $}! !

!Array methodsFor: 'printing' stamp: 'sd 7/31/2005 21:44'!
printOn: aStream
	self isLiteral ifTrue: [self printAsLiteralFormOn: aStream. ^ self].
	self isSelfEvaluating ifTrue: [self printAsSelfEvaluatingFormOn: aStream. ^ self].

	super printOn: aStream! !

!Array methodsFor: 'private' stamp: ''!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	super replaceFrom: start to: stop with: replacement startingAt: repStart! !

!Array methodsFor: 'printing' stamp: ''!
storeOn: aStream 
	"Use the literal form if possible."

	self isLiteral
		ifTrue: 
			[aStream nextPut: $#; nextPut: $(.
			self do: 
				[:element | 
				element printOn: aStream.
				aStream space].
			aStream nextPut: $)]
		ifFalse: [super storeOn: aStream]! !

!Array methodsFor: 'filter streaming' stamp: 'MPW 1/1/1901 01:55'!
storeOnStream:aStream
	self isLiteral ifTrue: [super storeOnStream:aStream] ifFalse:[aStream writeCollection:self].
! !

Array weakSubclass: #WeakArray
	instanceVariableNames: ''
	classVariableNames: 'FinalizationDependents FinalizationProcess FinalizationSemaphore FinalizationLock IsFinalizationSupported'
	poolDictionaries: ''
	category: 'Collections-Weak'!

!WeakArray commentStamp: '<historical>' prior: 0!
WeakArray is an array which holds only weakly on its elements. This means whenever an object is only referenced by instances of WeakArray it will be garbage collected.!

!WeakArray class methodsFor: 'accessing' stamp: 'ar 10/8/1998 11:17'!
addWeakDependent: anObject
	| finished index weakDependent |
	self isFinalizationSupported ifFalse:[^self].
	FinalizationLock critical:[
		finished := false.
		index := 0.
		[index := index + 1.
		finished not and:[index <= FinalizationDependents size]] whileTrue:[
			weakDependent := FinalizationDependents at: index.
			weakDependent isNil ifTrue:[
				FinalizationDependents at: index put: anObject.
				finished := true.
			].
		].
		finished ifFalse:[
			"Grow linearly"
			FinalizationDependents := FinalizationDependents, (WeakArray new: 10).
			FinalizationDependents at: index put: anObject.
		].
	] ifError:[:msg :rcvr| rcvr error: msg].! !

!WeakArray class methodsFor: 'private' stamp: 'di 5/21/2001 21:49'!
finalizationProcess

	[true] whileTrue:
		[FinalizationSemaphore wait.
		FinalizationLock critical:
			[FinalizationDependents do:
				[:weakDependent |
				weakDependent ifNotNil:
					[weakDependent finalizeValues.
					"***Following statement is required to keep weakDependent
					from holding onto its value as garbage.***"
					weakDependent := nil]]]
			ifError:
			[:msg :rcvr | rcvr error: msg].
		].
! !

!WeakArray class methodsFor: 'class initialization' stamp: 'nk 6/21/2004 10:22'!
initialize
	"WeakArray initialize"

	"Do we need to initialize specialObjectsArray?"
	Smalltalk specialObjectsArray size < 42 
		ifTrue:[Smalltalk recreateSpecialObjectsArray].

	Smalltalk addToStartUpList: self.
	self restartFinalizationProcess.! !

!WeakArray class methodsFor: 'accessing' stamp: 'ar 10/7/1998 15:30'!
isFinalizationSupported
	"Check if this VM supports the finalization mechanism"
	| tempObject |
	IsFinalizationSupported ifNotNil:[^IsFinalizationSupported].
	tempObject := WeakArray new: 1.
	"Check if the class format 4 is correctly understood by the VM.
	If the weak class support is not installed then the VM will report
	any weak class as containing 32bit words - not pointers"
	(tempObject at: 1) = nil 
		ifFalse:[^IsFinalizationSupported :=false].
	"Check if objects are correctly freed"
	self pvtCreateTemporaryObjectIn: tempObject.
	Smalltalk garbageCollect.
	^IsFinalizationSupported := (tempObject at: 1) == nil! !

!WeakArray class methodsFor: 'private' stamp: 'ar 10/7/1998 15:24'!
pvtCreateTemporaryObjectIn: tempObject
	"We have to create the temporary object in a separate stack frame"
	tempObject at: 1 put: Object new! !

!WeakArray class methodsFor: 'accessing' stamp: 'ar 10/8/1998 11:17'!
removeWeakDependent: anObject
	self isFinalizationSupported ifFalse:[^self].
	FinalizationLock critical:[
		1 to: FinalizationDependents size do:[:i|
			((FinalizationDependents at: i) == anObject) ifTrue:[
				FinalizationDependents at: i put: nil.
			].
		].
	] ifError:[:msg :rcvr| rcvr error: msg].! !

!WeakArray class methodsFor: 'private' stamp: 'nk 6/21/2004 10:22'!
restartFinalizationProcess
	"kill any old process, just in case"
	FinalizationProcess
		ifNotNil: [FinalizationProcess terminate.
			FinalizationProcess := nil].

	"Check if Finalization is supported by this VM"
	IsFinalizationSupported := nil.
	self isFinalizationSupported
		ifFalse: [^ self].

	FinalizationSemaphore := Smalltalk specialObjectsArray at: 42.
	FinalizationDependents ifNil: [FinalizationDependents := WeakArray new: 10].
	FinalizationLock := Semaphore forMutualExclusion.
	FinalizationProcess := [self finalizationProcess]
		forkAt: Processor userInterruptPriority! !

!WeakArray class methodsFor: 'accessing' stamp: 'nk 10/28/2000 20:26'!
runningFinalizationProcess
	"Answer the FinalizationProcess I am running, if any"
	^FinalizationProcess! !

!WeakArray class methodsFor: 'system startup' stamp: 'nk 6/21/2004 09:22'!
startUp: resuming
	resuming ifFalse: [ ^self ].
	self restartFinalizationProcess.! !

!ArrayedCollection class methodsFor: 'plugin generation' stamp: 'acg 9/20/1999 10:03'!
ccg: cg generateCoerceToOopFrom: aNode on: aStream

	self instSize > 0 ifTrue: 
		[self error: 'cannot auto-coerce arrays with named instance variables'].
	cg generateCoerceToObjectFromPtr: aNode on: aStream! !

!ArrayedCollection class methodsFor: 'plugin generation' stamp: 'acg 10/5/1999 06:18'!
ccg: cg generateCoerceToValueFrom: aNode on: aStream

	cg 
		generateCoerceToPtr: (self ccgDeclareCForVar: '')
		fromObject: aNode on: aStream! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: ''!
new
	"Answer a new instance of me, with size = 0."

	^self new: 0! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: ''!
new: size withAll: value 
	"Answer an instance of me, with number of elements equal to size, each 
	of which refers to the argument, value."

	^(self new: size) atAllPut: value! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: ''!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."
	| newArray |
	newArray := self new: aCollection size.
	1 to: aCollection size do: [:i | newArray at: i put: (aCollection at: i)].
	^ newArray

"	Array newFrom: {1. 2. 3}
	{1. 2. 3} as: Array
	{1. 2. 3} as: ByteArray
	{$c. $h. $r} as: String
	{$c. $h. $r} as: Text
"! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: 'ar 5/17/2001 19:50'!
newFromStream: s
	"Only meant for my subclasses that are raw bits and word-like.  For quick unpack form the disk."
	| len |

	self isPointers | self isWords not ifTrue: [^ super newFromStream: s].
		"super may cause an error, but will not be called."

	s next = 16r80 ifTrue:
		["A compressed format.  Could copy what BitMap does, or use a 
		special sound compression format.  Callers normally compress their own way."
		^ self error: 'not implemented'].
	s skip: -1.
	len := s nextInt32.
	^ s nextWordsInto: (self basicNew: len)! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: ''!
with: anObject 
	"Answer a new instance of me, containing only anObject."

	| newCollection |
	newCollection := self new: 1.
	newCollection at: 1 put: anObject.
	^newCollection! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: ''!
with: firstObject with: secondObject 
	"Answer a new instance of me, containing firstObject and secondObject."

	| newCollection |
	newCollection := self new: 2.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	^newCollection! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: ''!
with: firstObject with: secondObject with: thirdObject 
	"Answer a new instance of me, containing only the three arguments as
	elements."

	| newCollection |
	newCollection := self new: 3.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	^newCollection! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: ''!
with: firstObject with: secondObject with: thirdObject with: fourthObject 
	"Answer a new instance of me, containing only the three arguments as
	elements."

	| newCollection |
	newCollection := self new: 4.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	newCollection at: 4 put: fourthObject.
	^newCollection! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: ''!
with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject
	"Answer a new instance of me, containing only the five arguments as
	elements."

	| newCollection |
	newCollection := self new: 5.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	newCollection at: 4 put: fourthObject.
	newCollection at: 5 put: fifthObject.
	^newCollection! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: 'sw 10/24/1998 22:22'!
with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject with: sixthObject
	"Answer a new instance of me, containing only the 6 arguments as elements."

	| newCollection |
	newCollection := self new: 6.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	newCollection at: 4 put: fourthObject.
	newCollection at: 5 put: fifthObject.
	newCollection at: 6 put: sixthObject.
	^ newCollection! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: 'sma 5/12/2000 17:37'!
withAll: aCollection
	"Create a new collection containing all the elements from aCollection."

	^ (self new: aCollection size) replaceFrom: 1 to: aCollection size with: aCollection! !

!ArrayedCollection methodsFor: 'adding' stamp: 'sma 5/12/2000 14:09'!
add: newObject
	self shouldNotImplement! !

!ArrayedCollection methodsFor: 'sorting' stamp: 'sma 5/12/2000 18:18'!
asSortedArray
	self isSorted ifTrue: [^ self asArray].
	^ super asSortedArray! !

!ArrayedCollection methodsFor: 'objects from disk' stamp: 'nk 3/17/2004 15:22'!
byteSize
	^self basicSize * self bytesPerBasicElement
! !

!ArrayedCollection methodsFor: 'objects from disk' stamp: 'nk 3/17/2004 16:28'!
bytesPerBasicElement
	"Answer the number of bytes that each of my basic elements requires.
	In other words:
		self basicSize * self bytesPerBasicElement
	should equal the space required on disk by my variable sized representation."
	^self class isBytes ifTrue: [ 1 ] ifFalse: [ 4 ]! !

!ArrayedCollection methodsFor: 'objects from disk' stamp: 'nk 3/17/2004 18:51'!
bytesPerElement
	^self class isBytes ifTrue: [ 1 ] ifFalse: [ 4 ].
! !

!ArrayedCollection methodsFor: 'private' stamp: ''!
defaultElement

	^nil! !

!ArrayedCollection methodsFor: 'filter streaming' stamp: 'sma 5/12/2000 14:20'!
flattenOnStream: aStream 
	aStream writeArrayedCollection: self! !

!ArrayedCollection methodsFor: 'sorting' stamp: 'sma 6/1/2000 11:57'!
isSorted
	"Return true if the receiver is sorted by the given criterion.
	Optimization for isSortedBy: [:a :b | a <= b]."

	| lastElm elm |
	self isEmpty ifTrue: [^ true].
	lastElm := self first.
	2 to: self size do: 
		[:index | 
		elm := self at: index.
		lastElm <= elm ifFalse: [^ false].
		lastElm := elm].
	^ true! !

!ArrayedCollection methodsFor: 'sorting' stamp: 'sma 6/1/2000 11:58'!
isSortedBy: aBlock
	"Return true if the receiver is sorted by the given criterion."

	| lastElm elm |
	self isEmpty ifTrue: [^ true].
	lastElm := self first.
	2 to: self size do: 
		[:index | 
		elm := self at: index.
		(aBlock value: lastElm value: elm) ifFalse: [^ false].
		lastElm := elm].
	^ true! !

!ArrayedCollection methodsFor: 'sorting' stamp: 'sma 5/12/2000 14:28'!
mergeFirst: first middle: middle last: last into: dst by: aBlock
	"Private. Merge the sorted ranges [first..middle] and [middle+1..last] 
	of the receiver into the range [first..last] of dst."

	| i1 i2 val1 val2 out |
	i1 := first.
	i2 := middle + 1.
	val1 := self at: i1.
	val2 := self at: i2.
	out := first - 1.  "will be pre-incremented"

	"select 'lower' half of the elements based on comparator"
	[(i1 <= middle) and: [i2 <= last]] whileTrue:
		[(aBlock value: val1 value: val2)
			ifTrue: [dst at: (out := out + 1) put: val1.
					val1 := self at: (i1 := i1 + 1)]
			ifFalse: [dst at: (out := out + 1) put: val2.
					i2 := i2 + 1.
					i2 <= last ifTrue: [val2 := self at: i2]]].

	"copy the remaining elements"
	i1 <= middle
		ifTrue: [dst replaceFrom: out + 1 to: last with: self startingAt: i1]
		ifFalse: [dst replaceFrom: out + 1 to: last with: self startingAt: i2]! !

!ArrayedCollection methodsFor: 'sorting' stamp: 'sma 5/12/2000 14:25'!
mergeSortFrom: startIndex to: stopIndex by: aBlock
	"Sort the given range of indices using the mergesort algorithm.
	Mergesort is a worst-case O(N log N) sorting algorithm that usually
	does only half as many comparisons as heapsort or quicksort."

	"Details: recursively split the range to be sorted into two halves,
	mergesort each half, then merge the two halves together. An extra 
	copy of the data is used as temporary storage and successive merge 
	phases copy data back and forth between the receiver and this copy.
	The recursion is set up so that the final merge is performed into the
	receiver, resulting in the receiver being completely sorted."

	self size <= 1 ifTrue: [^ self].  "nothing to do"
	startIndex = stopIndex ifTrue: [^ self].
	self assert: [startIndex >= 1 and: [startIndex < stopIndex]]. "bad start index"
	self assert: [stopIndex <= self size]. "bad stop index"
	self
		mergeSortFrom: startIndex
		to: stopIndex 
		src: self clone 
		dst: self 
		by: aBlock! !

!ArrayedCollection methodsFor: 'sorting' stamp: 'sma 5/12/2000 14:26'!
mergeSortFrom: first to: last src: src dst: dst by: aBlock
	"Private. Split the range to be sorted in half, sort each half, and 
	merge the two half-ranges into dst."

	| middle |
	first = last ifTrue: [^ self].
	middle := (first + last) // 2.
	self mergeSortFrom: first to: middle src: dst dst: src by: aBlock.
	self mergeSortFrom: middle + 1 to: last src: dst dst: src by: aBlock.
	src mergeFirst: first middle: middle last: last into: dst by: aBlock! !

!ArrayedCollection methodsFor: 'objects from disk' stamp: 'nk 7/30/2004 17:50'!
restoreEndianness
	"This word object was just read in from a stream.  It was stored in Big Endian (Mac) format.  Reverse the byte order if the current machine is Little Endian.
	We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	self class isPointers | self class isWords not ifTrue: [^self].
	SmalltalkImage current  isLittleEndian 
		ifTrue: 
			[Bitmap 
				swapBytesIn: self
				from: 1
				to: self basicSize]! !

!ArrayedCollection methodsFor: 'accessing' stamp: 'sma 5/12/2000 11:36'!
size
	"Answer how many elements the receiver contains."

	<primitive: 62>
	^ self basicSize! !

!ArrayedCollection methodsFor: 'sorting' stamp: 'sma 5/12/2000 14:22'!
sort
	"Sort this array into ascending order using the '<=' operator."

	self sort: [:a :b | a <= b]! !

!ArrayedCollection methodsFor: 'sorting' stamp: 'sma 5/12/2000 14:21'!
sort: aSortBlock 
	"Sort this array using aSortBlock. The block should take two arguments
	and return true if the first element should preceed the second one."

	self
		mergeSortFrom: 1
		to: self size
		by: aSortBlock! !

!ArrayedCollection methodsFor: 'private' stamp: ''!
storeElementsFrom: firstIndex to: lastIndex on: aStream

	| noneYet defaultElement arrayElement |
	noneYet := true.
	defaultElement := self defaultElement.
	firstIndex to: lastIndex do: 
		[:index | 
		arrayElement := self at: index.
		arrayElement = defaultElement
			ifFalse: 
				[noneYet
					ifTrue: [noneYet := false]
					ifFalse: [aStream nextPut: $;].
				aStream nextPutAll: ' at: '.
				aStream store: index.
				aStream nextPutAll: ' put: '.
				aStream store: arrayElement]].
	^noneYet! !

!ArrayedCollection methodsFor: 'printing' stamp: ''!
storeOn: aStream

	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new: '.
	aStream store: self size.
	aStream nextPut: $).
	(self storeElementsFrom: 1 to: self size on: aStream)
		ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)! !

!ArrayedCollection methodsFor: 'objects from disk' stamp: 'tk 3/7/2001 17:36'!
swapHalves
		"A normal switch in endianness (byte order in words) reverses the order of 4 bytes.  That is not correct for SoundBuffers, which use 2-bytes units.  If a normal switch has be done, this method corrects it further by swapping the two halves of the long word.
	This method is only used for 16-bit quanities in SoundBuffer, ShortIntegerArray, etc."

	| hack blt |
	"The implementation is a hack, but fast for large ranges"
	hack := Form new hackBits: self.
	blt := (BitBlt toForm: hack) sourceForm: hack.
	blt combinationRule: Form reverse.  "XOR"
	blt sourceY: 0; destY: 0; height: self size; width: 2.
	blt sourceX: 0; destX: 2; copyBits.  "Exchange bytes 0&1 with 2&3"
	blt sourceX: 2; destX: 0; copyBits.
	blt sourceX: 0; destX: 2; copyBits.! !

!ArrayedCollection methodsFor: 'objects from disk' stamp: 'ar 5/17/2001 19:50'!
writeOn: aStream 
	"Store the array of bits onto the argument, aStream.  (leading byte ~= 16r80) identifies this as raw bits (uncompressed).  Always store in Big Endian (Mac) byte order.  Do the writing at BitBlt speeds. We only intend this for non-pointer arrays.  Do nothing if I contain pointers."
	self class isPointers | self class isWords not ifTrue: [^ super writeOn: aStream].
				"super may cause an error, but will not be called."
	aStream nextInt32Put: self basicSize.
	aStream nextWordsPutAll: self.! !

!ArrayedCollection methodsFor: 'objects from disk' stamp: 'tk 3/7/2001 18:07'!
writeOnGZIPByteStream: aStream 
	"We only intend this for non-pointer arrays.  Do nothing if I contain pointers."

	self class isPointers | self class isWords not ifTrue: [^ super writeOnGZIPByteStream: aStream].
		"super may cause an error, but will not be called."
	
	aStream nextPutAllWordArray: self! !

ArrayedCollection variableByteSubclass: #ByteArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

!ByteArray commentStamp: '<historical>' prior: 0!
I represent an ArrayedCollection whose elements are integers between 0 and 255.
!

!ByteArray class methodsFor: 'byte based hash' stamp: 'SqR 8/21/2002 16:21'!
hashBytes: aByteArray startingWith: speciesHash
	"Answer the hash of a byte-indexed collection,
	using speciesHash as the initial value.
	See SmallInteger>>hashMultiply.

	The primitive should be renamed at a
	suitable point in the future"

	| byteArraySize hash low |
	<primitive: 'primitiveStringHash' module: 'MiscPrimitivePlugin'>

	self var: #aHash declareC: 'int speciesHash'.
	self var: #aByteArray declareC: 'unsigned char *aByteArray'.

	byteArraySize := aByteArray size.
	hash := speciesHash bitAnd: 16rFFFFFFF.
	1 to: byteArraySize do: [:pos |
		hash := hash + (aByteArray basicAt: pos).
		"Begin hashMultiply"
		low := hash bitAnd: 16383.
		hash := (16r260D * low + ((16r260D * (hash bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384)) bitAnd: 16r0FFFFFFF.
	].
	^ hash! !

!ByteArray methodsFor: 'converting' stamp: 'sma 5/12/2000 17:35'!
asByteArray
	^ self! !

!ByteArray methodsFor: 'private' stamp: 'ar 1/28/2000 17:45'!
asByteArrayPointer
	"Return a ByteArray describing a pointer to the contents of the receiver."
	^self shouldNotImplement! !

!ByteArray methodsFor: 'converting' stamp: ''!
asString
	"Convert to a String with Characters for each byte.
	Fast code uses primitive that avoids character conversion"

	^ (String new: self size) replaceFrom: 1 to: self size with: self! !

!ByteArray methodsFor: 'accessing' stamp: 'ar 4/12/2005 17:35'!
asWideString

	^ WideString fromByteArray: self.
! !

!ByteArray methodsFor: 'accessing' stamp: 'sma 4/22/2000 17:47'!
atAllPut: value
	"Fill the receiver with the given value"

	<primitive: 145>
	super atAllPut: value! !

!ByteArray methodsFor: 'accessing' stamp: 'ar 12/5/1998 14:52'!
byteAt: index
	<primitive: 60>
	^self at: index! !

!ByteArray methodsFor: 'accessing' stamp: 'ar 12/5/1998 14:52'!
byteAt: index put: value
	<primitive: 61>
	^self at: index put: value! !

!ByteArray methodsFor: 'accessing' stamp: 'ar 3/3/2001 16:17'!
byteSize
	^self size! !

!ByteArray methodsFor: 'accessing' stamp: 'tk 3/13/2000 14:46'!
bytesPerElement
	"Number of bytes in each item.  This multiplied by (self size)*8 gives the number of bits stored."
	^ 1! !

!ByteArray methodsFor: 'private' stamp: ''!
defaultElement

	^0! !

!ByteArray methodsFor: 'platform independent access' stamp: 'jmb 12/3/2004 14:54'!
doubleAt: index bigEndian: bool 
	"Return a 64 bit float starting from the given byte index"
	| w1 w2 dbl |
	w1 := self unsignedLongAt: index bigEndian: bool.
	w2 := self unsignedLongAt: index + 4 bigEndian: bool.
	dbl := Float new: 2. 
	bool
		ifTrue: [dbl basicAt: 1 put: w1.
			dbl basicAt: 2 put: w2]
		ifFalse: [dbl basicAt: 1 put: w2.
			dbl basicAt: 2 put: w1].
	^ dbl! !

!ByteArray methodsFor: 'platform independent access' stamp: 'jmb 12/3/2004 14:54'!
doubleAt: index put: value bigEndian: bool 
	"Store a 64 bit float starting from the given byte index"
	| w1 w2 |
	bool
		ifTrue: [w1 := value basicAt: 1.
			w2 := value basicAt: 2]
		ifFalse: [w1 := value basicAt: 2.
			w2 := value basicAt: 1]. 
	self unsignedLongAt: index put: w1 bigEndian: bool.
	self unsignedLongAt: index + 4 put: w2 bigEndian: bool.
	^ value! !

!ByteArray methodsFor: 'comparing' stamp: 'SqR 8/13/2002 10:52'!
hash
	"#hash is implemented, because #= is implemented"

	^self class
		hashBytes: self
		startingWith: self species hash! !

!ByteArray methodsFor: 'zip archive' stamp: 'nk 8/21/2004 15:23'!
lastIndexOfPKSignature: aSignature
	"Answer the last index in me where aSignature (4 bytes long) occurs, or 0 if not found"
	| a b c d |
	a := aSignature first.
	b := aSignature second.
	c := aSignature third.
	d := aSignature fourth.
	(self size - 3) to: 1 by: -1 do: [ :i |
		(((self at: i) = a)
			and: [ ((self at: i + 1) = b)
				and: [ ((self at: i + 2) = c)
					and: [ ((self at: i + 3) = d) ]]])
						ifTrue: [ ^i ]
	].
	^0! !

!ByteArray methodsFor: 'platform independent access' stamp: 'ar 11/1/1998 20:44'!
longAt: index bigEndian: aBool
	"Return a 32bit integer quantity starting from the given byte index"
	| b0 b1 b2 w h |
	aBool ifTrue:[
		b0 := self at: index.
		b1 := self at: index+1.
		b2 := self at: index+2.
		w := self at: index+3.
	] ifFalse:[
		w := self at: index.
		b2 := self at: index+1.
		b1 := self at: index+2.
		b0 := self at: index+3.
	].
	"Minimize LargeInteger arithmetic"
	h := ((b0 bitAnd: 16r7F) - (b0 bitAnd: 16r80) bitShift: 8) + b1.
	b2 = 0 ifFalse:[w := (b2 bitShift: 8) + w].
	h = 0 ifFalse:[w := (h bitShift: 16) + w].
	^w! !

!ByteArray methodsFor: 'platform independent access' stamp: 'ar 8/2/2003 19:29'!
longAt: index put: value bigEndian: aBool
	"Return a 32bit integer quantity starting from the given byte index"
	| b0 b1 b2 b3 |
	b0 := value bitShift: -24.
	b0 := (b0 bitAnd: 16r7F) - (b0 bitAnd: 16r80).
	b0 < 0 ifTrue:[b0 := 256 + b0].
	b1 := (value bitShift: -16) bitAnd: 255.
	b2 := (value bitShift: -8) bitAnd: 255.
	b3 := value bitAnd: 255.
	aBool ifTrue:[
		self at: index put: b0.
		self at: index+1 put: b1.
		self at: index+2 put: b2.
		self at: index+3 put: b3.
	] ifFalse:[
		self at: index put: b3.
		self at: index+1 put: b2.
		self at: index+2 put: b1.
		self at: index+3 put: b0.
	].
	^value! !

!ByteArray methodsFor: 'private' stamp: ''!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	super replaceFrom: start to: stop with: replacement startingAt: repStart! !

!ByteArray methodsFor: 'platform independent access' stamp: 'ar 11/1/1998 20:57'!
shortAt: index bigEndian: aBool
	"Return a 16 bit integer quantity starting from the given byte index"
	| uShort |
	uShort := self unsignedShortAt: index bigEndian: aBool.
	^(uShort bitAnd: 16r7FFF) - (uShort bitAnd: 16r8000)! !

!ByteArray methodsFor: 'platform independent access' stamp: 'ar 11/3/1998 14:20'!
shortAt: index put: value bigEndian: aBool
	"Store a 16 bit integer quantity starting from the given byte index"
	self unsignedShortAt: index put: (value bitAnd: 16r7FFF) - (value bitAnd: -16r8000) bigEndian: aBool.
	^value! !

!ByteArray methodsFor: 'platform independent access' stamp: 'ar 11/1/1998 20:49'!
unsignedLongAt: index bigEndian: aBool
	"Return a 32bit unsigned integer quantity starting from the given byte index"
	| b0 b1 b2 w |
	aBool ifTrue:[
		b0 := self at: index.
		b1 := self at: index+1.
		b2 := self at: index+2.
		w := self at: index+3.
	] ifFalse:[
		w := self at: index.
		b2 := self at: index+1.
		b1 := self at: index+2.
		b0 := self at: index+3.
	].
	"Minimize LargeInteger arithmetic"
	b2 = 0 ifFalse:[w := (b2 bitShift: 8) + w].
	b1 = 0 ifFalse:[w := (b1 bitShift: 16) + w].
	b0 = 0 ifFalse:[w := (b0 bitShift: 24) + w].
	^w! !

!ByteArray methodsFor: 'platform independent access' stamp: 'ar 11/1/1998 20:49'!
unsignedLongAt: index put: value bigEndian: aBool
	"Store a 32bit unsigned integer quantity starting from the given byte index"
	| b0 b1 b2 b3 |
	b0 := value bitShift: -24.
	b1 := (value bitShift: -16) bitAnd: 255.
	b2 := (value bitShift: -8) bitAnd: 255.
	b3 := value bitAnd: 255.
	aBool ifTrue:[
		self at: index put: b0.
		self at: index+1 put: b1.
		self at: index+2 put: b2.
		self at: index+3 put: b3.
	] ifFalse:[
		self at: index put: b3.
		self at: index+1 put: b2.
		self at: index+2 put: b1.
		self at: index+3 put: b0.
	].
	^value! !

!ByteArray methodsFor: 'platform independent access' stamp: 'ar 11/1/1998 20:51'!
unsignedShortAt: index bigEndian: aBool
	"Return a 16 bit unsigned integer quantity starting from the given byte index"
	^aBool 
		ifTrue:[((self at: index) bitShift: 8) + (self at: index+1)]
		ifFalse:[((self at: index+1) bitShift: 8) + (self at: index)].! !

!ByteArray methodsFor: 'platform independent access' stamp: 'ar 11/1/1998 20:53'!
unsignedShortAt: index put: value bigEndian: aBool
	"Store a 16 bit unsigned integer quantity starting from the given byte index"
	aBool ifTrue:[
		self at: index put: (value bitShift: -8).
		self at: index+1 put: (value bitAnd: 255).
	] ifFalse:[
		self at: index+1 put: (value bitShift: -8).
		self at: index put: (value bitAnd: 255).
	].
	^value! !

ArrayedCollection variableWordSubclass: #ColorArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

!ColorArray methodsFor: 'converting' stamp: 'ar 3/3/2001 20:06'!
asColorArray
	^self! !

!ColorArray methodsFor: 'accessing' stamp: 'ar 3/3/2001 20:03'!
at: index
	^(super at: index) asColorOfDepth: 32! !

!ColorArray methodsFor: 'accessing' stamp: 'ar 3/3/2001 20:04'!
at: index put: aColor
	^super at: index put: (aColor pixelWordForDepth: 32).! !

!ColorArray methodsFor: 'converting' stamp: 'RAA 3/8/2001 06:24'!
bytesPerElement

	^4! !

ArrayedCollection variableWordSubclass: #FloatArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

!FloatArray commentStamp: '<historical>' prior: 0!
FloatArrays store 32bit IEEE floating point numbers.!

!FloatArray methodsFor: 'arithmetic' stamp: 'ar 9/14/1998 22:33'!
* anObject
	^self clone *= anObject! !

!FloatArray methodsFor: 'arithmetic' stamp: 'ar 9/14/1998 22:49'!
*= anObject
	^anObject isNumber
		ifTrue:[self primMulScalar: anObject asFloat]
		ifFalse:[self primMulArray: anObject]! !

!FloatArray methodsFor: 'arithmetic' stamp: 'ar 9/14/1998 22:33'!
+ anObject
	^self clone += anObject! !

!FloatArray methodsFor: 'arithmetic' stamp: 'ar 9/14/1998 22:48'!
+= anObject
	^anObject isNumber
		ifTrue:[self primAddScalar: anObject asFloat]
		ifFalse:[self primAddArray: anObject]! !

!FloatArray methodsFor: 'arithmetic' stamp: 'ar 9/14/1998 22:33'!
- anObject
	^self clone -= anObject! !

!FloatArray methodsFor: 'arithmetic' stamp: 'ar 9/14/1998 22:49'!
-= anObject
	^anObject isNumber
		ifTrue:[self primSubScalar: anObject asFloat]
		ifFalse:[self primSubArray: anObject]! !

!FloatArray methodsFor: 'arithmetic' stamp: 'ar 9/14/1998 22:34'!
/ anObject
	^self clone /= anObject! !

!FloatArray methodsFor: 'arithmetic' stamp: 'ar 10/7/1998 19:58'!
/= anObject
	^anObject isNumber
		ifTrue:[self primDivScalar: anObject asFloat]
		ifFalse:[self primDivArray: anObject]! !

!FloatArray methodsFor: 'comparing' stamp: 'ar 2/2/2001 15:47'!
= aFloatArray 
	| length |
	<primitive: 'primitiveEqual' module: 'FloatArrayPlugin'>
	aFloatArray class = self class ifFalse: [^ false].
	length := self size.
	length = aFloatArray size ifFalse: [^ false].
	1 to: self size do: [:i | (self at: i)
			= (aFloatArray at: i) ifFalse: [^ false]].
	^ true! !

!FloatArray methodsFor: 'arithmetic' stamp: 'yo 9/14/2004 17:12'!
\\= other

	other isNumber ifTrue: [
		1 to: self size do: [:i |
			self at: i put: (self at: i) \\ other
		].
		^ self.
	].
	1 to: (self size min: other size) do: [:i |
		self at: i put: (self at: i) \\ (other at: i).
	].

! !

!FloatArray methodsFor: 'arithmetic' stamp: 'ar 9/7/2001 23:07'!
adaptToNumber: rcvr andSend: selector
	"If I am involved in arithmetic with a Number. If possible,
	convert it to a float and perform the (more efficient) primitive operation."
	selector == #+ ifTrue:[^self + rcvr].
	selector == #* ifTrue:[^self * rcvr].
	selector == #- ifTrue:[^self negated += rcvr].
	selector == #/ ifTrue:[^self * (1.0 / rcvr)].
	^super adaptToNumber: rcvr andSend: selector! !

!FloatArray methodsFor: 'converting' stamp: 'ar 9/14/1998 23:46'!
asFloatArray
	^self! !

!FloatArray methodsFor: 'accessing' stamp: 'ar 2/2/2001 15:47'!
at: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)! !

!FloatArray methodsFor: 'accessing' stamp: 'ar 2/2/2001 15:47'!
at: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue:[self basicAt: index put: value asIEEE32BitWord]
		ifFalse:[self at: index put: value asFloat].
	^value! !

!FloatArray methodsFor: 'accessing' stamp: 'ar 11/2/1998 12:19'!
defaultElement
	"Return the default element of the receiver"
	^0.0! !

!FloatArray methodsFor: 'arithmetic' stamp: 'laza 3/24/2000 13:07'!
dot: aFloatVector
	"Primitive. Return the dot product of the receiver and the argument.
	Fail if the argument is not of the same size as the receiver."
	| result |
	"<primitive:'primitiveFloatArrayDotProduct'>"
	self size = aFloatVector size ifFalse:[^self error:'Must be equal size'].
	result := 0.0.
	1 to: self size do:[:i|
		result := result + ((self at: i) * (aFloatVector at: i)).
	].
	^result! !

!FloatArray methodsFor: 'comparing' stamp: 'ar 5/3/2001 13:02'!
hash
	| result |
	<primitive:'primitiveHashArray' module: 'FloatArrayPlugin'>
	result := 0.
	1 to: self size do:[:i| result := result + (self basicAt: i) ].
	^result bitAnd: 16r1FFFFFFF! !

!FloatArray methodsFor: 'accessing' stamp: 'laza 3/24/2000 13:08'!
length
	"Return the length of the receiver"
	^self squaredLength sqrt! !

!FloatArray methodsFor: 'arithmetic' stamp: 'ar 9/7/2001 23:04'!
negated
	^self clone *= -1! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'ar 2/2/2001 15:47'!
primAddArray: floatArray

	<primitive: 'primitiveAddFloatArray' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) + (floatArray at: i)].! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'ar 2/2/2001 15:47'!
primAddScalar: scalarValue

	<primitive: 'primitiveAddScalar' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) + scalarValue].! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'ar 2/2/2001 15:47'!
primDivArray: floatArray

	<primitive: 'primitiveDivFloatArray' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) / (floatArray at: i)].! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'ar 2/2/2001 15:47'!
primDivScalar: scalarValue

	<primitive: 'primitiveDivScalar' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) / scalarValue].! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'ar 2/2/2001 15:47'!
primMulArray: floatArray

	<primitive: 'primitiveMulFloatArray' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) * (floatArray at: i)].! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'ar 2/2/2001 15:47'!
primMulScalar: scalarValue

	<primitive: 'primitiveMulScalar' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) * scalarValue].! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'ar 2/2/2001 15:47'!
primSubArray: floatArray

	<primitive: 'primitiveSubFloatArray' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) - (floatArray at: i)].! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'ar 2/2/2001 15:47'!
primSubScalar: scalarValue

	<primitive: 'primitiveSubScalar' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) - scalarValue].! !

!FloatArray methodsFor: 'private' stamp: 'ar 10/9/1998 11:27'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	super replaceFrom: start to: stop with: replacement startingAt: repStart! !

!FloatArray methodsFor: 'accessing' stamp: 'laza 3/24/2000 13:08'!
squaredLength
	"Return the squared length of the receiver"
	^self dot: self! !

!FloatArray methodsFor: 'primitives-plugin' stamp: 'jcg 6/12/2003 17:54'!
sum

	<primitive: 'primitiveSum' module: 'FloatArrayPlugin'>
	^ super sum! !

ArrayedCollection variableWordSubclass: #IntegerArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

!IntegerArray commentStamp: '<historical>' prior: 0!
IntegerArrays store 32bit signed Integer values.
Negative values are stored as 2's complement.!

!IntegerArray methodsFor: 'converting' stamp: 'ar 10/10/1998 16:18'!
asIntegerArray
	^self! !

!IntegerArray methodsFor: 'accessing' stamp: 'ar 3/3/2001 22:40'!
at: index
	| word |
	<primitive: 165>
	word := self basicAt: index.
	word < 16r3FFFFFFF ifTrue:[^word]. "Avoid LargeInteger computations"
	^word >= 16r80000000	"Negative?!!"
		ifTrue:["word - 16r100000000"
				(word bitInvert32 + 1) negated]
		ifFalse:[word]! !

!IntegerArray methodsFor: 'accessing' stamp: 'ar 3/3/2001 22:40'!
at: index put: anInteger
	| word |
	<primitive: 166>
	anInteger < 0
		ifTrue:["word := 16r100000000 + anInteger"
				word := (anInteger + 1) negated bitInvert32]
		ifFalse:[word := anInteger].
	self  basicAt: index put: word.
	^anInteger! !

!IntegerArray methodsFor: 'accessing' stamp: 'ar 3/3/2001 23:34'!
atAllPut: anInteger
	| word |
	anInteger < 0
		ifTrue:["word := 16r100000000 + anInteger"
				word := (anInteger + 1) negated bitInvert32]
		ifFalse:[word := anInteger].
	self primFill: word.! !

!IntegerArray methodsFor: 'accessing' stamp: 'ar 11/2/1998 12:19'!
defaultElement
	"Return the default element of the receiver"
	^0! !

!IntegerArray methodsFor: 'private' stamp: 'ar 3/3/2001 23:34'!
primFill: aPositiveInteger
	"Fill the receiver, an indexable bytes or words object, with the given positive integer. The range of possible fill values is [0..255] for byte arrays and [0..(2^32 - 1)] for word arrays."

	<primitive: 145>
	self errorImproperStore.! !

ArrayedCollection subclass: #RunArray
	instanceVariableNames: 'runs values lastIndex lastRun lastOffset'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

!RunArray commentStamp: '<historical>' prior: 0!
My instances provide space-efficient storage of data which tends to be constant over long runs of the possible indices. Essentially repeated values are stored singly and then associated with a "run" length that denotes the number of consecutive occurrences of the value.

My two important variables are
	runs	An array of how many elements are in each run
	values	An array of what the value is over those elements

The variables lastIndex, lastRun and lastOffset cache the last access
so that streaming through RunArrays is not an N-squared process.

Many complexities of access can be bypassed by using the method
	RunArray withStartStopAndValueDo:!

!RunArray class methodsFor: 'instance creation' stamp: 'ar 10/16/2001 19:03'!
new

	^self runs: Array new values: Array new! !

!RunArray class methodsFor: 'instance creation' stamp: 'dc 5/24/2007 10:53'!
new: aSize
	^ self new: aSize withAll: nil! !

!RunArray class methodsFor: 'instance creation' stamp: 'ar 10/16/2001 19:04'!
new: size withAll: value 
	"Answer a new instance of me, whose every element is equal to the
	argument, value."

	size = 0 ifTrue: [^self new].
	^self runs: (Array with: size) values: (Array with: value)! !

!RunArray class methodsFor: 'instance creation' stamp: ''!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."

	| newCollection |
	newCollection := self new.
	aCollection do: [:x | newCollection addLast: x].
	^newCollection

"	RunArray newFrom: {1. 2. 2. 3}
	{1. $a. $a. 3} as: RunArray
	({1. $a. $a. 3} as: RunArray) values
"! !

!RunArray class methodsFor: 'instance creation' stamp: ''!
readFrom: aStream
	"Answer an instance of me as described on the stream, aStream."

	| size runs values |
	size := aStream nextWord.
	runs := Array new: size.
	values := Array new: size.
	1 to: size do:
		[:x |
		runs at: x put: aStream nextWord.
		values at: x put: aStream nextWord].
	^ self runs: runs values: values! !

!RunArray class methodsFor: 'instance creation' stamp: ''!
runs: newRuns values: newValues 
	"Answer an instance of me with runs and values specified by the 
	arguments."

	| instance |
	instance := self basicNew.
	instance setRuns: newRuns setValues: newValues.
	^instance! !

!RunArray class methodsFor: 'instance creation' stamp: 'nk 9/3/2004 15:12'!
scanFrom: strm
	"Read the style section of a fileOut or sources file.  nextChunk has already been done.  We need to return a RunArray of TextAttributes of various kinds.  These are written by the implementors of writeScanOn:"
	| rr vv aa this |
	(strm peekFor: $( ) ifFalse: [^ nil].
	rr := OrderedCollection new.
	[strm skipSeparators.
	 strm peekFor: $)] whileFalse: 
		[rr add: (Number readFrom: strm)].
	vv := OrderedCollection new.	"Value array"
	aa := OrderedCollection new.	"Attributes list"
	[(this := strm next) == nil] whileFalse: [
		this == $, ifTrue: [vv add: aa asArray.  aa := OrderedCollection new].
		this == $a ifTrue: [aa add: 
			(TextAlignment new alignment: (Integer readFrom: strm))].
		this == $f ifTrue: [aa add: 
			(TextFontChange new fontNumber: (Integer readFrom: strm))].
		this == $F ifTrue: [aa add: (TextFontReference toFont: 
			(StrikeFont familyName: (strm upTo: $#) size: (Integer readFrom: strm)))].
		this == $b ifTrue: [aa add: (TextEmphasis bold)].
		this == $i ifTrue: [aa add: (TextEmphasis italic)].
		this == $u ifTrue: [aa add: (TextEmphasis underlined)].
		this == $= ifTrue: [aa add: (TextEmphasis struckOut)].
		this == $n ifTrue: [aa add: (TextEmphasis normal)].
		this == $- ifTrue: [aa add: (TextKern kern: -1)].
		this == $+ ifTrue: [aa add: (TextKern kern: 1)].
		this == $c ifTrue: [aa add: (TextColor scanFrom: strm)]. "color"
		this == $L ifTrue: [aa add: (TextLink scanFrom: strm)].	"L not look like 1"
		this == $R ifTrue: [aa add: (TextURL scanFrom: strm)].
				"R capitalized so it can follow a number"
		this == $q ifTrue: [aa add: (TextSqkPageLink scanFrom: strm)].
		this == $p ifTrue: [aa add: (TextSqkProjectLink scanFrom: strm)].
		this == $P ifTrue: [aa add: (TextPrintIt scanFrom: strm)].
		this == $d ifTrue: [aa add: (TextDoIt scanFrom: strm)].
		"space, cr do nothing"
		].
	aa size > 0 ifTrue: [vv add: aa asArray].
	^ self runs: rr asArray values: vv asArray
"
RunArray scanFrom: (ReadStream on: '(14 50 312)f1,f1b,f1LInteger +;i')
"! !

!RunArray methodsFor: 'copying' stamp: 'ar 10/16/2001 18:57'!
, aRunArray 
	"Answer a new RunArray that is a concatenation of the receiver and
	aRunArray."

	| new newRuns |
	(aRunArray isMemberOf: RunArray)
		ifFalse: 
			[new := self copy.
			"attempt to be sociable"
			aRunArray do: [:each | new addLast: each].
			^new].
	runs size = 0 ifTrue: [^aRunArray copy].
	aRunArray runs size = 0 ifTrue: [^self copy].
	(values at: values size) ~= (aRunArray values at: 1)
		ifTrue: [^RunArray
					runs: runs , aRunArray runs
					values: values , aRunArray values].
	newRuns := runs
					copyReplaceFrom: runs size
					to: runs size
					with: aRunArray runs.
	newRuns at: runs size put: (runs at: runs size) + (aRunArray runs at: 1).
	^RunArray
		runs: newRuns
		values: 
			(values
				copyReplaceFrom: values size
				to: values size
				with: aRunArray values)! !

!RunArray methodsFor: 'accessing' stamp: 'di 1/15/1999 00:04'!
= otherArray 
	"Test if all my elements are equal to those of otherArray"

	(otherArray isMemberOf: RunArray) ifFalse: [^ self hasEqualElements: otherArray].

	"Faster test between two RunArrays"
 	^ (runs hasEqualElements: otherArray runs)
		and: [values hasEqualElements: otherArray values]! !

!RunArray methodsFor: 'adding' stamp: 'ar 10/16/2001 18:47'!
addFirst: value
	"Add value as the first element of the receiver."
	lastIndex := nil.  "flush access cache"
	(runs size=0 or: [values first ~= value])
	  ifTrue:
		[runs := {1}, runs.
		values := {value}, values]
	  ifFalse:
		[runs at: 1 put: runs first+1]! !

!RunArray methodsFor: 'adding' stamp: 'ar 10/16/2001 20:25'!
addLast: value
	"Add value as the last element of the receiver."
	lastIndex := nil.  "flush access cache"
	(runs size=0 or: [values last ~= value])
	  ifTrue:
		[runs := runs copyWith: 1.
		values := values copyWith: value]
	  ifFalse:
		[runs at: runs size put: runs last+1]! !

!RunArray methodsFor: 'adding' stamp: 'ar 10/16/2001 18:47'!
addLast: value  times: times
	"Add value as the last element of the receiver, the given number of times"
	times = 0 ifTrue: [ ^self ].
	lastIndex := nil.  "flush access cache"
	(runs size=0 or: [values last ~= value])
	  ifTrue:
		[runs := runs copyWith: times.
		values := values copyWith: value]
	  ifFalse:
		[runs at: runs size put: runs last+times]! !

!RunArray methodsFor: 'accessing' stamp: ''!
at: index

	self at: index setRunOffsetAndValue: [:run :offset :value | ^value]! !

!RunArray methodsFor: 'accessing' stamp: 'nice 2/14/2007 21:59'!
at: index put: aValue 
	"Set an element of the RunArray"
	| runIndex offsetInRun lastValue runLength runReplacement valueReplacement iStart iStop |
	index isInteger
		ifFalse: [self errorNonIntegerIndex].
	(index >= 1
			and: [index <= self size])
		ifFalse: [self errorSubscriptBounds: index].
	self
		at: index
		setRunOffsetAndValue: [:run :offset :value | 
			runIndex := run.
			offsetInRun := offset.
			lastValue := value].
	aValue = lastValue
		ifTrue: [^ aValue].
	runLength := runs at: runIndex.
	runReplacement := Array
				with: offsetInRun
				with: 1
				with: runLength - offsetInRun - 1.
	valueReplacement := Array
				with: lastValue
				with: aValue
				with: lastValue.
	iStart := offsetInRun = 0
				ifTrue: [2]
				ifFalse: [1].
	iStop := offsetInRun = (runLength - 1)
				ifTrue: [2]
				ifFalse: [3].
	self
		setRuns: (runs copyReplaceFrom: runIndex to: runIndex with: (runReplacement copyFrom: iStart to: iStop))
		setValues: (values copyReplaceFrom: runIndex to: runIndex with: (valueReplacement copyFrom: iStart to: iStop)).
	self coalesce.
	^ aValue! !

!RunArray methodsFor: 'private' stamp: ''!
at: index setRunOffsetAndValue: aBlock 
	"Supply all run information to aBlock."
	"Tolerates index=0 and index=size+1 for copyReplace: "
	| run limit offset |
	limit := runs size.
	(lastIndex == nil or: [index < lastIndex])
		ifTrue:  "cache not loaded, or beyond index - start over"
			[run := 1.
			offset := index-1]
		ifFalse:  "cache loaded and before index - start at cache"
			[run := lastRun.
			offset := lastOffset + (index-lastIndex)].
	[run <= limit and: [offset >= (runs at: run)]]
		whileTrue: 
			[offset := offset - (runs at: run).
			run := run + 1].
	lastIndex := index.  "Load cache for next access"
	lastRun := run.
	lastOffset := offset.
	run > limit
		ifTrue: 
			["adjustment for size+1"
			run := run - 1.
			offset := offset + (runs at: run)].
	^aBlock
		value: run	"an index into runs and values"
		value: offset	"zero-based offset from beginning of this run"
		value: (values at: run)	"value for this run"! !

!RunArray methodsFor: 'adding' stamp: 'tk 1/28/98 09:28'!
coalesce
	"Try to combine adjacent runs"
	| ind |
	ind := 2.
	[ind > values size] whileFalse: [
		(values at: ind-1) = (values at: ind) 
			ifFalse: [ind := ind + 1]
			ifTrue: ["two are the same, combine them"
				values := values copyReplaceFrom: ind to: ind with: #().
				runs at: ind-1 put: (runs at: ind-1) + (runs at: ind).
				runs := runs copyReplaceFrom: ind to: ind with: #().
				"self error: 'needed to combine runs' "]].
			! !

!RunArray methodsFor: 'copying' stamp: 'ls 10/10/1999 13:15'!
copyFrom: start to: stop
	| newRuns run1 run2 offset1 offset2 | 
	stop < start ifTrue: [^RunArray new].
	self at: start setRunOffsetAndValue: [:r :o :value1 | run1 := r. offset1
:= o.  value1].
	self at: stop setRunOffsetAndValue: [:r :o :value2 | run2 := r. offset2
:= o. value2].
	run1 = run2
		ifTrue: 
			[newRuns := Array with: offset2 - offset1 + 1]
		ifFalse: 
			[newRuns := runs copyFrom: run1 to: run2.
			newRuns at: 1 put: (newRuns at: 1) - offset1.
			newRuns at: newRuns size put: offset2 + 1].
	^RunArray runs: newRuns values: (values copyFrom: run1 to: run2)! !

!RunArray methodsFor: 'copying' stamp: ''!
copyReplaceFrom: start to: stop with: replacement

	^(self copyFrom: 1 to: start - 1)
		, replacement 
		, (self copyFrom: stop + 1 to: self size)! !

!RunArray methodsFor: 'accessing' stamp: 'ar 10/16/2001 18:56'!
first
	^values at: 1! !

!RunArray methodsFor: 'self evaluating' stamp: 'sd 7/31/2005 21:49'!
isSelfEvaluating
	^ self class == RunArray! !

!RunArray methodsFor: 'accessing' stamp: 'ar 10/16/2001 18:56'!
last
	^values at: values size! !

!RunArray methodsFor: 'private' stamp: ''!
mapValues: mapBlock
	"NOTE: only meaningful to an entire set of runs"
	values := values collect: [:val | mapBlock value: val]! !

!RunArray methodsFor: 'printing' stamp: 'sma 6/1/2000 09:47'!
printOn: aStream
	self printNameOn: aStream.
	aStream
		nextPutAll: ' runs: ';
		print: runs;
		nextPutAll: ' values: ';
		print: values! !

!RunArray methodsFor: 'adding' stamp: 'BG 6/12/2003 11:07'!
rangeOf: attr startingAt: startPos
	"Answer an interval that gives the range of attr at index position  startPos. An empty interval with start value startPos is returned when the attribute attr is not present at position startPos.  self size > 0 is assumed, it is the responsibility of the caller to test for emptiness of self.
Note that an attribute may span several adjancent runs. "

	self at: startPos 
		setRunOffsetAndValue: 
            [:run :offset :value | 
               ^(value includes: attr)
                  ifFalse: [startPos to: startPos - 1]
                  ifTrue:
                    [ | firstRelevantPosition lastRelevantPosition idxOfCandidateRun |
                     lastRelevantPosition := startPos - offset + (runs at: run) - 1.
                     firstRelevantPosition := startPos - offset.
                     idxOfCandidateRun := run + 1.
                     [idxOfCandidateRun <= runs size 
                             and: [(values at: idxOfCandidateRun) includes: attr]]
                        whileTrue:
                          [lastRelevantPosition := lastRelevantPosition + (runs at: idxOfCandidateRun).
                           idxOfCandidateRun := idxOfCandidateRun + 1]. 
                     idxOfCandidateRun := run - 1.
                     [idxOfCandidateRun >= 1 
                             and: [(values at: idxOfCandidateRun) includes: attr]]
                        whileTrue:
                          [firstRelevantPosition := firstRelevantPosition - (runs at: idxOfCandidateRun).
                           idxOfCandidateRun := idxOfCandidateRun - 1]. 
 
                    firstRelevantPosition to: lastRelevantPosition]
		  ]! !

!RunArray methodsFor: 'adding' stamp: 'ar 10/16/2001 18:48'!
repeatLast: times  ifEmpty: defaultBlock
	"add the last value back again, the given number of times.  If we are empty, add (defaultBlock value)"
	times = 0 ifTrue: [^self ].
	lastIndex := nil.  "flush access cache"
	(runs size=0)
	  ifTrue:
		[runs := runs copyWith: times.
		values := values copyWith: defaultBlock value]
	  ifFalse:
		[runs at: runs size put: runs last+times] ! !

!RunArray methodsFor: 'adding' stamp: 'ar 10/16/2001 18:48'!
repeatLastIfEmpty: defaultBlock
	"add the last value back again.  If we are empty, add (defaultBlock value)"
	lastIndex := nil.  "flush access cache"
	(runs size=0)
	  ifTrue:[
		 runs := runs copyWith: 1.
		values := values copyWith: defaultBlock value]
	  ifFalse:
		[runs at: runs size put: runs last+1]! !

!RunArray methodsFor: 'converting' stamp: 'BG 6/8/2003 15:17'!
reversed

  ^self class runs: runs reversed values: values reversed! !

!RunArray methodsFor: 'accessing' stamp: ''!
runLengthAt: index 
	"Answer the length remaining in run beginning at index."

	self at: index 
		setRunOffsetAndValue: [:run :offset :value | ^(runs at: run) - offset]! !

!RunArray methodsFor: 'private' stamp: ''!
runs

	^runs! !

!RunArray methodsFor: 'enumerating' stamp: 'ar 12/27/1999 13:43'!
runsAndValuesDo: aBlock
	"Evaluate aBlock with run lengths and values from the receiver"
	^runs with: values do: aBlock.! !

!RunArray methodsFor: 'enumerating' stamp: 'ar 12/17/2001 00:00'!
runsFrom: start to: stop do: aBlock
	"Evaluate aBlock with all existing runs in the range from start to stop"
	| run value index |
	start > stop ifTrue:[^self].
	self at: start setRunOffsetAndValue:[:firstRun :offset :firstValue|
		run := firstRun.
		value := firstValue.
		index := start + (runs at: run) - offset.
		[aBlock value: value.
		index <= stop] whileTrue:[
			run := run + 1.
			value := values at: run.
			index := index + (runs at: run)]].
! !

!RunArray methodsFor: 'private' stamp: 'ar 10/16/2001 18:47'!
setRuns: newRuns setValues: newValues
	lastIndex := nil.  "flush access cache"
	runs := newRuns asArray.
	values := newValues asArray.! !

!RunArray methodsFor: 'accessing' stamp: ''!
size
	| size |
	size := 0.
	1 to: runs size do: [:i | size := size + (runs at: i)].
	^size! !

!RunArray methodsFor: 'printing' stamp: ''!
storeOn: aStream

	aStream nextPut: $(.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' runs: '.
	runs storeOn: aStream.
	aStream nextPutAll: ' values: '.
	values storeOn: aStream.
	aStream nextPut: $)! !

!RunArray methodsFor: 'private' stamp: ''!
values
	"Answer the values in the receiver."

	^values! !

!RunArray methodsFor: 'accessing' stamp: 'di 11/10/97 13:34'!
withStartStopAndValueDo: aBlock
	| start stop |
	start := 1.
	runs with: values do:
		[:len : val | stop := start + len - 1.
		aBlock value: start value: stop value: val.
		start := stop + 1]
		! !

!RunArray methodsFor: 'printing' stamp: ''!
writeOn: aStream

	aStream nextWordPut: runs size.
	1 to: runs size do:
		[:x |
		aStream nextWordPut: (runs at: x).
		aStream nextWordPut: (values at: x)]! !

!RunArray methodsFor: 'printing' stamp: 'tk 12/16/97 09:18'!
writeScanOn: strm
	"Write out the format used for text runs in source files. (14 50 312)f1,f1b,f1LInteger +;i"

	strm nextPut: $(.
	runs do: [:rr | rr printOn: strm.  strm space].
	strm skip: -1; nextPut: $).
	values do: [:vv |
		vv do: [:att | att writeScanOn: strm].
		strm nextPut: $,].
	strm skip: -1.  "trailing comma"! !

ArrayedCollection variableSubclass: #SparseLargeTable
	instanceVariableNames: 'base size chunkSize defaultValue'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

!SparseLargeTable commentStamp: '<historical>' prior: 0!
Derivated from Stephan Pair's LargeArray, but to hold a sparse table, in which most of the entries are the same default value, it uses some tricks.!

!SparseLargeTable class methodsFor: 'accessing' stamp: 'yo 12/1/2003 15:54'!
defaultChunkSize

	^100! !

!SparseLargeTable class methodsFor: 'accessing' stamp: 'yo 12/1/2003 15:54'!
defaultChunkSizeForFiles

	^8000! !

!SparseLargeTable class methodsFor: 'instance creation' stamp: 'yo 12/1/2003 16:06'!
new: size

	^self new: size chunkSize: self defaultChunkSize
! !

!SparseLargeTable class methodsFor: 'instance creation' stamp: 'yo 12/1/2003 16:07'!
new: size chunkSize: chunkSize

	^self new: size chunkSize: chunkSize arrayClass: Array
! !

!SparseLargeTable class methodsFor: 'instance creation' stamp: 'yo 12/1/2003 16:08'!
new: size chunkSize: chunkSize arrayClass: aClass

	^self new: size chunkSize: chunkSize arrayClass: Array base: 1.
! !

!SparseLargeTable class methodsFor: 'instance creation' stamp: 'yo 12/1/2003 16:37'!
new: size chunkSize: chunkSize arrayClass: aClass base: b

	^self new: size chunkSize: chunkSize arrayClass: Array base: 1 defaultValue: nil.
! !

!SparseLargeTable class methodsFor: 'instance creation' stamp: 'yo 12/1/2003 16:37'!
new: size chunkSize: chunkSize arrayClass: aClass base: b defaultValue: d

	| basicSize |
	(basicSize := ((size - 1) // chunkSize) + 1) = 0
		ifTrue: [basicSize := 1].
	^(self basicNew: basicSize)
		initChunkSize: chunkSize size: size arrayClass: aClass base: b defaultValue: d;
		yourself
! !

!SparseLargeTable methodsFor: 'private' stamp: 'yo 12/1/2003 18:58'!
allDefaultValueSubtableAt: index

	| t |
	t := self basicAt: index.
	t ifNil: [^ true].
	t do: [:e |
		e ~= defaultValue ifTrue: [^ false].
	].
	^ true.
! !

!SparseLargeTable methodsFor: 'private' stamp: 'yo 12/1/2003 17:10'!
analyzeSpaceSaving

	| total elems tablesTotal nonNilTables |
	total := size - base + 1.
	elems := 0.
	base to: size do: [:i | (self at: i) ~= defaultValue ifTrue: [elems := elems + 1]].
	tablesTotal := self basicSize.
	nonNilTables := 0.
	1 to: self basicSize do: [:i | (self basicAt: i) ifNotNil: [nonNilTables := nonNilTables + 1]].

	^ String streamContents: [:strm |
		strm nextPutAll: 'total: '.
		strm nextPutAll: total printString.
		strm nextPutAll: ' elements: '.
		strm nextPutAll: elems printString.
		strm nextPutAll: ' tables: '.
		strm nextPutAll: tablesTotal printString.
		strm nextPutAll: ' non-nil: '.
		strm nextPutAll: nonNilTables printString.
	].

! !

!SparseLargeTable methodsFor: 'accessing' stamp: 'yo 12/1/2003 15:58'!
arrayClass

	^(self basicAt: 1) class
! !

!SparseLargeTable methodsFor: 'accessing' stamp: 'yo 12/1/2003 15:58'!
at: index

	self pvtCheckIndex: index.
	^self noCheckAt: index.
! !

!SparseLargeTable methodsFor: 'accessing' stamp: 'yo 12/1/2003 15:58'!
at: index put: value
	
	self pvtCheckIndex: index.
	^self noCheckAt: index put: value
! !

!SparseLargeTable methodsFor: 'accessing' stamp: 'yo 12/1/2003 17:56'!
base

	^ base.
! !

!SparseLargeTable methodsFor: 'accessing' stamp: 'yo 12/1/2003 15:58'!
chunkSize

	^chunkSize
! !

!SparseLargeTable methodsFor: 'private' stamp: 'nk 8/31/2004 08:34'!
copyEmpty
	"Answer a copy of the receiver that contains no elements."
	^self speciesNew: 0
! !

!SparseLargeTable methodsFor: 'private' stamp: 'yo 12/1/2003 22:34'!
findLastNonNilSubTable

	(self basicAt: self basicSize) ifNotNil: [^ self basicSize].

	self basicSize - 1 to: 1 by: -1 do: [:lastIndex |
		(self basicAt: lastIndex) ifNotNil: [^ lastIndex].
	].
	^ 0.
! !

!SparseLargeTable methodsFor: 'initialization' stamp: 'yo 12/1/2003 16:58'!
initChunkSize: aChunkSize size: aSize arrayClass: aClass base: b defaultValue: d

	| lastChunkSize |
	chunkSize := aChunkSize.
	size := aSize.
	base := b.
	defaultValue := d.
	1 to: (self basicSize - 1) do: [ :in | self basicAt: in put: (aClass new: chunkSize withAll: defaultValue) ].
	lastChunkSize := size \\ chunkSize.
	lastChunkSize = 0 ifTrue: [lastChunkSize := chunkSize].
	size = 0 
		ifTrue: [self basicAt: 1 put: (aClass new: 0)]
		ifFalse: [self basicAt: self basicSize put: (aClass new: lastChunkSize withAll: defaultValue)].
! !

!SparseLargeTable methodsFor: 'accessing' stamp: 'tak 12/21/2004 16:59'!
noCheckAt: index
	| chunkIndex t |

	chunkIndex := index - base // chunkSize + 1.
	(chunkIndex > self basicSize or: [chunkIndex < 1]) ifTrue: [^ defaultValue].
	t := self basicAt: chunkIndex.
	t ifNil: [^ defaultValue].
	^ t at: (index - base + 1 - (chunkIndex - 1 * chunkSize))
! !

!SparseLargeTable methodsFor: 'accessing' stamp: 'yo 12/1/2003 19:18'!
noCheckAt: index put: value
	| chunkIndex t |

	chunkIndex := index - base // chunkSize + 1.
	chunkIndex > self basicSize ifTrue: [^ value].
	t :=  self basicAt: chunkIndex.
	t ifNil: [^ value].
	^ t at: (index - base + 1 - (chunkIndex - 1 * chunkSize)) put: value
! !

!SparseLargeTable methodsFor: 'printing' stamp: 'yo 12/1/2003 17:06'!
printElementsOn: aStream
	| element |
	aStream nextPut: $(.
	base to: size do: [:index | element := self at: index. aStream print: element; space].
	self isEmpty ifFalse: [aStream skip: -1].
	aStream nextPut: $)
! !

!SparseLargeTable methodsFor: 'printing' stamp: 'yo 12/1/2003 15:58'!
printOn: aStream

	(#(String) includes: self arrayClass name) 
		ifTrue: [^self storeOn: aStream].
	^super printOn: aStream
! !

!SparseLargeTable methodsFor: 'private' stamp: 'yo 12/1/2003 19:19'!
privateSize: s

	size := s.
! !

!SparseLargeTable methodsFor: 'private' stamp: 'yo 12/1/2003 17:34'!
pvtCheckIndex: index 

	index isInteger ifFalse: [self errorNonIntegerIndex].
	index < 1 ifTrue: [self errorSubscriptBounds: index].
	index > size ifTrue: [self errorSubscriptBounds: index].
! !

!SparseLargeTable methodsFor: 'private' stamp: 'yo 12/1/2003 15:59'!
similarInstance

	^self class
		new: self size 
		chunkSize: self chunkSize 
		arrayClass: self arrayClass
! !

!SparseLargeTable methodsFor: 'private' stamp: 'yo 12/1/2003 15:59'!
similarInstance: newSize

	^self class
		new: newSize 
		chunkSize: self chunkSize 
		arrayClass: self arrayClass
! !

!SparseLargeTable methodsFor: 'private' stamp: 'yo 12/1/2003 15:59'!
similarSpeciesInstance

	^self similarInstance
! !

!SparseLargeTable methodsFor: 'private' stamp: 'yo 12/1/2003 16:00'!
similarSpeciesInstance: newSize

	^self similarInstance: newSize
! !

!SparseLargeTable methodsFor: 'accessing' stamp: 'yo 12/1/2003 15:58'!
size

	^size
! !

!SparseLargeTable methodsFor: 'private' stamp: 'yo 12/1/2003 16:00'!
speciesNew

	^self species
		new: self size 
		chunkSize: self chunkSize 
		arrayClass: self arrayClass
! !

!SparseLargeTable methodsFor: 'private' stamp: 'yo 12/1/2003 16:00'!
speciesNew: newSize

	^self species
		new: newSize 
		chunkSize: self chunkSize 
		arrayClass: self arrayClass
! !

!SparseLargeTable methodsFor: 'printing' stamp: 'yo 12/1/2003 15:59'!
storeOn: aStream

	| x |
	(#(String) includes: self arrayClass name) ifTrue: 
		[aStream nextPut: $'.
		1 to: self size do:
			[:i |
			aStream nextPut: (x := self at: i).
			x == $' ifTrue: [aStream nextPut: x]].
		aStream nextPutAll: ''' asLargeArrayChunkSize: '.
		aStream nextPutAll: self chunkSize asString.
		^self].
	^super storeOn: aStream
! !

!SparseLargeTable methodsFor: 'accessing' stamp: 'kwl 6/30/2006 03:02'!
zapDefaultOnlyEntries

	| lastIndex newInst |
	1 to: self basicSize do: [:i |
		(self allDefaultValueSubtableAt: i) ifTrue: [self basicAt: i put: nil].
	].

	lastIndex _ self findLastNonNilSubTable.
	lastIndex = 0 ifTrue: [^ self].
	
	newInst _ self class new: lastIndex*chunkSize chunkSize: chunkSize arrayClass: (self basicAt: lastIndex) class base: base defaultValue: defaultValue.
	newInst privateSize: self size.
	base to: newInst size do: [:i | newInst at: i put: (self at: i)].
	1 to: newInst basicSize do: [:i |
		(newInst allDefaultValueSubtableAt: i) ifTrue: [newInst basicAt: i put: nil].
	].

	" this is not allowed in production: self becomeForward: newInst. "
	^ newInst.
! !

ArrayedCollection subclass: #String
	instanceVariableNames: ''
	classVariableNames: 'CSNonSeparators CaseInsensitiveOrder AsciiOrder HtmlEntities Tokenish LowercasingTable UppercasingTable CSSeparators CSLineEnders CaseSensitiveOrder'
	poolDictionaries: ''
	category: 'Collections-Strings'!

!String commentStamp: '<historical>' prior: 0!
A String is an indexed collection of Characters. Class String provides the abstract super class for ByteString (that represents an array of 8-bit Characters) and WideString (that represents an array of  32-bit characters).  In the similar manner of LargeInteger and SmallInteger, those subclasses are chosen accordingly for a string; namely as long as the system can figure out so, the String is used to represent the given string.

Strings support a vast array of useful methods, which can best be learned by browsing and trying out examples as you find them in the code.

Here are a few useful methods to look at...
	String match:
	String contractTo:

String also inherits many useful methods from its hierarchy, such as
	SequenceableCollection ,
	SequenceableCollection copyReplaceAll:with:
!

String variableByteSubclass: #ByteString
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Strings'!

!ByteString commentStamp: '<historical>' prior: 0!
This class represents the array of 8 bit wide characters.
!

!ByteString class methodsFor: 'primitives' stamp: 'yo 12/15/2005 13:44'!
compare: string1 with: string2 collated: order
	"Return 1, 2 or 3, if string1 is <, =, or > string2, with the collating order of characters given by the order array."

	| len1 len2 c1 c2 |
	<primitive: 'primitiveCompareString' module: 'MiscPrimitivePlugin'>
	self var: #string1 declareC: 'unsigned char *string1'.
	self var: #string2 declareC: 'unsigned char *string2'.
	self var: #order declareC: 'unsigned char *order'.

	len1 := string1 size.
	len2 := string2 size.
	1 to: (len1 min: len2) do:
		[:i |
		c1 := order at: (string1 basicAt: i) + 1.
		c2 := order at: (string2 basicAt: i) + 1.
		c1 = c2 ifFalse: 
			[c1 < c2 ifTrue: [^ 1] ifFalse: [^ 3]]].
	len1 = len2 ifTrue: [^ 2].
	len1 < len2 ifTrue: [^ 1] ifFalse: [^ 3].
! !

!ByteString class methodsFor: 'primitives' stamp: 'ar 2/3/2001 16:12'!
findFirstInString: aString  inSet: inclusionMap  startingAt: start
	| i stringSize |
	<primitive: 'primitiveFindFirstInString' module: 'MiscPrimitivePlugin'>
	self var: #aString declareC: 'unsigned char *aString'.
	self var: #inclusionMap  declareC: 'char *inclusionMap'.

	inclusionMap size ~= 256 ifTrue: [ ^0 ].

	i := start.
	stringSize := aString size.
	[ i <= stringSize and: [ (inclusionMap at: (aString at: i) asciiValue+1) = 0 ] ] whileTrue: [ 
		i := i + 1 ].

	i > stringSize ifTrue: [ ^0 ].
	^i! !

!ByteString class methodsFor: 'primitives' stamp: 'ar 2/3/2001 16:13'!
indexOfAscii: anInteger inString: aString startingAt: start

	| stringSize |
	<primitive: 'primitiveIndexOfAsciiInString' module: 'MiscPrimitivePlugin'>

	self var: #aCharacter declareC: 'int anInteger'.
	self var: #aString declareC: 'unsigned char *aString'.

	stringSize := aString size.
	start to: stringSize do: [:pos |
		(aString at: pos) asciiValue = anInteger ifTrue: [^ pos]].

	^ 0
! !

!ByteString class methodsFor: 'primitives' stamp: 'ar 9/28/2001 04:35'!
stringHash: aString initialHash: speciesHash

	| stringSize hash low |
	<primitive: 'primitiveStringHash' module: 'MiscPrimitivePlugin'>

	self var: #aHash declareC: 'int speciesHash'.
	self var: #aString declareC: 'unsigned char *aString'.

	stringSize := aString size.
	hash := speciesHash bitAnd: 16rFFFFFFF.
	1 to: stringSize do: [:pos |
		hash := hash + (aString at: pos) asciiValue.
		"Begin hashMultiply"
		low := hash bitAnd: 16383.
		hash := (16r260D * low + ((16r260D * (hash bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384)) bitAnd: 16r0FFFFFFF.
	].
	^ hash! !

!ByteString class methodsFor: 'primitives' stamp: 'ar 2/3/2001 16:12'!
translate: aString from: start  to: stop  table: table
	"translate the characters in the string by the given table, in place"
	<primitive: 'primitiveTranslateStringWithTable' module: 'MiscPrimitivePlugin'>
	self var: #table  declareC: 'unsigned char *table'.
	self var: #aString  declareC: 'unsigned char *aString'.

	start to: stop do: [ :i |
		aString at: i put: (table at: (aString at: i) asciiValue+1) ]! !

!ByteString methodsFor: 'converting' stamp: 'ar 4/10/2005 17:20'!
asByteArray
	| ba sz |
	sz := self byteSize.
	ba := ByteArray new: sz.
	ba replaceFrom: 1 to: sz with: self startingAt: 1.
	^ba! !

!ByteString methodsFor: 'converting' stamp: 'yo 8/28/2002 16:52'!
asOctetString

	^ self.
! !

!ByteString methodsFor: 'accessing' stamp: 'yo 8/26/2002 20:33'!
at: index 
	"Primitive. Answer the Character stored in the field of the receiver
	indexed by the argument. Fail if the index argument is not an Integer or
	is out of bounds. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 63>
	^ Character value: (super at: index)! !

!ByteString methodsFor: 'accessing' stamp: 'ar 4/12/2005 17:33'!
at: index put: aCharacter
	"Primitive. Store the Character in the field of the receiver indicated by
	the index. Fail if the index is not an Integer or is out of bounds, or if
	the argument is not a Character. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 64>
	aCharacter isCharacter 
		ifFalse:[^self errorImproperStore].
	aCharacter isOctetCharacter ifFalse:[
		"Convert to WideString"
		self becomeForward: (WideString from: self).
		^self at: index put: aCharacter.
	].
	index isInteger
		ifTrue: [self errorSubscriptBounds: index]
		ifFalse: [self errorNonIntegerIndex]! !

!ByteString methodsFor: 'accessing' stamp: 'ar 12/27/1999 13:44'!
byteAt: index
	<primitive: 60>
	^(self at: index) asciiValue! !

!ByteString methodsFor: 'accessing' stamp: 'ar 12/27/1999 13:44'!
byteAt: index put: value
	<primitive: 61>
	self at: index put: value asCharacter.
	^value! !

!ByteString methodsFor: 'accessing' stamp: 'ar 3/3/2001 16:17'!
byteSize
	^self size! !

!ByteString methodsFor: 'converting' stamp: 'yo 11/11/2002 12:20'!
convertFromCompoundText

	| readStream writeStream converter |
	readStream := self readStream.
	writeStream := String new writeStream.
	converter := CompoundTextConverter new.
	converter ifNil: [^ self].
	[readStream atEnd] whileFalse: [
		writeStream nextPut: (converter nextFromStream: readStream)].
	^ writeStream contents
! !

!ByteString methodsFor: 'converting' stamp: 'mir 7/20/2004 15:50'!
convertFromSystemString

	| readStream writeStream converter |
	readStream := self readStream.
	writeStream := String new writeStream.
	converter := LanguageEnvironment defaultSystemConverter.
	converter ifNil: [^ self].
	[readStream atEnd] whileFalse: [
		writeStream nextPut: (converter nextFromStream: readStream)].
	^ writeStream contents
! !

!ByteString methodsFor: 'comparing' stamp: 'bf 8/31/2004 13:50'!
findSubstring: key in: body startingAt: start matchTable: matchTable
	"Answer the index in the string body at which the substring key first occurs, at or beyond start.  The match is determined using matchTable, which can be used to effect, eg, case-insensitive matches.  If no match is found, zero will be returned.

	The algorithm below is not optimum -- it is intended to be translated to C which will go so fast that it wont matter."
	| index |
	<primitive: 'primitiveFindSubstring' module: 'MiscPrimitivePlugin'>
	self var: #key declareC: 'unsigned char *key'.
	self var: #body declareC: 'unsigned char *body'.
	self var: #matchTable declareC: 'unsigned char *matchTable'.

	key size = 0 ifTrue: [^ 0].
	start to: body size - key size + 1 do:
		[:startIndex |
		index := 1.
			[(matchTable at: (body at: startIndex+index-1) asciiValue + 1)
				= (matchTable at: (key at: index) asciiValue + 1)]
				whileTrue:
				[index = key size ifTrue: [^ startIndex].
				index := index+1]].
	^ 0
"
' ' findSubstring: 'abc' in: 'abcdefabcd' startingAt: 1 matchTable: CaseSensitiveOrder 1
' ' findSubstring: 'abc' in: 'abcdefabcd' startingAt: 2 matchTable: CaseSensitiveOrder 7
' ' findSubstring: 'abc' in: 'abcdefabcd' startingAt: 8 matchTable: CaseSensitiveOrder 0
' ' findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseSensitiveOrder 0
' ' findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseInsensitiveOrder 7
"! !

!ByteString methodsFor: 'testing' stamp: 'ar 4/10/2005 18:04'!
isByteString
	"Answer whether the receiver is a ByteString"
	^true! !

!ByteString methodsFor: 'testing' stamp: 'ar 4/10/2005 17:28'!
isOctetString
	"Answer whether the receiver can be represented as a byte string. 
	This is different from asking whether the receiver *is* a ByteString 
	(i.e., #isByteString)"
	^ true.
! !

!ByteString methodsFor: 'accessing' stamp: 'ar 4/12/2005 17:33'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	replacement class == WideString ifTrue: [
		self becomeForward: (WideString from: self).
	]. 

	super replaceFrom: start to: stop with: replacement startingAt: repStart.
! !

!ByteString methodsFor: 'converting' stamp: 'ar 7/21/2007 21:51'!
substrings
	"Answer an array of the substrings that compose the receiver."
	| result end beginning |
	result := WriteStream on: (Array new: 10).
	end := 0.
	"find one substring each time through this loop"
	[ "find the beginning of the next substring"
	beginning := self indexOfAnyOf: CSNonSeparators 
							startingAt: end+1 ifAbsent: [ nil ].
	beginning ~~ nil ] whileTrue: [
		"find the end"
		end := self indexOfAnyOf: CSSeparators 
					startingAt: beginning ifAbsent: [ self size + 1 ].
		end := end - 1.
		result nextPut: (self copyFrom: beginning to: end).
	].
	^result contents! !

!String class methodsFor: 'primitives' stamp: 'yo 12/15/2005 13:41'!
compare: string1 with: string2 collated: order
	"Return 1, 2 or 3, if string1 is <, =, or > string2, with the collating order of characters given by the order array."

	| len1 len2 c1 c2 |
	order == nil ifTrue: [
		len1 := string1 size.
		len2 := string2 size.
		1 to: (len1 min: len2) do:[:i |
			c1 := (string1 at: i) asInteger.
			c2 := (string2 at: i) asInteger.
			c1 = c2 ifFalse: [c1 < c2 ifTrue: [^ 1] ifFalse: [^ 3]].
		].
		len1 = len2 ifTrue: [^ 2].
		len1 < len2 ifTrue: [^ 1] ifFalse: [^ 3].
	].
	len1 := string1 size.
	len2 := string2 size.
	1 to: (len1 min: len2) do:[:i |
		c1 := (string1 at: i) asInteger.
		c2 := (string2 at: i) asInteger.
		c1 < 256 ifTrue: [c1 := order at: c1 + 1].
		c2 < 256 ifTrue: [c2 := order at: c2 + 1].
		c1 = c2 ifFalse:[c1 < c2 ifTrue: [^ 1] ifFalse: [^ 3]].
	].
	len1 = len2 ifTrue: [^ 2].
	len1 < len2 ifTrue: [^ 1] ifFalse: [^ 3].
! !

!String class methodsFor: 'instance creation' stamp: 'yo 11/3/2004 19:24'!
cr
	"Answer a string containing a single carriage return character."

	^ self with: Character cr
! !

!String class methodsFor: 'instance creation' stamp: 'yo 11/3/2004 19:24'!
crlf
	"Answer a string containing a carriage return and a linefeed."

	^ self with: Character cr with: Character lf
! !

!String class methodsFor: 'instance creation' stamp: 'yo 11/3/2004 19:24'!
crlfcrlf
	^self crlf , self crlf.
! !

!String class methodsFor: 'examples' stamp: 'yo 11/3/2004 19:24'!
example
	"To see the string displayed at the cursor point, execute this expression
	and select a point by pressing a mouse button."

	'this is some text' displayOn: Display at: Sensor waitButton! !

!String class methodsFor: 'formatting' stamp: 'md 6/5/2005 07:49'!
expandMacro: macroType argument: argument withExpansions: expansions 
	macroType = $s ifTrue: [^expansions at: argument].
	macroType = $p ifTrue: [^(expansions at: argument) printString].
	macroType = $n ifTrue: [^String cr].
	macroType = $t ifTrue: [^String tab].
	self error: 'unknown expansion type'! !

!String class methodsFor: 'primitives' stamp: 'nice 3/15/2007 20:59'!
findFirstInString: aString inCharacterSet: aCharacterSet startingAt: start 
	"Trivial, non-primitive version"
	
	start
		to: aString size
		do: [:i | (aCharacterSet
					includes: (aString at: i))
				ifTrue: [^ i]].
	^ 0! !

!String class methodsFor: 'primitives' stamp: 'nice 5/9/2006 20:17'!
findFirstInString: aString inSet: inclusionMap startingAt: start
	"Trivial, non-primitive version"
	
	| i stringSize ascii more |
	inclusionMap size ~= 256 ifTrue: [^ 0].
	stringSize := aString size.
	more := true.
	i := start - 1.
	[more and: [(i := i + 1) <= stringSize]] whileTrue: [
		ascii := (aString at: i) asciiValue.
		more := ascii < 256 ifTrue: [(inclusionMap at: ascii + 1) = 0] ifFalse: [true].
	].

	i > stringSize ifTrue: [^ 0].
	^ i! !

!String class methodsFor: 'instance creation' stamp: 'ar 4/10/2005 16:24'!
fromByteArray: aByteArray

	^ aByteArray asString
! !

!String class methodsFor: 'instance creation' stamp: 'yo 11/3/2004 19:24'!
fromPacked: aLong
	"Convert from a longinteger to a String of length 4."

	| s |
	s := self new: 4.
	s at: 1 put: (aLong digitAt: 4) asCharacter.
	s at: 2 put: (aLong digitAt: 3) asCharacter.
	s at: 3 put: (aLong digitAt: 2) asCharacter.
	s at: 4 put: (aLong digitAt: 1) asCharacter.
	^s

"String fromPacked: 'TEXT' asPacked"
! !

!String class methodsFor: 'instance creation' stamp: 'yo 11/3/2004 19:24'!
fromString: aString 
	"Answer an instance of me that is a copy of the argument, aString."
	
	^ aString copyFrom: 1 to: aString size! !

!String class methodsFor: 'primitives' stamp: 'ar 4/10/2005 16:36'!
indexOfAscii: anInteger inString: aString startingAt: start
	"Trivial, non-primitive version"
	| stringSize |
	stringSize := aString size.
	start to: stringSize do: [:pos |
		(aString at: pos) asInteger = anInteger ifTrue: [^ pos]].
	^ 0
! !

!String class methodsFor: 'initialization' stamp: 'ar 4/9/2005 22:37'!
initialize   "self initialize"

	| order |
	AsciiOrder := (0 to: 255) as: ByteArray.

	CaseInsensitiveOrder := AsciiOrder copy.
	($a to: $z) do:
		[:c | CaseInsensitiveOrder at: c asciiValue + 1
				put: (CaseInsensitiveOrder at: c asUppercase asciiValue +1)].

	"Case-sensitive compare sorts space, digits, letters, all the rest..."
	CaseSensitiveOrder := ByteArray new: 256 withAll: 255.
	order := -1.
	' 0123456789' do:  "0..10"
		[:c | CaseSensitiveOrder at: c asciiValue + 1 put: (order := order+1)].
	($a to: $z) do:     "11-64"
		[:c | CaseSensitiveOrder at: c asUppercase asciiValue + 1 put: (order := order+1).
		CaseSensitiveOrder at: c asciiValue + 1 put: (order := order+1)].
	1 to: CaseSensitiveOrder size do:
		[:i | (CaseSensitiveOrder at: i) = 255 ifTrue:
			[CaseSensitiveOrder at: i put: (order := order+1)]].
	order = 255 ifFalse: [self error: 'order problem'].

	"a table for translating to lower case"
	LowercasingTable := String withAll: (Character allByteCharacters collect: [:c | c asLowercase]).

	"a table for translating to upper case"
	UppercasingTable := String withAll: (Character allByteCharacters collect: [:c | c asUppercase]).

	"a table for testing tokenish (for fast numArgs)"
	Tokenish := String withAll: (Character allByteCharacters collect:
									[:c | c tokenish ifTrue: [c] ifFalse: [$~]]).

	"CR and LF--characters that terminate a line"
	CSLineEnders := CharacterSet empty.
	CSLineEnders add: Character cr.
	CSLineEnders add: Character lf.

 	"separators and non-separators"
	CSSeparators := CharacterSet separators.
	CSNonSeparators := CSSeparators complement.! !

!String class methodsFor: 'initialization' stamp: 'yo 8/11/2003 21:11'!
initializeHtmlEntities
	"self initializeHtmlEntities"

	HtmlEntities := (Dictionary new: 128)
		at: 'amp'	put: $&;
		at: 'lt'		put: $<;
		at: 'gt'		put: $>;
		at: 'quot'	put: $";
		at: 'euro'	put: Character euro;
		yourself.
	#('nbsp' 'iexcl' 'cent' 'pound' 'curren' 'yen' 'brvbar' 'sect' 'uml' 'copy' 'ordf' 'laquo' 'not' 'shy' 'reg' 'hibar' 'deg' 'plusmn' 'sup2' 'sup3' 'acute' 'micro' 'para' 'middot' 'cedil' 'sup1' 'ordm' 'raquo' 'frac14' 'frac12' 'frac34' 'iquest' 'Agrave' 'Aacute' 'Acirc' 'Atilde' 'Auml' 'Aring' 'AElig' 'Ccedil' 'Egrave' 'Eacute' 'Ecirc' 'Euml' 'Igrave' 'Iacute' 'Icirc' 'Iuml' 'ETH' 'Ntilde' 'Ograve' 'Oacute' 'Ocirc' 'Otilde' 'Ouml' 'times' 'Oslash' 'Ugrave' 'Uacute' 'Ucirc' 'Uuml' 'Yacute' 'THORN' 'szlig' 'agrave' 'aacute' 'acirc' 'atilde' 'auml' 'aring' 'aelig' 'ccedil' 'egrave' 'eacute' 'ecirc' 'euml' 'igrave' 'iacute' 'icirc' 'iuml' 'eth' 'ntilde' 'ograve' 'oacute' 'ocirc' 'otilde' 'ouml' 'divide' 'oslash' 'ugrave' 'uacute' 'ucirc' 'uuml' 'yacute' 'thorn' 'yuml' ) withIndexDo: [:each :index | HtmlEntities at: each put: (index + 159) asCharacter]! !

!String class methodsFor: 'instance creation' stamp: 'yo 11/3/2004 19:24'!
lf
	"Answer a string containing a single carriage return character."

	^ self with: Character lf! !

!String class methodsFor: 'instance creation' stamp: 'ar 4/10/2005 23:26'!
new: sizeRequested 
	"Answer an instance of this class with the number of indexable
	variables specified by the argument, sizeRequested."
	self == String 
		ifTrue:[^ByteString new: sizeRequested]
		ifFalse:[^self basicNew: sizeRequested].! !

!String class methodsFor: 'instance creation' stamp: 'yo 8/28/2002 13:27'!
readFrom: inStream
	"Answer an instance of me that is determined by reading the stream, 
	inStream. Embedded double quotes become the quote Character."

	| outStream char done |
	outStream := WriteStream on: (self new: 16).
	"go to first quote"
	inStream skipTo: $'.
	done := false.
	[done or: [inStream atEnd]]
		whileFalse: 
			[char := inStream next.
			char = $'
				ifTrue: 
					[char := inStream next.
					char = $'
						ifTrue: [outStream nextPut: char]
						ifFalse: [done := true]]
				ifFalse: [outStream nextPut: char]].
	^outStream contents! !

!String class methodsFor: 'instance creation' stamp: 'dc 2/12/2007 10:47'!
space
	"Answer a string containing a single space character."

	^ self with: Character space
! !

!String class methodsFor: 'primitives' stamp: 'ar 4/10/2005 16:29'!
stringHash: aString initialHash: speciesHash
	| stringSize hash low |
	stringSize := aString size.
	hash := speciesHash bitAnd: 16rFFFFFFF.
	1 to: stringSize do: [:pos |
		hash := hash + (aString at: pos) asInteger.
		"Begin hashMultiply"
		low := hash bitAnd: 16383.
		hash := (16r260D * low + ((16r260D * (hash bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384)) bitAnd: 16r0FFFFFFF.
	].
	^ hash.
! !

!String class methodsFor: 'instance creation' stamp: 'yo 11/3/2004 19:24'!
tab
	"Answer a string containing a single tab character."

	^ self with: Character tab
! !

!String class methodsFor: 'primitives' stamp: 'ar 4/10/2005 16:36'!
translate: aString from: start  to: stop  table: table
	"Trivial, non-primitive version"
	| char |
	start to: stop do: [:i |
		char := (aString at: i) asInteger.
		char < 256 ifTrue: [aString at: i put: (table at: char+1)].
	].
! !

!String class methodsFor: 'instance creation' stamp: 'yo 8/28/2002 13:29'!
value: anInteger

	^ self with: (Character value: anInteger).
! !

!String class methodsFor: 'instance creation' stamp: 'ar 4/12/2005 17:34'!
with: aCharacter
	| newCollection |
	aCharacter asInteger < 256
		ifTrue:[newCollection := ByteString new: 1]
		ifFalse:[newCollection := WideString new: 1].
	newCollection at: 1 put: aCharacter.
	^newCollection! !

!String methodsFor: 'arithmetic' stamp: 'yo 11/3/2004 19:24'!
* arg

	^ arg adaptToString: self andSend: #*! !

!String methodsFor: 'arithmetic' stamp: 'yo 11/3/2004 19:24'!
+ arg

	^ arg adaptToString: self andSend: #+! !

!String methodsFor: 'arithmetic' stamp: 'yo 11/3/2004 19:24'!
- arg

	^ arg adaptToString: self andSend: #-! !

!String methodsFor: 'arithmetic' stamp: 'yo 11/3/2004 19:24'!
/ arg

	^ arg adaptToString: self andSend: #/! !

!String methodsFor: 'arithmetic' stamp: 'yo 11/3/2004 19:24'!
// arg

	^ arg adaptToString: self andSend: #//! !

!String methodsFor: 'comparing' stamp: 'lr 7/7/2006 11:19'!
< aString 
	"Answer whether the receiver sorts before aString.
	The collation order is simple ascii (with case differences)."

	^ (self compare: self with: aString collated: AsciiOrder) = 1! !

!String methodsFor: 'comparing' stamp: 'lr 7/7/2006 11:20'!
<= aString 
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is simple ascii (with case differences)."
	
	^ (self compare: self with: aString collated: AsciiOrder) <= 2! !

!String methodsFor: 'comparing' stamp: 'lr 7/7/2006 11:21'!
= aString 
	"Answer whether the receiver sorts equally as aString.
	The collation order is simple ascii (with case differences)."
	
	aString isString ifFalse: [ ^ false ].
	^ (self compare: self with: aString collated: AsciiOrder) = 2! !

!String methodsFor: 'comparing' stamp: 'lr 7/7/2006 11:21'!
> aString 
	"Answer whether the receiver sorts after aString.
	The collation order is simple ascii (with case differences)."

	^ (self compare: self with: aString collated: AsciiOrder) = 3! !

!String methodsFor: 'comparing' stamp: 'lr 7/7/2006 11:21'!
>= aString 
	"Answer whether the receiver sorts after or equal to aString.
	The collation order is simple ascii (with case differences)."

	^ (self compare: self with: aString collated: AsciiOrder) >= 2! !

!String methodsFor: 'arithmetic' stamp: 'yo 11/3/2004 19:24'!
\\ arg

	^ arg adaptToString: self andSend: #\\! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
adaptToCollection: rcvr andSend: selector
	"If I am involved in arithmetic with a collection, convert me to a number."

	^ rcvr perform: selector with: self asNumber! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
adaptToNumber: rcvr andSend: selector
	"If I am involved in arithmetic with a number, convert me to a number."

	^ rcvr perform: selector with: self asNumber! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
adaptToPoint: rcvr andSend: selector
	"If I am involved in arithmetic with a point, convert me to a number."

	^ rcvr perform: selector with: self asNumber! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
adaptToString: rcvr andSend: selector
	"If I am involved in arithmetic with a string, convert us both to
	numbers, and return the printString of the result."

	^ (rcvr asNumber perform: selector with: self asNumber) printString! !

!String methodsFor: 'comparing' stamp: 'yo 11/3/2004 19:24'!
alike: aString 
	"Answer some indication of how alike the receiver is to the argument,  0 is no match, twice aString size is best score.  Case is ignored."

	| i j k minSize bonus |
	minSize := (j := self size) min: (k := aString size).
	bonus := (j - k) abs < 2 ifTrue: [ 1 ] ifFalse: [ 0 ].
	i := 1.
	[(i <= minSize) and: [((super at: i) bitAnd: 16rDF)  = ((aString at: i) asciiValue bitAnd: 16rDF)]]
		whileTrue: [ i := i + 1 ].
	[(j > 0) and: [(k > 0) and:
		[((super at: j) bitAnd: 16rDF) = ((aString at: k) asciiValue bitAnd: 16rDF)]]]
			whileTrue: [ j := j - 1.  k := k - 1. ].
	^ i - 1 + self size - j + bonus. ! !

!String methodsFor: 'converting' stamp: 'mw 1/30/2004 11:53'!
asAlphaNumeric: totalSize extraChars: additionallyAllowed mergeUID: minimalSizeOfRandomPart
	"Generates a String with unique identifier ( UID ) qualities, the difference to a
	 UUID is that its beginning is derived from the receiver, so that it has a meaning
	 for a human reader.

	 Answers a String of totalSize, which consists of 3 parts
	 1.part: the beginning of the receiver only consisting of
		a-z, A-Z, 0-9 and extraChars in Collection additionallyAllowed ( which can be nil )
	 2.part: a single _
	 3.part: a ( random ) UID of size >= minimalSizeOfRandomPart consisting of
		a-z, A-Z, 0-9

	 Starting letters are capitalized. 
	 TotalSize must be at least 1.
	 Exactly 1 occurrence of $_ is guaranteed ( unless additionallyAllowed includes $_ ).
	 The random part has even for small sizes good UID qualitites for many practical purposes.
	 If only lower- or uppercase letters are demanded, simply convert the answer with
	 say #asLowercase. The probability of a duplicate will rise only moderately ( see below ).

	 Example: 
		size of random part = 10
		in n generated UIDs the chance p of having non-unique UIDs is
			n = 10000 ->  p < 1e-10		if answer is reduced to lowerCase: p < 1.4 e-8
			n = 100000 -> p < 1e-8
		at the bottom is a snippet for your own calculations  
		Note: the calculated propabilites are theoretical,
			for the actually used random generator they may be much worse"

	| stream out sizeOfFirstPart index ascii ch skip array random |
	totalSize > minimalSizeOfRandomPart 
		ifFalse: [ self errorOutOfBounds ].
	stream := ReadStream on: self.
	out := WriteStream on: ( String new: totalSize ).
	index := 0.
	skip := true.
	sizeOfFirstPart := totalSize - minimalSizeOfRandomPart - 1.
	[ stream atEnd or: [ index >= sizeOfFirstPart ]]
	whileFalse: [
		((( ascii := ( ch := stream next ) asciiValue ) >= 65 and: [ ascii <= 90 ]) or: [
			( ascii >= 97 and: [ ascii <= 122 ]) or: [			 
			ch isDigit or: [
			additionallyAllowed notNil and: [ additionallyAllowed includes: ch ]]]])
		ifTrue: [
			skip
				ifTrue: [ out nextPut: ch asUppercase ]
				ifFalse: [ out nextPut: ch ].
			index := index + 1.
			skip := false ]
		ifFalse: [ skip := true ]].
	out nextPut: $_.
	array := Array new: 62.
	1 to: 26 do: [ :i |
		array at: i put: ( i + 64 ) asCharacter.
		array at: i + 26 put: ( i + 96 ) asCharacter ].
	53 to: 62 do: [ :i |
		array at: i put: ( i - 5 ) asCharacter ].
	random := UUIDGenerator default randomGenerator. 
	totalSize - index - 1 timesRepeat: [
		out nextPut: ( array atRandom: random )].
	^out contents

	"	calculation of probability p for failure of uniqueness in n UIDs
		Note: if answer will be converted to upper or lower case replace 62 with 36
	| n i p all |
	all := 62 raisedTo: sizeOfRandomPart.
	i := 1.
	p := 0.0 .
	n := 10000.
	[ i <= n ]
	whileTrue: [
		p := p + (( i - 1 ) / all ).
		i := i + 1 ].
	p   

	approximation formula: n squared / ( 62.0 raisedTo: sizeOfRandomPart ) / 2 
	" 

	"'Crop SketchMorphs and Grab Screen Rect to JPG' 
			asAlphaNumeric: 31 extraChars: nil mergeUID: 10  
	 			'CropSketchMorphsAndG_iOw94jquN6'
	 'Monticello' 
			asAlphaNumeric: 31 extraChars: nil mergeUID: 10    
				'Monticello_kp6aV2l0IZK9uBULGOeG' 
	 'version-', ( '1.1.2' replaceAll: $. with: $- )
			asAlphaNumeric: 31 extraChars: #( $- ) mergeUID: 10    
				'Version-1-1-2_kuz2tMg2xX9iRLDVR'"
		! !

!String methodsFor: 'converting' stamp: 'ar 4/10/2005 17:18'!
asByteArray
	"Convert to a ByteArray with the ascii values of the string."
	| b |
	b := ByteArray new: self byteSize.
	1 to: self size * 4 do: [:i |
		b at: i put: (self byteAt: i).
	].
	^ b.
! !

!String methodsFor: 'converting' stamp: 'ar 4/10/2005 16:22'!
asByteString
	"Convert the receiver into a ByteString"
	^self asOctetString! !

!String methodsFor: 'converting' stamp: 'ar 4/10/2005 17:03'!
asCharacter
	"Answer the receiver's first character, or '*' if none.  Idiosyncratic, provisional."

	^ self size > 0 ifTrue: [self first] ifFalse:[$·]! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asDate
	"Many allowed forms, see Date>>#readFrom:"

	^ Date fromString: self! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asDateAndTime

	"Convert from UTC format" 	^ DateAndTime fromString: self! !

!String methodsFor: 'converting' stamp: 'yo 10/22/2002 17:38'!
asDefaultDecodedString

	^ self
! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asDisplayText
	"Answer a DisplayText whose text string is the receiver."

	^DisplayText text: self asText! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asDuration
	"convert from [nnnd]hh:mm:ss[.nanos] format. [] implies optional elements"

	^ Duration fromString: self
! !

!String methodsFor: 'user interface' stamp: 'md 7/28/2005 20:42'!
asExplorerString

	^ self printString! !

!String methodsFor: 'converting' stamp: 'ar 4/12/2005 13:55'!
asFileName
	"Answer a String made up from the receiver that is an acceptable file 
	name."

	| string checkedString |
	string := FileDirectory checkName: self fixErrors: true.
	checkedString := (FilePath pathName: string) asVmPathName.
	^ (FilePath pathName: checkedString isEncoded: true) asSqueakPathName.
! !

!String methodsFor: 'converting' stamp: 'yo 8/27/2002 14:38'!
asFourCode

	| result |
	self size = 4 ifFalse: [^self error: 'must be exactly four characters'].
	result := self inject: 0 into: [:val :each | 256 * val + each asciiValue].
	(result bitAnd: 16r80000000) = 0 
		ifFalse: [self error: 'cannot resolve fourcode'].
	(result bitAnd: 16r40000000) = 0 ifFalse: [^result - 16r80000000].
	^ result
! !

!String methodsFor: 'converting' stamp: 'yo 8/26/2002 23:06'!
asHex
	| stream |
	stream := WriteStream on: (String new: self size * 4).
	self do: [ :ch | stream nextPutAll: ch hex ].
	^stream contents! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asHtml
	"Do the basic character conversion for HTML.  Leave all original return 
	and tabs in place, so can conver back by simply removing bracked 
	things. 4/4/96 tk"
	| temp |
	temp := self copyReplaceAll: '&' with: '&amp;'.
	HtmlEntities keysAndValuesDo:
		[:entity :char |
		char = $& ifFalse:
			[temp := temp copyReplaceAll: char asString with: '&' , entity , ';']].
	temp := temp copyReplaceAll: '	' with: '	<IMG SRC="tab.gif" ALT="    ">'.
	temp := temp copyReplaceAll: '
' with: '
<BR>'.
	^ temp

"
	'A<&>B' asHtml
"! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asIRCLowercase
	"Answer a String made up from the receiver whose characters are all 
	lowercase, where 'lowercase' is by IRC's definition"

	^self collect: [ :c | c asIRCLowercase ]! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asIdentifier: shouldBeCapitalized
	"Return a legal identifier, with first character in upper case if shouldBeCapitalized is true, else lower case.  This will always return a legal identifier, even for an empty string"

	| aString firstChar firstLetterPosition |
	aString := self select: [:el | el isAlphaNumeric].
	firstLetterPosition := aString findFirst: [:ch | ch isLetter].
	aString := firstLetterPosition == 0
		ifFalse:
			[aString copyFrom: firstLetterPosition to: aString size]
		ifTrue:
			['a', aString].
	firstChar := shouldBeCapitalized ifTrue: [aString first asUppercase] ifFalse: [aString first asLowercase].

	^ firstChar asString, (aString copyFrom: 2 to: aString size)
"
'234Fred987' asIdentifier: false
'235Fred987' asIdentifier: true
'' asIdentifier: true
'()87234' asIdentifier: false
'())z>=PPve889  U >' asIdentifier: false

"! !

!String methodsFor: 'converting' stamp: 'laza 10/1/2004 09:55'!
asInteger 
	^self asSignedInteger
! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asLegalSelector
	| toUse |
	toUse := ''.
	self do:
		[:char | char isAlphaNumeric ifTrue: [toUse := toUse copyWith: char]].
	(self size == 0 or: [self first isLetter not])
		ifTrue:		[toUse := 'v', toUse].

	^ toUse withFirstCharacterDownshifted

"'234znak 43 ) 2' asLegalSelector"! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asLowercase
	"Answer a String made up from the receiver whose characters are all 
	lowercase."

	^ self copy asString translateToLowercase! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asNumber 
	"Answer the Number created by interpreting the receiver as the string 
	representation of a number."

	^Number readFromString: self! !

!String methodsFor: 'converting' stamp: 'ar 4/10/2005 20:55'!
asOctetString
	"Convert the receiver into an octet string"
	| string |
	string := String new: self size.
	1 to: self size do: [:i | string at: i put: (self at: i)].
	^string! !

!String methodsFor: 'converting' stamp: 'yo 8/27/2002 14:39'!
asPacked
	"Convert to a longinteger that describes the string"

	^ self inject: 0 into: [ :pack :next | pack := pack * 256 + next asInteger ].! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asParagraph
	"Answer a Paragraph whose text string is the receiver."

	^Paragraph withText: self asText! !

!String methodsFor: 'converting' stamp: 'dew 9/13/2001 01:17'!
asPluralBasedOn: aNumberOrCollection
	"Append an 's' to this string based on whether aNumberOrCollection is 1 or of size 1."

	^ (aNumberOrCollection = 1 or:
		[aNumberOrCollection isCollection and: [aNumberOrCollection size = 1]])
			ifTrue: [self]
			ifFalse: [self, 's']
! !

!String methodsFor: 'converting' stamp: 'dc 8/1/2007 16:02'!
asSignedInteger
	"Returns the first signed integer it can find or nil."

	| start stream |
	start := self findFirst: [:char | char isDigit].
	start isZero ifTrue: [^ nil].
	stream := (ReadStream on: self) position: start.
	stream oldBack = $- ifTrue: [stream oldBack].
	^ Integer readFrom: stream! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asSmalltalkComment
	"return this string, munged so that it can be treated as a comment in Smalltalk code.  Quote marks are added to the beginning and end of the string, and whenever a solitary quote mark appears within the string, it is doubled"

	^String streamContents:  [ :str |
		| quoteCount first |

		str nextPut: $".
	
		quoteCount := 0.
		first := true.
		self do: [ :char |
			char = $"
				ifTrue: [
					first ifFalse: [
						str nextPut: char.
						quoteCount := quoteCount + 1 ] ]
				ifFalse: [
					quoteCount odd ifTrue: [
						"add a quote to even the number of quotes in a row"
						str nextPut: $" ].
					quoteCount := 0.
					str nextPut: char ].
			first := false ]. 

		quoteCount odd ifTrue: [
			"check at the end"
			str nextPut: $". ].

		str nextPut: $".
	].
	! !

!String methodsFor: 'converting' stamp: 'yo 12/19/2003 21:16'!
asSqueakPathName

	^ self.
! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asString
	"Answer this string."

	^ self
! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asStringOrText
	"Answer this string."

	^ self
! !

!String methodsFor: 'converting' stamp: 'ar 4/10/2005 19:24'!
asSymbol
	"Answer the unique Symbol whose characters are the characters of the 
	string."
	^Symbol intern: self! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asText
	"Answer a Text whose string is the receiver."

	^Text fromString: self! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asTime
	"Many allowed forms, see Time>>readFrom:"

	^ Time fromString: self.! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asTimeStamp
	"Convert from obsolete TimeStamp format"

	^ TimeStamp fromString: self! !

!String methodsFor: 'converting' stamp: 'edc 4/23/2007 09:38'!
asUnHtml
	"Strip out all Html stuff (commands in angle brackets <>) and convert
the characters &<> back to their real value.  Leave actual cr and tab as
they were in text."
	| in out char rest did |
	in := ReadStream on: self.
	out := WriteStream on: (String new: self size).
	[in atEnd] whileFalse:
		[in peek = $<
			ifTrue: [in unCommand] 	"Absorb <...><...>"
			ifFalse: [(char := in next) = $&
						ifTrue: [rest := in upTo: $;.
								did := out position.
								rest = 'lt' ifTrue: [out nextPut: $<].
								rest = 'gt' ifTrue: [out nextPut: $>].
								rest = 'amp' ifTrue: [out nextPut: $&].
								rest = 'deg' ifTrue: [out nextPut: $°].
								rest = 'quot' ifTrue: [out nextPut: $"].
								rest = 'nbsp' ifTrue: [out nextPut: Character space].
								did = out position ifTrue: [
									self error: 'unknown encoded HTML char'.
									"Please add it to this method"]]
						ifFalse: [out nextPut: char]].
		].
	^ out contents! !

!String methodsFor: 'converting' stamp: 'laza 10/1/2004 10:02'!
asUnsignedInteger 
	"Returns the first integer it can find or nil."

	| start stream |
	start := self findFirst: [:char | char isDigit].
	start isZero ifTrue: [^nil].
	stream := (ReadStream on: self) position: start - 1.
	^Integer readFrom: stream! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asUppercase
	"Answer a String made up from the receiver whose characters are all 
	uppercase."

	^self copy asString translateToUppercase! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asUrl
	"convert to a Url"
	"'http://www.cc.gatech.edu/' asUrl"
	"msw://chaos.resnet.gatech.edu:9000/' asUrl"
	^Url absoluteFromText: self! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
asUrlRelativeTo: aUrl
	^aUrl newFromRelativeText: self! !

!String methodsFor: 'converting' stamp: 'yo 2/24/2005 18:33'!
asVmPathName

	^ (FilePath pathName: self) asVmPathName.
! !

!String methodsFor: 'converting' stamp: 'ar 4/12/2005 17:36'!
asWideString 
	self isWideString
		ifTrue:[^self]
		ifFalse:[^WideString from: self]! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
askIfAddStyle: priorMethod req: requestor
	^ self   "we are a string with no text style"! !

!String methodsFor: 'printing' stamp: 'yo 11/3/2004 19:24'!
basicType
	"Answer a symbol representing the inherent type of the receiver"

	"Number String Boolean player collection sound color etc"
	^ #String! !

!String methodsFor: 'comparing' stamp: 'yo 11/3/2004 19:24'!
beginsWith: prefix
	"Answer whether the receiver begins with the given prefix string.
	The comparison is case-sensitive."

	self size < prefix size ifTrue: [^ false].
	^ (self findSubstring: prefix in: self startingAt: 1
			matchTable: CaseSensitiveOrder) = 1
! !

!String methodsFor: 'accessing' stamp: 'ar 4/12/2005 16:30'!
byteAt: index
	^self subclassResponsibility! !

!String methodsFor: 'accessing' stamp: 'ar 4/12/2005 16:30'!
byteAt: index put: value
	^self subclassResponsibility! !

!String methodsFor: 'filter streaming' stamp: 'yo 8/26/2002 22:31'!
byteEncode:aStream

	^aStream writeString: self.
! !

!String methodsFor: 'accessing' stamp: 'ar 4/12/2005 16:30'!
byteSize
	^self subclassResponsibility! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
capitalized
	"Return a copy with the first letter capitalized"
	| cap |
	self isEmpty ifTrue: [ ^self copy ].
	cap := self copy.
	cap at: 1 put: (cap at: 1) asUppercase.
	^ cap! !

!String methodsFor: 'comparing' stamp: 'ar 4/10/2005 16:39'!
caseInsensitiveLessOrEqual: aString 
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case insensitive."
	^(self compare: aString caseSensitive: false) <= 2! !

!String methodsFor: 'comparing' stamp: 'ar 4/10/2005 16:39'!
caseSensitiveLessOrEqual: aString 
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case sensitive."
	^(self compare: aString caseSensitive: true) <= 2! !

!String methodsFor: 'comparing' stamp: 'yo 8/27/2002 14:15'!
charactersExactlyMatching: aString
	"Do a character-by-character comparison between the receiver and aString.  Return the index of the final character that matched exactly."

	| count |
	count := self size min: aString size.
	1 to: count do: [:i | 
		(self at: i) = (aString at: i) ifFalse: [
			^ i - 1]].
	^ count! !

!String methodsFor: 'comparing' stamp: 'ar 4/10/2005 16:38'!
compare: aString 
	"Answer a comparison code telling how the receiver sorts relative to aString:
		1 - before
		2 - equal
		3 - after.
	The collation sequence is ascii with case differences ignored.
	To get the effect of a <= b, but ignoring case, use (a compare: b) <= 2."
	^self compare: aString caseSensitive: false! !

!String methodsFor: 'comparing' stamp: 'ar 4/10/2005 16:42'!
compare: aString caseSensitive: aBool
	"Answer a comparison code telling how the receiver sorts relative to aString:
		1 - before
		2 - equal
		3 - after.
	"
	| map |
	map := aBool ifTrue:[CaseSensitiveOrder] ifFalse:[CaseInsensitiveOrder].
	^self compare: self with: aString collated: map! !

!String methodsFor: 'comparing' stamp: 'yo 12/15/2005 14:28'!
compare: string1 with: string2 collated: order

	(string1 isByteString and: [string2 isByteString]) ifTrue: [
		^ ByteString compare: string1 with: string2 collated: order
	].
     "Primitive does not fail properly right now"
      ^ String compare: string1 with: string2 collated: order

"
self assert: 'abc' = 'abc' asWideString.
self assert: 'abc' asWideString = 'abc'.
self assert: ((ByteArray with: 97 with: 0 with: 0 with: 0) asString ~= 'a000' asWideString).
self assert: ('a000' asWideString ~= (ByteArray with: 97 with: 0 with: 0 with: 0) asString).

self assert: ('abc' sameAs: 'aBc' asWideString).
self assert: ('aBc' asWideString sameAs: 'abc').
self assert: ((ByteArray with: 97 with: 0 with: 0 with: 0) asString sameAs: 'Abcd' asWideString) not.
self assert: ('a000' asWideString sameAs: (ByteArray with: 97 with: 0 with: 0 with: 0) asString) not.

"! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
compressWithTable: tokens
	"Return a string with all substrings that occur in tokens replaced
	by a character with ascii code = 127 + token index.
	This will work best if tokens are sorted by size.
	Assumes this string contains no characters > 127, or that they
	are intentionally there and will not interfere with this process."
	| str null finalSize start result ri c ts |
	null := Character value: 0.
	str := self copyFrom: 1 to: self size.  "Working string will get altered"
	finalSize := str size.
	tokens doWithIndex:
		[:token :tIndex |
		start := 1.
		[(start := str findString: token startingAt: start) > 0]
			whileTrue:
			[ts := token size.
			((start + ts) <= str size
				and: [(str at: start + ts) = $  and: [tIndex*2 <= 128]])
				ifTrue: [ts := token size + 1.  "include training blank"
						str at: start put: (Character value: tIndex*2 + 127)]
				ifFalse: [str at: start put: (Character value: tIndex + 127)].
			str at: start put: (Character value: tIndex + 127).
			1 to: ts-1 do: [:i | str at: start+i put: null].
			finalSize := finalSize - (ts - 1).
			start := start + ts]].
	result := String new: finalSize.
	ri := 0.
	1 to: str size do:
		[:i | (c := str at: i) = null ifFalse: [result at: (ri := ri+1) put: c]].
	^ result! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
contractTo: smallSize
	"return myself or a copy shortened by ellipsis to smallSize"
	| leftSize |
	self size <= smallSize
		ifTrue: [^ self].  "short enough"
	smallSize < 5
		ifTrue: [^ self copyFrom: 1 to: smallSize].    "First N characters"
	leftSize := smallSize-2//2.
	^ self copyReplaceFrom: leftSize+1		"First N/2 ... last N/2"
		to: self size - (smallSize - leftSize - 3)
		with: '...'
"
	'A clear but rather long-winded summary' contractTo: 18
"! !

!String methodsFor: 'converting' stamp: 'KR 1/30/2006 21:47'!
convertFromEncoding: encodingName
	^self convertFromWithConverter: (TextConverter newForEncoding: encodingName)! !

!String methodsFor: 'converting' stamp: 'KR 1/30/2006 21:47'!
convertFromSuperSwikiServerString
	^self convertFromEncoding: 'shift_jis'! !

!String methodsFor: 'converting' stamp: 'yo 7/8/2004 12:02'!
convertFromWithConverter: converter

	| readStream writeStream c |
	readStream := self readStream.
	writeStream := String new writeStream.
	converter ifNil: [^ self].
	[readStream atEnd] whileFalse: [
		c := converter nextFromStream: readStream.
		c ifNotNil: [writeStream nextPut: c] ifNil: [^ writeStream contents]
	].
	^ writeStream contents
! !

!String methodsFor: 'converting' stamp: 'KR 1/30/2006 21:49'!
convertToEncoding: encodingName
	^self convertToWithConverter: (TextConverter newForEncoding: encodingName).! !

!String methodsFor: 'converting' stamp: 'KR 1/30/2006 21:50'!
convertToSuperSwikiServerString
	^self convertToEncoding: 'shift_jis'! !

!String methodsFor: 'converting' stamp: 'ar 4/12/2005 14:01'!
convertToSystemString

	| readStream writeStream converter |
	readStream := self readStream.
	writeStream := String new writeStream.
	converter := LanguageEnvironment defaultSystemConverter.
	converter ifNil: [^ self].
	[readStream atEnd] whileFalse: [
		converter nextPut: readStream next toStream: writeStream
	].
	converter emitSequenceToResetStateIfNeededOn: writeStream.
	^ writeStream contents.
! !

!String methodsFor: 'converting' stamp: 'yo 7/8/2004 12:01'!
convertToWithConverter: converter

	| readStream writeStream |
	readStream := self readStream.
	writeStream := String new writeStream.
	converter ifNil: [^ self].
	[readStream atEnd] whileFalse: [
		converter nextPut: readStream next toStream: writeStream
	].
	converter emitSequenceToResetStateIfNeededOn: writeStream.
	^ writeStream contents.
! !

!String methodsFor: 'copying' stamp: 'yo 11/3/2004 19:24'!
copyReplaceTokens: oldSubstring with: newSubstring 
	"Replace all occurrences of oldSubstring that are surrounded
	by non-alphanumeric characters"
	^ self copyReplaceAll: oldSubstring with: newSubstring asTokens: true
	"'File asFile Files File''s File' copyReplaceTokens: 'File' with: 'Snick'"! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
correctAgainst: wordList
	"Correct the receiver: assume it is a misspelled word and return the (maximum of five) nearest words in the wordList.  Depends on the scoring scheme of alike:"
	| results |
	results := self correctAgainst: wordList continuedFrom: nil.
	results := self correctAgainst: nil continuedFrom: results.
	^ results! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
correctAgainst: wordList continuedFrom: oldCollection
	"Like correctAgainst:.  Use when you want to correct against several lists, give nil as the first oldCollection, and nil as the last wordList."

	^ wordList isNil
		ifTrue: [ self correctAgainstEnumerator: nil
					continuedFrom: oldCollection ]
		ifFalse: [ self correctAgainstEnumerator: [ :action | wordList do: action without: nil]
					continuedFrom: oldCollection ]! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
correctAgainstDictionary: wordDict continuedFrom: oldCollection
	"Like correctAgainst:continuedFrom:.  Use when you want to correct against a dictionary."

	^ wordDict isNil
		ifTrue: [ self correctAgainstEnumerator: nil
					continuedFrom: oldCollection ]
		ifFalse: [ self correctAgainstEnumerator: [ :action | wordDict keysDo: action ]
					continuedFrom: oldCollection ]! !

!String methodsFor: 'private' stamp: 'yo 11/3/2004 19:24'!
correctAgainstEnumerator: wordBlock continuedFrom: oldCollection
	"The guts of correction, instead of a wordList, there is a block that should take another block and enumerate over some list with it."

	| choices scoreMin results score maxChoices |
	scoreMin := self size // 2 min: 3.
	maxChoices := 10.
	oldCollection isNil
		ifTrue: [ choices := SortedCollection sortBlock: [ :x :y | x value > y value ] ]
		ifFalse: [ choices := oldCollection ].
	wordBlock isNil
		ifTrue:
			[ results := OrderedCollection new.
			1 to: (maxChoices min: choices size) do: [ :i | results add: (choices at: i) key ] ]
		ifFalse:
			[ wordBlock value: [ :word |
				(score := self alike: word) >= scoreMin ifTrue:
					[ choices add: (Association key: word value: score).
						(choices size >= maxChoices) ifTrue: [ scoreMin := (choices at: maxChoices) value] ] ].
			results := choices ].
	^ results! !

!String methodsFor: 'comparing' stamp: 'ar 4/10/2005 17:27'!
crc16
	"Compute a 16 bit cyclic redundancy check."

	| crc |
	crc := 0.
	1 to: self byteSize do: [:i |
		crc := (crc bitShift: -8) bitXor: (
		 #(	16r0000	16rC0C1	16rC181	16r0140	16rC301	16r03C0	16r0280	16rC241
			16rC601	16r06C0	16r0780	16rC741	16r0500	16rC5C1	16rC481	16r0440
			16rCC01	16r0CC0	16r0D80	16rCD41	16r0F00	16rCFC1	16rCE81	16r0E40
			16r0A00	16rCAC1	16rCB81	16r0B40	16rC901	16r09C0	16r0880	16rC841
			16rD801	16r18C0	16r1980	16rD941	16r1B00	16rDBC1	16rDA81	16r1A40
			16r1E00	16rDEC1	16rDF81	16r1F40	16rDD01	16r1DC0	16r1C80	16rDC41
			16r1400	16rD4C1	16rD581	16r1540	16rD701	16r17C0	16r1680	16rD641
			16rD201	16r12C0	16r1380	16rD341	16r1100	16rD1C1	16rD081	16r1040
			16rF001	16r30C0	16r3180	16rF141	16r3300	16rF3C1	16rF281	16r3240
			16r3600	16rF6C1	16rF781	16r3740	16rF501	16r35C0	16r3480	16rF441
			16r3C00	16rFCC1	16rFD81	16r3D40	16rFF01	16r3FC0	16r3E80	16rFE41
			16rFA01	16r3AC0	16r3B80	16rFB41	16r3900	16rF9C1	16rF881	16r3840
			16r2800	16rE8C1	16rE981	16r2940	16rEB01	16r2BC0	16r2A80	16rEA41
			16rEE01	16r2EC0	16r2F80	16rEF41	16r2D00	16rEDC1	16rEC81	16r2C40
			16rE401	16r24C0	16r2580	16rE541	16r2700	16rE7C1	16rE681	16r2640
			16r2200	16rE2C1	16rE381	16r2340	16rE101	16r21C0	16r2080	16rE041
			16rA001	16r60C0	16r6180	16rA141	16r6300	16rA3C1	16rA281	16r6240
			16r6600	16rA6C1	16rA781	16r6740	16rA501	16r65C0	16r6480	16rA441
			16r6C00	16rACC1	16rAD81	16r6D40	16rAF01	16r6FC0	16r6E80	16rAE41
			16rAA01	16r6AC0	16r6B80	16rAB41	16r6900	16rA9C1	16rA881	16r6840
			16r7800	16rB8C1	16rB981	16r7940	16rBB01	16r7BC0	16r7A80	16rBA41
			16rBE01	16r7EC0	16r7F80	16rBF41	16r7D00	16rBDC1	16rBC81	16r7C40
			16rB401	16r74C0	16r7580	16rB541	16r7700	16rB7C1	16rB681	16r7640
			16r7200	16rB2C1	16rB381	16r7340	16rB101	16r71C0	16r7080	16rB041
			16r5000	16r90C1	16r9181	16r5140	16r9301	16r53C0	16r5280	16r9241
			16r9601	16r56C0	16r5780	16r9741	16r5500	16r95C1	16r9481	16r5440
			16r9C01	16r5CC0	16r5D80	16r9D41	16r5F00	16r9FC1	16r9E81	16r5E40
			16r5A00	16r9AC1	16r9B81	16r5B40	16r9901	16r59C0	16r5880	16r9841
			16r8801	16r48C0	16r4980	16r8941	16r4B00	16r8BC1	16r8A81	16r4A40
			16r4E00	16r8EC1	16r8F81	16r4F40	16r8D01	16r4DC0	16r4C80	16r8C41
			16r4400	16r84C1	16r8581	16r4540	16r8701	16r47C0	16r4680	16r8641
			16r8201	16r42C0	16r4380	16r8341	16r4100	16r81C1	16r8081	16r4040)
			 at: ((crc bitXor: (self byteAt: i)) bitAnd: 16rFF) + 1) ].
	^crc! !

!String methodsFor: 'internet' stamp: 'yo 12/28/2003 01:17'!
decodeMimeHeader
	"See RFC 2047, MIME Part Three: Message Header Extension for Non-ASCII  
	Text. Text containing non-ASCII characters is encoded by the sequence  
	=?character-set?encoding?encoded-text?=  
	Encoding is Q (quoted printable) or B (Base64), handled by  
	Base64MimeConverter / RFC2047MimeConverter.

	Thanks to Yokokawa-san, it works in m17n package.  Try the following:

	'=?ISO-2022-JP?B?U1dJS0lQT1AvGyRCPUJDKyVpJXMlQRsoQi8=?= =?ISO-2022-JP?B?GyRCJVElRiUjJSobKEIoUGF0aW8p?=' decodeMimeHeader.
"
	| input output temp charset decoder encodedStream encoding pos |
	input := ReadStream on: self.
	output := WriteStream on: String new.
	[output
		nextPutAll: (input upTo: $=).
	"ASCII Text"
	input atEnd]
		whileFalse: [(temp := input next) = $?
				ifTrue: [charset := input upTo: $?.
					encoding := (input upTo: $?) asUppercase.
					temp := input upTo: $?.
					input next.
					"Skip final ="
					(charset isNil or: [charset size = 0]) ifTrue: [charset := 'LATIN-1'].
					encodedStream := MultiByteBinaryOrTextStream on: String new encoding: charset.
					decoder := encoding = 'B'
								ifTrue: [Base64MimeConverter new]
								ifFalse: [RFC2047MimeConverter new].
					decoder
						mimeStream: (ReadStream on: temp);
						 dataStream: encodedStream;
						 mimeDecode.
					output nextPutAll: encodedStream reset contents.
					pos := input position.
					input skipSeparators.
					"Delete spaces if followed by ="
					input peek = $=
						ifFalse: [input position: pos]]
				ifFalse: [output nextPut: $=;
						 nextPut: temp]].
	^ output contents! !

!String methodsFor: 'internet' stamp: 'yo 11/3/2004 19:24'!
decodeQuotedPrintable
	"Assume receiver is in MIME 'quoted-printable' encoding, and decode it."
  
	^QuotedPrintableMimeConverter mimeDecode: self as: self class! !

!String methodsFor: 'copying' stamp: 'yo 11/3/2004 19:24'!
deepCopy
	"DeepCopy would otherwise mean make a copy of the character;  since 
	characters are unique, just return a shallowCopy."

	^self shallowCopy! !

!String methodsFor: 'displaying' stamp: 'yo 11/3/2004 19:24'!
displayAt: aPoint 
	"Display the receiver as a DisplayText at aPoint on the display screen."

	self displayOn: Display at: aPoint! !

!String methodsFor: 'displaying' stamp: 'yo 11/3/2004 19:24'!
displayOn: aDisplayMedium
	"Display the receiver on the given DisplayMedium.  5/16/96 sw"

	self displayOn: aDisplayMedium at: 0 @ 0! !

!String methodsFor: 'displaying' stamp: 'yo 11/3/2004 19:24'!
displayOn: aDisplayMedium at: aPoint 
	"Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, using black-colored text."

	self displayOn: aDisplayMedium at: aPoint textColor: Color black! !

!String methodsFor: 'displaying' stamp: 'yo 11/3/2004 19:24'!
displayOn: aDisplayMedium at: aPoint textColor: aColor
	"Show a representation of the receiver as a DisplayText at location aPoint on aDisplayMedium, rendering the text in the designated color"

	(self asDisplayText foregroundColor: (aColor ifNil: [Color black]) backgroundColor: Color white)
		displayOn: aDisplayMedium at: aPoint! !

!String methodsFor: 'displaying' stamp: 'yo 11/3/2004 19:24'!
displayProgressAt: aPoint from: minVal to: maxVal during: workBlock 
	"Display this string as a caption over a progress bar while workBlock is evaluated.

EXAMPLE (Select next 6 lines and Do It)
'Now here''s some Real Progress'
	displayProgressAt: Sensor cursorPoint
	from: 0 to: 10
	during: [:bar |
	1 to: 10 do: [:x | bar value: x.
			(Delay forMilliseconds: 500) wait]].

HOW IT WORKS (Try this in any other language :-)
Since your code (the last 2 lines in the above example) is in a block,
this method gets control to display its heading before, and clean up 
the screen after, its execution.
The key, though, is that the block is supplied with an argument,
named 'bar' in the example, which will update the bar image every 
it is sent the message value: x, where x is in the from:to: range.
"
	^ProgressInitiationException 
		display: self
		at: aPoint 
		from: minVal 
		to: maxVal 
		during: workBlock! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
do: aBlock toFieldNumber: aNumber
	"Considering the receiver as a holder of tab-delimited fields, evaluate aBlock on behalf of a field in this string"

	| start end index |
	start := 1.
	index := 1.
	[start <= self size] whileTrue: 
		[end := self indexOf: Character tab startingAt: start ifAbsent: [self size + 1].
		end := end - 1.
		aNumber = index ifTrue:
			[aBlock value: (self copyFrom: start  to: end).
			^ self].
		index := index + 1.
		start := end + 2]

"
1 to: 6 do:
	[:aNumber |
		'fred	charlie	elmo		wimpy	friml' do:
			[:aField | Transcript cr; show: aField] toFieldNumber: aNumber]
"! !

!String methodsFor: 'printing' stamp: 'yo 8/26/2002 22:57'!
encodeDoublingQuoteOn: aStream 
	"Print inside string quotes, doubling inbedded quotes."
	| x |
	aStream print: $'.
	1 to: self size do:
		[:i |
		aStream print: (x := self at: i).
		x = $' ifTrue: [aStream print: x]].
	aStream print: $'! !

!String methodsFor: 'converting' stamp: 'yo 7/29/2005 16:04'!
encodeForHTTP
	"change dangerous characters to their %XX form, for use in HTTP transactions"

	^ self encodeForHTTPWithTextEncoding: 'utf-8' conditionBlock: [:c | c isSafeForHTTP].
! !

!String methodsFor: 'converting' stamp: 'yo 7/29/2005 16:04'!
encodeForHTTPWithTextEncoding: encodingName

	^ self encodeForHTTPWithTextEncoding: encodingName conditionBlock: [:c | c isSafeForHTTP].
! !

!String methodsFor: 'converting' stamp: 'yo 7/29/2005 16:03'!
encodeForHTTPWithTextEncoding: encodingName conditionBlock: conditionBlock
	"change dangerous characters to their %XX form, for use in HTTP transactions"

	| httpSafeStream encodedStream cont |
	httpSafeStream := WriteStream on: (String new).
	encodedStream := MultiByteBinaryOrTextStream on: (String new: 6).
	encodedStream converter: (TextConverter newForEncoding: encodingName).
	self do: [:c |
		(conditionBlock value: c)
			ifTrue: [httpSafeStream nextPut: (Character value: c charCode)]
			ifFalse: [
				encodedStream text; reset.
				encodedStream nextPut: c.
				encodedStream position: 0.
				encodedStream binary.
				cont := encodedStream contents.
				cont do: [:byte |
					httpSafeStream nextPut: $%.
					httpSafeStream nextPut: (byte // 16) asHexDigit.
					httpSafeStream nextPut: (byte \\ 16) asHexDigit.
				].
			].
	].
	^ httpSafeStream contents.
! !

!String methodsFor: 'comparing' stamp: 'yo 11/3/2004 19:24'!
endsWith: suffix
	"Answer whether the tail end of the receiver is the same as suffix.
	The comparison is case-sensitive."
	| extra |
	(extra := self size - suffix size) < 0 ifTrue: [^ false].
	^ (self findSubstring: suffix in: self startingAt: extra + 1
			matchTable: CaseSensitiveOrder) > 0
"
  'Elvis' endsWith: 'vis'
"! !

!String methodsFor: 'system primitives' stamp: 'sw 10/20/2004 17:51'!
endsWithAColon 
	"Answer whether the final character of the receiver is a colon"

	^ self size > 0 and: [self last == $:]

"
#fred: endsWithAColon
'fred' endsWithAColon
"! !

!String methodsFor: 'comparing' stamp: 'yo 11/3/2004 19:24'!
endsWithAnyOf: aCollection
	aCollection do:[:suffix|
		(self endsWith: suffix) ifTrue:[^true].
	].
	^false! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
endsWithDigit
	"Answer whether the receiver's final character represents a digit.  3/11/96 sw"

	^ self size > 0 and: [self last isDigit]! !

!String methodsFor: 'private' stamp: 'yo 11/3/2004 19:24'!
evaluateExpression: aString parameters: aCollection 
	"private - evaluate the expression aString with  
	aCollection as the parameters and answer the  
	evaluation result as an string"
	| index |
	index := ('0' , aString) asNumber.

	index isZero
		ifTrue: [^ '[invalid subscript: {1}]' format: {aString}].

	index > aCollection size
		ifTrue: [^ '[subscript is out of bounds: {1}]' format: {aString}].

	^ (aCollection at: index) asString! !

!String methodsFor: 'formatting' stamp: 'md 5/26/2005 13:34'!
expandMacros
	^self expandMacrosWithArguments: #()! !

!String methodsFor: 'formatting' stamp: 'md 5/26/2005 13:34'!
expandMacrosWith: anObject 
	^self expandMacrosWithArguments: (Array with: anObject)! !

!String methodsFor: 'formatting' stamp: 'md 5/26/2005 13:34'!
expandMacrosWith: anObject with: anotherObject 
	^self 
		expandMacrosWithArguments: (Array with: anObject with: anotherObject)! !

!String methodsFor: 'formatting' stamp: 'md 5/26/2005 13:34'!
expandMacrosWith: anObject with: anotherObject with: thirdObject 
	^self expandMacrosWithArguments: (Array 
				with: anObject
				with: anotherObject
				with: thirdObject)! !

!String methodsFor: 'formatting' stamp: 'md 5/26/2005 13:34'!
expandMacrosWith: anObject with: anotherObject with: thirdObject with: fourthObject 
	^self expandMacrosWithArguments: (Array 
				with: anObject
				with: anotherObject
				with: thirdObject
				with: fourthObject)! !

!String methodsFor: 'formatting' stamp: 'md 5/26/2005 13:34'!
expandMacrosWithArguments: anArray 
	| newStream readStream char index |
	newStream := WriteStream on: (String new: self size).
	readStream := ReadStream on: self.
	[readStream atEnd] whileFalse: 
			[char := readStream next.
			char == $< 
				ifTrue: 
					[| nextChar |
					nextChar := readStream next asUppercase.
					nextChar == $N ifTrue: [newStream cr].
					nextChar == $T ifTrue: [newStream tab].
					nextChar isDigit 
						ifTrue: 
							[index := nextChar digitValue.
							
							[readStream atEnd 
								or: [(nextChar := readStream next asUppercase) isDigit not]] 
									whileFalse: [index := index * 10 + nextChar digitValue]].
					nextChar == $? 
						ifTrue: 
							[| trueString falseString |
							trueString := readStream upTo: $:.
							falseString := readStream upTo: $>.
							readStream position: readStream position - 1.
							newStream 
								nextPutAll: ((anArray at: index) ifTrue: [trueString] ifFalse: [falseString])].
					nextChar == $P 
						ifTrue: [newStream nextPutAll: (anArray at: index) printString].
					nextChar == $S ifTrue: [newStream nextPutAll: (anArray at: index)].
					readStream skipTo: $>]
				ifFalse: 
					[newStream nextPut: (char == $% ifTrue: [readStream next] ifFalse: [char])]].
	^newStream contents! !

!String methodsFor: 'accessing' stamp: 'ar 4/10/2005 17:12'!
findAnySubStr: delimiters startingAt: start 
	"Answer the index of the character within the receiver, starting at start, that begins a substring matching one of the delimiters.  delimiters is an Array of Strings (Characters are permitted also).  If the receiver does not contain any of the delimiters, answer size + 1."

	| min ind |
	min := self size + 1.
	delimiters do: [:delim |	"May be a char, a string of length 1, or a substring"
		delim isCharacter 
			ifTrue: [ind := self indexOfSubCollection: (String with: delim) 
						startingAt: start ifAbsent: [min]]
			ifFalse: [ind := self indexOfSubCollection: delim 
						startingAt: start ifAbsent: [min]].
			min := min min: ind].
	^ min! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
findBetweenSubStrs: delimiters
	"Answer the collection of String tokens that result from parsing self.  Tokens are separated by 'delimiters', which can be a collection of Strings, or a collection of Characters.  Several delimiters in a row are considered as just one separation."

	| tokens keyStart keyStop |
	tokens := OrderedCollection new.
	keyStop := 1.
	[keyStop <= self size] whileTrue:
		[keyStart := self skipAnySubStr: delimiters startingAt: keyStop.
		keyStop := self findAnySubStr: delimiters startingAt: keyStart.
		keyStart < keyStop
			ifTrue: [tokens add: (self copyFrom: keyStart to: (keyStop - 1))]].
	^tokens! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
findCloseParenthesisFor: startIndex
	"assume (self at: startIndex) is $(.  Find the matching $), allowing parentheses to nest."
	" '(1+(2-3))-3.14159' findCloseParenthesisFor: 1 "
	" '(1+(2-3))-3.14159' findCloseParenthesisFor: 4 "
	| pos nestLevel |
	pos := startIndex+1.
	nestLevel := 1.
	[ pos <= self size ] whileTrue: [
		(self at: pos) = $( ifTrue: [ nestLevel := nestLevel + 1 ].
		(self at: pos) = $) ifTrue: [ nestLevel := nestLevel - 1 ].
		nestLevel = 0 ifTrue: [ ^pos ].
		pos := pos + 1.
	].
	^self size + 1! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
findDelimiters: delimiters startingAt: start 
	"Answer the index of the character within the receiver, starting at start, that matches one of the delimiters. If the receiver does not contain any of the delimiters, answer size + 1."

	start to: self size do: [:i |
		delimiters do: [:delim | delim = (self at: i) ifTrue: [^ i]]].
	^ self size + 1! !

!String methodsFor: 'accessing' stamp: 'yo 10/15/2003 15:32'!
findLastOccuranceOfString: subString startingAt: start 
	"Answer the index of the last occurance of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	| last now |
	last := self findSubstring: subString in: self startingAt: start matchTable: CaseSensitiveOrder.
	last = 0 ifTrue: [^ 0].
	[last > 0] whileTrue: [
		now := last.
		last := self findSubstring: subString in: self startingAt: last + subString size matchTable: CaseSensitiveOrder.
	].

	^ now.
! !

!String methodsFor: 'converting' stamp: 'yo 7/5/2004 16:48'!
findSelector
	"Dan's code for hunting down selectors with keyword parts; while this doesn't give a true parse, in most cases it does what we want, in where it doesn't, we're none the worse for it."
	| sel possibleParens level n |
	sel := self withBlanksTrimmed.
	(sel includes: $:) ifTrue:
		[sel := sel copyReplaceAll: ':' with: ': '.	"for the style (aa max:bb) with no space"
		possibleParens := sel findTokens: Character separators.
		sel := self class streamContents:
			[:s | level := 0.
			possibleParens do:
				[:token |
				(level = 0 and: [token endsWith: ':'])
					ifTrue: [s nextPutAll: token]
					ifFalse: [(n := token occurrencesOf: $( ) > 0 ifTrue: [level := level + n].
							(n := token occurrencesOf: $[ ) > 0 ifTrue: [level := level + n].
							(n := token occurrencesOf: $] ) > 0 ifTrue: [level := level - n].
							(n := token occurrencesOf: $) ) > 0 ifTrue: [level := level - n]]]]].

	sel isEmpty ifTrue: [^ nil].
	sel isOctetString ifTrue: [sel := sel asOctetString].
	Symbol hasInterned: sel ifTrue:
		[:aSymbol | ^ aSymbol].
	^ nil! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
findString: subString
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."
	^self findString: subString startingAt: 1.! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
findString: subString startingAt: start 
	"Answer the index of subString within the receiver, starting at start. If 
	the receiver does not contain subString, answer 0."

	^ self findSubstring: subString in: self startingAt: start matchTable: CaseSensitiveOrder! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
findString: key startingAt: start caseSensitive: caseSensitive
	"Answer the index in this String at which the substring key first occurs, at or beyond start.  The match can be case-sensitive or not.  If no match is found, zero will be returned."

	caseSensitive
	ifTrue: [^ self findSubstring: key in: self startingAt: start matchTable: CaseSensitiveOrder]
	ifFalse: [^ self findSubstring: key in: self startingAt: start matchTable: CaseInsensitiveOrder]! !

!String methodsFor: 'system primitives' stamp: 'ar 4/10/2005 16:55'!
findSubstring: key in: body startingAt: start matchTable: matchTable
	"Answer the index in the string body at which the substring key first occurs, at or beyond start.  The match is determined using matchTable, which can be used to effect, eg, case-insensitive matches.  If no match is found, zero will be returned."
	| index c1 c2 |
	matchTable == nil ifTrue: [
		key size = 0 ifTrue: [^ 0].
		start to: body size - key size + 1 do:
			[:startIndex |
			index := 1.
				[(body at: startIndex+index-1)
					= (key at: index)]
					whileTrue:
					[index = key size ifTrue: [^ startIndex].
					index := index+1]].
		^ 0
	].

	key size = 0 ifTrue: [^ 0].
	start to: body size - key size + 1 do:
		[:startIndex |
		index := 1.
		[c1 := body at: startIndex+index-1.
		c2 := key at: index.
		((c1 leadingChar = 0) ifTrue: [(matchTable at: c1 asciiValue + 1)]
						ifFalse: [c1 asciiValue + 1])
			= ((c2 leadingChar = 0) ifTrue: [(matchTable at: c2 asciiValue + 1)]
								ifFalse: [c2 asciiValue + 1])]
			whileTrue:
				[index = key size ifTrue: [^ startIndex].
				index := index+1]].
	^ 0
! !

!String methodsFor: 'accessing' stamp: 'ar 4/10/2005 17:13'!
findTokens: delimiters
	"Answer the collection of tokens that result from parsing self.  Return strings between the delimiters.  Any character in the Collection delimiters marks a border.  Several delimiters in a row are considered as just one separation.  Also, allow delimiters to be a single character."

	| tokens keyStart keyStop separators |

	tokens := OrderedCollection new.
	separators := delimiters isCharacter 
		ifTrue: [Array with: delimiters]
		ifFalse: [delimiters].
	keyStop := 1.
	[keyStop <= self size] whileTrue:
		[keyStart := self skipDelimiters: separators startingAt: keyStop.
		keyStop := self findDelimiters: separators startingAt: keyStart.
		keyStart < keyStop
			ifTrue: [tokens add: (self copyFrom: keyStart to: (keyStop - 1))]].
	^tokens! !

!String methodsFor: 'accessing' stamp: 'dtl 8/14/2004 11:27'!
findTokens: delimiters escapedBy: quoteDelimiters 
	"Answer a collection of Strings separated by the delimiters, where  
	delimiters is a Character or collection of characters. Two delimiters in a  
	row produce an empty string (compare this to #findTokens, which  
	treats sequential delimiters as one).  
	 
	The characters in quoteDelimiters are treated as quote characters, such  
	that any delimiter within a pair of matching quoteDelimiter characters  
	is treated literally, rather than as a delimiter.  
	 
	The quoteDelimiter characters may be escaped within a quoted string.  
	Two sequential quote characters within a quoted string are treated as  
	a single character.  
	 
	This method is useful for parsing comma separated variable strings for  
	spreadsheet import and export."

	| tokens rs activeEscapeCharacter ts char token delimiterChars quoteChars |
	delimiterChars := (delimiters isNil
				ifTrue: ['']
				ifFalse: [delimiters]) asString.
	quoteChars := (quoteDelimiters isNil
				ifTrue: ['']
				ifFalse: [quoteDelimiters]) asString.
	tokens := OrderedCollection new.
	rs := ReadStream on: self.
	activeEscapeCharacter := nil.
	ts := WriteStream on: ''.
	[rs atEnd]
		whileFalse: [char := rs next.
			activeEscapeCharacter isNil
				ifTrue: [(quoteChars includes: char)
						ifTrue: [activeEscapeCharacter := char]
						ifFalse: [(delimiterChars includes: char)
								ifTrue: [token := ts contents.
									tokens add: token.
									ts := WriteStream on: '']
								ifFalse: [ts nextPut: char]]]
				ifFalse: [char == activeEscapeCharacter
						ifTrue: [rs peek == activeEscapeCharacter
								ifTrue: [ts nextPut: rs next]
								ifFalse: [activeEscapeCharacter := nil]]
						ifFalse: [ts nextPut: char]]].
	token := ts contents.
	(tokens isEmpty and: [token isEmpty])
		ifFalse: [tokens add: token].
	^ tokens! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
findTokens: delimiters includes: subString
	"Divide self into pieces using delimiters.  Return the piece that includes subString anywhere in it.  Is case sensitive (say asLowercase to everything beforehand to make insensitive)."

^ (self findTokens: delimiters) 
	detect: [:str | (str includesSubString: subString)] 
	ifNone: [nil]! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
findTokens: delimiters keep: keepers
	"Answer the collection of tokens that result from parsing self.  The tokens are seperated by delimiters, any of a string of characters.  If a delimiter is also in keepers, make a token for it.  (Very useful for carriage return.  A sole return ends a line, but is also saved as a token so you can see where the line breaks were.)"

	| tokens keyStart keyStop |
	tokens := OrderedCollection new.
	keyStop := 1.
	[keyStop <= self size] whileTrue:
		[keyStart := self skipDelimiters: delimiters startingAt: keyStop.
		keyStop to: keyStart-1 do: [:ii | 
			(keepers includes: (self at: ii)) ifTrue: [
				tokens add: (self copyFrom: ii to: ii)]].	"Make this keeper be a token"
		keyStop := self findDelimiters: delimiters startingAt: keyStart.
		keyStart < keyStop
			ifTrue: [tokens add: (self copyFrom: keyStart to: (keyStop - 1))]].
	^tokens! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
findWordStart: key startingAt: start
	| ind |
	"HyperCard style searching.  Answer the index in self of the substring key, when that key is preceeded by a separator character.  Must occur at or beyond start.  The match is case-insensitive.  If no match is found, zero will be returned."

	ind := start.
	[ind := self findSubstring: key in: self startingAt: ind matchTable: CaseInsensitiveOrder.
	ind = 0 ifTrue: [^ 0].	"not found"
	ind = 1 ifTrue: [^ 1].	"First char is the start of a word"
	(self at: ind-1) isSeparator] whileFalse: [ind := ind + 1].
	^ ind	"is a word start"! !

!String methodsFor: 'formatting' stamp: 'yo 11/3/2004 19:24'!
format: aCollection 
	"format the receiver with aCollection  
	 
	simplest example:  
	'foo {1} bar' format: {Date today}.
	 
	complete example:  
	'\{ \} \\ foo {1} bar {2}' format: {12. 'string'}.  
	"
	| result stream |
	result := String new writeStream.
	stream := self readStream.

	[stream atEnd]
		whileFalse: [| currentChar | 
			currentChar := stream next.
			currentChar == ${
				ifTrue: [| expression | 
					expression := self getEnclosedExpressionFrom: stream.
					result
						nextPutAll: (self evaluateExpression: expression parameters: aCollection)]
				ifFalse: [
					currentChar == $\
						ifTrue: [stream atEnd
								ifFalse: [result nextPut: stream next]]
						ifFalse: [result nextPut: currentChar]]].

	^ result contents! !

!String methodsFor: 'private' stamp: 'yo 11/3/2004 19:24'!
getEnclosedExpressionFrom: aStream 
	"private - get the expression enclosed between '{' and 
	'}' and remove all the characters from the stream"
	| result currentChar |
	result := String new writeStream.

	[aStream atEnd 
		or: [(currentChar := aStream next) == $}]]
		whileFalse: [result nextPut: currentChar].

	^ result contents withBlanksTrimmed! !

!String methodsFor: 'encoding' stamp: 'ar 4/10/2005 17:16'!
getInteger32: location
	| integer |
	<primitive: 'getInteger' module: 'IntegerPokerPlugin'>
	"^IntegerPokerPlugin doPrimitive: #getInteger"

	"the following is about 7x faster than interpreting the plugin if not compiled"

	integer := 
		((self at: location) asInteger bitShift: 24) +
		((self at: location+1) asInteger bitShift: 16) +
		((self at: location+2) asInteger bitShift: 8) +
		(self at: location+3) asInteger.

	integer > 1073741824 ifTrue: [^1073741824 - integer ].
	^integer
! !

!String methodsFor: 'testing' stamp: 'yo 11/3/2004 19:24'!
hasContentsInExplorer

	^false! !

!String methodsFor: 'comparing' stamp: 'md 2/16/2006 17:49'!
hash
	"#hash is implemented, because #= is implemented"
	"ar 4/10/2005: I had to change this to use ByteString hash as initial 
	hash in order to avoid having to rehash everything and yet compute
	the same hash for ByteString and WideString.
	md 16/10/2006: use identityHash as initialHash, as behavior hash will 
    use String hash (name) to have a better hash soon"
	^ self class stringHash: self initialHash: ByteString identityHash! !

!String methodsFor: 'comparing' stamp: 'yo 11/3/2004 19:24'!
hashMappedBy: map
	"My hash is independent of my oop."

	^self hash! !

!String methodsFor: 'comparing' stamp: 'yo 11/3/2004 19:24'!
howManyMatch: string 
	"Count the number of characters that match up in self and aString."
	| count shorterLength |
	
	count  :=  0 .
	shorterLength  :=  ((self size ) min: (string size ) ) .
	(1 to: shorterLength  do: [:index |
		 (((self at: index ) = (string at: index )  ) ifTrue: [count  :=  (count + 1 ) .
			]   ).
		]   ).
	^  count 
	
	! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
includesSubString: subString
	^ (self findString: subString startingAt: 1) > 0! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
includesSubstring: aString caseSensitive: caseSensitive
	
	^ (self findString: aString startingAt: 1 caseSensitive: caseSensitive) > 0! !

!String methodsFor: 'testing' stamp: 'ar 4/10/2005 16:49'!
includesUnifiedCharacter
	^false! !

!String methodsFor: 'paragraph support' stamp: 'yo 8/26/2002 22:19'!
indentationIfBlank: aBlock
	"Answer the number of leading tabs in the receiver.  If there are
	 no visible characters, pass the number of tabs to aBlock and return its value."

	| reader leadingTabs lastSeparator cr tab ch |
	cr := Character cr.
	tab := Character tab.
	reader := ReadStream on: self.
	leadingTabs := 0.
	[reader atEnd not and: [(ch := reader next) = tab]]
		whileTrue: [leadingTabs := leadingTabs + 1].
	lastSeparator := leadingTabs + 1.
	[reader atEnd not and: [ch isSeparator and: [ch ~= cr]]]
		whileTrue: [lastSeparator := lastSeparator + 1. ch := reader next].
	lastSeparator = self size | (ch = cr)
		ifTrue: [^aBlock value: leadingTabs].
	^ leadingTabs.
! !

!String methodsFor: 'accessing' stamp: 'yo 8/28/2002 16:45'!
indexOf: aCharacter

	aCharacter isCharacter ifFalse: [^ 0].
	^ self class
		indexOfAscii: aCharacter asciiValue
		inString: self
		startingAt: 1.
! !

!String methodsFor: 'accessing' stamp: 'ar 4/12/2005 16:31'!
indexOf: aCharacter startingAt: start

	(aCharacter isCharacter) ifFalse: [^ 0].
	^ self class indexOfAscii: aCharacter asciiValue inString: self startingAt: start! !

!String methodsFor: 'accessing' stamp: 'ar 4/12/2005 16:31'!
indexOf: aCharacter  startingAt: start  ifAbsent: aBlock
	| ans |
	(aCharacter isCharacter) ifFalse: [ ^ aBlock value ].
	ans := self class indexOfAscii: aCharacter asciiValue inString: self  startingAt: start.
	ans = 0
		ifTrue: [ ^ aBlock value ]
		ifFalse: [ ^ ans ]! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
indexOfAnyOf: aCharacterSet
	"returns the index of the first character in the given set.  Returns 0 if none are found"
	^self indexOfAnyOf: aCharacterSet  startingAt: 1! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
indexOfAnyOf: aCharacterSet  ifAbsent: aBlock
	"returns the index of the first character in the given set.  Returns the evaluation of aBlock if none are found"
	^self indexOfAnyOf: aCharacterSet  startingAt: 1  ifAbsent: aBlock! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
indexOfAnyOf: aCharacterSet  startingAt: start
	"returns the index of the first character in the given set, starting from start.  Returns 0 if none are found"
	^self indexOfAnyOf: aCharacterSet  startingAt: start  ifAbsent: [ 0 ]! !

!String methodsFor: 'accessing' stamp: 'nice 3/15/2007 21:03'!
indexOfAnyOf: aCharacterSet startingAt: start ifAbsent: aBlock 
	"returns the index of the first character in the given set, starting from start "
	
	| ans |
	ans := self isWideString
				ifTrue: ["Fallback to naive implementation"
					self class
						findFirstInString: self
						inCharacterSet: aCharacterSet
						startingAt: start]
				ifFalse: ["We know we contain only byte characters
						So use a byteArrayMap opimized for primitive call"
					self class
						findFirstInString: self
						inSet: aCharacterSet byteArrayMap
						startingAt: start].
	ans = 0
		ifTrue: [^ aBlock value]
		ifFalse: [^ ans]! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
indexOfSubCollection: sub 
	#Collectn.
	"Added 2000/04/08 For ANSI <sequenceReadableCollection> protocol."
	^ self
		indexOfSubCollection: sub
		startingAt: 1
		ifAbsent: [0]! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
indexOfSubCollection: sub startingAt: start ifAbsent: exceptionBlock
	| index |
	index := self findSubstring: sub in: self startingAt: start matchTable: CaseSensitiveOrder.
	index = 0 ifTrue: [^ exceptionBlock value].
	^ index! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
initialIntegerOrNil
	"Answer the integer represented by the leading digits of the receiver, or nil if the receiver does not begin with a digit"
	| firstNonDigit |
	(self size == 0 or: [self first isDigit not]) ifTrue: [^ nil].
	firstNonDigit := (self findFirst: [:m | m isDigit not]).
	firstNonDigit = 0 ifTrue: [firstNonDigit := self size + 1].
	^ (self copyFrom: 1  to: (firstNonDigit - 1)) asNumber
"
'234Whoopie' initialIntegerOrNil
'wimpy' initialIntegerOrNil
'234' initialIntegerOrNil
'2N' initialIntegerOrNil
'2' initialIntegerOrNil
'  89Ten ' initialIntegerOrNil
'78 92' initialIntegerOrNil
"
! !

!String methodsFor: 'testing' stamp: 'yo 11/3/2004 19:24'!
isAllDigits
	"whether the receiver is composed entirely of digits"
	self do: [:c | c isDigit ifFalse: [^ false]].
	^ true! !

!String methodsFor: 'testing' stamp: 'yo 11/3/2004 19:24'!
isAllSeparators
	"whether the receiver is composed entirely of separators"
	self do: [ :c | c isSeparator ifFalse: [ ^false ] ].
	^true! !

!String methodsFor: 'testing' stamp: 'yo 8/4/2003 12:26'!
isAsciiString

	| c |
	c := self detect: [:each | each asciiValue > 127] ifNone: [nil].
	^ c isNil.
! !

!String methodsFor: 'testing' stamp: 'ar 4/10/2005 16:23'!
isByteString
	"Answer whether the receiver is a ByteString"
	^false! !

!String methodsFor: 'printing' stamp: 'yo 11/3/2004 19:24'!
isLiteral

	^true! !

!String methodsFor: 'testing' stamp: 'ar 4/10/2005 23:25'!
isOctetString
	"Answer whether the receiver can be represented as a byte string. 
	This is different from asking whether the receiver *is* a ByteString 
	(i.e., #isByteString)"
	1 to: self size do: [:pos |
		(self at: pos) asInteger >= 256 ifTrue: [^ false].
	].
	^ true.
! !

!String methodsFor: 'testing' stamp: 'yo 11/3/2004 19:24'!
isString
	^ true! !

!String methodsFor: 'testing' stamp: 'ar 4/12/2005 19:52'!
isWideString
	"Answer whether the receiver is a WideString"
	^false! !

!String methodsFor: 'internet' stamp: 'ar 4/9/2005 22:16'!
isoToSqueak
	^self "no longer needed"! !

!String methodsFor: 'internet' stamp: 'yo 11/3/2004 19:24'!
isoToUtf8
	"Convert ISO 8559-1 to UTF-8"
	| s v |
	s := WriteStream on: (String new: self size).

	self do: [:c |
		v := c asciiValue.
		(v > 128)
			ifFalse: [s nextPut: c]
			ifTrue: [
				s nextPut: (192+(v >> 6)) asCharacter.
				s nextPut: (128+(v bitAnd: 63)) asCharacter]].
	^s contents. 
! !

!String methodsFor: 'converting' stamp: 'dc 4/10/2007 09:36'!
keywords
	"Answer an array of the keywords that compose the receiver."
	| kwd char keywords |
	keywords := Array streamContents:
		[:kwds | kwd := WriteStream on: (String new: 16).
		1 to: self size do:
			[:i |
			kwd nextPut: (char := self at: i).
			char = $: ifTrue: 
					[kwds nextPut: kwd contents.
					kwd reset]].
		(kwd position = 0) ifFalse: [kwds nextPut: kwd contents]].
	(keywords size >= 1 and: [(keywords at: 1) = ':']) ifTrue:
		["Has an initial keyword, as in #:if:then:else:"
		keywords := keywords allButFirst].
	(keywords size >= 2 and: [(keywords at: keywords size - 1) = ':']) ifTrue:
		["Has a final keyword, as in #nextPut::andCR"
		keywords := keywords copyReplaceFrom: keywords size - 1
								to: keywords size with: {':' , keywords last}].
	^ keywords! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
lastIndexOfPKSignature: aSignature
	"Answer the last index in me where aSignature (4 bytes long) occurs, or 0 if not found"
	| a b c d |
	a := aSignature first.
	b := aSignature second.
	c := aSignature third.
	d := aSignature fourth.
	(self size - 3) to: 1 by: -1 do: [ :i |
		(((self at: i) = a)
			and: [ ((self at: i + 1) = b)
				and: [ ((self at: i + 2) = c)
					and: [ ((self at: i + 3) = d) ]]])
						ifTrue: [ ^i ]
	].
	^0! !

!String methodsFor: 'testing' stamp: 'yo 11/3/2004 19:24'!
lastSpacePosition
	"Answer the character position of the final space or other separator character in the receiver, and 0 if none"
	self size to: 1 by: -1 do:
		[:i | ((self at: i) isSeparator) ifTrue: [^ i]].
	^ 0

"
'fred the bear' lastSpacePosition
'ziggie' lastSpacePosition
'elvis ' lastSpacePosition
'wimpy  ' lastSpacePosition
'' lastSpacePosition
"! !

!String methodsFor: 'accessing' stamp: 'yo 12/17/2002 16:56'!
leadingCharRunLengthAt: index

	| leadingChar |
	leadingChar := (self at: index) leadingChar.
	index to: self size do: [:i |
		(self at: i) leadingChar ~= leadingChar ifTrue: [^ i - index].
	].
	^ self size - index + 1.
! !

!String methodsFor: 'accessing' stamp: 'yo 8/27/2002 14:33'!
lineCorrespondingToIndex: anIndex
	"Answer a string containing the line at the given character position.  1/15/96 sw:  Inefficient first stab at this"

	| cr aChar answer |
	cr := Character cr.
	answer := ''.
	1 to: self size do:
		[:i | 
			aChar := self at: i.
			aChar = cr
				ifTrue:
					[i > anIndex
						ifTrue:
							[^ answer]
						ifFalse:
							[answer := '']]
				ifFalse:
					[answer := answer copyWith: aChar]].
	^ answer! !

!String methodsFor: 'accessing' stamp: 'yo 8/27/2002 14:34'!
lineCount
	"Answer the number of lines represented by the receiver, where every cr adds one line.  5/10/96 sw"

	| cr count |
	cr := Character cr.
	count := 1  min: self size..
	1 to: self size do:
		[:i | (self at: i) = cr ifTrue: [count := count + 1]].
	^ count

"
'Fred
the
Bear' lineCount
"! !

!String methodsFor: 'accessing' stamp: 'yo 8/27/2002 14:34'!
lineNumber: anIndex
	"Answer a string containing the characters in the given line number.  5/10/96 sw"

	| crString pos finalPos |
	crString := String with: Character cr.
	pos := 0.
	1 to: anIndex - 1 do:
		[:i | pos := self findString: crString startingAt: pos + 1.
			pos = 0 ifTrue: [^ nil]].
	finalPos := self findString: crString startingAt: pos + 1.
	finalPos = 0 ifTrue: [finalPos := self size + 1].
	^ self copyFrom: pos + 1 to: finalPos - 1

"
'Fred
the
Bear' lineNumber: 3
"! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
linesDo: aBlock
	"execute aBlock with each line in this string.  The terminating CR's are not included in what is passed to aBlock"
	| start end |
	start := 1.
	[ start <= self size ] whileTrue: [
		end := self indexOf: Character cr  startingAt: start  ifAbsent: [ self size + 1 ].
		end := end - 1.

		aBlock value: (self copyFrom: start  to: end).
		start := end + 2. ].! !

!String methodsFor: 'internet' stamp: 'ar 4/10/2005 15:58'!
macToSqueak
	"Convert the receiver from MacRoman to Squeak encoding"
	^ self collect: [:each | each macToSqueak]! !

!String methodsFor: 'comparing' stamp: 'yo 11/3/2004 19:24'!
match: text
	"Answer whether text matches the pattern in this string.
	Matching ignores upper/lower case differences.
	Where this string contains #, text may contain any character.
	Where this string contains *, text may contain any sequence of characters."

	^ self startingAt: 1 match: text startingAt: 1
"
	'*'			match: 'zort' true
	'*baz'		match: 'mobaz' true
	'*baz'		match: 'mobazo' false
	'*baz*'		match: 'mobazo' true
	'*baz*'		match: 'mozo' false
	'foo*'		match: 'foozo' true
	'foo*'		match: 'bozo' false
	'foo*baz'	match: 'foo23baz' true
	'foo*baz'	match: 'foobaz' true
	'foo*baz'	match: 'foo23bazo' false
	'foo'		match: 'Foo' true
	'foo*baz*zort' match: 'foobazort' false
	'foo*baz*zort' match: 'foobazzort' false
	'*foo#zort'	match: 'afoo3zortthenfoo3zort' true
	'*foo*zort'	match: 'afoodezortorfoo3zort' true
"! !

!String methodsFor: 'system primitives' stamp: 'yo 11/3/2004 19:24'!
numArgs 
	"Answer either the number of arguments that the receiver would take if considered a selector.  Answer -1 if it couldn't be a selector.  Note that currently this will answer -1 for anything begining with an uppercase letter even though the system will accept such symbols as selectors.  It is intended mostly for the assistance of spelling correction."

	| firstChar numColons excess start ix |
	self size = 0 ifTrue: [^ -1].
	firstChar := self at: 1.
	(firstChar isLetter or: [firstChar = $:]) ifTrue:
		["Fast reject if any chars are non-alphanumeric"
		(self findSubstring: '~' in: self startingAt: 1 matchTable: Tokenish) > 0 ifTrue: [^ -1].
		"Fast colon count"
		numColons := 0.  start := 1.
		[(ix := self findSubstring: ':' in: self startingAt: start matchTable: CaseSensitiveOrder) > 0]
			whileTrue:
				[numColons := numColons + 1.
				start := ix + 1].
		numColons = 0 ifTrue: [^ 0].
		firstChar = $:
			ifTrue: [excess := 2 "Has an initial keyword, as #:if:then:else:"]
			ifFalse: [excess := 0].
		self last = $:
			ifTrue: [^ numColons - excess]
			ifFalse: [^ numColons - excess - 1 "Has a final keywords as #nextPut::andCR"]].
	firstChar isSpecial ifTrue:
		[self size = 1 ifTrue: [^ 1].
		2 to: self size do: [:i | (self at: i) isSpecial ifFalse: [^ -1]].
		^ 1].
	^ -1.! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
numericSuffix
	^ self stemAndNumericSuffix last

"
'abc98' numericSuffix
'98abc' numericSuffix
"! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
onlyLetters
	"answer the receiver with only letters"
	^ self select:[:each | each isLetter]! !

!String methodsFor: 'user interface' stamp: 'ar 9/27/2005 20:02'!
openInWorkspaceWithTitle: aTitle
	"Open up a workspace with the receiver as its contents, with the given title"
	UIManager default edit: self label: aTitle! !

!String methodsFor: 'copying' stamp: 'yo 11/3/2004 19:24'!
padded: leftOrRight to: length with: char
	leftOrRight = #left ifTrue:
		[^ (String new: (length - self size max: 0) withAll: char) , self].
	leftOrRight = #right ifTrue:
		[^ self , (String new: (length - self size max: 0) withAll: char)].! !

!String methodsFor: 'printing' stamp: 'sd 7/8/2006 18:06'!
printOn: aStream 
	"Print inside string quotes, doubling inbedded quotes."
 
	self storeOn: aStream! !

!String methodsFor: 'encoding' stamp: 'ar 4/10/2005 17:17'!
putInteger32: anInteger at: location
	| integer |
	<primitive: 'putInteger' module: 'IntegerPokerPlugin'>
	"IntegerPokerPlugin doPrimitive: #putInteger"

	"the following is close to 20x faster than the above if the primitive is not compiled"
	"PUTCOUNTER := PUTCOUNTER + 1."
	integer := anInteger.
	integer < 0 ifTrue: [integer :=  1073741824 - integer. ].
	self at: location+3 put: (Character value: (integer \\ 256)).
	self at: location+2 put: (Character value: (integer bitShift: -8) \\ 256).
	self at: location+1 put: (Character value: (integer bitShift: -16) \\ 256).
	self at: location put: (Character value: (integer bitShift: -24) \\ 256).

"Smalltalk at: #PUTCOUNTER put: 0"! !

!String methodsFor: 'filter streaming' stamp: 'yo 8/26/2002 22:31'!
putOn:aStream

	^aStream nextPutAll: self.
! !

!String methodsFor: 'private' stamp: 'yo 8/26/2002 22:53'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	super replaceFrom: start to: stop with: replacement startingAt: repStart! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
romanNumber
	| value v1 v2 |
	value := v1 := v2 := 0.
	self reverseDo:
		[:each |
		v1 := #(1 5 10 50 100 500 1000) at: ('IVXLCDM' indexOf: each).
		v1 >= v2
			ifTrue: [value := value + v1]
			ifFalse: [value := value - v1].
		v2 := v1].
	^ value! !

!String methodsFor: 'comparing' stamp: 'ar 4/10/2005 17:35'!
sameAs: aString 
	"Answer whether the receiver sorts equal to aString. The 
	collation sequence is ascii with case differences ignored."
	^(self compare: aString caseSensitive: false) = 2! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
sansPeriodSuffix
	"Return a copy of the receiver up to, but not including, the first period.  If the receiver's *first* character is a period, then just return the entire receiver. "

	| likely |
	likely := self copyUpTo: $..
	^ likely size == 0
		ifTrue:	[self]
		ifFalse:	[likely]! !

!String methodsFor: 'accessing' stamp: 'yo 8/28/2002 14:28'!
skipAnySubStr: delimiters startingAt: start 
	"Answer the index of the last character within the receiver, starting at start, that does NOT match one of the delimiters. delimiters is a Array of substrings (Characters also allowed).  If the receiver is all delimiters, answer size + 1."

	| any this ind ii |
	ii := start-1.
	[(ii := ii + 1) <= self size] whileTrue: [ "look for char that does not match"
		any := false.
		delimiters do: [:delim |
			delim isCharacter 
				ifTrue: [(self at: ii) == delim ifTrue: [any := true]]
				ifFalse: ["a substring"
					delim size > (self size - ii + 1) ifFalse: "Here's where the one-off error was."
						[ind := 0.
						this := true.
						delim do: [:dd | 
							dd == (self at: ii+ind) ifFalse: [this := false].
							ind := ind + 1].
						this ifTrue: [ii := ii + delim size - 1.  any := true]]
							ifTrue: [any := false] "if the delim is too big, it can't match"]].
		any ifFalse: [^ ii]].
	^ self size + 1! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
skipDelimiters: delimiters startingAt: start 
	"Answer the index of the character within the receiver, starting at start, that does NOT match one of the delimiters. If the receiver does not contain any of the delimiters, answer size + 1.  Assumes the delimiters to be a non-empty string."

	start to: self size do: [:i |
		delimiters detect: [:delim | delim = (self at: i)]
				ifNone: [^ i]].
	^ self size + 1! !

!String methodsFor: 'converting' stamp: 'yo 8/27/2002 11:13'!
splitInteger
	"Answer an array that is a splitting of self into a string and an integer.
	'43Sam' ==> #(43 'Sam').  'Try90' ==> #('Try' 90)
	BUT NOTE: 'Sam' ==> #('Sam' 0), and '90' ==> #('' 90)  ie, (<string> <integer>)."

	| pos |
	(pos := self findFirst: [:d | d isDigit not]) = 0 ifTrue: [^ Array with: '' with: self asNumber].
	self first isDigit ifTrue: [
		^ Array with: (self copyFrom: 1 to: pos - 1) asNumber 
				with: (self copyFrom: pos to: self size)].
	(pos := self findFirst: [:d | d isDigit]) = 0 ifTrue: [^ Array with: self with: 0].
	^ Array with: (self copyFrom: 1 to: pos - 1)
			with: (self copyFrom: pos to: self size) asNumber! !

!String methodsFor: 'internet' stamp: 'ar 4/9/2005 22:16'!
squeakToIso
	^self "no longer needed"! !

!String methodsFor: 'internet' stamp: 'ar 4/10/2005 15:55'!
squeakToMac
	"Convert the receiver from Squeak to MacRoman encoding"
	^ self collect: [:each | each squeakToMac]! !

!String methodsFor: 'converting' stamp: 'ar 6/3/2007 18:32'!
squeakToUtf8
	"Convert the receiver into a UTF8-encoded string"
	^self convertToWithConverter: UTF8TextConverter new.! !

!String methodsFor: 'comparing' stamp: 'yo 11/3/2004 19:24'!
startingAt: keyStart match: text startingAt: textStart
	"Answer whether text matches the pattern in this string.
	Matching ignores upper/lower case differences.
	Where this string contains #, text may contain any character.
	Where this string contains *, text may contain any sequence of characters."
	| anyMatch matchStart matchEnd i matchStr j ii jj |
	i := keyStart.
	j := textStart.

	"Check for any #'s"
	[i > self size ifTrue: [^ j > text size "Empty key matches only empty string"].
	(self at: i) = $#] whileTrue:
		["# consumes one char of key and one char of text"
		j > text size ifTrue: [^ false "no more text"].
		i := i+1.  j := j+1].

	"Then check for *"
	(self at: i) = $*
		ifTrue: [i = self size ifTrue:
					[^ true "Terminal * matches all"].
				"* means next match string can occur anywhere"
				anyMatch := true.
				matchStart := i + 1]
		ifFalse: ["Otherwise match string must occur immediately"
				anyMatch := false.
				matchStart := i].

	"Now determine the match string"
	matchEnd := self size.
	(ii := self indexOf: $* startingAt: matchStart) > 0 ifTrue:
		[ii = 1 ifTrue: [self error: '** not valid -- use * instead'].
		matchEnd := ii-1].
	(ii := self indexOf: $# startingAt: matchStart) > 0 ifTrue:
		[ii = 1 ifTrue: [self error: '*# not valid -- use #* instead'].
		matchEnd := matchEnd min: ii-1].
	matchStr := self copyFrom: matchStart to: matchEnd.

	"Now look for the match string"
	[jj := text findString: matchStr startingAt: j caseSensitive: false.
	anyMatch ifTrue: [jj > 0] ifFalse: [jj = j]]
		whileTrue:
		["Found matchStr at jj.  See if the rest matches..."
		(self startingAt: matchEnd+1 match: text startingAt: jj + matchStr size) ifTrue:
			[^ true "the rest matches -- success"].
		"The rest did not match."
		anyMatch ifFalse: [^ false].
		"Preceded by * -- try for a later match"
		j := j+1].
	^ false "Failed to find the match string"! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
startsWithDigit
	"Answer whether the receiver's first character represents a digit"

	^ self size > 0 and: [self first isDigit]! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
stemAndNumericSuffix
	"Parse the receiver into a string-valued stem and a numeric-valued suffix.  6/7/96 sw"

	| stem suffix position |

	stem := self.
	suffix := 0.
	position := 1.
	[stem endsWithDigit and: [stem size > 1]] whileTrue:
		[suffix :=  stem last digitValue * position + suffix.
		position := position * 10.
		stem := stem copyFrom: 1 to: stem size - 1].
	^ Array with: stem with: suffix

"'Fred2305' stemAndNumericSuffix"! !

!String methodsFor: 'printing' stamp: 'yo 8/26/2002 22:58'!
storeOn: aStream 
	"Print inside string quotes, doubling inbedded quotes."
	| x |
	aStream nextPut: $'.
	1 to: self size do:
		[:i |
		aStream nextPut: (x := self at: i).
		x = $' ifTrue: [aStream nextPut: x]].
	aStream nextPut: $'! !

!String methodsFor: 'accessing' stamp: 'md 5/26/2005 13:35'!
string
	^self! !

!String methodsFor: 'printing' stamp: 'yo 11/3/2004 19:24'!
stringRepresentation
	"Answer a string that represents the receiver.  For most objects this is simply its printString, but for strings themselves, it's themselves, to avoid the superfluous extra pair of quotes.  6/12/96 sw"

	^ self ! !

!String methodsFor: 'private' stamp: 'yo 8/28/2002 15:22'!
stringhash

	^ self hash.
! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
subStrings
	"Answer an array of the substrings that compose the receiver."
	#Collectn.
	"Added 2000/04/08 For ANSI <readableString> protocol."
	^ self substrings! !

!String methodsFor: 'converting' stamp: 'ar 4/12/2005 16:32'!
subStrings: separators 
	"Answer an array containing the substrings in the receiver separated 
	by the elements of separators."
	| char result sourceStream subString |
	#Collectn.
	"Changed 2000/04/08 For ANSI <readableString> protocol."
	(separators isString or:[separators allSatisfy: [:element | element isKindOf: Character]])
		ifFalse: [^ self error: 'separators must be Characters.'].
	sourceStream := ReadStream on: self.
	result := OrderedCollection new.
	subString := String new.
	[sourceStream atEnd]
		whileFalse: 
			[char := sourceStream next.
			(separators includes: char)
				ifTrue: [subString notEmpty
						ifTrue: 
							[result add: subString copy.
							subString := String new]]
				ifFalse: [subString := subString , (String with: char)]].
	subString notEmpty ifTrue: [result add: subString copy].
	^ result asArray! !

!String methodsFor: 'converting' stamp: 'ar 7/21/2007 21:50'!
substrings
	"Answer an array of the substrings that compose the receiver."
	| result end beginning |
	result := WriteStream on: (Array new: 10).
	end := 0.
	"find one substring each time through this loop"
	[ "find the beginning of the next substring"
	beginning := end+1.
	[beginning <= self size and:[(self at: beginning) isSeparator]]
		whileTrue:[beginning := beginning + 1].
	beginning <= self size] whileTrue: [
		"find the end"
		end := beginning.
		[end <= self size and:[(self at: end) isSeparator not]]
			whileTrue:[end := end + 1].
		end := end - 1.
		result nextPut: (self copyFrom: beginning to: end).
	].
	^result contents! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
surroundedBySingleQuotes
	"Answer the receiver with leading and trailing quotes.  "

	^ $' asString, self, $' asString! !

!String methodsFor: 'accessing' stamp: 'yo 11/3/2004 19:24'!
tabDelimitedFieldsDo: aBlock
	"Considering the receiver as a holder of tab-delimited fields, evaluate execute aBlock with each field in this string.  The separatilng tabs are not included in what is passed to aBlock"

	| start end |
	"No senders but was useful enough in earlier work that it's retained for the moment."
	start := 1.
	[start <= self size] whileTrue: 
		[end := self indexOf: Character tab startingAt: start ifAbsent: [self size + 1].
		end := end - 1.
		aBlock value: (self copyFrom: start  to: end).
		start := end + 2]

"
'fred	charlie	elmo		2' tabDelimitedFieldsDo: [:aField | Transcript cr; show: aField]
"! !

!String methodsFor: 'converting' stamp: 'yo 8/28/2002 15:14'!
translateFrom: start  to: stop  table: table
	"translate the characters in the string by the given table, in place"
	self class translate: self from: start to: stop table: table! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
translateToLowercase
	"Translate all characters to lowercase, in place"

	self translateWith: LowercasingTable! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
translateToUppercase
	"Translate all characters to lowercase, in place"

	self translateWith: UppercasingTable! !

!String methodsFor: 'converting' stamp: 'yo 8/28/2002 15:13'!
translateWith: table
	"translate the characters in the string by the given table, in place"
	^ self translateFrom: 1 to: self size table: table! !

!String methodsFor: 'translating' stamp: 'dgd 8/24/2004 19:42'!
translated
	"answer the receiver translated to the default language"
	^ NaturalLanguageTranslator current  translate: self! !

!String methodsFor: 'translating' stamp: 'dgd 8/27/2004 18:43'!
translatedIfCorresponds
	"answer the receiver translated to the default language only if 
	the receiver begins and ends with an underscore (_)"
	^ ('_*_' match: self)
		ifTrue: [(self copyFrom: 2 to: self size - 1) translated]
		ifFalse: [self]! !

!String methodsFor: 'translating' stamp: 'dgd 8/24/2004 19:38'!
translatedTo: localeID 
	"answer the receiver translated to the given locale id"
	^ localeID translator translate: self! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
truncateTo: smallSize
	"return myself or a copy shortened to smallSize.  1/18/96 sw"

	^ self size <= smallSize
		ifTrue:
			[self]
		ifFalse:
			[self copyFrom: 1 to: smallSize]! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
truncateWithElipsisTo: maxLength
	"Return myself or a copy suitably shortened but with elipsis added"

	^ self size <= maxLength
		ifTrue:
			[self]
		ifFalse:
			[(self copyFrom: 1 to: (maxLength - 3)), '...']


	"'truncateWithElipsisTo:' truncateWithElipsisTo: 20"! !

!String methodsFor: 'converting' stamp: 'KR 9/22/2005 23:06'!
unescapePercents
	"decode %xx form.  This is the opposite of #encodeForHTTP"
	^ self unescapePercentsWithTextEncoding: 'utf-8'.! !

!String methodsFor: 'converting' stamp: 'ky 7/8/2006 17:56'!
unescapePercentsWithTextEncoding: encodingName 
	"decode string including %XX form"
	| unescaped char asciiVal specialChars oldPos pos converter |
	unescaped := ReadWriteStream on: String new.
	specialChars := '+%' asCharacterSet.
	oldPos := 1.
	[pos := self indexOfAnyOf: specialChars startingAt: oldPos.
	pos > 0]
		whileTrue: [unescaped
				nextPutAll: (self copyFrom: oldPos to: pos - 1).
			char := self at: pos.
			(char = $%
					and: [pos + 2 <= self size])
				ifTrue: [asciiVal := (self at: pos + 1) asUppercase digitValue * 16 + (self at: pos + 2) asUppercase digitValue.
					asciiVal > 255
						ifTrue: [^ self].
					unescaped
						nextPut: (Character value: asciiVal).
					pos := pos + 3.
					pos <= self size
						ifFalse: [char := nil].
					oldPos := pos]
				ifFalse: [char = $+
						ifTrue: [unescaped nextPut: Character space]
						ifFalse: [unescaped nextPut: char].
					oldPos := pos + 1]].
	oldPos <= self size
		ifTrue: [unescaped
				nextPutAll: (self copyFrom: oldPos to: self size)].
	converter := (TextConverter newForEncoding: encodingName)
				ifNil: [TextConverter newForEncoding: nil].
	^ [unescaped contents convertFromWithConverter: converter]
		on: Error
		do: ["the contents may be squeak-encoded"
			unescaped contents]! !

!String methodsFor: 'converting' stamp: 'yo 8/27/2002 11:20'!
unparenthetically
	"If the receiver starts with (..( and ends with matching )..), strip them"

	| curr |
	curr := self.
	[((curr first = $() and: [curr last = $)])] whileTrue:
		[curr := curr copyFrom: 2 to: (curr size - 1)].

	^ curr

"

'((fred the bear))' unparenthetically

"
		! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
unzipped
	| magic1 magic2 |
	magic1 := (self at: 1) asInteger.
	magic2 := (self at: 2) asInteger.
	(magic1 = 16r1F and:[magic2 = 16r8B]) ifFalse:[^self].
	^(GZipReadStream on: self) upToEnd! !

!String methodsFor: 'internet' stamp: 'yo 11/3/2004 19:24'!
utf8ToIso
	"Only UTF-8 characters that maps to 8-bit ISO-8559-1 values are converted. Others raises an error"
	| s i c v c2 v2 |
	s := WriteStream on: (String new: self size).
	
	i := 1.
	[i <= self size] whileTrue: [
		c := self at: i. i:=i+1.
		v := c asciiValue.
		(v > 128)
			ifFalse: [ s nextPut: c ]
			ifTrue: [((v bitAnd: 252) == 192)
				ifFalse: [self error: 'illegal UTF-8 ISO character']
				ifTrue: [
					(i > self size) ifTrue: [ self error: 'illegal end-of-string, expected 2nd byte of UTF-8'].
					c2 := self at: i. i:=i+1.
					v2 := c2 asciiValue.
					((v2 bitAnd: 192) = 128) ifFalse: [self error: 'illegal 2nd UTF-8 char']. 
					s nextPut: ((v2 bitAnd: 63) bitOr: ((v << 6) bitAnd: 192)) asCharacter]]].
	^s contents. 
! !

!String methodsFor: 'converting' stamp: 'ar 6/3/2007 18:32'!
utf8ToSqueak
	"Convert the receiver from a UTF8-encoded string"
	^self convertFromWithConverter: UTF8TextConverter new.! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
withBlanksCondensed
	"Return a copy of the receiver with leading/trailing blanks removed
	 and consecutive white spaces condensed."

	| trimmed lastBlank |
	trimmed := self withBlanksTrimmed.
	^String streamContents: [:stream |
		lastBlank := false.
		trimmed do: [:c | (c isSeparator and: [lastBlank]) ifFalse: [stream nextPut: c].
			lastBlank := c isSeparator]].

	" ' abc  d   ' withBlanksCondensed"
! !

!String methodsFor: 'converting' stamp: 'yo 7/5/2004 16:43'!
withBlanksTrimmed
	"Return a copy of the receiver from which leading and trailing blanks have been trimmed."

	| first result |
	first := self findFirst: [:c | c isSeparator not].
	first = 0 ifTrue: [^ ''].  "no non-separator character"
	result :=  self
		copyFrom: first
		to: (self findLast: [:c | c isSeparator not]).
	result isOctetString ifTrue: [^ result asOctetString] ifFalse: [^ result].

	" ' abc  d   ' withBlanksTrimmed"
! !

!String methodsFor: 'formatting' stamp: 'yo 11/3/2004 19:24'!
withCRs
	"Return a copy of the receiver in which backslash (\) characters have been replaced with carriage returns."

	^ self collect: [ :c | c = $\ ifTrue: [ Character cr ] ifFalse: [ c ]].! !

!String methodsFor: 'converting' stamp: 'md 9/19/2004 15:19'!
withFirstCharacterDownshifted
	"Return a copy with the first letter downShifted"
	
	| answer |
	
	self ifEmpty: [^ self copy].
	answer := self copy.
	answer at: 1 put: (answer at: 1) asLowercase.
	^ answer. ! !

!String methodsFor: 'internet' stamp: 'yo 11/3/2004 19:24'!
withInternetLineEndings
	"change line endings from CR's to CRLF's.  This is probably in
prepration for sending a string over the Internet"
	| cr lf |
	cr := Character cr.
	lf := Character linefeed.
	^self class streamContents: [ :stream |
		self do: [ :c |
			stream nextPut: c.
			c = cr ifTrue:[ stream nextPut: lf ]. ] ].! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
withNoLineLongerThan: aNumber
	"Answer a string with the same content as receiver, but rewrapped so that no line has more characters than the given number"
	| listOfLines currentLast currentStart resultString putativeLast putativeLine crPosition |
	aNumber isNumber not | (aNumber < 1) ifTrue: [self error: 'too narrow'].
	listOfLines := OrderedCollection new.
	currentLast := 0.
	[currentLast < self size] whileTrue:
		[currentStart := currentLast + 1.
		putativeLast := (currentStart + aNumber - 1) min: self size.
		putativeLine := self copyFrom: currentStart to: putativeLast.
		(crPosition := putativeLine indexOf: Character cr) > 0 ifTrue:
			[putativeLast := currentStart + crPosition - 1.
			putativeLine := self copyFrom: currentStart to: putativeLast].
		currentLast := putativeLast == self size
			ifTrue:
				[putativeLast]
			ifFalse:
				[currentStart + putativeLine lastSpacePosition - 1].
		currentLast <= currentStart ifTrue:
			["line has NO spaces; baleout!!"
			currentLast := putativeLast].
		listOfLines add: (self copyFrom: currentStart to: currentLast) withBlanksTrimmed].

	listOfLines size > 0 ifFalse: [^ ''].
	resultString := listOfLines first.
	2 to: listOfLines size do:
		[:i | resultString := resultString, String cr, (listOfLines at: i)].
	^ resultString

"#(5 7 20) collect:
	[:i | 'Fred the bear went down to the brook to read his book in silence' withNoLineLongerThan: i]"! !

!String methodsFor: 'converting' stamp: 'tak 4/25/2004 12:57'!
withSeparatorsCompacted
	"replace each sequences of whitespace by a single space character"
	"' test ' withSeparatorsCompacted = ' test '"
	"' test test' withSeparatorsCompacted = ' test test'"
	"'test test		' withSeparatorsCompacted = 'test test '"

	| out in next isSeparator |
	self isEmpty ifTrue: [^ self].

	out := WriteStream on: (String new: self size).
	in := self readStream.
	isSeparator := [:char | char asciiValue < 256
				and: [CSSeparators includes: char]].
	[in atEnd] whileFalse: [
		next := in next.
		(isSeparator value: next)
			ifTrue: [
				out nextPut: $ .
				[in atEnd or:
					[next := in next.
					(isSeparator value: next)
						ifTrue: [false]
						ifFalse: [out nextPut: next. true]]] whileFalse]
			ifFalse: [out nextPut: next]].
	^ out contents! !

!String methodsFor: 'internet' stamp: 'yo 11/3/2004 19:24'!
withSqueakLineEndings
	"assume the string is textual, and that CR, LF, and CRLF are all 
	valid line endings.  Replace each occurence with a single CR"
	| cr lf input c crlf inPos outPos outString lineEndPos newOutPos |
	cr := Character cr.
	lf := Character linefeed.
	crlf := CharacterSet new.
	crlf add: cr; add: lf.

	inPos := 1.
	outPos := 1.
	outString :=
 String new: self size.

	[ lineEndPos := self indexOfAnyOf: crlf startingAt: inPos ifAbsent: [0].
		lineEndPos ~= 0 ] whileTrue: [
			newOutPos := outPos + (lineEndPos - inPos + 1).
			outString replaceFrom: outPos to: newOutPos - 2 with: self startingAt: inPos.
			outString at: newOutPos-1 put: cr.
			outPos := newOutPos.

			((self at: lineEndPos) = cr and: [ lineEndPos < self size and: [ (self at: lineEndPos+1) = lf ] ]) ifTrue: [
				"CRLF ending"
				inPos := lineEndPos + 2 ]
			ifFalse: [ 
				"CR or LF ending"
				inPos := lineEndPos + 1 ]. ].

	"no more line endings.  copy the rest"
	newOutPos := outPos + (self size - inPos + 1).
	outString replaceFrom: outPos to: newOutPos-1 with: self startingAt: inPos.

	^outString copyFrom: 1 to: newOutPos-1
	! !

!String methodsFor: 'converting' stamp: 'md 10/5/2005 11:01'!
withoutLeadingBlanks
	
	"Return a copy of the receiver from which leading blanks have been
trimmed."

	
	| first |
	
	first := self findFirst: [:c | c isSeparator not ].

	first = 0 ifTrue: [^ ''].  
	
	"no non-separator character"
	
	^ self copyFrom: first to: self size

	
		
	" '    abc  d' withoutLeadingBlanks"
! !

!String methodsFor: 'converting' stamp: 'yo 8/27/2002 14:06'!
withoutLeadingDigits
	"Answer the portion of the receiver that follows any leading series of digits and blanks.  If the receiver consists entirely of digits and blanks, return an empty string"
	| firstNonDigit |
	firstNonDigit := (self findFirst: [:m | m isDigit not and: [m ~= $ ]]).
	^ firstNonDigit > 0
		ifTrue:
			[self copyFrom: firstNonDigit  to: self size]
		ifFalse:
			['']

"
'234Whoopie' withoutLeadingDigits
' 4321 BlastOff!!' withoutLeadingDigits
'wimpy' withoutLeadingDigits
'  89Ten ' withoutLeadingDigits
'78 92' withoutLeadingDigits
"
! !

!String methodsFor: 'internet' stamp: 'yo 11/3/2004 19:24'!
withoutQuoting
	"remove the initial and final quote marks, if present"
	"'''h''' withoutQuoting"
	| quote |
	self size < 2 ifTrue: [ ^self ].
	quote := self first.
	(quote = $' or: [ quote = $" ])
		ifTrue: [ ^self copyFrom: 2 to: self size - 1 ]
		ifFalse: [ ^self ].! !

!String methodsFor: 'converting' stamp: 'yo 11/3/2004 19:24'!
withoutTrailingBlanks
	"Return a copy of the receiver from which trailing blanks have been trimmed."

	| last |
	last := self findLast: [:c | c isSeparator not].
	last = 0 ifTrue: [^ ''].  "no non-separator character"
	^ self copyFrom: 1 to: last

	" ' abc  d   ' withoutTrailingBlanks"
! !

!String methodsFor: 'converting' stamp: 'yo 8/27/2002 14:06'!
withoutTrailingDigits
	"Answer the portion of the receiver that precedes any trailing series of digits and blanks.  If the receiver consists entirely of digits and blanks, return an empty string"
	| firstDigit |
	firstDigit := (self findFirst: [:m | m isDigit or: [m = $ ]]).
	^ firstDigit > 0
		ifTrue:
			[self copyFrom: 1 to: firstDigit-1]
		ifFalse:
			[self]

"
'Whoopie234' withoutTrailingDigits
' 4321 BlastOff!!' withoutLeadingDigits
'wimpy' withoutLeadingDigits
'  89Ten ' withoutLeadingDigits
'78 92' withoutLeadingDigits
"
! !

!String methodsFor: 'encoding' stamp: 'ar 4/10/2005 17:18'!
writeLeadingCharRunsOn: stream

	| runLength runValues runStart leadingChar |
	self isEmpty ifTrue: [^ self].

	runLength := OrderedCollection new.
	runValues := OrderedCollection new.
	runStart := 1.
	leadingChar := (self at: runStart) leadingChar.
	2 to: self size do: [:index |
		(self at: index) leadingChar = leadingChar ifFalse: [
			runValues add: leadingChar.
			runLength add: (index - runStart).
			leadingChar := (self at: index) leadingChar.
			runStart := index.
		].
	].
	runValues add: (self last) leadingChar.
	runLength add: self size + 1 -  runStart.

	stream nextPut: $(.
	runLength do: [:rr | rr printOn: stream. stream space].
	stream skip: -1; nextPut: $).
	runValues do: [:vv | vv printOn: stream. stream nextPut: $,].
	stream skip: -1.
! !

!String methodsFor: 'converting' stamp: 'dgd 11/26/2005 21:19'!
zipped
	| stream gzstream |

	stream := RWBinaryOrTextStream on: String new.

	gzstream := GZipWriteStream on: stream.
	gzstream nextPutAll: self.
	gzstream close.
	stream reset.

	^ stream contents.
! !

String subclass: #Symbol
	instanceVariableNames: ''
	classVariableNames: 'SymbolTable OneCharacterSymbols NewSymbols'
	poolDictionaries: ''
	category: 'Collections-Strings'!

!Symbol commentStamp: '<historical>' prior: 0!
I represent Strings that are created uniquely. Thus, someString asSymbol == someString asSymbol.!

Symbol variableByteSubclass: #ByteSymbol
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Strings'!

!ByteSymbol commentStamp: '<historical>' prior: 0!
This class represents the symbols containing 8bit characters.!

!ByteSymbol class methodsFor: 'primitives' stamp: 'ar 8/18/2005 13:46'!
findFirstInString: aString inSet: inclusionMap startingAt: start
	^ByteString findFirstInString: aString  inSet: inclusionMap startingAt: start! !

!ByteSymbol class methodsFor: 'primitives' stamp: 'ar 8/18/2005 13:46'!
indexOfAscii: anInteger inString: aString startingAt: start
	^ByteString indexOfAscii: anInteger inString: aString startingAt: start! !

!ByteSymbol class methodsFor: 'primitives' stamp: 'ar 8/18/2005 13:46'!
stringHash: aString initialHash: speciesHash
	^ByteString stringHash: aString initialHash: speciesHash! !

!ByteSymbol class methodsFor: 'primitives' stamp: 'ar 8/18/2005 13:47'!
translate: aString from: start  to: stop  table: table
	^ByteString translate: aString from: start  to: stop  table: table! !

!ByteSymbol methodsFor: 'converting' stamp: 'ar 4/10/2005 22:12'!
asByteArray
	| ba sz |
	sz := self byteSize.
	ba := ByteArray new: sz.
	ba replaceFrom: 1 to: sz with: self startingAt: 1.
	^ba! !

!ByteSymbol methodsFor: 'converting' stamp: 'ar 4/10/2005 22:12'!
asOctetString
	^ self! !

!ByteSymbol methodsFor: 'accessing' stamp: 'ar 4/10/2005 22:10'!
at: index 
	"Primitive. Answer the Character stored in the field of the receiver
	indexed by the argument. Fail if the index argument is not an Integer or
	is out of bounds. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 63>
	^ Character value: (super at: index)! !

!ByteSymbol methodsFor: 'accessing' stamp: 'ar 4/10/2005 22:10'!
at: anInteger put: anObject 
	"You cannot modify the receiver."
	self errorNoModification! !

!ByteSymbol methodsFor: 'accessing' stamp: 'ar 4/10/2005 22:10'!
byteAt: index
	<primitive: 60>
	^(self at: index) asciiValue! !

!ByteSymbol methodsFor: 'accessing' stamp: 'ar 4/10/2005 22:10'!
byteAt: anInteger put: anObject 
	"You cannot modify the receiver."
	self errorNoModification! !

!ByteSymbol methodsFor: 'accessing' stamp: 'ar 4/10/2005 22:11'!
byteSize
	^self size! !

!ByteSymbol methodsFor: 'comparing' stamp: 'ar 4/10/2005 22:14'!
findSubstring: key in: body startingAt: start matchTable: matchTable
	"Answer the index in the string body at which the substring key first occurs, at or beyond start.  The match is determined using matchTable, which can be used to effect, eg, case-insensitive matches.  If no match is found, zero will be returned."
	<primitive: 'primitiveFindSubstring' module: 'MiscPrimitivePlugin'>
	^super findSubstring: key in: body startingAt: start matchTable: matchTable! !

!ByteSymbol methodsFor: 'testing' stamp: 'ar 4/10/2005 22:14'!
isByteString
	"Answer whether the receiver is a ByteString"
	^true! !

!ByteSymbol methodsFor: 'testing' stamp: 'ar 4/10/2005 22:14'!
isOctetString
	"Answer whether the receiver can be represented as a byte string. 
	This is different from asking whether the receiver *is* a ByteString 
	(i.e., #isByteString)"
	^ true.
! !

!ByteSymbol methodsFor: 'private' stamp: 'ar 4/11/2005 00:08'!
pvtAt: index put: aCharacter
	"Primitive. Store the Character in the field of the receiver indicated by
	the index. Fail if the index is not an Integer or is out of bounds, or if
	the argument is not a Character. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 64>
	aCharacter isCharacter 
		ifFalse:[^self errorImproperStore].
	index isInteger
		ifTrue: [self errorSubscriptBounds: index]
		ifFalse: [self errorNonIntegerIndex]! !

!ByteSymbol methodsFor: 'accessing' stamp: 'ar 4/12/2005 17:51'!
species
	"Answer the preferred class for reconstructing the receiver."
	^ByteString
! !

!ByteSymbol methodsFor: 'private' stamp: 'ar 4/10/2005 23:02'!
string: aString
	1 to: aString size do: [:j | self pvtAt: j put: (aString at: j)].
	^self! !

!Symbol class methodsFor: 'class initialization' stamp: 'RAA 5/29/2001 08:21'!
allSymbolTablesDo: aBlock

	NewSymbols do: aBlock.
	SymbolTable do: aBlock.! !

!Symbol class methodsFor: 'class initialization' stamp: 'RAA 5/29/2001 14:35'!
allSymbolTablesDo: aBlock after: aSymbol

	NewSymbols do: aBlock after: aSymbol.
	SymbolTable do: aBlock after: aSymbol.! !

!Symbol class methodsFor: 'access' stamp: 'ar 4/10/2005 22:49'!
allSymbols
	"Answer all interned symbols"
	^Array streamContents:[:s|
		s nextPutAll: NewSymbols.
		s nextPutAll: OneCharacterSymbols.
		s nextPutAll: SymbolTable.
	].
! !

!Symbol class methodsFor: 'class initialization' stamp: 'RAA 12/17/2000 18:05'!
compactSymbolTable
	"Reduce the size of the symbol table so that it holds all existing symbols + 25% (changed from 1000 since sets like to have 25% free and the extra space would grow back in a hurry)"

	| oldSize |

	Smalltalk garbageCollect.
	oldSize := SymbolTable array size.
	SymbolTable growTo: SymbolTable size * 4 // 3 + 100.
	^oldSize printString,'  ',(oldSize - SymbolTable array size) printString, ' slot(s) reclaimed'! !

!Symbol class methodsFor: 'instance creation' stamp: ''!
findInterned:aString

	self hasInterned:aString ifTrue:[:symbol| ^symbol].
	^nil.! !

!Symbol class methodsFor: 'private' stamp: 'ar 4/10/2005 22:43'!
hasInterned: aString ifTrue: symBlock 
	"Answer with false if aString hasnt been interned (into a Symbol),  
	otherwise supply the symbol to symBlock and return true."

	| symbol |
	^ (symbol := self lookup: aString)
		ifNil: [false]
		ifNotNil: [symBlock value: symbol.
			true]! !

!Symbol class methodsFor: 'class initialization' stamp: 'RAA 5/29/2001 09:04'!
initialize

	"Symbol initialize"

	Symbol rehash.
	OneCharacterSymbols := nil.
	OneCharacterSymbols := (1 to: 256) collect: [ :i | (i - 1) asCharacter asSymbol].
	Smalltalk addToShutDownList: self.
! !

!Symbol class methodsFor: 'instance creation' stamp: 'ar 4/12/2005 17:37'!
intern: aStringOrSymbol 

	^(self lookup: aStringOrSymbol) ifNil:[
		| aClass aSymbol |
		aStringOrSymbol isSymbol ifTrue:[
			aSymbol := aStringOrSymbol.
		] ifFalse:[
			aClass := aStringOrSymbol isOctetString ifTrue:[ByteSymbol] ifFalse:[WideSymbol].
			aSymbol := aClass new: aStringOrSymbol size.
			aSymbol string: aStringOrSymbol.
		].
		NewSymbols add: aSymbol.
		aSymbol].! !

!Symbol class methodsFor: 'instance creation' stamp: 'ar 4/10/2005 23:04'!
internCharacter: aCharacter
	aCharacter asciiValue > 256 ifTrue:[^self intern: aCharacter asString].
	OneCharacterSymbols ifNil: [^self intern: aCharacter asString].
	^OneCharacterSymbols at: aCharacter asciiValue + 1
! !

!Symbol class methodsFor: 'instance creation' stamp: 'RAA 5/29/2001 08:09'!
lookup: aStringOrSymbol

	^(SymbolTable like: aStringOrSymbol) ifNil: [
		NewSymbols like: aStringOrSymbol
	]! !

!Symbol class methodsFor: 'instance creation' stamp: ''!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."

	^ (aCollection as: String) asSymbol

"	Symbol newFrom: {$P. $e. $n}
	{$P. $e. $n} as: Symbol
"! !

!Symbol class methodsFor: 'private' stamp: 'RAA 5/29/2001 14:33'!
possibleSelectorsFor: misspelled 
	"Answer an ordered collection of possible corrections
	for the misspelled selector in order of likelyhood"

	| numArgs candidates lookupString best binary short long first ss |
	lookupString := misspelled asLowercase. "correct uppercase selectors to lowercase"
	numArgs := lookupString numArgs.
	(numArgs < 0 or: [lookupString size < 2]) ifTrue: [^ OrderedCollection new: 0].
	first := lookupString first.
	short := lookupString size - (lookupString size // 4 max: 3) max: 2.
	long := lookupString size + (lookupString size // 4 max: 3).

	"First assemble candidates for detailed scoring"
	candidates := OrderedCollection new.
	self allSymbolTablesDo: [:s | (((ss := s size) >= short	"not too short"
			and: [ss <= long			"not too long"
					or: [(s at: 1) = first]])	"well, any length OK if starts w/same letter"
			and: [s numArgs = numArgs])	"and numArgs is the same"
			ifTrue: [candidates add: s]].

	"Then further prune these by correctAgainst:"
	best := lookupString correctAgainst: candidates.
	((misspelled last ~~ $:) and: [misspelled size > 1]) ifTrue: [
		binary := misspelled, ':'.		"try for missing colon"
		Symbol hasInterned: binary ifTrue: [:him | best addFirst: him]].
	^ best! !

!Symbol class methodsFor: 'instance creation' stamp: 'di 10/11/1999 00:02'!
readFrom: strm  "Symbol readFromString: '#abc'"

	strm peek = $# ifFalse: [self error: 'Symbols must be introduced by #'].
	^ (Scanner new scan: strm) advance  "Just do what the code scanner does"! !

!Symbol class methodsFor: 'private' stamp: 'ar 9/27/2005 20:01'!
rehash		"Symbol rehash"
	"Rebuild the hash table, reclaiming unreferenced Symbols."

	SymbolTable := WeakSet withAll: self allSubInstances.
	NewSymbols := WeakSet new.! !

!Symbol class methodsFor: 'access' stamp: 'yo 11/3/2004 19:24'!
selectorsContaining: aString
	"Answer a list of selectors that contain aString within them. Case-insensitive.  Does return symbols that begin with a capital letter."

	| size selectorList ascii |

	selectorList := OrderedCollection new.
	(size := aString size) = 0 ifTrue: [^selectorList].

	aString size = 1 ifTrue:
		[
			ascii := aString first asciiValue.
			ascii < 128 ifTrue: [selectorList add: (OneCharacterSymbols at: ascii+1)]
		].

	aString first isLetter ifFalse:
		[
			aString size == 2 ifTrue: 
				[Symbol hasInterned: aString ifTrue:
					[:s | selectorList add: s]].
			^selectorList
		].

	selectorList := selectorList copyFrom: 2 to: selectorList size.

	self allSymbolTablesDo: [:each |
		each size >= size ifTrue:
			[(each findSubstring: aString in: each startingAt: 1 
				matchTable: CaseInsensitiveOrder) > 0
						ifTrue: [selectorList add: each]]].

	^selectorList reject: [:each | "reject non-selectors, but keep ones that begin with an uppercase"
		each numArgs < 0 and: [each asString withFirstCharacterDownshifted numArgs < 0]].

"Symbol selectorsContaining: 'scon'"! !

!Symbol class methodsFor: 'private' stamp: 'RAA 5/29/2001 09:04'!
shutDown: aboutToQuit

	SymbolTable addAll: NewSymbols.
	NewSymbols := WeakSet new.! !

!Symbol class methodsFor: 'access' stamp: 'RAA 5/29/2001 14:35'!
thatStarts: leadingCharacters skipping: skipSym
	"Answer a selector symbol that starts with leadingCharacters.
	Symbols beginning with a lower-case letter handled directly here.
	Ignore case after first char.
	If skipSym is not nil, it is a previous answer; start searching after it.
	If no symbols are found, answer nil.
	Used by Alt-q (Command-q) routines"

	| size firstMatch key |

	size := leadingCharacters size.
	size = 0 ifTrue: [^skipSym ifNil: [#''] ifNotNil: [nil]].

	firstMatch := leadingCharacters at: 1.
	size > 1 ifTrue: [key := leadingCharacters copyFrom: 2 to: size].

	self allSymbolTablesDo: [:each |
			each size >= size ifTrue:
				[
					((each at: 1) == firstMatch and:
						[key == nil or:
							[(each findString: key startingAt: 2 caseSensitive: false) = 2]])
								ifTrue: [^each]
				]
		] after: skipSym.

	^nil

"Symbol thatStarts: 'sf' skipping: nil"
"Symbol thatStarts: 'sf' skipping: #sfpGetFile:with:with:with:with:with:with:with:with:"
"Symbol thatStarts: 'candidate' skipping: nil"
! !

!Symbol class methodsFor: 'access' stamp: 'tween 9/13/2004 10:09'!
thatStartsCaseSensitive: leadingCharacters skipping: skipSym
	"Same as thatStarts:skipping: but caseSensitive"
	| size firstMatch key |

	size := leadingCharacters size.
	size = 0 ifTrue: [^skipSym ifNil: [#''] ifNotNil: [nil]].
	firstMatch := leadingCharacters at: 1.
	size > 1 ifTrue: [key := leadingCharacters copyFrom: 2 to: size].
	self allSymbolTablesDo: [:each |
			each size >= size ifTrue:
				[
					((each at: 1) == firstMatch and:
						[key == nil or:
							[(each findString: key startingAt: 2 caseSensitive: true) = 2]])
								ifTrue: [^each]
				]
		] after: skipSym.

	^nil
! !

!Symbol methodsFor: 'comparing' stamp: 'ar 4/10/2005 23:45'!
= aSymbol
	"Compare the receiver and aSymbol." 
	self == aSymbol ifTrue: [^ true].
	self class == aSymbol class ifTrue: [^ false].
	"Use String comparison otherwise"
	^ super = aSymbol! !

!Symbol methodsFor: 'user interface' stamp: 'sma 11/12/2000 11:46'!
asExplorerString
	^ self printString! !

!Symbol methodsFor: 'converting' stamp: 'st 11/22/2004 17:26'!
asMutator
	"Return a setter message from a getter message. For example,
	#name asMutator returns #name:"
	^ (self copyWith: $:) asSymbol! !

!Symbol methodsFor: 'converting' stamp: 'ar 4/10/2005 22:42'!
asString 
	"Refer to the comment in String|asString."
	| newString |
	newString := self species new: self size.
	newString replaceFrom: 1 to: newString size with: self startingAt: 1.
	^newString! !

!Symbol methodsFor: 'converting' stamp: ''!
asSymbol 
	"Refer to the comment in String|asSymbol."! !

!Symbol methodsFor: 'accessing' stamp: ''!
at: anInteger put: anObject 
	"You cannot modify the receiver."

	self errorNoModification! !

!Symbol methodsFor: 'filter streaming' stamp: 'mpw 1/1/1901 00:20'!
byteEncode:aStream
	^aStream writeSymbol:self.
! !

!Symbol methodsFor: 'converting' stamp: 'sw 1/28/98 18:18'!
capitalized
	^ self asString capitalized asSymbol! !

!Symbol methodsFor: 'copying' stamp: 'tk 6/26/1998 11:35'!
clone
	"Answer with the receiver, because Symbols are unique."! !

!Symbol methodsFor: 'copying' stamp: ''!
copy
	"Answer with the receiver, because Symbols are unique."! !

!Symbol methodsFor: 'private' stamp: ''!
errorNoModification

	self error: 'symbols can not be modified.'! !

!Symbol methodsFor: 'system primitives' stamp: 'di 1/2/1999 17:00'!
flushCache
	"Tell the interpreter to remove all entries with this symbol as a selector from its method lookup cache, if it has one.  This primitive must be called whenever a method is defined or removed.
	NOTE:  Only one of the two selective flush methods needs to be used.
	Squeak 2.3 and later uses 116 (See CompiledMethod flushCache)."

	<primitive: 119>
! !

!Symbol methodsFor: 'testing' stamp: 'md 1/20/2006 16:16'!
includesKey: sym
	^self == sym.! !

!Symbol methodsFor: 'testing' stamp: 'md 8/27/2005 16:33'!
isDoIt

	^ (self == #DoIt) or: [self == #DoItIn:].! !

!Symbol methodsFor: 'testing' stamp: 'sma 2/5/2000 12:32'!
isInfix
	"Answer whether the receiver is an infix message selector."

	^ self precedence == 2! !

!Symbol methodsFor: 'testing' stamp: 'sma 2/5/2000 12:34'!
isKeyword
	"Answer whether the receiver is a message keyword."

	^ self precedence == 3! !

!Symbol methodsFor: 'testing' stamp: 'di 4/25/2000 12:32'!
isLiteral
	"Answer whether the receiver is a valid Smalltalk literal."

	^ true! !

!Symbol methodsFor: 'printing' stamp: 'sw 8/19/1999 11:30'!
isOrientedFill
	"Needs to be implemented here because symbols can occupy 'color' slots of morphs."

	^ false! !

!Symbol methodsFor: 'testing' stamp: 'sma 2/5/2000 12:13'!
isPvtSelector
	"Answer whether the receiver is a private message selector, that is,
	begins with 'pvt' followed by an uppercase letter, e.g. pvtStringhash."

	^ (self beginsWith: 'pvt') and: [self size >= 4 and: [(self at: 4) isUppercase]]! !

!Symbol methodsFor: 'testing' stamp: 'md 4/30/2003 15:31'!
isSymbol
	^ true ! !

!Symbol methodsFor: 'testing' stamp: 'sma 2/5/2000 12:34'!
isUnary
	"Answer whether the receiver is an unary message selector."

	^ self precedence == 1! !

!Symbol methodsFor: 'system primitives' stamp: 'md 2/16/2006 17:17'!
numArgs: n
	"Answer a string that can be used as a selector with n arguments.
	 TODO: need to be extended to support shrinking and for selectors like #+ " 

	| selector numArgs aStream offs |
	
	selector := self.
	(numArgs := selector numArgs) >= n ifTrue: [^self].	
	aStream := WriteStream on: (String new: 16).
	aStream nextPutAll: self.
	
	(numArgs = 0) ifTrue: [aStream nextPutAll: ':'. offs := 0] ifFalse: [offs := 1].
	2 to: n - numArgs + offs do: [:i | aStream nextPutAll: 'with:'].	
	^aStream contents asSymbol
	
! !

!Symbol methodsFor: 'accessing' stamp: 'sma 2/5/2000 12:32'!
precedence
	"Answer the receiver's precedence, assuming it is a valid Smalltalk
	message selector or 0 otherwise.  The numbers are 1 for unary,
	2 for binary and 3 for keyword selectors."

	self size = 0 ifTrue: [^ 0].
	self first isLetter ifFalse: [^ 2].
	self last = $: ifTrue: [^ 3].
	^ 1! !

!Symbol methodsFor: 'accessing' stamp: ''!
replaceFrom: start to: stop with: replacement startingAt: repStart

	self errorNoModification! !

!Symbol methodsFor: 'copying' stamp: ''!
shallowCopy
	"Answer with the receiver, because Symbols are unique."! !

!Symbol methodsFor: 'printing' stamp: 'di 4/25/2000 12:32'!
storeOn: aStream 

	aStream nextPut: $#.
	(Scanner isLiteralSymbol: self)
		ifTrue: [aStream nextPutAll: self]
		ifFalse: [super storeOn: aStream]! !

!Symbol methodsFor: 'private' stamp: ''!
string: aString

	1 to: aString size do: [:j | super at: j put: (aString at: j)].
	^self  ! !

!Symbol methodsFor: 'evaluating' stamp: 'md 3/24/2006 12:09'!
value: anObject 
	^anObject perform: self.! !

!Symbol methodsFor: 'copying' stamp: 'tk 8/19/1998 16:05'!
veryDeepCopyWith: deepCopier
	"Return self.  I am immutable in the Morphic world.  Do not record me."! !

!Symbol methodsFor: 'converting' stamp: 'md 8/10/2004 10:54'!
withFirstCharacterDownshifted
	"Answer an object like the receiver but with first character downshifted if necesary"

	^self asString withFirstCharacterDownshifted asSymbol.! !

Symbol variableWordSubclass: #WideSymbol
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Strings'!

!WideSymbol commentStamp: '<historical>' prior: 0!
This class represents the symbols containing 32bit characters.!

!WideSymbol class methodsFor: 'class initialization' stamp: 'ar 4/10/2005 23:55'!
initialize
	Smalltalk removeFromShutDownList: self. "@@@ Remove this later @@@"! !

!WideSymbol methodsFor: 'accessing' stamp: 'ar 4/10/2005 23:38'!
at: index 
	"Answer the Character stored in the field of the receiver indexed by the argument."
	^ Character value: (self wordAt: index).
! !

!WideSymbol methodsFor: 'accessing' stamp: 'yo 8/30/2002 14:15'!
at: anInteger put: anObject 
	"You cannot modify the receiver."

	self errorNoModification
! !

!WideSymbol methodsFor: 'accessing' stamp: 'ar 4/10/2005 23:38'!
byteAt: index

	| d r |
	d := (index + 3) // 4.
	r := (index - 1) \\ 4 + 1.
	^ (self wordAt: d) digitAt: ((4 - r) + 1).
! !

!WideSymbol methodsFor: 'accessing' stamp: 'ar 4/10/2005 23:38'!
byteAt: index put: aByte
	self errorNoModification.! !

!WideSymbol methodsFor: 'accessing' stamp: 'ar 4/10/2005 23:38'!
byteSize

	^ self size * 4.
! !

!WideSymbol methodsFor: 'private' stamp: 'ar 4/12/2005 14:12'!
fixUponLoad: aProject seg: anImageSegment
	"We are in an old project that is being loaded from disk. 
	Fix up conventions that have changed."
	| ms |
	"Yoshiki did not put MultiSymbols into outPointers in older 
images!!
	When all old images are gone, remove this method."
	ms := Symbol intern: self asString.
	self == ms ifFalse: [
		"For a project from older m17n image, this is necessary."
		self becomeForward: ms.
		aProject projectParameters at: #MultiSymbolInWrongPlace put: true
	].

	"MultiString>>capitalized was not implemented 
correctly. 
	Fix eventual accessors and mutators here."
	((self beginsWith: 'get')
		and:[(self at: 4) asInteger < 256
		and:[(self at: 4) isLowercase]]) ifTrue:[
			ms := self asString.
			ms at: 4 put: (ms at: 4) asUppercase.
			ms := ms asSymbol.
			self becomeForward: ms.
			aProject projectParameters at: #MultiSymbolInWrongPlace put: true.
		].
	((self beginsWith: 'set')
		and:[(self at: 4) asInteger < 256
		and:[(self at: 4) isLowercase
		and:[self last = $:
		and:[(self occurrencesOf: $:) = 1]]]]) ifTrue:[
			ms := self asString.
			ms at: 4 put: (ms at: 4) asUppercase.
			ms := ms asSymbol.
			self becomeForward: ms.
			aProject projectParameters at: #MultiSymbolInWrongPlace put: true.
		].
	^ super fixUponLoad: aProject seg: anImageSegment	"me, 
not the label"
! !

!WideSymbol methodsFor: 'testing' stamp: 'ar 4/12/2005 19:52'!
isWideString
	"Answer whether the receiver is a WideString"
	^true! !

!WideSymbol methodsFor: 'private' stamp: 'yo 7/29/2005 21:53'!
mutateJISX0208StringToUnicode

	| c |
	1 to: self size do: [:i |
		c := self at: i.
		(c leadingChar = JISX0208 leadingChar or: [
			c leadingChar = (JISX0208 leadingChar bitShift: 2)]) ifTrue: [
			self basicAt: i put: (Character leadingChar: JapaneseEnvironment leadingChar code: (c asUnicode)) asciiValue.
		]
	].
! !

!WideSymbol methodsFor: 'private' stamp: 'ar 4/11/2005 00:09'!
pvtAt: index put: aCharacter
	"Primitive. Store the Character in the field of the receiver indicated by
	the index. Fail if the index is not an Integer or is out of bounds, or if
	the argument is not a Character. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 61>
	index isInteger
		ifTrue: [self errorSubscriptBounds: index]
		ifFalse: [self errorNonIntegerIndex]! !

!WideSymbol methodsFor: 'accessing' stamp: 'ar 4/12/2005 17:34'!
species
	"Answer the preferred class for reconstructing the receiver."
	^WideString
! !

!WideSymbol methodsFor: 'private' stamp: 'ar 4/10/2005 23:58'!
string: aString
	1 to: aString size do: [:j | self pvtAt: j put: (aString at: j) asInteger].
	^self! !

!WideSymbol methodsFor: 'accessing' stamp: 'ar 4/10/2005 23:38'!
wordAt: index
	<primitive: 60>
	^ (self basicAt: index).
! !

!WideSymbol methodsFor: 'accessing' stamp: 'ar 4/10/2005 23:39'!
wordAt: index put: anInteger
	self errorNoModification.! !

String variableWordSubclass: #WideString
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Strings'!

!WideString commentStamp: 'yo 10/19/2004 22:34' prior: 0!
This class represents the array of 32 bit wide characters.
!

!WideString class methodsFor: 'enumeration' stamp: 'yo 8/27/2003 07:01'!
allMethodsWithEncodingTag: encodingTag
	"Answer a SortedCollection of all the methods that implement the message 
	aSelector."

	| list adder num i |
	list := Set new.
	adder := [ :mrClass :mrSel |
		list add: (
			MethodReference new
				setStandardClass: mrClass
				methodSymbol: mrSel
		)
	].

	num := CompiledMethod allInstances size.
	i := 0.
	'processing...' displayProgressAt: Sensor cursorPoint from: 0 to: num during: [:bar |
		SystemNavigation new allBehaviorsDo: [ :class |
			class selectors do: [:s |
				bar value: (i := i + 1).				
				(self string: (class sourceCodeAt: s) asString hasEncoding: encodingTag) ifTrue: [
					adder value: class value: s.
				]
			]
		]
	].

	^ list.
! !

!WideString class methodsFor: 'enumeration' stamp: 'yo 8/12/2003 17:14'!
allMultiStringMethods  
	"Answer a SortedCollection of all the methods that implement the message 
	aSelector."

	| list adder num i |
	list := Set new.
	adder := [ :mrClass :mrSel |
		list add: (
			MethodReference new
				setStandardClass: mrClass
				methodSymbol: mrSel
		)
	].

	num := CompiledMethod allInstances size.
	i := 0.
	'processing...' displayProgressAt: Sensor cursorPoint from: 0 to: num during: [:bar |
		SystemNavigation new allBehaviorsDo: [ :class |
			class selectors do: [:s |
				bar value: (i := i + 1).				
				((class sourceCodeAt: s) asString isOctetString) ifFalse: [
					adder value: class value: s.
				]
			]
		]
	].

	^ list.
! !

!WideString class methodsFor: 'enumeration' stamp: 'yo 8/27/2003 07:00'!
allNonAsciiMethods  
	"Answer a SortedCollection of all the methods that implement the message 
	aSelector."

	| list adder num i |
	list := Set new.
	adder := [ :mrClass :mrSel |
		list add: (
			MethodReference new
				setStandardClass: mrClass
				methodSymbol: mrSel
		)
	].

	num := CompiledMethod allInstances size.
	i := 0.
	'processing...' displayProgressAt: Sensor cursorPoint from: 0 to: num during: [:bar |
		SystemNavigation new allBehaviorsDo: [ :class |
			class selectors do: [:s |
				bar value: (i := i + 1).				
				((class sourceCodeAt: s) asString isAsciiString) ifFalse: [
					adder value: class value: s.
				]
			]
		]
	].

	^ list.
! !

!WideString class methodsFor: 'instance creation' stamp: 'ar 4/12/2005 20:00'!
from: aString 

	| newString |
	(aString isMemberOf: self)
		ifTrue: [^ aString copy].
	newString := self new: aString size.
	1 to: aString size do: [:index | newString basicAt: index put: (aString basicAt: index)].
	^ newString
! !

!WideString class methodsFor: 'instance creation' stamp: 'ar 4/10/2005 19:38'!
fromByteArray: aByteArray 

	| inst |
	aByteArray size \\ 4 = 0 ifFalse: [^ ByteString fromByteArray: aByteArray ].
	inst := self new: aByteArray size // 4.
	4 to: aByteArray size by: 4 do: [:i |
		inst basicAt: i // 4
			put: ((aByteArray at: i - 3) << 24) + 
				((aByteArray at: i - 2) << 16) +
				 ((aByteArray at: i - 1) << 8) +
				(aByteArray at: i)
	].

	^ inst
! !

!WideString class methodsFor: 'instance creation' stamp: 'yo 8/30/2002 17:00'!
fromISO2022JPString: string 

	| tempFileName stream contents |
	tempFileName := Time millisecondClockValue printString , '.txt'.
	FileDirectory default deleteFileNamed: tempFileName ifAbsent: [].
	stream := StandardFileStream fileNamed: tempFileName.
	[stream nextPutAll: string]
		ensure: [stream close].
	stream := FileStream fileNamed: tempFileName.
	contents := stream contentsOfEntireFile.
	FileDirectory default deleteFileNamed: tempFileName ifAbsent: [].
	^ contents
! !

!WideString class methodsFor: 'instance creation' stamp: 'ar 4/12/2005 19:58'!
fromPacked: aLong
	"Convert from a longinteger to a String of length 4."

	| s val |
	s := self new: 1.
	val := (((aLong digitAt: 4) << 24) bitOr:((aLong digitAt: 3) << 16))
				bitOr: (((aLong digitAt: 2) << 8) bitOr: (aLong digitAt: 1)).
	s basicAt: 1 put: val.
	^ s.

"WideString fromPacked: 'TEXT' asPacked"
! !

!WideString class methodsFor: 'instance creation' stamp: 'yo 8/28/2002 13:39'!
fromString: aString 
	"Answer an instance of me that is a copy of the argument, aString."

	| inst |
	(aString isMemberOf: self) ifTrue: [
		^ aString copy.
	].
	inst := self new: aString size.
	1 to: aString size do: [:pos |
		inst basicAt: pos put: (aString basicAt: pos).
	].
	^ inst.
! !

!WideString methodsFor: 'converting' stamp: 'yo 8/28/2002 14:46'!
asFourCode

	| result |
	self size = 1 ifFalse: [^self error: 'must be exactly four octets'].
	result := self basicAt: 1.
	(result bitAnd: 16r80000000) = 0 
		ifFalse: [self error: 'cannot resolve fourcode'].
	(result bitAnd: 16r40000000) = 0 ifFalse: [^result - 16r80000000].
	^ result
! !

!WideString methodsFor: 'converting' stamp: 'yo 8/28/2002 14:47'!
asPacked

	self inject: 0 into: [:pack :next | pack := pack * 16r100000000 + next asInteger].
! !

!WideString methodsFor: 'accessing' stamp: 'ar 4/10/2005 17:14'!
at: index 
	"Answer the Character stored in the field of the receiver indexed by the argument."
	^ Character value: (self wordAt: index).
! !

!WideString methodsFor: 'accessing' stamp: 'ar 4/10/2005 17:14'!
at: index put: aCharacter 
	"Store the Character in the field of the receiver indicated by the index."
	aCharacter isCharacter ifFalse:[self errorImproperStore].
	self wordAt: index put: aCharacter asInteger.
! !

!WideString methodsFor: 'accessing' stamp: 'yo 10/31/2002 22:29'!
byteAt: index

	| d r |
	d := (index + 3) // 4.
	r := (index - 1) \\ 4 + 1.
	^ (self wordAt: d) digitAt: ((4 - r) + 1).
! !

!WideString methodsFor: 'accessing' stamp: 'yo 11/3/2002 13:19'!
byteAt: index put: aByte

	| d r w |
	d := (index + 3) // 4.
	r := (index - 1) \\ 4 + 1.
	w := (self wordAt: d) bitAnd: ((16rFF<<((4 - r)*8)) bitInvert32).
	w := w + (aByte<<((4 - r)*8)).
	self basicAt: d put: w.
	^ aByte.
! !

!WideString methodsFor: 'accessing' stamp: 'yo 8/27/2002 14:22'!
byteSize

	^ self size * 4.
! !

!WideString methodsFor: 'converting' stamp: 'yo 3/14/2005 11:41'!
copyFrom: start to: stop

	| n |
	n := super copyFrom: start to: stop.
	n isOctetString ifTrue: [^ n asOctetString].
	^ n.
! !

!WideString methodsFor: 'testing' stamp: 'yo 7/29/2003 14:10'!
includesUnifiedCharacter

	^ self isUnicodeStringWithCJK
! !

!WideString methodsFor: 'testing' stamp: 'ar 4/12/2005 14:10'!
isUnicodeStringWithCJK

	self do: [:c |
		(c isTraditionalDomestic not and: [Unicode isUnifiedKanji: c charCode]) ifTrue: [
			^ true
		].
	].

	^ false.
! !

!WideString methodsFor: 'testing' stamp: 'ar 4/12/2005 19:52'!
isWideString
	"Answer whether the receiver is a WideString"
	^true! !

!WideString methodsFor: 'private' stamp: 'ar 4/9/2005 22:31'!
mutateJISX0208StringToUnicode

	| c |
	1 to: self size do: [:i |
		c := self at: i.
		(c leadingChar = JISX0208 leadingChar or: [
			c leadingChar = (JISX0208 leadingChar bitShift: 2)]) ifTrue: [
			self basicAt: i put: (Character leadingChar: JapaneseEnvironment leadingChar code: (c asUnicode)) asciiValue.
		]
	].
! !

!WideString methodsFor: 'accessing' stamp: 'ar 4/12/2005 17:36'!
replaceFrom: start to: stop with: replacement startingAt: repStart 

	<primitive: 105>
	replacement class == String ifTrue: [
		^ self replaceFrom: start to: stop with: (replacement asWideString) startingAt: repStart.
	]. 

	^ super replaceFrom: start to: stop with: replacement startingAt: repStart.
! !

!WideString methodsFor: 'accessing' stamp: 'ar 4/10/2005 17:14'!
wordAt: index
	<primitive: 60>
	^ (self basicAt: index).
! !

!WideString methodsFor: 'accessing' stamp: 'ar 4/10/2005 17:14'!
wordAt: index put: anInteger
	<primitive: 61>
	self basicAt: index put: anInteger.
! !

ArrayedCollection subclass: #Text
	instanceVariableNames: 'string runs'
	classVariableNames: ''
	poolDictionaries: 'TextConstants'
	category: 'Collections-Text'!

!Text commentStamp: '<historical>' prior: 0!
I represent a character string that has been marked with abstract changes in character appearance. Actual display is performed in the presence of a TextStyle which indicates, for each abstract code, an actual font to be used.  A Text associates a set of TextAttributes with each character in its character string.  These attributes may be font numbers, emphases such as bold or italic, or hyperling actions.  Font numbers are interpreted relative to whatever textStyle appears, along with the text, in a Paragraph.  Since most characters have the same attributes as their neighbors, the attributes are stored in a RunArray for efficiency.  Each of my instances has
	string		a String
	runs		a RunArray!

!Text class methodsFor: 'private' stamp: 'di 10/31/97 11:22'!
addAttribute: att toArray: others 
	"Add a new text attribute to an existing set"
	"NOTE: The use of reset and set in this code is a specific
	hack for merging TextKerns."
	att reset.
	^ Array streamContents:
		[:strm | others do:
			[:other | (att dominates: other) ifFalse: [strm nextPut: other]].
		att set ifTrue: [strm nextPut: att]]! !

!Text class methodsFor: 'instance creation' stamp: 'sw 12/6/1999 14:14'!
fromString: aString 
	"Answer an instance of me whose characters are those of the argument, aString."

	^ self string: aString attribute: (TextFontChange fontNumber: TextStyle default defaultFontIndex)! !

!Text class methodsFor: 'instance creation' stamp: 'rbb 3/1/2005 11:18'!
fromUser
	"Answer an instance of me obtained by requesting the user to type a string."
	"Text fromUser"

	^ self fromString:
		(UIManager default request: 'Enter text followed by carriage return')
! !

!Text class methodsFor: 'class initialization' stamp: 'mir 8/3/2004 13:30'!
initTextConstants 
	"Initialize constants shared by classes associated with text display, e.g., 
	Space, Tab, Cr, Bs, ESC."
		"1/24/96 sw: in exasperation and confusion, changed cmd-g mapping from 231 to 232 to see if I could gain any relief?!!"


	| letter varAndValue tempArray width |
	"CtrlA..CtrlZ, Ctrla..Ctrlz"
	letter := $A.
 	#(		212 230 228 196 194 226 241 243 214 229 200 217 246 
			245 216 202 210 239 211 240 197 198 209 215 242 231
	 		1 166 228 132 130 12 232 179 150 165 136 153 182 
			14 15 138 17 18 19 11 21 134 145 151 178 167 ) do:
		[:kbd |
		TextConstants at: ('Ctrl', letter asSymbol) asSymbol put: kbd asCharacter.
		letter := letter == $Z ifTrue: [$a] ifFalse: [(letter asciiValue + 1) asCharacter]].

	varAndValue := #(
		Space	32
		Tab		9
		CR		13
		Enter	3
		BS		8
		BS2		158
		ESC		160
		Clear 	173
	).

	varAndValue size odd ifTrue: [self error: 'unpaired text constant'].
	(2 to: varAndValue size by: 2) do:
		[:i | TextConstants at: (varAndValue at: i - 1) put: (varAndValue at: i) asCharacter].

	varAndValue := #(
		CtrlDigits 			(159 144 143 128 127 129 131 180 149 135)
		CtrlOpenBrackets	(201 7 218 249 219 15)
			"lparen gottn by ctrl-:= = 201; should be 213 but can't type that on Mac"

			"location of non-character stop conditions"
		EndOfRun	257
		CrossedX	258

			"values for alignment"
		LeftFlush	0
		RightFlush	1
		Centered	2
		Justified	3

			"subscripts for a marginTabsArray tuple"
		LeftMarginTab	1
		RightMarginTab	2

			"font faces"
		Basal	0
		Bold	1
		Italic	2

			"in case font doesn't have a width for space character"
			"some plausible numbers-- are they the right ones?"
		DefaultSpace			4
		DefaultTab				24
		DefaultLineGrid			16
		DefaultBaseline			12
		DefaultFontFamilySize	3	"basal, bold, italic"
	).

	varAndValue size odd ifTrue: [self error: 'unpaired text constant'].
	(2 to: varAndValue size by: 2) do:
		[:i | TextConstants at: (varAndValue at: i - 1) put: (varAndValue at: i)].

	TextConstants at: #DefaultRule	put: Form over.
	TextConstants at: #DefaultMask	put: Color black.

	width := Display width max: 720.
	tempArray := Array new: width // DefaultTab.
	1 to: tempArray size do:
		[:i | tempArray at: i put: DefaultTab * i].
	TextConstants at: #DefaultTabsArray put: tempArray.
	tempArray := Array new: (width // DefaultTab) // 2.
	1 to: tempArray size do:
		[:i | tempArray at: i put: (Array with: (DefaultTab*i) with: (DefaultTab*i))].
	TextConstants at: #DefaultMarginTabsArray put: tempArray.

"Text initTextConstants "! !

!Text class methodsFor: 'class initialization' stamp: ''!
initialize	"Text initialize"
	"Initialize constants shared by classes associated with text display."

	TextConstants at: #CaretForm put:
				(Form extent: 16@5
					fromArray: #(2r001100e26 2r001100e26 2r011110e26 2r111111e26 2r110011e26)
					offset: -3@0).
	self initTextConstants! !

!Text class methodsFor: 'instance creation' stamp: ''!
new: stringSize

	^self fromString: (String new: stringSize)! !

!Text class methodsFor: 'instance creation' stamp: ''!
streamContents: blockWithArg 
	| stream |
	stream := TextStream on: (self new: 400).
	blockWithArg value: stream.
	^ stream contents! !

!Text class methodsFor: 'instance creation' stamp: ''!
string: aString attribute: att
	"Answer an instance of me whose characters are aString.
	att is a TextAttribute."

	^self string: aString attributes: (Array with: att)! !

!Text class methodsFor: 'instance creation' stamp: ''!
string: aString attributes: atts
	"Answer an instance of me whose characters are those of aString.
	atts is an array of TextAttributes."

	^self string: aString runs: (RunArray new: aString size withAll: atts)! !

!Text class methodsFor: 'instance creation' stamp: ''!
string: aString emphasis: emphasis
	"This is an old method that is mainly used by old applications"

	emphasis isNumber ifTrue:
		[self halt: 'Numeric emphasis is not supported in Squeak'.
		"But if you proceed, we will do our best to give you what you want..."
		^ self string: aString runs: (RunArray new: aString size withAll: 
			(Array with: (TextFontChange new fontNumber: emphasis)))].
	^ self string: aString attributes: emphasis! !

!Text class methodsFor: 'private' stamp: ''!
string: aString runs: anArray
 
	^self basicNew setString: aString setRuns: anArray! !

!Text methodsFor: 'comparing' stamp: 'tk 10/19/2001 17:48'!
= other
	"Am I equal to the other Text or String?  
	***** Warning ***** Two Texts are considered equal if they have the same characters in them.  They might have completely different emphasis, fonts, sizes, text actions, or embedded morphs.  If you need to find out if one is a true copy of the other, you must do (text1 = text2 and: [text1 runs = text2 runs])."

	other isText ifTrue:	["This is designed to run fast even for megabytes"
				^ string == other string or: [string = other string]].
	other isString ifTrue: [^ string == other or: [string = other]].
	^ false! !

!Text methodsFor: 'emphasis' stamp: ''!
addAttribute: att 
	^ self addAttribute: att from: 1 to: self size! !

!Text methodsFor: 'emphasis' stamp: ''!
addAttribute: att from: start to: stop 
	"Set the attribute for characters in the interval start to stop."
	runs :=  runs copyReplaceFrom: start to: stop
			with: ((runs copyFrom: start to: stop)
				mapValues:
				[:attributes | Text addAttribute: att toArray: attributes])
! !

!Text methodsFor: 'emphasis' stamp: 'ar 12/17/2001 23:48'!
alignmentAt: characterIndex ifAbsent: aBlock
	| attributes emph |
	self size = 0 ifTrue: [^aBlock value].
	emph := nil.
	attributes := runs at: characterIndex.
	attributes do:[:att | (att isKindOf: TextAlignment) ifTrue:[emph := att]].
	^ emph ifNil: aBlock ifNotNil:[emph alignment]! !

!Text methodsFor: 'emphasis' stamp: ''!
allBold 
	"Force this whole text to be bold."
	string size = 0 ifTrue: [^self].
	self makeBoldFrom: 1 to: string size! !

!Text methodsFor: 'accessing' stamp: 'tk 9/4/2000 16:04'!
append: stringOrText

	self replaceFrom: string size + 1
				to: string size with: stringOrText! !

!Text methodsFor: 'converting' stamp: ''!
asDisplayText
	"Answer a DisplayText whose text is the receiver."

	^DisplayText text: self! !

!Text methodsFor: 'converting' stamp: ''!
asNumber
	"Answer the number created by interpreting the receiver as the textual 
	representation of a number."

	^string asNumber! !

!Text methodsFor: 'converting' stamp: 'ar 4/12/2005 17:32'!
asOctetStringText

	string class == WideString ifTrue: [
		^ self class string: string asOctetString runs: self runs copy.
	].
	^self.
! !

!Text methodsFor: 'converting' stamp: ''!
asParagraph
	"Answer a Paragraph whose text is the receiver."

	^Paragraph withText: self! !

!Text methodsFor: 'converting' stamp: ''!
asString
	"Answer a String representation of the textual receiver."

	^string! !

!Text methodsFor: 'converting' stamp: 'RAA 5/28/2001 06:19'!
asStringOrText	
	"Answer the receiver itself."

	^self! !

!Text methodsFor: 'converting' stamp: ''!
asText	
	"Answer the receiver itself."

	^self! !

!Text methodsFor: 'converting' stamp: 'ls 7/14/1998 03:17'!
asUrl
	^self asString asUrl! !

!Text methodsFor: 'converting' stamp: 'ls 7/14/1998 03:20'!
asUrlRelativeTo: aUrl
	^self asString asUrlRelativeTo: aUrl! !

!Text methodsFor: 'attributes' stamp: 'rbb 2/18/2005 09:14'!
askIfAddStyle: priorMethod req: requestor
	"Ask the user if we have a complex style (i.e. bold) for the first time"
	| tell answ old |
	(Preferences browseWithPrettyPrint and: [Preferences colorWhenPrettyPrinting])
		ifTrue: [self couldDeriveFromPrettyPrinting ifTrue: [^ self asString]].
	self runs coalesce.
	self unembellished ifTrue: [^ self asString].
	priorMethod ifNotNil: [old := priorMethod getSourceFromFile].
	(old == nil or: [old unembellished])
		ifTrue:
			[tell := 'This method contains style for the first time (e.g. bold or colored text).
Do you really want to save the style info?'.
			answ := (UIManager default 
						chooseFrom: #('Save method with style' 'Save method simply')
						title: tell).
			answ = 2 ifTrue: [^ self asString]]! !

!Text methodsFor: 'accessing' stamp: ''!
at: index

	^string at: index! !

!Text methodsFor: 'accessing' stamp: ''!
at: index put: character

	^string at: index put: character! !

!Text methodsFor: 'emphasis' stamp: 'sw 12/7/1999 12:30'!
attributesAt: characterIndex 
	"Answer the code for characters in the run beginning at characterIndex."
	"NB: no senders any more (supplanted by #attributesAt:forStyle: but retained for the moment in order not to break user code that may exist somewhere that still calls this"
	| attributes |
	self size = 0
		ifTrue: [^ Array with: (TextFontChange new fontNumber: 1)].  "null text tolerates access"
	attributes := runs at: characterIndex.
	^ attributes! !

!Text methodsFor: 'emphasis' stamp: 'ar 12/17/2001 01:17'!
attributesAt: characterIndex do: aBlock
	"Answer the code for characters in the run beginning at characterIndex."
	"NB: no senders any more (supplanted by #attributesAt:forStyle: but retained for the moment in order not to break user code that may exist somewhere that still calls this"
	self size = 0 ifTrue:[^self].
	(runs at: characterIndex) do: aBlock! !

!Text methodsFor: 'emphasis' stamp: 'sw 12/7/1999 11:32'!
attributesAt: characterIndex forStyle: aTextStyle
	"Answer the code for characters in the run beginning at characterIndex."
	| attributes |
	self size = 0
		ifTrue: [^ Array with: (TextFontChange new fontNumber: aTextStyle defaultFontIndex)].  "null text tolerates access"
	attributes := runs at: characterIndex.
	^ attributes! !

!Text methodsFor: 'attributes' stamp: 'tk 11/1/2001 14:37'!
basicType
	"Answer a symbol representing the inherent type I hold"

	"Number String Boolean player collection sound color etc"
	^ #Text! !

!Text methodsFor: 'html' stamp: 'dgd 9/1/2005 13:05'!
closeHtmlAttributes: anArray on: aStream 
	anArray
		do: [:each | each closeHtmlOn: aStream].! !

!Text methodsFor: 'copying' stamp: ''!
copy

	^ self class new setString: string copy setRuns: runs copy
! !

!Text methodsFor: 'copying' stamp: ''!
copyFrom: start to: stop 
	"Answer a copied subrange of the receiver."

	| realStart realStop |
	stop > self size
		ifTrue: [realStop := self size]		"handle selection at end of string"
		ifFalse: [realStop := stop].
	start < 1
		ifTrue: [realStart := 1]			"handle selection before start of string"
		ifFalse: [realStart := start].
	^Text 
		string: (string copyFrom: realStart to: realStop)
		runs: (runs copyFrom: realStart to: realStop)! !

!Text methodsFor: 'copying' stamp: 'BG 6/12/2003 13:11'!
copyReplaceFrom: start to: stop with: aTextOrString

	| txt |
	txt := aTextOrString asText.	"might be a string"
	^self class 
             string: (string copyReplaceFrom: start to: stop with: txt string)
             runs: (runs copyReplaceFrom: start to: stop with: txt runs)
! !

!Text methodsFor: 'copying' stamp: 'tk 1/7/98 10:58'!
copyReplaceTokens: oldSubstring with: newSubstring 
	"Replace all occurrences of oldSubstring that are surrounded
	by non-alphanumeric characters"
	^ self copyReplaceAll: oldSubstring with: newSubstring asTokens: true
	"'File asFile Files File''s File' copyReplaceTokens: 'File' with: 'Snick'"! !

!Text methodsFor: 'attributes' stamp: 'sw 11/16/1999 22:33'!
couldDeriveFromPrettyPrinting
	"Return true if the receiver has any TextAttributes that are functional rather than simply appearance-related"
	runs values do:
		[:emphArray | emphArray do:
			[:emph | emph couldDeriveFromPrettyPrinting ifFalse: [^ false]]].
	^ true! !

!Text methodsFor: 'copying' stamp: 'di 11/9/97 17:13'!
deepCopy

	^ self copy "Both string and runs are assumed to be read-only"! !

!Text methodsFor: 'accessing' stamp: 'gm 2/15/2003 14:59'!
embeddedMorphs
	"return the list of morphs embedded in me"

	| morphs |
	morphs := IdentitySet new.
	runs withStartStopAndValueDo: 
			[:start :stop :attribs | 
			attribs 
				do: [:attrib | attrib anchoredMorph ifNotNil: [morphs add: attrib anchoredMorph]]].
	^morphs select: [:m | m isMorph]! !

!Text methodsFor: 'accessing' stamp: 'gm 2/15/2003 14:59'!
embeddedMorphsFrom: start to: stop 
	"return the list of morphs embedded in me"

	| morphs |
	morphs := IdentitySet new.
	runs 
		runsFrom: start
		to: stop
		do: 
			[:attribs | 
			attribs 
				do: [:attr | attr anchoredMorph ifNotNil: [morphs add: attr anchoredMorph]]].
	^morphs select: [:m | m isMorph]! !

!Text methodsFor: 'emphasis' stamp: 'di 4/1/1999 15:17'!
emphasisAt: characterIndex
	"Answer the fontfor characters in the run beginning at characterIndex."
	| attributes emph |
	self size = 0 ifTrue: [^ 0].	"null text tolerates access"
	emph := 0.
	attributes := runs at: characterIndex.
	attributes do: 
		[:att | emph := emph bitOr: att emphasisCode].
	^ emph
	! !

!Text methodsFor: 'emphasis' stamp: 'di 11/10/97 13:36'!
find: attribute
	"Return the first interval over which this attribute applies"
	| begin end |
	begin := 0.
	runs withStartStopAndValueDo:
		[:start :stop :attributes |
		(attributes includes: attribute)
			ifTrue: [begin = 0 ifTrue: [begin := start].
					end := stop]
			ifFalse: [begin > 0 ifTrue: [^ begin to: end]]].
	begin > 0 ifTrue: [^ begin to: end].
	^ nil! !

!Text methodsFor: 'accessing' stamp: ''!
findString: aString startingAt: start 
	"Answer the index of subString within the receiver, starting at index 
	start. If the receiver does not contain subString, answer 0."

	^string findString: aString asString startingAt: start! !

!Text methodsFor: 'accessing' stamp: 'di 11/23/1998 11:53'!
findString: aString startingAt: start caseSensitive: caseSensitive
	"Answer the index of subString within the receiver, starting at index 
	start. If the receiver does not contain subString, answer 0."

	^string findString: aString asString startingAt: start caseSensitive: caseSensitive! !

!Text methodsFor: 'emphasis' stamp: 'sw 12/7/1999 10:58'!
fontAt: characterIndex withStyle: aTextStyle
	"Answer the fontfor characters in the run beginning at characterIndex."
	| attributes font |
	self size = 0 ifTrue: [^ aTextStyle defaultFont].	"null text tolerates access"
	attributes := runs at: characterIndex.
	font := aTextStyle defaultFont.  "default"
	attributes do: 
		[:att | att forFontInStyle: aTextStyle do: [:f | font := f]].
	^ font! !

!Text methodsFor: 'emphasis' stamp: ''!
fontNumberAt: characterIndex 
	"Answer the fontNumber for characters in the run beginning at characterIndex."
	| attributes fontNumber |
	self size = 0 ifTrue: [^1].	"null text tolerates access"
	attributes := runs at: characterIndex.
	fontNumber := 1.
	attributes do: [:att | (att isMemberOf: TextFontChange) ifTrue: [fontNumber := att fontNumber]].
	^ fontNumber
	! !

!Text methodsFor: 'comparing' stamp: 'tk 10/17/2001 14:12'!
hash
	"#hash is implemented, because #= is implemented.  We are now equal to a string with the same characters.  Hash must reflect that."

	^ string hash! !

!Text methodsFor: 'comparing' stamp: 'tk 9/6/2000 11:59'!
howManyMatch: aString

	^ self string howManyMatch: aString! !

!Text methodsFor: 'comparing' stamp: ''!
isText
	^ true! !

!Text methodsFor: 'converting' stamp: 'ar 4/9/2005 22:16'!
isoToSqueak
	^self "no longer needed"! !

!Text methodsFor: 'accessing' stamp: 'tk 9/6/2000 12:33'!
lineCount

	^ string lineCount! !

!Text methodsFor: 'converting' stamp: 'ar 4/10/2005 15:58'!
macToSqueak
	"Convert the receiver from MacRoman to Squeak encoding"
	^ self class new setString: string macToSqueak setRuns: runs copy! !

!Text methodsFor: 'emphasis' stamp: ''!
makeBoldFrom: start to: stop

	^ self addAttribute: TextEmphasis bold from: start to: stop! !

!Text methodsFor: 'emphasis' stamp: 'md 1/20/2006 17:11'!
makeSelectorBold
	"For formatting Smalltalk source code, set the emphasis of that portion of 
	the receiver's string that parses as a message selector to be bold."

	| parser i |
	string size = 0 ifTrue: [^ self].
	i := 0.
	[(string at: (i := i + 1)) isSeparator] whileTrue.
	(string at: i) = $[ ifTrue: [^ self].  "block, no selector"
	[(parser := Compiler parserClass new) parseSelector: string] on: Error do: [^ self].
	self makeBoldFrom: 1 to: (parser endOfLastToken min: string size)! !

!Text methodsFor: 'emphasis' stamp: 'sma 2/5/2000 12:03'!
makeSelectorBoldIn: aClass
	"For formatting Smalltalk source code, set the emphasis of that portion of 
	the receiver's string that parses as a message selector to be bold."

	| parser |
	string size = 0 ifTrue: [^self].
	(parser := aClass parserClass new) parseSelector: string.
	self makeBoldFrom: 1 to: (parser endOfLastToken min: string size)! !

!Text methodsFor: 'html' stamp: 'dgd 9/1/2005 13:05'!
openHtmlAttributes: anArray on: aStream 
	anArray
		do: [:each | each openHtmlOn: aStream ]! !

!Text methodsFor: 'accessing' stamp: 'ar 12/27/2001 00:03'!
prepend: stringOrText

	self replaceFrom: 1 to: 0 with: stringOrText! !

!Text methodsFor: 'html' stamp: 'dgd 9/2/2005 12:20'!
printHtmlOn: aStream 
	self runs
		withStartStopAndValueDo: [:start :stop :attributes | 
			| att str | 
			att := self attributesAt: start.
			str := self string copyFrom: start to: stop.
			""
			self openHtmlAttributes: att on: aStream.
			self printStringHtml: str on: aStream.

			self closeHtmlAttributes: att on: aStream]! !

!Text methodsFor: 'html' stamp: 'dgd 9/2/2005 12:40'!
printHtmlString
	"answer a string whose characters are the html representation 
	of the receiver"
	| html |
	html := String new writeStream.
	self printHtmlOn: html.
	^ html contents! !

!Text methodsFor: 'printing' stamp: 'sma 6/1/2000 09:49'!
printOn: aStream
	self printNameOn: aStream.
	aStream nextPutAll: ' for '; print: string! !

!Text methodsFor: 'html' stamp: 'dgd 9/2/2005 12:30'!
printStringHtml: aString on: aStream 
	| html |
	html := aString.
	""
	html := html copyReplaceAll: '&' with: '&amp;'.
	html := html copyReplaceAll: '>' with: '&gt;'.
	html := html copyReplaceAll: '<' with: '&lt;'.
	""
	html := html copyReplaceAll: '¬¨¬®¬¨¬é¬¨¬®¬¨¦Ö' with: '&aacute;'.
	html := html copyReplaceAll: '¬¨¬®¬¨¬é¬¨¬®¬¨¬©' with: '&eacute;'.
	html := html copyReplaceAll: '¬¨¬®¬¨¬é¬¨¬®¬¶¦ë' with: '&iacute;'.
	html := html copyReplaceAll: '¬¨¬®¬¨¬é¬¨¬®¬¶¦ü' with: '&oacute;'.
	html := html copyReplaceAll: '¬¨¬®¬¨¬é¬¨¬®¬¶¬ö' with: '&uacute;'.
	html := html copyReplaceAll: '¬¨¬®¬¨¬é¬¨¬®¬¨¬±' with: '&ntilde;'.
	""
	html := html copyReplaceAll: '¬¨¬®¬¨¬é¬¨¬Ž¬¶¦±' with: '&Aacute;'.
	html := html copyReplaceAll: '¬¨¬®¬¨¬é¬¨¬Ž¬¨¬¢' with: '&Eacute;'.
	html := html copyReplaceAll: '¬¨¬®¬¨¬é¬¨¬Ž¬¶¦º' with: '&Iacute;'.
	html := html copyReplaceAll: '¬¨¬®¬¨¬é¬¨¬Ž¬¨¬Æ' with: '&Oacute;'.
	html := html copyReplaceAll: '¬¨¬®¬¨¬é¬¨¬Ž¬¨¦©' with: '&Uacute;'.
	html := html copyReplaceAll: '¬¨¬®¬¨¬é¬¨¬Ž¬¨¬·' with: '&Ntilde;'.
	""
	html := html copyReplaceAll: '
' with: '<br>
'.
	html := html copyReplaceAll: '	' with: '&nbsp;&nbsp;&nbsp;&nbsp;'.
	""
	aStream nextPutAll: html! !

!Text methodsFor: 'accessing' stamp: 'BG 6/8/2003 16:18'!
rangeOf: attribute startingAt: index
"Answer an interval that gives the range of attribute at index position  index. An empty interval with start value index is returned when the attribute is not present at position index.  "
   ^string size = 0
      ifTrue: [index to: index - 1]
	 ifFalse: [runs rangeOf: attribute startingAt: index]! !

!Text methodsFor: 'emphasis' stamp: ''!
removeAttribute: att from: start to: stop 
	"Remove the attribute over the interval start to stop."
	runs :=  runs copyReplaceFrom: start to: stop
			with: ((runs copyFrom: start to: stop)
				mapValues:
				[:attributes | attributes copyWithout: att])
! !

!Text methodsFor: 'converting' stamp: 'ar 12/17/2001 00:38'!
removeAttributesThat: removalBlock replaceAttributesThat: replaceBlock by: convertBlock
	"Enumerate all attributes in the receiver. Remove those passing removalBlock and replace those passing replaceBlock after converting it through convertBlock"
	| added removed new |
	"Deliberately optimized for the no-op default."
	added := removed := nil.
	runs withStartStopAndValueDo: [ :start :stop :attribs | 
		attribs do: [ :attrib |
			(removalBlock value: attrib) ifTrue:[
				removed ifNil:[removed := WriteStream on: #()].
				removed nextPut: {start. stop. attrib}.
			] ifFalse:[
				(replaceBlock value: attrib) ifTrue:[
					removed ifNil:[removed := WriteStream on: #()].
					removed nextPut: {start. stop. attrib}.
					new := convertBlock value: attrib.
					added ifNil:[added := WriteStream on: #()].
					added nextPut: {start. stop. new}.
				].
			].
		].
	].
	(added == nil and:[removed == nil]) ifTrue:[^self].
	"otherwise do the real work"
	removed ifNotNil:[removed contents do:[:spec|
		self removeAttribute: spec last from: spec first to: spec second]].
	added ifNotNil:[added contents do:[:spec|
		self addAttribute: spec last from: spec first to: spec second]].! !

!Text methodsFor: 'accessing' stamp: 'tk 12/30/97 07:17'!
replaceFrom: start to: stop with: aText

	| txt |
	txt := aText asText.	"might be a string"
	string := string copyReplaceFrom: start to: stop with: txt string.
	runs := runs copyReplaceFrom: start to: stop with: txt runs! !

!Text methodsFor: 'converting' stamp: 'dvf 10/1/2003 02:58'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"This destructively replaces elements from start to stop in the receiver starting at index, repStart, in replacementCollection. Do it to both the string and the runs."

	| rep newRepRuns |
	rep := replacement asText.	"might be a string"
	string replaceFrom: start to: stop with: rep string startingAt: repStart.
	newRepRuns := rep runs copyFrom: repStart to: repStart + stop - start.
	runs := runs copyReplaceFrom: start to: stop with: newRepRuns! !

!Text methodsFor: 'converting' stamp: 'BG 6/8/2003 16:38'!
reversed

	"Answer a copy of the receiver with element order reversed."

	^ self class string: string reversed runs: runs reversed.

  "  It is assumed that  self size = runs size  holds. "! !

!Text methodsFor: 'emphasis' stamp: ''!
runLengthFor: characterIndex 
	"Answer the count of characters remaining in run beginning with 
	characterIndex."

	^runs runLengthAt: characterIndex! !

!Text methodsFor: 'private' stamp: ''!
runs

	^runs! !

!Text methodsFor: 'accessing' stamp: 'tween 9/13/2004 10:07'!
runs: anArray

	runs := anArray! !

!Text methodsFor: 'private' stamp: ''!
setString: aString setRuns: anArray

	string := aString.
	runs := anArray! !

!Text methodsFor: 'private' stamp: 'tk 12/16/97 14:14'!
setString: aString setRunsChecking: aRunArray
	"Check runs and do the best you can to make them fit..."

	string := aString.
	"check the runs"
	aRunArray ifNil: [^ aString asText].
	(aRunArray isKindOf: RunArray) ifFalse: [^ aString asText].
	aRunArray runs size = aRunArray values size ifFalse: [^ aString asText].
	(aRunArray values includes: #()) ifTrue: [^ aString asText].	"not allowed?"
	aRunArray size = aString size ifFalse: [^ aString asText].
	
	runs := aRunArray.! !

!Text methodsFor: 'accessing' stamp: ''!
size

	^string size! !

!Text methodsFor: 'converting' stamp: 'ar 4/9/2005 22:16'!
squeakToIso
	^self "no longer needed"! !

!Text methodsFor: 'converting' stamp: 'ar 4/10/2005 15:56'!
squeakToMac
	"Convert the receiver from Squeak to MacRoman encoding"
	^ self class new setString: string squeakToMac setRuns: runs copy! !

!Text methodsFor: 'printing' stamp: ''!
storeOn: aStream

	aStream nextPutAll: '(Text string: ';
		store: string;
		nextPutAll: ' runs: ';
		store: runs;
		nextPut: $)! !

!Text methodsFor: 'accessing' stamp: ''!
string
	"Answer the string representation of the receiver."

	^string! !

!Text methodsFor: 'attributes' stamp: 'sw 12/7/1999 12:31'!
unembellished 
	"Return true if the only emphases are the default font and bold"
	| font1 bold |
	font1 := TextFontChange defaultFontChange.
	bold := TextEmphasis bold.
	Preferences ignoreStyleIfOnlyBold ifFalse:
		["Ignore font1 only or font1-bold followed by font1-plain"
		^ (runs values = (Array with: (Array with: font1)))
		or: [runs values = (Array with: (Array with: font1 with: bold)
 								with: (Array with: font1))]].

	"If preference is set, then ignore any combo of font1 and bold"
	runs withStartStopAndValueDo:
		[:start :stop :emphArray |
		emphArray do:
			[:emph | (font1 = emph or: [bold = emph]) ifFalse: [^ false]]].
	^ true! !

!Text methodsFor: 'converting' stamp: 'nk 9/16/2003 16:46'!
withSqueakLineEndings
	"Answer a copy of myself in which all sequences of <CR><LF> or <LF> have been changed to <CR>"
	| newText |
	(string includes: Character lf) ifFalse: [ ^self copy ].
	newText := self copyReplaceAll: String crlf with: String cr asTokens: false.
	(newText asString includes: Character lf) ifFalse: [ ^newText ].
	^newText copyReplaceAll: String lf with: String cr asTokens: false.! !

ArrayedCollection variableWordSubclass: #WordArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

!WordArray commentStamp: '<historical>' prior: 0!
WordArrays store 32-bit unsigned Integer values.
!

!WordArray class methodsFor: 'as yet unclassified' stamp: 'RAA 5/17/2001 16:07'!
bobsTest
	| wa s1 s2 wa2 answer rawData |
"
WordArray bobsTest
"
	answer := OrderedCollection new.
	wa := WordArray with: 16r01020304 with: 16r05060708.
	{false. true} do: [ :pad |
		0 to: 3 do: [ :skip |
			s1 := RWBinaryOrTextStream on: ByteArray new.

			s1 next: skip put: 0.		"start at varying positions"
			wa writeOn: s1.
			pad ifTrue: [s1 next: 4-skip put: 0].	"force length to be multiple of 4"

			rawData := s1 contents.
			s2 := RWBinaryOrTextStream with: rawData.
			s2 reset.
			s2 skip: skip.			"get to beginning of object"
			wa2 := WordArray newFromStream: s2.
			answer add: {
				rawData size. 
				skip. 
				wa2 = wa. 
				wa2 asArray collect: [ :each | each radix: 16]
			}
		].
	].
	^answer explore! !

!WordArray methodsFor: 'array arithmetic' stamp: 'yo 10/25/2004 15:52'!
* other

	| result |
	other isNumber ifTrue: [
		other isFloat ifTrue: [
			result := KedamaFloatArray new: self size.
			^ self primMulScalar: self and: other into: result.
		] ifFalse: [
			result := WordArray new: self size.
			^ self primMulScalar: self and: other into: result.
		].
	].
	(other isMemberOf: WordArray) ifTrue: [	
		result := WordArray new: self size.
		^ self primMulArray: self and: other into: result.
	].
	(other isMemberOf: KedamaFloatArray) ifTrue: [	
		result := KedamaFloatArray new: self size.
		^ self primMulArray: self and: other into: result.
	].
	^ super * other.
! !

!WordArray methodsFor: 'array arithmetic' stamp: 'yo 10/25/2004 14:58'!
+ other

	| result |
	other isNumber ifTrue: [
		other isFloat ifTrue: [
			result := KedamaFloatArray new: self size.
			^ self primAddScalar: self and: other into: result.
		] ifFalse: [
			result := WordArray new: self size.
			^ self primAddScalar: self and: other into: result.
		].
	].
	(other isMemberOf: WordArray) ifTrue: [	
		result := WordArray new: self size.
		^ self primAddArray: self and: other into: result.
	].
	(other isMemberOf: KedamaFloatArray) ifTrue: [	
		result := KedamaFloatArray new: self size.
		^ self primAddArray: self and: other into: result.
	].
	^ super + other.
! !

!WordArray methodsFor: 'array arithmetic' stamp: 'yo 10/25/2004 15:52'!
- other

	| result |
	other isNumber ifTrue: [
		other isFloat ifTrue: [
			result := KedamaFloatArray new: self size.
			^ self primSubScalar: self and: other into: result.
		] ifFalse: [
			result := WordArray new: self size.
			^ self primSubScalar: self and: other into: result.
		].
	].
	(other isMemberOf: WordArray) ifTrue: [	
		result := WordArray new: self size.
		^ self primSubArray: self and: other into: result.
	].
	(other isMemberOf: KedamaFloatArray) ifTrue: [	
		result := KedamaFloatArray new: self size.
		^ self primSubArray: self and: other into: result.
	].
	^ super - other.
! !

!WordArray methodsFor: 'array arithmetic' stamp: 'yo 10/25/2004 15:53'!
/ other

	| result |
	other isNumber ifTrue: [
		other isFloat ifTrue: [
			result := KedamaFloatArray new: self size.
			^ self primDivScalar: self and: other into: result.
		] ifFalse: [
			result := WordArray new: self size.
			^ self primDivScalar: self and: other into: result.
		].
	].
	(other isMemberOf: WordArray) ifTrue: [	
		result := WordArray new: self size.
		^ self primDivArray: self and: other into: result.
	].
	(other isMemberOf: KedamaFloatArray) ifTrue: [	
		result := KedamaFloatArray new: self size.
		^ self primDivArray: self and: other into: result.
	].
	^ super / other.
! !

!WordArray methodsFor: 'converting' stamp: 'ar 9/14/1998 23:46'!
asWordArray
	^self! !

!WordArray methodsFor: 'accessing' stamp: 'sma 4/22/2000 17:47'!
atAllPut: value
	"Fill the receiver with the given value"

	<primitive: 145>
	super atAllPut: value! !

!WordArray methodsFor: 'accessing' stamp: 'ar 3/3/2001 16:18'!
byteSize
	^self size * 4! !

!WordArray methodsFor: 'accessing' stamp: 'tk 3/13/2000 14:46'!
bytesPerElement
	"Number of bytes in each item.  This multiplied by (self size)*8 gives the number of bits stored."
	^ 4! !

!WordArray methodsFor: 'accessing' stamp: 'ar 11/2/1998 12:19'!
defaultElement
	"Return the default element of the receiver"
	^0! !

!WordArray methodsFor: 'array arithmetic primitives' stamp: 'yo 10/25/2004 15:50'!
primAddArray: rcvr and: other into: result

	<primitive: 'primitiveAddArrays' module:'KedamaPlugin'>
	"^ KedamaPlugin doPrimitive: #primitiveAddArrays."

	1 to: rcvr size do: [:i |
		result at: i put: (rcvr at: i) + (other at: i)
	].
	^ result.
! !

!WordArray methodsFor: 'array arithmetic primitives' stamp: 'yo 10/25/2004 15:10'!
primAddScalar: rcvr and: other into: result

	<primitive: 'primitiveAddScalar' module:'KedamaPlugin'>
	"^ KedamaPlugin doPrimitive: #primitiveAddScalar."

	1 to: rcvr size do: [:i |
		result at: i put: (rcvr at: i) + other.
	].
	^ result.
! !

!WordArray methodsFor: 'array arithmetic primitives' stamp: 'yo 10/25/2004 15:51'!
primDivArray: rcvr and: other into: result

	<primitive: 'primitiveDivArrays' module:'KedamaPlugin'>
	"^ KedamaPlugin doPrimitive: #primitiveDivArrays."

	1 to: rcvr size do: [:i |
		result at: i put: (rcvr at: i) / (other at: i)
	].
	^ result.
! !

!WordArray methodsFor: 'array arithmetic primitives' stamp: 'yo 10/25/2004 15:49'!
primDivScalar: rcvr and: other into: result

	<primitive: 'primitiveDivScalar' module:'KedamaPlugin'>
	"^ KedamaPlugin doPrimitive: #primitiveDivScalar."

	1 to: rcvr size do: [:i |
		result at: i put: (rcvr at: i) / other.
	].
	^ result.
! !

!WordArray methodsFor: 'array arithmetic primitives' stamp: 'yo 10/25/2004 15:51'!
primMulArray: rcvr and: other into: result

	<primitive: 'primitiveMulArrays' module:'KedamaPlugin'>
	"^ KedamaPlugin doPrimitive: #primitiveMulArrays."

	1 to: rcvr size do: [:i |
		result at: i put: (rcvr at: i) * (other at: i)
	].
	^ result.
! !

!WordArray methodsFor: 'array arithmetic primitives' stamp: 'yo 10/25/2004 15:49'!
primMulScalar: rcvr and: other into: result

	<primitive: 'primitiveMulScalar' module:'KedamaPlugin'>
	"^ KedamaPlugin doPrimitive: #primitiveMulScalar."

	1 to: rcvr size do: [:i |
		result at: i put: (rcvr at: i) * other.
	].
	^ result.
! !

!WordArray methodsFor: 'array arithmetic primitives' stamp: 'yo 10/25/2004 15:50'!
primSubArray: rcvr and: other into: result

	<primitive: 'primitiveSubArrays' module:'KedamaPlugin'>
	"^ KedamaPlugin doPrimitive: #primitiveSubArrays."

	1 to: rcvr size do: [:i |
		result at: i put: (rcvr at: i) - (other at: i)
	].
	^ result.
! !

!WordArray methodsFor: 'array arithmetic primitives' stamp: 'yo 10/25/2004 15:49'!
primSubScalar: rcvr and: other into: result

	<primitive: 'primitiveSubScalar' module:'KedamaPlugin'>
	"^ KedamaPlugin doPrimitive: #primitiveSubScalar."

	1 to: rcvr size do: [:i |
		result at: i put: (rcvr at: i) - other.
	].
	^ result.
! !

!WordArray methodsFor: 'private' stamp: 'ar 2/15/1999 00:51'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	<primitive: 105>
	^super replaceFrom: start to: stop with: replacement startingAt: repStart ! !

WordArray variableWordSubclass: #WordArrayForSegment
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

!WordArrayForSegment methodsFor: 'as yet unclassified' stamp: 'tk 1/24/2000 23:22'!
restoreEndianness
	"This word object was just read in from a stream.  Do not correct the Endianness because the load primitive will reverse bytes as needed."

	"^ self"
! !

!WordArrayForSegment methodsFor: 'as yet unclassified' stamp: 'tk 1/24/2000 23:22'!
writeOn: aByteStream
	"Write quickly and disregard the endianness of the words.  Store the array of bits onto the argument, aStream.  (leading byte ~= 16r80) identifies this as raw bits (uncompressed)."

	aByteStream nextInt32Put: self size.	"4 bytes"
	aByteStream nextPutAll: self
! !

SequenceableCollection subclass: #Heap
	instanceVariableNames: 'array tally sortBlock indexUpdateBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Sequenceable'!

!Heap commentStamp: '<historical>' prior: 0!
Class Heap implements a special data structure commonly referred to as 'heap'. Heaps are more efficient than SortedCollections if:
a) Elements are only removed at the beginning
b) Elements are added with arbitrary sort order.
The sort time for a heap is O(n log n) in all cases.

Instance variables:
	array		<Array>		The data repository
	tally		<Integer>	The number of elements in the heap
	sortBlock	<Block|nil>	A two-argument block defining the sort order,
							or nil in which case the default sort order is
								[:element1 :element2| element1 <= element2]
	indexUpdateBlock 	<Block|nil> 
							A two-argument block of the form [:data :index | ... ]
							which allows an application object to keep track of its
							index within the heap.  Useful for quick heap update
							when object's sort value changes (for example, when an
							object in a priority queue has its priority increased
							by an external event, you don't want to have to search
							through the whole heap to find the index before fixing
							the heap).  No update occurs if nil.!

!Heap class methodsFor: 'instance creation' stamp: 'ar 7/1/1999 04:20'!
new
	^self new: 10! !

!Heap class methodsFor: 'instance creation' stamp: 'ar 7/1/1999 04:20'!
new: n
	^super new setCollection: (Array new: n)! !

!Heap class methodsFor: 'instance creation' stamp: 'ar 9/10/1999 14:13'!
sortBlock: aBlock
	"Create a new heap sorted by the given block"
	^self new sortBlock: aBlock! !

!Heap class methodsFor: 'instance creation' stamp: 'ar 9/10/1999 13:23'!
withAll: aCollection
	"Create a new heap with all the elements from aCollection"
	^(self basicNew)
		setCollection: aCollection asArray copy tally: aCollection size;
		reSort;
		yourself! !

!Heap class methodsFor: 'instance creation' stamp: 'ar 5/23/2001 17:22'!
withAll: aCollection sortBlock: sortBlock
	"Create a new heap with all the elements from aCollection"
	^(self basicNew)
		setCollection: aCollection asArray copy tally: aCollection size;
		sortBlock: sortBlock;
		yourself! !

!Heap methodsFor: 'comparing' stamp: 'rhi 8/14/2003 10:05'!
= anObject

	^ self == anObject
		ifTrue: [true]
		ifFalse: [anObject isHeap
			ifTrue: [sortBlock = anObject sortBlock and: [super = anObject]]
			ifFalse: [super = anObject]]! !

!Heap methodsFor: 'adding' stamp: 'jcg 3/8/2003 02:07'!
add: anObject
	"Include newObject as one of the receiver's elements. Answer newObject."
	tally = array size ifTrue:[self grow].
	array at: (tally := tally + 1) put: anObject.
	self updateObjectIndex: tally.
	self upHeap: tally.
	^anObject! !

!Heap methodsFor: 'private' stamp: 'ar 7/1/1999 04:19'!
array
	^array! !

!Heap methodsFor: 'accessing' stamp: 'ar 9/10/1999 13:02'!
at: index
	"Return the element at the given position within the receiver"
	(index < 1 or:[index > tally]) ifTrue:[^self errorSubscriptBounds: index].
	^array at: index! !

!Heap methodsFor: 'accessing' stamp: 'ar 7/1/1999 04:14'!
at: index put: newObject
	"Heaps are accessed with #add: not #at:put:"
	^self shouldNotImplement! !

!Heap methodsFor: 'enumerating' stamp: 'ar 9/10/1999 13:05'!
do: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument."
	1 to: tally do:[:i| aBlock value: (array at: i)]! !

!Heap methodsFor: 'private-heap' stamp: 'jcg 3/8/2003 02:11'!
downHeap: anIndex
	"Check the heap downwards for correctness starting at anIndex.
	 Everything above (i.e. left of) anIndex is ok."
	| value k n j |
	anIndex = 0 ifTrue:[^self].
	n := tally bitShift: -1.
	k := anIndex.
	value := array at: anIndex.
	[k <= n] whileTrue:[
		j := k + k.
		"use max(j,j+1)"
		(j < tally and:[self sorts: (array at: j+1) before: (array at: j)])
				ifTrue:[ j := j + 1].
		"check if position k is ok"
		(self sorts: value before: (array at: j)) 
			ifTrue:[	"yes -> break loop"
					n := k - 1]
			ifFalse:[	"no -> make room at j by moving j-th element to k-th position"
					array at: k put: (array at: j).
					self updateObjectIndex: k.
					"and try again with j"
					k := j]].
	array at: k put: value.
	self updateObjectIndex: k.! !

!Heap methodsFor: 'private-heap' stamp: 'jcg 3/8/2003 02:11'!
downHeapSingle: anIndex
	"This version is optimized for the case when only one element in the receiver can be at a wrong position. It avoids one comparison at each node when travelling down the heap and checks the heap upwards after the element is at a bottom position. Since the probability for being at the bottom of the heap is much larger than for being somewhere in the middle this version should be faster."
	| value k n j |
	anIndex = 0 ifTrue:[^self].
	n := tally bitShift: -1.
	k := anIndex.
	value := array at: anIndex.
	[k <= n] whileTrue:[
		j := k + k.
		"use max(j,j+1)"
		(j < tally and:[self sorts: (array at: j+1) before: (array at: j)])
				ifTrue:[	j := j + 1].
		array at: k put: (array at: j).
		self updateObjectIndex: k.
		"and try again with j"
		k := j].
	array at: k put: value.
	self updateObjectIndex: k.
	self upHeap: k! !

!Heap methodsFor: 'accessing' stamp: 'md 1/19/2006 09:56'!
first
	"Return the first element in the receiver"
	^array at: 1! !

!Heap methodsFor: 'growing' stamp: 'ar 7/1/1999 04:17'!
grow
	"Become larger."
	self growTo: self size + self growSize.! !

!Heap methodsFor: 'growing' stamp: 'ar 7/1/1999 04:18'!
growSize
	"Return the size by which the receiver should grow if there are no empty slots left."
	^array size max: 5! !

!Heap methodsFor: 'growing' stamp: 'ar 7/1/1999 04:18'!
growTo: newSize
	"Grow to the requested size."
	| newArray |
	newArray := Array new: (newSize max: tally).
	newArray replaceFrom: 1 to: array size with: array startingAt: 1.
	array := newArray! !

!Heap methodsFor: 'accessing' stamp: 'jcg 3/8/2003 02:00'!
indexUpdateBlock: aBlockOrNil

	indexUpdateBlock := aBlockOrNil.
	indexUpdateBlock fixTemps.
! !

!Heap methodsFor: 'testing' stamp: 'ar 9/10/1999 13:03'!
isEmpty
	"Answer whether the receiver contains any elements."
	^tally = 0! !

!Heap methodsFor: 'testing' stamp: 'rhi 8/14/2003 08:51'!
isHeap

	^ true! !

!Heap methodsFor: 'private' stamp: 'ar 9/15/2000 17:12'!
privateRemoveAt: index
	"Remove the element at the given index and make sure the sorting order is okay"
	| removed |
	removed := array at: index.
	array at: index put: (array at: tally).
	array at: tally put: nil.
	tally := tally - 1.
	index > tally ifFalse:[
		"Use #downHeapSingle: since only one element has been removed"
		self downHeapSingle: index].
	^removed! !

!Heap methodsFor: 'accessing' stamp: 'ar 9/10/1999 14:08'!
reSort
	"Resort the entire heap"
	self isEmpty ifTrue:[^self].
	tally // 2 to: 1 by: -1 do:[:i| self downHeap: i].! !

!Heap methodsFor: 'removing' stamp: 'ar 9/10/1999 13:04'!
remove: oldObject ifAbsent: aBlock
	"Remove oldObject as one of the receiver's elements. If several of the 
	elements are equal to oldObject, only one is removed. If no element is 
	equal to oldObject, answer the result of evaluating anExceptionBlock. 
	Otherwise, answer the argument, oldObject."
	1 to: tally do:[:i| 
		(array at: i) = oldObject ifTrue:[^self privateRemoveAt: i]].
	^aBlock value! !

!Heap methodsFor: 'removing' stamp: 'ar 9/10/1999 13:05'!
removeAt: index
	"Remove the element at given position"
	(index < 1 or:[index > tally]) ifTrue:[^self errorSubscriptBounds: index].
	^self privateRemoveAt: index! !

!Heap methodsFor: 'removing' stamp: 'ar 9/10/1999 13:05'!
removeFirst
	"Remove the first element from the receiver"
	^self removeAt: 1! !

!Heap methodsFor: 'private' stamp: 'ar 7/1/1999 04:35'!
setCollection: aCollection
	array := aCollection.
	tally := 0.! !

!Heap methodsFor: 'private' stamp: 'ar 9/10/1999 13:18'!
setCollection: aCollection tally: newTally
	array := aCollection.
	tally := newTally.! !

!Heap methodsFor: 'accessing' stamp: 'sma 5/12/2000 11:37'!
size
	"Answer how many elements the receiver contains."

	^ tally! !

!Heap methodsFor: 'accessing' stamp: 'ar 7/1/1999 04:21'!
sortBlock
	^sortBlock! !

!Heap methodsFor: 'accessing' stamp: 'ar 7/1/1999 04:21'!
sortBlock: aBlock
	sortBlock := aBlock.
	sortBlock fixTemps.
	self reSort.! !

!Heap methodsFor: 'testing' stamp: 'ar 9/10/1999 13:03'!
sorts: element1 before: element2
	"Return true if element1 should be sorted before element2.
	This method defines the sort order in the receiver"
	^sortBlock == nil
		ifTrue:[element1 <= element2]
		ifFalse:[sortBlock value: element1 value: element2].! !

!Heap methodsFor: 'private' stamp: 'sma 4/22/2000 19:30'!
species
	^ Array! !

!Heap methodsFor: 'growing' stamp: 'ar 7/1/1999 04:18'!
trim
	"Remove any empty slots in the receiver."
	self growTo: self size.! !

!Heap methodsFor: 'private-heap' stamp: 'jcg 3/8/2003 02:12'!
upHeap: anIndex
	"Check the heap upwards for correctness starting at anIndex.
	 Everything below anIndex is ok."
	| value k kDiv2 tmp |
	anIndex = 0 ifTrue:[^self].
	k := anIndex.
	value := array at: anIndex.
	[ (k > 1) and:[self sorts: value before: (tmp := array at: (kDiv2 := k bitShift: -1))] ] 
		whileTrue:[
			array at: k put: tmp.
			self updateObjectIndex: k.
			k := kDiv2].
	array at: k put: value.
	self updateObjectIndex: k.! !

!Heap methodsFor: 'private' stamp: 'jcg 3/8/2003 02:08'!
updateObjectIndex: index
	"If indexUpdateBlock is not nil, notify the object at index of its new position in the heap array."
	indexUpdateBlock ifNotNil: [
		indexUpdateBlock value: (array at: index) value: index]! !

SequenceableCollection subclass: #Interval
	instanceVariableNames: 'start stop step'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Sequenceable'!

!Interval commentStamp: '<historical>' prior: 0!
I represent a finite arithmetic progression.!

!Interval class methodsFor: 'instance creation' stamp: ''!
from: startInteger to: stopInteger 
	"Answer an instance of me, starting at startNumber, ending at 
	stopNumber, and with an interval increment of 1."

	^self new
		setFrom: startInteger
		to: stopInteger
		by: 1! !

!Interval class methodsFor: 'instance creation' stamp: ''!
from: startInteger to: stopInteger by: stepInteger 
	"Answer an instance of me, starting at startNumber, ending at 
	stopNumber, and with an interval increment of stepNumber."

	^self new
		setFrom: startInteger
		to: stopInteger
		by: stepInteger! !

!Interval class methodsFor: 'instance creation' stamp: ''!
new
	"Primitive. Create and answer with a new instance of the receiver
	(a class) with no indexable fields. Fail if the class is indexable. Override
	SequenceableCollection new. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 70>
	self isVariable ifTrue: [ ^ self new: 0 ].
	"space must be low"
	Smalltalk signalLowSpace.
	^ self new  "retry if user proceeds"
! !

!Interval class methodsFor: 'instance creation' stamp: 'md 1/14/2004 11:42'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."

    | newInterval n |

    (n := aCollection size) <= 1 ifTrue: [
		n = 0 ifTrue: [^self from: 1 to: 0].
		^self from: aCollection first to: aCollection last].
    	newInterval := self from: aCollection first to: aCollection last
	by: (aCollection last - aCollection first) // (n - 1).
	aCollection ~= newInterval
		ifTrue: [self error: 'The argument is not an arithmetic progression'].
	^newInterval

"	Interval newFrom: {1. 2. 3}
	{33. 5. -23} as: Interval
	{33. 5. -22} as: Interval    (an error)
	(-4 to: -12 by: -1) as: Interval
"! !

!Interval methodsFor: 'arithmetic' stamp: 'ajh 3/13/2003 15:45'!
+ number

	^ start + number to: stop + number by: step! !

!Interval methodsFor: 'arithmetic' stamp: 'ajh 3/13/2003 15:46'!
- number

	^ start - number to: stop - number by: step! !

!Interval methodsFor: 'comparing' stamp: 'rhi 8/14/2003 10:08'!
= anObject

	^ self == anObject
		ifTrue: [true]
		ifFalse: [anObject isInterval
			ifTrue: [start = anObject first
				and: [step = anObject increment
					and: [self last = anObject last]]]
			ifFalse: [super = anObject]]! !

!Interval methodsFor: 'adding' stamp: ''!
add: newObject 
	"Adding to an Interval is not allowed."

	self shouldNotImplement! !

!Interval methodsFor: 'accessing' stamp: ''!
at: anInteger 
	"Answer the anInteger'th element."

	(anInteger >= 1 and: [anInteger <= self size])
		ifTrue: [^start + (step * (anInteger - 1))]
		ifFalse: [self errorSubscriptBounds: anInteger]! !

!Interval methodsFor: 'accessing' stamp: ''!
at: anInteger put: anObject 
	"Storing into an Interval is not allowed."

	self error: 'you can not store into an interval'! !

!Interval methodsFor: 'enumerating' stamp: ''!
collect: aBlock
	| nextValue result |
	result := self species new: self size.
	nextValue := start.
	1 to: result size do:
		[:i |
		result at: i put: (aBlock value: nextValue).
		nextValue := nextValue + step].
	^ result! !

!Interval methodsFor: 'copying' stamp: ''!
copy
	"Return a copy of me. Override the superclass because my species is
	Array and copy, as inherited from SequenceableCollection, uses
	copyFrom:to:, which creates a new object of my species."

	^self shallowCopy! !

!Interval methodsFor: 'enumerating' stamp: ''!
do: aBlock

	| aValue |
	aValue := start.
	step < 0
		ifTrue: [[stop <= aValue]
				whileTrue: 
					[aBlock value: aValue.
					aValue := aValue + step]]
		ifFalse: [[stop >= aValue]
				whileTrue: 
					[aBlock value: aValue.
					aValue := aValue + step]]! !

!Interval methodsFor: 'accessing' stamp: 'stp 8/19/2000 23:52'!
extent 
	"Answer the max - min of the receiver interval."
	"(10 to: 50) extent"

	^stop - start! !

!Interval methodsFor: 'accessing' stamp: ''!
first 
	"Refer to the comment in SequenceableCollection|first."

	^start! !

!Interval methodsFor: 'comparing' stamp: ''!
hash
	"Hash is reimplemented because = is implemented."

	^(((start hash bitShift: 2)
		bitOr: stop hash)
		bitShift: 1)
		bitOr: self size! !

!Interval methodsFor: 'comparing' stamp: ''!
hashMappedBy: map
	"My hash is independent of my oop."

	^self hash! !

!Interval methodsFor: 'accessing' stamp: 'rpj 11/30/1999 11:04'!
includes: aNumber
	"Determine if aNumber is an element of this interval."
	^ (self rangeIncludes: aNumber) and: [ self valuesInclude: aNumber ]! !

!Interval methodsFor: 'accessing' stamp: ''!
increment
	"Answer the receiver's interval increment."

	^step! !

!Interval methodsFor: 'accessing' stamp: 'BG 6/15/2004 22:58'!
indexOf: anElement startingAt: startIndex ifAbsent: exceptionBlock
   "  startIndex is an positive integer, the collection index where the search is started. "
   " during the computation of  val , floats are only used when the receiver contains floats "

   | index val |
   (self rangeIncludes: anElement) 
      ifFalse: [^0]. 
  	val _ (anElement - self first)  / self increment.
	val fractionPart abs * 100000000 < step abs
	  ifFalse: [^0]
	  ifTrue: [index := val rounded + 1].
   " finally, the value of  startIndex  comes into play: "
   ^index < startIndex
      ifTrue: [0]
      ifFalse: [index].! !

!Interval methodsFor: 'testing' stamp: 'rhi 8/12/2003 09:52'!
isInterval

	^ true! !

!Interval methodsFor: 'self evaluating' stamp: 'sd 7/31/2005 22:03'!
isSelfEvaluating
	^ self class == Interval! !

!Interval methodsFor: 'accessing' stamp: ''!
last 
	"Refer to the comment in SequenceableCollection|last."

	^stop - (stop - start \\ step)! !

!Interval methodsFor: 'enumerating' stamp: 'dtl 5/31/2003 16:45'!
permutationsDo: aBlock
	"Repeatly value aBlock with a single copy of the receiver. Reorder the copy
	so that aBlock is presented all (self size factorial) possible permutations."
	"(1 to: 4) permutationsDo: [:each | Transcript cr; show: each printString]"

	self asArray permutationsDo: aBlock
! !

!Interval methodsFor: 'printing' stamp: 'sma 6/1/2000 09:50'!
printOn: aStream
	aStream nextPut: $(;
	 print: start;
	 nextPutAll: ' to: ';
	 print: stop.
	step ~= 1 ifTrue: [aStream nextPutAll: ' by: '; print: step].
	aStream nextPut: $)! !

!Interval methodsFor: 'accessing' stamp: 'di 12/6/1999 11:00'!
rangeIncludes: aNumber
	"Return true if the number lies in the interval between start and stop."

	step >= 0
		ifTrue: [^ aNumber between: start and: stop]
		ifFalse: [^ aNumber between: stop and: start]
! !

!Interval methodsFor: 'removing' stamp: ''!
remove: newObject 
	"Removing from an Interval is not allowed."

	self error: 'elements cannot be removed from an Interval'! !

!Interval methodsFor: 'enumerating' stamp: 'apb 4/22/2007 12:38'!
reverseDo: aBlock 
	"Evaluate aBlock for each element of my interval, in reverse order."
	| aValue |
	aValue := self last.
	step < 0
		ifTrue: [[start >= aValue]
				whileTrue: [aBlock value: aValue.
					aValue := aValue - step]]
		ifFalse: [[start <= aValue]
				whileTrue: [aBlock value: aValue.
					aValue := aValue - step]]! !

!Interval methodsFor: 'private' stamp: ''!
setFrom: startInteger to: stopInteger by: stepInteger

	start := startInteger.
	stop := stopInteger.
	step := stepInteger! !

!Interval methodsFor: 'copying' stamp: 'sma 3/3/2000 13:18'!
shallowCopy
	"Without this method, #copy would return an array instead of a new interval.
	The whole problem is burried in the class hierarchy and every fix will worsen
	the problem, so once the whole issue is resolved one should come back to this 
	method fix it."

	^ self class from: start to: stop by: step! !

!Interval methodsFor: 'accessing' stamp: 'sma 5/12/2000 11:38'!
size
	"Answer how many elements the receiver contains."

	step < 0
		ifTrue: [start < stop
				ifTrue: [^ 0]
				ifFalse: [^ stop - start // step + 1]]
		ifFalse: [stop < start
				ifTrue: [^ 0]
				ifFalse: [^ stop - start // step + 1]]! !

!Interval methodsFor: 'private' stamp: ''!
species

	^Array! !

!Interval methodsFor: 'printing' stamp: ''!
storeOn: aStream 
	"This is possible because we know numbers store and print the same."

	self printOn: aStream! !

!Interval methodsFor: 'private' stamp: 'di 4/24/2000 13:56'!
valuesInclude: aNumber
	"Private - answer whether or not aNumber is one of the enumerated values in this interval."

	| val |
	val := (aNumber - self first) asFloat / self increment.
	^ val fractionPart abs < (step * 1.0e-10)! !

SequenceableCollection subclass: #LinkedList
	instanceVariableNames: 'firstLink lastLink'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Sequenceable'!

!LinkedList commentStamp: '<historical>' prior: 0!
I represent a collection of links, which are containers for other objects. Using the message sequence addFirst:/removeLast causes the receiver to behave as a stack; using addLast:/removeFirst causes the receiver to behave as a queue.!

!LinkedList methodsFor: 'adding' stamp: ''!
add: aLink 
	"Add aLink to the end of the receiver's list. Answer aLink."

	^self addLast: aLink! !

!LinkedList methodsFor: 'adding' stamp: 'md 10/13/2004 13:50'!
add: link after: otherLink

	"Add otherLink  after link in the list. Answer aLink."

	| savedLink |

	savedLink := otherLink nextLink.
	otherLink nextLink: link.
	link nextLink:  savedLink.
	^link.! !

!LinkedList methodsFor: 'adding' stamp: 'ajh 8/22/2002 14:17'!
add: link before: otherLink

	| aLink |
	firstLink == otherLink ifTrue: [^ self addFirst: link].
	aLink := firstLink.
	[aLink == nil] whileFalse: [
		aLink nextLink == otherLink ifTrue: [
			link nextLink: aLink nextLink.
			aLink nextLink: link.
			^ link
		].
		 aLink := aLink nextLink.
	].
	^ self errorNotFound: otherLink! !

!LinkedList methodsFor: 'adding' stamp: ''!
addFirst: aLink 
	"Add aLink to the beginning of the receiver's list. Answer aLink."

	self isEmpty ifTrue: [lastLink := aLink].
	aLink nextLink: firstLink.
	firstLink := aLink.
	^aLink! !

!LinkedList methodsFor: 'adding' stamp: ''!
addLast: aLink 
	"Add aLink to the end of the receiver's list. Answer aLink."

	self isEmpty
		ifTrue: [firstLink := aLink]
		ifFalse: [lastLink nextLink: aLink].
	lastLink := aLink.
	^aLink! !

!LinkedList methodsFor: 'accessing' stamp: 'ajh 8/6/2002 15:46'!
at: index

	| i |
	i := 0.
	self do: [:link |
		(i := i + 1) = index ifTrue: [^ link]].
	^ self errorSubscriptBounds: index! !

!LinkedList methodsFor: 'enumerating' stamp: ''!
do: aBlock

	| aLink |
	aLink := firstLink.
	[aLink == nil] whileFalse:
		[aBlock value: aLink.
		 aLink := aLink nextLink]! !

!LinkedList methodsFor: 'accessing' stamp: ''!
first
	"Answer the first link. Create an error notification if the receiver is 
	empty."

	self emptyCheck.
	^firstLink! !

!LinkedList methodsFor: 'testing' stamp: ''!
isEmpty

	^firstLink == nil! !

!LinkedList methodsFor: 'accessing' stamp: ''!
last
	"Answer the last link. Create an error notification if the receiver is 
	empty."

	self emptyCheck.
	^lastLink! !

!LinkedList methodsFor: 'removing' stamp: ''!
remove: aLink ifAbsent: aBlock  
	"Remove aLink from the receiver. If it is not there, answer the result of
	evaluating aBlock."

	| tempLink |
	aLink == firstLink
		ifTrue: [firstLink := aLink nextLink.
				aLink == lastLink
					ifTrue: [lastLink := nil]]
		ifFalse: [tempLink := firstLink.
				[tempLink == nil ifTrue: [^aBlock value].
				 tempLink nextLink == aLink]
					whileFalse: [tempLink := tempLink nextLink].
				tempLink nextLink: aLink nextLink.
				aLink == lastLink
					ifTrue: [lastLink := tempLink]].
	aLink nextLink: nil.
	^aLink! !

!LinkedList methodsFor: 'removing' stamp: ''!
removeFirst
	"Remove the first element and answer it. If the receiver is empty, create 
	an error notification."

	| oldLink |
	self emptyCheck.
	oldLink := firstLink.
	firstLink == lastLink
		ifTrue: [firstLink := nil. lastLink := nil]
		ifFalse: [firstLink := oldLink nextLink].
	oldLink nextLink: nil.
	^oldLink! !

!LinkedList methodsFor: 'removing' stamp: ''!
removeLast
	"Remove the receiver's last element and answer it. If the receiver is 
	empty, create an error notification."

	| oldLink aLink |
	self emptyCheck.
	oldLink := lastLink.
	firstLink == lastLink
		ifTrue: [firstLink := nil. lastLink := nil]
		ifFalse: [aLink := firstLink.
				[aLink nextLink == oldLink] whileFalse:
					[aLink := aLink nextLink].
				 aLink nextLink: nil.
				 lastLink := aLink].
	oldLink nextLink: nil.
	^oldLink! !

!LinkedList methodsFor: 'enumerating' stamp: 'ajh 8/6/2002 16:39'!
species

	^ Array! !

SequenceableCollection subclass: #OrderedCollection
	instanceVariableNames: 'array firstIndex lastIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Sequenceable'!

!OrderedCollection commentStamp: '<historical>' prior: 0!
I represent a collection of objects ordered by the collector.!

!OrderedCollection class methodsFor: 'instance creation' stamp: 'sma 5/12/2000 17:41'!
new
	^ self new: 10! !

!OrderedCollection class methodsFor: 'instance creation' stamp: 'md 7/5/2005 15:16'!
new: anInteger 
	^ super basicNew setCollection: (Array new: anInteger)! !

!OrderedCollection class methodsFor: 'instance creation' stamp: ''!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."

	| newCollection |
	newCollection := self new: aCollection size.
	newCollection addAll: aCollection.
	^newCollection

"	OrderedCollection newFrom: {1. 2. 3}
	{1. 2. 3} as: OrderedCollection
	{4. 2. 7} as: SortedCollection
"! !

!OrderedCollection class methodsFor: 'instance creation' stamp: 'apb 10/15/2000 22:02'!
ofSize: n
	"Create a new collection of size n with nil as its elements.
	This method exists because OrderedCollection new: n creates an
	empty collection,  not one of size n."
	| collection |
	collection := self new: n.
	collection setContents: (collection collector).
	^ collection
! !

!OrderedCollection methodsFor: 'adding' stamp: ''!
add: newObject

	^self addLast: newObject! !

!OrderedCollection methodsFor: 'adding' stamp: ''!
add: newObject after: oldObject 
	"Add the argument, newObject, as an element of the receiver. Put it in 
	the sequence just succeeding oldObject. Answer newObject."
	
	| index |
	index := self find: oldObject.
	self insert: newObject before: index + 1.
	^newObject! !

!OrderedCollection methodsFor: 'adding' stamp: 'di 3/15/1999 14:01'!
add: newObject afterIndex: index 
	"Add the argument, newObject, as an element of the receiver. Put it in 
	the sequence just after index. Answer newObject."

	self insert: newObject before: firstIndex + index.
	^ newObject! !

!OrderedCollection methodsFor: 'adding' stamp: ''!
add: newObject before: oldObject 
	"Add the argument, newObject, as an element of the receiver. Put it in 
	the sequence just preceding oldObject. Answer newObject."
	
	| index |
	index := self find: oldObject.
	self insert: newObject before: index.
	^newObject! !

!OrderedCollection methodsFor: 'adding' stamp: 'BG 1/9/2004 12:30'!
add: newObject beforeIndex: index 
 "Add the argument, newObject, as an element of the receiver. Put it in 
 the sequence just before index. Answer newObject."

 self add: newObject afterIndex: index - 1.
 ^ newObject! !

!OrderedCollection methodsFor: 'adding' stamp: 'sma 5/12/2000 11:26'!
addAll: aCollection 
	"Add each element of aCollection at my end. Answer	aCollection."

	^ self addAllLast: aCollection! !

!OrderedCollection methodsFor: 'adding' stamp: ''!
addAllFirst: anOrderedCollection 
	"Add each element of anOrderedCollection at the beginning of the 
	receiver. Answer anOrderedCollection."

	anOrderedCollection reverseDo: [:each | self addFirst: each].
	^anOrderedCollection! !

!OrderedCollection methodsFor: 'adding' stamp: 'sw 3/1/2001 11:03'!
addAllFirstUnlessAlreadyPresent: anOrderedCollection 
	"Add each element of anOrderedCollection at the beginning of the receiver, preserving the order, but do not add any items that are already in the receiver.  Answer anOrderedCollection."

	anOrderedCollection reverseDo:
		[:each | (self includes: each) ifFalse: [self addFirst: each]].
	^ anOrderedCollection! !

!OrderedCollection methodsFor: 'adding' stamp: ''!
addAllLast: anOrderedCollection 
	"Add each element of anOrderedCollection at the end of the receiver. 
	Answer anOrderedCollection."

	anOrderedCollection do: [:each | self addLast: each].
	^anOrderedCollection! !

!OrderedCollection methodsFor: 'adding' stamp: ''!
addFirst: newObject 
	"Add newObject to the beginning of the receiver. Answer newObject."

	firstIndex = 1 ifTrue: [self makeRoomAtFirst].
	firstIndex := firstIndex - 1.
	array at: firstIndex put: newObject.
	^ newObject! !

!OrderedCollection methodsFor: 'adding' stamp: ''!
addLast: newObject 
	"Add newObject to the end of the receiver. Answer newObject."

	lastIndex = array size ifTrue: [self makeRoomAtLast].
	lastIndex := lastIndex + 1.
	array at: lastIndex put: newObject.
	^ newObject! !

!OrderedCollection methodsFor: 'accessing' stamp: ''!
at: anInteger 
	"Answer my element at index anInteger. at: is used by a knowledgeable
	client to access an existing element"

	(anInteger < 1 or: [anInteger + firstIndex - 1 > lastIndex])
		ifTrue: [self errorNoSuchElement]
		ifFalse: [^ array at: anInteger + firstIndex - 1]! !

!OrderedCollection methodsFor: 'adding' stamp: 'ajh 5/22/2003 12:03'!
at: index ifAbsentPut: block
	"Return value at index, however, if value does not exist (nil or out of bounds) then add block's value at index (growing self if necessary)"

	| v |
	index <= self size ifTrue: [
		^ (v := self at: index)
			ifNotNil: [v]
			ifNil: [self at: index put: block value]
	].
	[self size < index] whileTrue: [self add: nil].
	^ self at: index put: block value! !

!OrderedCollection methodsFor: 'accessing' stamp: ''!
at: anInteger put: anObject 
	"Put anObject at element index anInteger. at:put: cannot be used to
	append, front or back, to an ordered collection; it is used by a
	knowledgeable client to replace an element."

	| index |
	index := anInteger asInteger.
	(index < 1 or: [index + firstIndex - 1 > lastIndex])
		ifTrue: [self errorNoSuchElement]
		ifFalse: [^array at: index + firstIndex - 1 put: anObject]! !

!OrderedCollection methodsFor: 'accessing' stamp: 'sma 5/12/2000 11:42'!
capacity
	"Answer the current capacity of the receiver."

	^ array size! !

!OrderedCollection methodsFor: 'enumerating' stamp: 'sma 2/5/2000 15:22'!
collect: aBlock 
	"Evaluate aBlock with each of my elements as the argument. Collect the 
	resulting values into a collection that is like me. Answer the new 
	collection. Override superclass in order to use addLast:, not at:put:."

	| newCollection |
	newCollection := self species new: self size.
	firstIndex to: lastIndex do:
		[:index |
		newCollection addLast: (aBlock value: (array at: index))].
	^ newCollection! !

!OrderedCollection methodsFor: 'enumerating' stamp: 'bf 5/18/2000 17:34'!
collect: aBlock from: fromIndex to: toIndex
	"Override superclass in order to use addLast:, not at:put:."
	| result |
	(fromIndex < 1 or:[toIndex + firstIndex - 1 > lastIndex])
		ifTrue: [^self errorNoSuchElement].
	result := self species new: toIndex - fromIndex + 1.
	firstIndex + fromIndex - 1 to: firstIndex + toIndex - 1 do:
		[:index | result addLast: (aBlock value: (array at: index))].
	^ result
! !

!OrderedCollection methodsFor: 'private' stamp: ''!
collector  "Private"
	^ array! !

!OrderedCollection methodsFor: 'copying' stamp: ''!
copyEmpty
	"Answer a copy of the receiver that contains no elements."

	^self species new! !

!OrderedCollection methodsFor: 'copying' stamp: 'di 12/12/2000 10:15'!
copyFrom: startIndex to: endIndex 
	"Answer a copy of the receiver that contains elements from position
	startIndex to endIndex."

	| targetCollection |
	endIndex < startIndex ifTrue: [^self species new: 0].
	targetCollection := self species new: endIndex + 1 - startIndex.
	startIndex to: endIndex do: [:index | targetCollection addLast: (self at: index)].
	^ targetCollection! !

!OrderedCollection methodsFor: 'copying' stamp: ''!
copyReplaceFrom: start to: stop with: replacementCollection 
	"Answer a copy of the receiver with replacementCollection's elements in
	place of the receiver's start'th to stop'th elements. This does not expect
	a 1-1 map from replacementCollection to the start to stop elements, so it
	will do an insert or append."

	| newOrderedCollection delta startIndex stopIndex |
	"if start is less than 1, ignore stop and assume this is inserting at the front. 
	if start greater than self size, ignore stop and assume this is appending. 
	otherwise, it is replacing part of me and start and stop have to be within my 
	bounds. "
	delta := 0.
	startIndex := start.
	stopIndex := stop.
	start < 1
		ifTrue: [startIndex := stopIndex := 0]
		ifFalse: [startIndex > self size
				ifTrue: [startIndex := stopIndex := self size + 1]
				ifFalse: 
					[(stopIndex < (startIndex - 1) or: [stopIndex > self size])
						ifTrue: [self errorOutOfBounds].
					delta := stopIndex - startIndex + 1]].
	newOrderedCollection := 
		self species new: self size + replacementCollection size - delta.
	1 to: startIndex - 1 do: [:index | newOrderedCollection add: (self at: index)].
	1 to: replacementCollection size do: 
		[:index | newOrderedCollection add: (replacementCollection at: index)].
	stopIndex + 1 to: self size do: [:index | newOrderedCollection add: (self at: index)].
	^newOrderedCollection! !

!OrderedCollection methodsFor: 'copying' stamp: ''!
copyWith: newElement 
	"Answer a copy of the receiver that is 1 bigger than the receiver and 
	includes the argument, newElement, at the end."

	| newCollection |
	newCollection := self copy.
	newCollection add: newElement.
	^newCollection! !

!OrderedCollection methodsFor: 'enumerating' stamp: ''!
do: aBlock 
	"Override the superclass for performance reasons."
	| index |
	index := firstIndex.
	[index <= lastIndex]
		whileTrue: 
			[aBlock value: (array at: index).
			index := index + 1]! !

!OrderedCollection methodsFor: 'private' stamp: ''!
errorConditionNotSatisfied

	self error: 'no element satisfies condition'! !

!OrderedCollection methodsFor: 'private' stamp: ''!
errorNoSuchElement

	self error: 'attempt to index non-existent element in an ordered collection'! !

!OrderedCollection methodsFor: 'private' stamp: 'BG 1/9/2004 12:26'!
find: oldObject
  "  This method answers an index in the range firstIndex .. lastIndex, which is meant for internal use only.
     Never use this method in your code, the methods for public use are:
        #indexOf:
        #indexOf:ifAbsent: "

	| index |
	index := firstIndex.
	[index <= lastIndex]
		whileTrue:
			[(array at: index) = oldObject ifTrue: [^ index].
			index := index + 1].
	self errorNotFound: oldObject! !

!OrderedCollection methodsFor: 'adding' stamp: ''!
grow
	"Become larger. Typically, a subclass has to override this if the subclass
	adds instance variables."
	| newArray |
	newArray := Array new: self size + self growSize.
	newArray replaceFrom: 1 to: array size with: array startingAt: 1.
	array := newArray! !

!OrderedCollection methodsFor: 'adding' stamp: ''!
growSize
	^ array size max: 2! !

!OrderedCollection methodsFor: 'testing' stamp: 'bf 8/20/1999 15:08'!
hasContentsInExplorer

	^self isEmpty not! !

!OrderedCollection methodsFor: 'private' stamp: 'BG 1/9/2004 12:29'!
insert: anObject before: spot

  "  spot is an index in the range firstIndex .. lastIndex, such an index is not known from outside the collection. 
     Never use this method in your code, it is meant for private use by OrderedCollection only.
     The methods for use are:
        #add:before:   to insert an object before another object
        #add:beforeIndex:   to insert an object before a given position. "
	| "index" delta spotIndex|
	spotIndex := spot.
	delta := spotIndex - firstIndex.
	firstIndex = 1
		ifTrue: 
			[self makeRoomAtFirst.
			spotIndex := firstIndex + delta].
	firstIndex := firstIndex - 1.
	array
		replaceFrom: firstIndex
		to: spotIndex - 2
		with: array
		startingAt: firstIndex + 1.
	array at: spotIndex - 1 put: anObject.
"	index := firstIndex := firstIndex - 1.
	[index < (spotIndex - 1)]
		whileTrue: 
			[array at: index put: (array at: index + 1).
			index := index + 1].
	array at: index put: anObject."
	^ anObject! !

!OrderedCollection methodsFor: 'private' stamp: ''!
makeRoomAtFirst
	| delta index |
	delta := array size - self size.
	delta = 0 ifTrue: 
			[self grow.
			delta := array size - self size].
	lastIndex = array size ifTrue: [^ self]. "just in case we got lucky"
	index := array size.
	[index > delta]
		whileTrue: 
			[array at: index put: (array at: index - delta + firstIndex - 1).
			array at: index - delta + firstIndex - 1 put: nil.
			index := index - 1].
	firstIndex := delta + 1.
	lastIndex := array size! !

!OrderedCollection methodsFor: 'private' stamp: ''!
makeRoomAtLast
	| newLast delta |
	newLast := self size.
	array size - self size = 0 ifTrue: [self grow].
	(delta := firstIndex - 1) = 0 ifTrue: [^ self].
	"we might be here under false premises or grow did the job for us"
	1 to: newLast do:
		[:index |
		array at: index put: (array at: index + delta).
		array at: index + delta put: nil].
	firstIndex := 1.
	lastIndex := newLast! !

!OrderedCollection methodsFor: 'removing' stamp: ''!
remove: oldObject ifAbsent: absentBlock

	| index |
	index := firstIndex.
	[index <= lastIndex]
		whileTrue: 
			[oldObject = (array at: index)
				ifTrue: 
					[self removeIndex: index.
					^ oldObject]
				ifFalse: [index := index + 1]].
	^ absentBlock value! !

!OrderedCollection methodsFor: 'removing' stamp: 'raok 4/27/2001 15:35'!
removeAllSuchThat: aBlock 
	"Remove each element of the receiver for which aBlock evaluates to true.
	The method in Collection is O(N^2), this is O(N)."

	| n |
	n := firstIndex.
	firstIndex to: lastIndex do: [:index |
	    (aBlock value: (array at: index)) ifFalse: [
			array at: n put: (array at: index).
			n := n + 1]].
	n to: lastIndex do: [:index | array at: index put: nil].
	lastIndex := n - 1! !

!OrderedCollection methodsFor: 'removing' stamp: 'ar 5/22/2000 12:19'!
removeAt: index
	| removed |
	removed := self at: index.
	self removeIndex: index + firstIndex - 1.
	^removed! !

!OrderedCollection methodsFor: 'removing' stamp: ''!
removeFirst
	"Remove the first element of the receiver and answer it. If the receiver is 
	empty, create an error notification."
	| firstObject |
	self emptyCheck.
	firstObject := array at: firstIndex.
	array at: firstIndex put: nil.
	firstIndex := firstIndex + 1.
	^ firstObject! !

!OrderedCollection methodsFor: 'removing' stamp: 'ajh 6/22/2003 14:37'!
removeFirst: n
	"Remove first n object into an array"

	| list |
	list := Array new: n.
	1 to: n do: [:i |
		list at: i put: self removeFirst].
	^ list! !

!OrderedCollection methodsFor: 'private' stamp: 'BG 1/9/2004 12:28'!
removeIndex: removedIndex
  "  removedIndex is an index in the range firstIndex .. lastIndex, such an index is not known from outside the collection.
    Never use this method in your code, it is meant for private use by OrderedCollection only.
     The method for public use is:
        #removeAt: "

	array 
		replaceFrom: removedIndex 
		to: lastIndex - 1 
		with: array 
		startingAt: removedIndex+1.
	array at: lastIndex put: nil.
	lastIndex := lastIndex - 1.! !

!OrderedCollection methodsFor: 'removing' stamp: ''!
removeLast
	"Remove the last element of the receiver and answer it. If the receiver is 
	empty, create an error notification."
	| lastObject |
	self emptyCheck.
	lastObject := array at: lastIndex.
	array at: lastIndex put: nil.
	lastIndex := lastIndex - 1.
	^ lastObject! !

!OrderedCollection methodsFor: 'removing' stamp: 'ajh 6/22/2003 14:36'!
removeLast: n
	"Remove last n object into an array with last in last position"

	| list |
	list := Array new: n.
	n to: 1 by: -1 do: [:i |
		list at: i put: self removeLast].
	^ list! !

!OrderedCollection methodsFor: 'private' stamp: 'di 11/14/97 12:54'!
reset
	firstIndex := array size // 3 max: 1.
	lastIndex := firstIndex - 1! !

!OrderedCollection methodsFor: 'private' stamp: 'ar 4/16/1999 07:59'!
resetTo: index
	firstIndex := index.
	lastIndex := firstIndex - 1! !

!OrderedCollection methodsFor: 'enumerating' stamp: ''!
reverseDo: aBlock 
	"Override the superclass for performance reasons."
	| index |
	index := lastIndex.
	[index >= firstIndex]
		whileTrue: 
			[aBlock value: (array at: index).
			index := index - 1]! !

!OrderedCollection methodsFor: 'copying' stamp: 'sw 1/26/96'!
reversed
	"Answer a copy of the receiver with element order reversed.  "
	| newCol |
	newCol := self species new.
	self reverseDo:
		[:elem | newCol addLast: elem].
	^ newCol

"#(2 3 4 'fred') reversed"! !

!OrderedCollection methodsFor: 'enumerating' stamp: 'sma 2/5/2000 15:13'!
select: aBlock 
	"Evaluate aBlock with each of my elements as the argument. Collect into
	a new collection like the receiver, only those elements for which aBlock
	evaluates to true."

	| newCollection element |
	newCollection := self copyEmpty.
	firstIndex to: lastIndex do:
		[:index |
		(aBlock value: (element := array at: index))
			ifTrue: [newCollection addLast: element]].
	^ newCollection! !

!OrderedCollection methodsFor: 'private' stamp: 'di 11/14/97 12:54'!
setCollection: anArray
	array := anArray.
	self reset! !

!OrderedCollection methodsFor: 'private' stamp: 'apb 10/15/2000 18:10'!
setContents: anArray
	array := anArray.
	firstIndex := 1.
	lastIndex := array size.! !

!OrderedCollection methodsFor: 'accessing' stamp: 'sma 5/12/2000 11:39'!
size
	"Answer how many elements the receiver contains."

	^ lastIndex - firstIndex + 1! !

!OrderedCollection methodsFor: 'enumerating' stamp: 'di 8/31/1999 13:13'!
with: otherCollection collect: twoArgBlock 
	"Collect and return the result of evaluating twoArgBlock with 
	corresponding elements from this collection and otherCollection."
	| result |
	otherCollection size = self size ifFalse: [self error: 'otherCollection must be the same size'].
	result := self species new: self size.
	1 to: self size do:
		[:index | result addLast: (twoArgBlock value: (self at: index)
									value: (otherCollection at: index))].
	^ result! !

!OrderedCollection methodsFor: 'enumerating' stamp: 'bf 5/16/2000 16:30'!
withIndexCollect: elementAndIndexBlock 
	"Just like with:collect: except that the iteration index supplies the second argument to the block. Override superclass in order to use addLast:, not at:put:."

	| newCollection |
	newCollection := self species new: self size.
	firstIndex to: lastIndex do:
		[:index |
		newCollection addLast: (elementAndIndexBlock
			value: (array at: index)
			value: index - firstIndex + 1)].
	^ newCollection! !

OrderedCollection subclass: #SortedCollection
	instanceVariableNames: 'sortBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Sequenceable'!

!SortedCollection commentStamp: '<historical>' prior: 0!
I represent a collection of objects ordered by some property of the objects themselves. The ordering is specified in a BlockContext.!

!SortedCollection class methodsFor: 'instance creation' stamp: 'stp 04/23/1999 05:34'!
new: anInteger 
	"The default sorting function is a <= comparison on elements."

	^(super new: anInteger) "sortBlock: [:x :y | x <= y]" 		"nil sortBlock OK"! !

!SortedCollection class methodsFor: 'instance creation' stamp: ''!
sortBlock: aBlock 
	"Answer an instance of me such that its elements are sorted according to 
	the criterion specified in aBlock."

	^(super new: 10) sortBlock: aBlock! !

!SortedCollection methodsFor: 'comparing' stamp: ''!
= aSortedCollection
	"Answer true if my and aSortedCollection's species are the same,
	and if our blocks are the same, and if our elements are the same."

	self species = aSortedCollection species ifFalse: [^ false].
	sortBlock = aSortedCollection sortBlock
		ifTrue: [^ super = aSortedCollection]
		ifFalse: [^ false]! !

!SortedCollection methodsFor: 'adding' stamp: 'go 4/27/2000 13:19'!
add: newObject
	^ super insert: newObject before: (self indexForInserting: newObject)! !

!SortedCollection methodsFor: 'adding' stamp: 'sma 4/28/2000 18:35'!
addAll: aCollection
	aCollection size > (self size // 3)
		ifTrue:
			[aCollection do: [:each | self addLast: each].
			self reSort]
		ifFalse: [aCollection do: [:each | self add: each]].
	^ aCollection! !

!SortedCollection methodsFor: 'adding' stamp: 'go 4/26/2000 17:26'!
addFirst: newObject
	self shouldNotImplement! !

!SortedCollection methodsFor: 'accessing' stamp: 'sma 4/28/2000 17:47'!
at: anInteger put: anObject
	self shouldNotImplement! !

!SortedCollection methodsFor: 'enumerating' stamp: 'sma 2/5/2000 15:22'!
collect: aBlock 
	"Evaluate aBlock with each of my elements as the argument. Collect the 
	resulting values into an OrderedCollection. Answer the new collection. 
	Override the superclass in order to produce an OrderedCollection instead
	of a SortedCollection."

	| newCollection | 
	newCollection := OrderedCollection new: self size.
	self do: [:each | newCollection addLast: (aBlock value: each)].
	^ newCollection! !

!SortedCollection methodsFor: 'copying' stamp: ''!
copy

	| newCollection |
	newCollection := self species sortBlock: sortBlock.
	newCollection addAll: self.
	^newCollection! !

!SortedCollection methodsFor: 'adding' stamp: 'MPH 10/23/2000 13:31'!
copyEmpty
	"Answer a copy of the receiver without any of the receiver's elements."

	^self species sortBlock: sortBlock! !

!SortedCollection methodsFor: 'private' stamp: 'stp 04/23/1999 05:36'!
indexForInserting: newObject

	| index low high |
	low := firstIndex.
	high := lastIndex.
	sortBlock isNil
		ifTrue: [[index := high + low // 2.  low > high]
			whileFalse: 
				[((array at: index) <= newObject)
					ifTrue: [low := index + 1]
					ifFalse: [high := index - 1]]]
		ifFalse: [[index := high + low // 2.  low > high]
			whileFalse: 
				[(sortBlock value: (array at: index) value: newObject)
					ifTrue: [low := index + 1]
					ifFalse: [high := index - 1]]].
	^low! !

!SortedCollection methodsFor: 'private' stamp: 'go 4/26/2000 17:17'!
insert: anObject before: spot
	self shouldNotImplement! !

!SortedCollection methodsFor: 'accessing' stamp: 'tk 3/28/1999 22:55'!
median
	"Return the middle element, or as close as we can get."

	^ self at: self size + 1 // 2! !

!SortedCollection methodsFor: 'private' stamp: 'sma 4/28/2000 17:46'!
reSort
	self sort: firstIndex to: lastIndex! !

!SortedCollection methodsFor: 'private' stamp: 'hg 12/17/2001 19:30'!
should: a precede: b

	^sortBlock ifNil: [a <= b] ifNotNil: [sortBlock value: a value: b]
! !

!SortedCollection methodsFor: 'private' stamp: 'hg 12/17/2001 20:22'!
sort: i to: j 
	"Sort elements i through j of self to be nondescending according to
	sortBlock."

	| di dij dj tt ij k l n |
	"The prefix d means the data at that index."
	(n := j + 1  - i) <= 1 ifTrue: [^self].	"Nothing to sort." 
	 "Sort di,dj."
	di := array at: i.
	dj := array at: j.
	(self should: di precede: dj)
		ifFalse: 
			[array swap: i with: j.
			 tt := di.
			 di := dj.
			 dj := tt].
	n > 2
		ifTrue:  "More than two elements."
			[ij := (i + j) // 2.  "ij is the midpoint of i and j."
			 dij := array at: ij.  "Sort di,dij,dj.  Make dij be their median."
			 (self should: di precede: dij)
			   ifTrue: 
				[(self should: dij precede: dj)
				  ifFalse: 
					[array swap: j with: ij.
					 dij := dj]]
			   ifFalse:
				[array swap: i with: ij.
				 dij := di].
			n > 3
			  ifTrue:  "More than three elements."
				["Find k>i and l<j such that dk,dij,dl are in reverse order.
				Swap k and l.  Repeat this procedure until k and l pass each other."
				 k := i.
				 l := j.
				 [[l := l - 1.  k <= l and: [self should: dij precede: (array at: l)]]
				   whileTrue.  "i.e. while dl succeeds dij"
				  [k := k + 1.  k <= l and: [self should: (array at: k) precede: dij]]
				   whileTrue.  "i.e. while dij succeeds dk"
				  k <= l]
				   whileTrue:
					[array swap: k with: l]. 
	"Now l<k (either 1 or 2 less), and di through dl are all less than or equal to dk
	through dj.  Sort those two segments."
				self sort: i to: l.
				self sort: k to: j]]! !

!SortedCollection methodsFor: 'accessing' stamp: ''!
sortBlock
	"Answer the blockContext which is the criterion for sorting elements of 
	the receiver."

	^sortBlock! !

!SortedCollection methodsFor: 'accessing' stamp: 'stp 12/05/1999 07:09'!
sortBlock: aBlock 
	"Make the argument, aBlock, be the criterion for ordering elements of the 
	receiver."

	aBlock
		ifNotNil: [sortBlock := aBlock fixTemps]
		ifNil: [sortBlock := aBlock].
	"The sortBlock must copy its home context, so as to avoid circularities!!"
	"Therefore sortBlocks with side effects may not work right"
	self size > 0 ifTrue: [self reSort]! !

!SortedCollection methodsFor: 'topological sort' stamp: 'hg 1/2/2002 13:34'!
sortTopologically
	"Plenty of room for increased efficiency in this one."

	| remaining result pick |
	remaining := self asOrderedCollection.
	result := OrderedCollection new.
	[remaining isEmpty] whileFalse: [
		pick := remaining select: [:item |
			remaining allSatisfy: [:anotherItem |
				item == anotherItem or: [self should: item precede: anotherItem]]].
		pick isEmpty ifTrue: [self error: 'bad topological ordering'].
		result addAll: pick.
		remaining removeAll: pick].
	^self copySameFrom: result! !

!SequenceableCollection class methodsFor: 'stream creation' stamp: ''!
streamContents: blockWithArg
	| stream |
	stream := WriteStream on: (self new: 100).
	blockWithArg value: stream.
	^stream contents! !

!SequenceableCollection class methodsFor: 'stream creation' stamp: 'di 6/20/97 09:07'!
streamContents: blockWithArg limitedTo: sizeLimit
	| stream |
	stream := LimitedWriteStream on: (self new: (100 min: sizeLimit)).
	stream setLimit: sizeLimit limitBlock: [^ stream contents].
	blockWithArg value: stream.
	^ stream contents
"
String streamContents: [:s | 1000 timesRepeat: [s nextPutAll: 'Junk']] limitedTo: 25
 'JunkJunkJunkJunkJunkJunkJ'
"! !

!SequenceableCollection methodsFor: 'copying' stamp: 'md 7/14/2006 12:11'!
, otherCollection 
	"Concatenate two Strings or Collections."
	
	^ self copyReplaceFrom: self size + 1
		  to: self size
		  with: otherCollection
"
#(2 4 6 8) , #(who do we appreciate)
((2989 storeStringBase: 16) copyFrom: 4 to: 6) , ' boy!!'
"! !

!SequenceableCollection methodsFor: 'comparing' stamp: 'sma 5/12/2000 14:04'!
= otherCollection 
	"Answer true if the receiver is equivalent to the otherCollection.
	First test for identity, then rule out different species and sizes of
	collections. As a last resort, examine each element of the receiver
	and the otherCollection."

	self == otherCollection ifTrue: [^ true].
	self species == otherCollection species ifFalse: [^ false].
	^ self hasEqualElements: otherCollection! !

!SequenceableCollection methodsFor: 'converting' stamp: 'TAG 11/6/1998 15:55'!
@ aCollection 
	^ self with: aCollection collect: [:a :b | a @ b]! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 5/12/2000 18:00'!
after: target
	"Answer the element after target.  Raise an error if target is not
	in the receiver, or if there are no elements after it."

	^ self after: target ifAbsent: [self errorNotFound: target]! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'ac 7/5/2004 22:35'!
after: target ifAbsent: exceptionBlock
	"Answer the element after target.  Answer the result of evaluation
	the exceptionBlock if target is not in the receiver, or if there are 
	no elements after it."

	| index |
	index := self indexOf: target.
	^ (index == 0 or: [index = self size])
		ifTrue: [exceptionBlock value]
		ifFalse: [self at: index + 1]! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:34'!
allButFirst
	"Answer a copy of the receiver containing all but the first
	element. Raise an error if there are not enough elements."

	^ self allButFirst: 1! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:35'!
allButFirst: n
	"Answer a copy of the receiver containing all but the first n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: n + 1 to: self size! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'ajh 8/6/2002 15:03'!
allButFirstDo: block

	2 to: self size do:
		[:index | block value: (self at: index)]! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:35'!
allButLast
	"Answer a copy of the receiver containing all but the last
	element. Raise an error if there are not enough elements."

	^ self allButLast: 1! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:35'!
allButLast: n
	"Answer a copy of the receiver containing all but the last n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: 1 to: self size - n! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'ajh 8/6/2002 15:01'!
allButLastDo: block

	1 to: self size - 1 do:
		[:index | block value: (self at: index)]! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 5/12/2000 11:33'!
anyOne
	^ self first! !

!SequenceableCollection methodsFor: 'converting' stamp: 'sma 5/12/2000 17:32'!
asArray
	"Answer an Array whose elements are the elements of the receiver."

	^ Array withAll: self! !

!SequenceableCollection methodsFor: 'converting' stamp: 'sma 5/12/2000 17:36'!
asByteArray
	"Answer a ByteArray whose elements are the elements of the receiver."

	^ ByteArray withAll: self! !

!SequenceableCollection methodsFor: 'converting' stamp: 'ar 3/3/2001 20:06'!
asColorArray
	^ColorArray withAll: self! !

!SequenceableCollection methodsFor: 'private' stamp: 'tk 7/30/97 12:42'!
asDigitsAt: anInteger in: aCollection do: aBlock
	"(0 to: 1) asDigitsToPower: 4 do: [:each | Transcript cr; show: each printString]"

	self do: 
		[:each | 
		aCollection at: anInteger put: each.
		anInteger = aCollection size 
			ifTrue: [aBlock value: aCollection]
			ifFalse: [self asDigitsAt: anInteger + 1 in: aCollection do: aBlock]].! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'tk 7/30/97 12:41'!
asDigitsToPower: anInteger do: aBlock
	"Repeatedly value aBlock with a single Array.  Adjust the collection
	so that aBlock is presented all (self size raisedTo: anInteger) possible 
	combinations of the receiver's elements taken as digits of an anInteger long number."
	"(0 to: 1) asDigitsToPower: 4 do: [:each | Transcript cr; show: each printString]"

	| aCollection |
	aCollection := Array new: anInteger.
	self asDigitsAt: 1 in: aCollection do: aBlock! !

!SequenceableCollection methodsFor: 'converting' stamp: 'ar 9/14/1998 23:47'!
asFloatArray
	"Answer a FloatArray whose elements are the elements of the receiver, in 
	the same order."

	| floatArray |
	floatArray := FloatArray new: self size.
	1 to: self size do:[:i| floatArray at: i put: (self at: i) asFloat ].
	^floatArray! !

!SequenceableCollection methodsFor: 'converting' stamp: 'ar 10/10/1998 16:19'!
asIntegerArray
	"Answer an IntegerArray whose elements are the elements of the receiver, in 
	the same order."

	| intArray |
	intArray := IntegerArray new: self size.
	1 to: self size do:[:i| intArray at: i put: (self at: i)].
	^intArray! !

!SequenceableCollection methodsFor: 'converting' stamp: 'NS 5/30/2001 20:56'!
asPointArray
	"Answer an PointArray whose elements are the elements of the receiver, in 
	the same order."

	| pointArray |
	pointArray := PointArray new: self size.
	1 to: self size do:[:i| pointArray at: i put: (self at: i)].
	^pointArray! !

!SequenceableCollection methodsFor: 'converting' stamp: 'ar 4/10/2005 18:02'!
asStringWithCr
	"Convert to a string with returns between items.  Elements are
usually strings.
	 Useful for labels for PopUpMenus."
	| labelStream |
	labelStream := WriteStream on: (String new: 200).
	self do: [:each |
		each isString
			ifTrue: [labelStream nextPutAll: each; cr]
			ifFalse: [each printOn: labelStream. labelStream cr]].
	self size > 0 ifTrue: [labelStream skip: -1].
	^ labelStream contents! !

!SequenceableCollection methodsFor: 'converting' stamp: 'ar 10/10/1998 16:20'!
asWordArray
	"Answer a WordArray whose elements are the elements of the receiver, in 
	the same order."

	| wordArray |
	wordArray := WordArray new: self size.
	1 to: self size do:[:i| wordArray at: i put: (self at: i)].
	^wordArray! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 5/12/2000 12:11'!
at: index ifAbsent: exceptionBlock 
	"Answer the element at my position index. If I do not contain an element 
	at index, answer the result of evaluating the argument, exceptionBlock."

	(index between: 1 and: self size) ifTrue: [^ self at: index].
	^ exceptionBlock value! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'raok 11/22/2002 12:34'!
at: index incrementBy: value
	^self at: index put: (self at: index) + value! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'apb 11/4/2000 22:51'!
atAll: indexArray
	"Answer a new collection like the receiver which contains all elements
	of the receiver at the indices of indexArray."
	"#('one' 'two' 'three' 'four') atAll: #(3 2 4)"

	| newCollection |
	newCollection := self species ofSize: indexArray size.
	1 to: indexArray size do:
		[:index |
		newCollection at: index put: (self at: (indexArray at: index))].
	^ newCollection! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 5/12/2000 12:17'!
atAll: aCollection put: anObject 
	"Put anObject at every index specified by the elements of aCollection."

	aCollection do: [:index | self at: index put: anObject].
	^ anObject! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 5/12/2000 12:18'!
atAll: indexArray putAll: valueArray
	"Store the elements of valueArray into the slots
	of this collection selected by indexArray."

	indexArray with: valueArray do: [:index :value | self at: index put: value].
	^ valueArray! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 4/22/2000 17:45'!
atAllPut: anObject 
	"Put anObject at every one of the receiver's indices."

	| size |
	(size := self size) > 26 "first method faster from 27 accesses and on"
		ifTrue: [self from: 1 to: size put: anObject]
		ifFalse: [1 to: size do: [:index | self at: index put: anObject]]! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'nk 6/12/2004 17:06'!
atLast: indexFromEnd
	"Return element at indexFromEnd from the last position.
	 atLast: 1, returns the last element"

	^ self atLast: indexFromEnd ifAbsent: [self error: 'index out of range']! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'ajh 6/27/2002 17:52'!
atLast: indexFromEnd ifAbsent: block
	"Return element at indexFromEnd from the last position.
	 atLast: 1 ifAbsent: [] returns the last element"

	^ self at: self size + 1 - indexFromEnd ifAbsent: block! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'ajh 6/27/2002 18:10'!
atLast: indexFromEnd put: obj
	"Set the element at indexFromEnd from the last position.
	 atLast: 1 put: obj, sets the last element"

	^ self at: self size + 1 - indexFromEnd put: obj! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'di 11/6/1998 14:32'!
atPin: index 
	"Return the index'th element of me if possible.
	Return the first or last element if index is out of bounds."

	index < 1 ifTrue: [^ self first].
	index > self size ifTrue: [^ self last].
	^ self at: index! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 5/12/2000 12:50'!
atRandom: aGenerator
	"Answer a random element of the receiver.  Uses aGenerator which
	should be kept by the user in a variable and used every time. Use
	this instead of #atRandom for better uniformity of random numbers 
	because only you use the generator.  Causes an error if self has no 
	elements."

	^ self at: (aGenerator nextInt: self size)! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 5/12/2000 13:51'!
atWrap: index 
	"Answer the index'th element of the receiver.  If index is out of bounds,
	let it wrap around from the end to the beginning until it is in bounds."

	^ self at: index - 1 \\ self size + 1! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 5/12/2000 13:52'!
atWrap: index put: value
	"Store value into the index'th element of the receiver.  If index is out
	of bounds, let it wrap around from the end to the beginning until it 
	is in bounds. Answer value."

	^ self at: index  - 1 \\ self size + 1 put: value! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 5/12/2000 18:04'!
before: target
	"Answer the receiver's element immediately before target. Raise an
	error if target is not an element of the receiver, or if there are no 
	elements before it (i.e. it is the first element)."

	^ self before: target ifAbsent: [self errorNotFound: target]! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'ac 7/5/2004 22:36'!
before: target ifAbsent: exceptionBlock
	"Answer the receiver's element immediately before target. Answer
	the result of evaluating the exceptionBlock if target is not an element
	of the receiver, or if there are no elements before it."

	| index |
	index := self indexOf: target.
	^ (index == 0 or: [index == 1])
		ifTrue: [exceptionBlock value]
		ifFalse: [self at: index - 1]! !

!SequenceableCollection methodsFor: 'testing' stamp: 'bp 2/23/2004 21:47'!
beginsWith: aSequenceableCollection

	(aSequenceableCollection isEmpty or: [self size < aSequenceableCollection size]) ifTrue: [^false].
	aSequenceableCollection withIndexDo: [:each :index | (self at: index) ~= each ifTrue: [^false]].
	^true! !

!SequenceableCollection methodsFor: 'private' stamp: 'sma 5/12/2000 13:57'!
checkedAt: index
	index > self size ifTrue: [self error: 'not enough elements'].
	^ self at: index! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'sma 5/12/2000 11:46'!
collect: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into a collection like the receiver. Answer  
	the new collection."

	| newCollection |
	newCollection := self species new: self size.
	1 to: self size do:
		[:index |
		newCollection at: index put: (aBlock value: (self at: index))].
	^ newCollection! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'sma 6/1/2000 11:47'!
collect: aBlock from: firstIndex to: lastIndex
	"Refer to the comment in Collection|collect:."

	| size result j |
	size := lastIndex - firstIndex + 1.
	result := self species new: size.
	j := firstIndex.
	1 to: size do: [:i | result at: i put: (aBlock value: (self at: j)). j := j + 1].
	^ result! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'di 7/13/97 09:44'!
collectWithIndex: elementAndIndexBlock
	"Use the new version with consistent naming"
	^ self withIndexCollect: elementAndIndexBlock! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'tk 7/30/97 12:52'!
combinations: kk atATimeDo: aBlock
	"Take the items in the receiver, kk at a time, and evaluate the block for each combination.  Hand in an array of elements of self as the block argument.  Each combination only occurs once, and order of the elements does not matter.  There are (self size take: kk) combinations."
	" 'abcde' combinations: 3 atATimeDo: [:each | Transcript cr; show: each printString]"

	| aCollection |
	aCollection := Array new: kk.
	self combinationsAt: 1 in: aCollection after: 0 do: aBlock! !

!SequenceableCollection methodsFor: 'private' stamp: 'tk 7/30/97 12:42'!
combinationsAt: jj in: aCollection after: nn do: aBlock
	"Choose k of N items and put in aCollection.  jj-1 already chosen.  Indexes of items are in numerical order, to avoid the same combo being used twice.  In this slot, we are allowed to use items in self indexed by nn+1 to self size.  nn is the index used for position jj-1."
	"(1 to: 6) combinationsSize: 3 do: [:each | Transcript cr; show: each printString]"

nn+1 to: self size do: [:index | 
		aCollection at: jj put: (self at: index).
		jj = aCollection size 
			ifTrue: [aBlock value: aCollection]
			ifFalse: [self combinationsAt: jj + 1 in: aCollection after: index do: aBlock]].! !

!SequenceableCollection methodsFor: 'converting' stamp: 'raok 6/23/2003 12:51'!
concatenation
	|result index|

	result := Array new: (self inject: 0 into: [:sum :each | sum + each size]).
	index := 0.
	self do: [:each | each do: [:item | result at: (index := index+1) put: item]].
	^result! !

!SequenceableCollection methodsFor: 'copying' stamp: 'sma 6/1/2000 16:05'!
copyAfter: anElement
	"Answer a copy of the receiver from after the first occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self indexOf: anElement ifAbsent: [^ self copyEmpty])! !

!SequenceableCollection methodsFor: 'copying' stamp: 'sma 6/1/2000 16:05'!
copyAfterLast: anElement
	"Answer a copy of the receiver from after the last occurence
	of anElement up to the end. If no such element exists, answer 
	an empty copy."

	^ self allButFirst: (self lastIndexOf: anElement ifAbsent: [^ self copyEmpty])! !

!SequenceableCollection methodsFor: 'copying' stamp: 'sma 6/1/2000 16:07'!
copyEmpty
	^ self species new: 0! !

!SequenceableCollection methodsFor: 'copying' stamp: ''!
copyFrom: start to: stop 
	"Answer a copy of a subset of the receiver, starting from element at 
	index start until element at index stop."

	| newSize |
	newSize := stop - start + 1.
	^(self species new: newSize)
		replaceFrom: 1
		to: newSize
		with: self
		startingAt: start! !

!SequenceableCollection methodsFor: 'copying' stamp: 'sma 6/1/2000 16:06'!
copyLast: num
	"Deprecated. Use #last:"

	^ self last: num! !

!SequenceableCollection methodsFor: 'copying' stamp: ''!
copyReplaceAll: oldSubstring with: newSubstring 
	"Default is not to do token matching.
	See also String copyReplaceTokens:with:"
	^ self copyReplaceAll: oldSubstring with: newSubstring asTokens: false
	"'How now brown cow?' copyReplaceAll: 'ow' with: 'ello'"
	"'File asFile Files File''s File' copyReplaceTokens: 'File' with: 'Pile'"! !

!SequenceableCollection methodsFor: 'private' stamp: 'yo 9/2/2002 18:22'!
copyReplaceAll: oldSubstring with: newSubstring asTokens: ifTokens
	"Answer a copy of the receiver in which all occurrences of
	oldSubstring have been replaced by newSubstring.
	ifTokens (valid for Strings only) specifies that the characters
	surrounding the recplacement must not be alphanumeric.
		Bruce Simth,  must be incremented by 1 and not 
	newSubstring if ifTokens is true.  See example below. "

	| aString startSearch currentIndex endIndex |
	(ifTokens and: [(self isString) not])
		ifTrue: [(self isKindOf: Text) ifFalse: [
			self error: 'Token replacement only valid for Strings']].
	aString := self.
	startSearch := 1.
	[(currentIndex := aString indexOfSubCollection: oldSubstring startingAt: startSearch)
			 > 0]
		whileTrue: 
		[endIndex := currentIndex + oldSubstring size - 1.
		(ifTokens not
			or: [(currentIndex = 1
					or: [(aString at: currentIndex-1) isAlphaNumeric not])
				and: [endIndex = aString size
					or: [(aString at: endIndex+1) isAlphaNumeric not]]])
			ifTrue: [aString := aString
					copyReplaceFrom: currentIndex
					to: endIndex
					with: newSubstring.
				startSearch := currentIndex + newSubstring size]
			ifFalse: [
				ifTokens 
					ifTrue: [startSearch := currentIndex + 1]
					ifFalse: [startSearch := currentIndex + newSubstring size]]].
	^ aString

"Test case:
	'test te string' copyReplaceAll: 'te' with: 'longone' asTokens: true   "
! !

!SequenceableCollection methodsFor: 'copying' stamp: 'ar 10/16/2001 19:03'!
copyReplaceFrom: start to: stop with: replacementCollection 
	"Answer a copy of the receiver satisfying the following conditions: If 
	stop is less than start, then this is an insertion; stop should be exactly 
	start-1, start = 1 means insert before the first character, start = size+1 
	means append after last character. Otherwise, this is a replacement; start 
	and stop have to be within the receiver's bounds."

	| newSequenceableCollection newSize endReplacement |
	newSize := self size - (stop - start + 1) + replacementCollection size.
	endReplacement := start - 1 + replacementCollection size.
	newSequenceableCollection := self species new: newSize.
	start > 1 ifTrue:[
		newSequenceableCollection
			replaceFrom: 1
			to: start - 1
			with: self
			startingAt: 1].
	start <= endReplacement ifTrue:[
		newSequenceableCollection
			replaceFrom: start
			to: endReplacement
			with: replacementCollection
			startingAt: 1].
	endReplacement < newSize ifTrue:[
		newSequenceableCollection
			replaceFrom: endReplacement + 1
			to: newSize
			with: self
			startingAt: stop + 1].
	^newSequenceableCollection! !

!SequenceableCollection methodsFor: 'copying' stamp: 'sma 6/1/2000 16:00'!
copyUpTo: anElement 
	"Answer all elements up to but not including anObject. If there
	is no such object, answer a copy of the receiver."

	^ self first: (self indexOf: anElement ifAbsent: [^ self copy]) - 1! !

!SequenceableCollection methodsFor: 'copying' stamp: 'sma 6/1/2000 16:02'!
copyUpToLast: anElement
	"Answer a copy of the receiver from index 1 to the last occurrence of 
	anElement, not including anElement."

	^ self first: (self lastIndexOf: anElement ifAbsent: [^ self copy]) - 1! !

!SequenceableCollection methodsFor: 'copying' stamp: ''!
copyWith: newElement 
	"Answer a copy of the receiver that is 1 bigger than the receiver and has 
	newElement at the last element."

	| newIC |
	newIC := self species new: self size + 1.
	newIC 
		replaceFrom: 1
		to: self size
		with: self
		startingAt: 1.
	newIC at: newIC size put: newElement.
	^newIC! !

!SequenceableCollection methodsFor: 'copying' stamp: 'ajh 9/27/2002 12:09'!
copyWithFirst: newElement 
	"Answer a copy of the receiver that is 1 bigger than the receiver with newElement as the first element."

	| newIC |
	newIC := self species ofSize: self size + 1.
	newIC 
		replaceFrom: 2
		to: self size + 1
		with: self
		startingAt: 1.
	newIC at: 1 put: newElement.
	^ newIC! !

!SequenceableCollection methodsFor: 'copying' stamp: 'sma 6/1/2000 15:38'!
copyWithoutFirst
	"Deprecatd. Return a copy of the receiver which doesn't include
	the first element."

	^ self allButFirst! !

!SequenceableCollection methodsFor: 'copying' stamp: 'rhi 12/6/2001 14:04'!
copyWithoutIndex: index
	"Return a copy containing all elements except the index-th."

	| copy |
	copy := self species ofSize: self size - 1.
	copy replaceFrom: 1 to: index-1 with: self startingAt: 1.
	copy replaceFrom: index to: copy size with: self startingAt: index+1.
	^ copy! !

!SequenceableCollection methodsFor: 'enumerating' stamp: ''!
do: aBlock 
	"Refer to the comment in Collection|do:."
	1 to: self size do:
		[:index | aBlock value: (self at: index)]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'ab 9/17/2002 01:02'!
do: aBlock displayingProgress: aString
	aString
		displayProgressAt: Sensor cursorPoint
		from: 0 to: self size
		during:
			[:bar |
			self withIndexDo:
				[:each :i |
				bar value: i.
				aBlock value: each]]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'sma 5/12/2000 11:56'!
do: elementBlock separatedBy: separatorBlock
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	1 to: self size do:
		[:index |
		index = 1 ifFalse: [separatorBlock value].
		elementBlock value: (self at: index)]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'ar 5/1/1999 05:01'!
do: aBlock without: anItem
	"Enumerate all elements in the receiver.
	Execute aBlock for those elements that are not equal to the given item"
	"Refer to the comment in Collection|do:."
	1 to: self size do:
		[:index | anItem = (self at: index) ifFalse:[aBlock value: (self at: index)]]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'di 7/13/97 09:43'!
doWithIndex: elementAndIndexBlock
	"Use the new version with consistent naming"
	^ self withIndexDo: elementAndIndexBlock! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'JMM 10/30/2005 09:21'!
eighth
	"Answer the eighth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 8! !

!SequenceableCollection methodsFor: 'testing' stamp: 'bp 2/23/2004 21:48'!
endsWith: aSequenceableCollection

	| start |
	(aSequenceableCollection isEmpty or: [self size < aSequenceableCollection size]) ifTrue: [^false].
	start := self size - aSequenceableCollection size.
	aSequenceableCollection withIndexDo: [:each :index | (self at: start + index) ~= each ifTrue: [^false]].
	^true! !

!SequenceableCollection methodsFor: 'private' stamp: 'sma 5/12/2000 18:06'!
errorFirstObject: anObject
	self error: 'specified object is first object'! !

!SequenceableCollection methodsFor: 'private' stamp: 'sma 5/12/2000 18:03'!
errorLastObject: anObject
	self error: 'specified object is last object'! !

!SequenceableCollection methodsFor: 'private' stamp: ''!
errorOutOfBounds

	self error: 'indices are out of bounds'! !

!SequenceableCollection methodsFor: 'explorer' stamp: 'hg 9/7/2001 12:01'!
explorerContents

	^self asOrderedCollection withIndexCollect: [:value :index |
		ObjectExplorerWrapper
			with: value
			name: index printString
			model: self]! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'JMM 10/30/2005 09:21'!
fifth
	"Answer the fifth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 5! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'ar 6/3/2000 15:53'!
findBinary: aBlock
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If no matching element is found, raise an error.
	Examples:
		#(1 3 5 7 11 15 23) findBinary:[:arg| 11 - arg]
	"
	^self findBinary: aBlock ifNone: [self errorNotFound: aBlock]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'ar 6/3/2000 15:52'!
findBinary: aBlock ifNone: exceptionBlock
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If no matching element is found, evaluate exceptionBlock."
	| index low high test item |
	low := 1.
	high := self size.
	[index := high + low // 2.
	low > high] whileFalse:[
		test := aBlock value: (item := self at: index).
		test = 0 
			ifTrue:[^item]
			ifFalse:[test > 0
				ifTrue: [low := index + 1]
				ifFalse: [high := index - 1]]].
	^exceptionBlock value! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'ar 6/3/2000 15:54'!
findBinaryIndex: aBlock
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If no matching element is found, raise an error.
	Examples:
		#(1 3 5 7 11 15 23) findBinaryIndex:[:arg| 11 - arg]
	"
	^self findBinaryIndex: aBlock ifNone: [self errorNotFound: aBlock]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'ar 6/3/2000 15:54'!
findBinaryIndex: aBlock ifNone: exceptionBlock
	"Search for an element in the receiver using binary search.
	The argument aBlock is a one-element block returning
		0 	- if the element is the one searched for
		<0	- if the search should continue in the first half
		>0	- if the search should continue in the second half
	If no matching element is found, evaluate exceptionBlock."
	| index low high test |
	low := 1.
	high := self size.
	[index := high + low // 2.
	low > high] whileFalse:[
		test := aBlock value: (self at: index).
		test = 0 
			ifTrue:[^index]
			ifFalse:[test > 0
				ifTrue: [low := index + 1]
				ifFalse: [high := index - 1]]].
	^exceptionBlock value! !

!SequenceableCollection methodsFor: 'enumerating' stamp: ''!
findFirst: aBlock
	"Return the index of my first element for which aBlock evaluates as true."

	| index |
	index := 0.
	[(index := index + 1) <= self size] whileTrue:
		[(aBlock value: (self at: index)) ifTrue: [^index]].
	^ 0! !

!SequenceableCollection methodsFor: 'enumerating' stamp: ''!
findLast: aBlock
	"Return the index of my last element for which aBlock evaluates as true."

	| index |
	index := self size + 1.
	[(index := index - 1) >= 1] whileTrue:
		[(aBlock value: (self at: index)) ifTrue: [^index]].
	^ 0! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'md 1/19/2006 09:57'!
first
	"Answer the first element of the receiver"

	^ self at: 1! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:31'!
first: n
	"Answer the first n elements of the receiver.
	Raise an error if there are not enough elements."

	^ self copyFrom: 1 to: n! !

!SequenceableCollection methodsFor: 'copying' stamp: 'fcs 1/20/2002 16:03'!
forceTo: length paddingStartWith: elem 
	"Force the length of the collection to length, padding  
	the beginning of the result if necessary with elem.  
	Note that this makes a copy."
	| newCollection padLen |
	newCollection := self species new: length.
	padLen := length - self size max: 0.
	newCollection
		from: 1
		to: padLen
		put: elem.
	newCollection
		replaceFrom: padLen + 1
		to: ((padLen + self size) min: length)
		with: self
		startingAt:  1.
	^ newCollection! !

!SequenceableCollection methodsFor: 'copying' stamp: 'sma 4/22/2000 18:01'!
forceTo: length paddingWith: elem
	"Force the length of the collection to length, padding
	if necessary with elem.  Note that this makes a copy."

	| newCollection copyLen |
	newCollection := self species new: length.
	copyLen := self size min: length.
	newCollection replaceFrom: 1 to: copyLen with: self startingAt: 1.
	newCollection from: copyLen + 1 to: length put: elem.
	^ newCollection! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'JMM 10/30/2005 09:21'!
fourth
	"Answer the fourth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 4! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'sma 5/12/2000 18:11'!
from: start to: stop do: aBlock
	"Evaluate aBlock for all elements between start and stop (inclusive)."

	start to: stop do: [:index | aBlock value: (self at: index)]! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'SqR 10/30/2000 22:06'!
from: startIndex to: endIndex put: anObject
	"Put anObject in all indexes between startIndex 
	and endIndex. Very fast. Faster than to:do: for
	more than 26 positions. Answer anObject"

	| written toWrite thisWrite |

	startIndex > endIndex ifTrue: [^self].
	self at: startIndex put: anObject.
	written := 1.
	toWrite := endIndex - startIndex + 1.
	[written < toWrite] whileTrue:
		[
			thisWrite := written min: toWrite - written.
			self 
				replaceFrom: startIndex + written
				to: startIndex + written + thisWrite - 1
				with: self startingAt: startIndex.
			written := written + thisWrite
		].
	^anObject! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'nk 12/30/2003 15:39'!
groupsOf: n atATimeCollect: aBlock 
	"Evaluate aBlock with my elements taken n at a time. Ignore any 
	leftovers at the end. 
	Allows use of a flattened  
	array for things that naturally group into groups of n. 
	If aBlock has a single argument, pass it an array of n items, 
	otherwise, pass the items as separate arguments. 
	See also pairsDo:"
	| passArray args  |
	passArray := aBlock numArgs = 1.
	^(n
		to: self size
		by: n)
		collect: [:index | 
			args := (self copyFrom: index - n + 1 to: index) asArray.
			passArray
				ifTrue: [aBlock value: args]
				ifFalse: [aBlock valueWithArguments: args]]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'nk 12/30/2003 15:37'!
groupsOf: n atATimeDo: aBlock 
	"Evaluate aBlock with my elements taken n at a time. Ignore any leftovers at the end.
	Allows use of a flattened 
	array for things that naturally group into groups of n.
	If aBlock has a single argument, pass it an array of n items,
	otherwise, pass the items as separate arguments.
	See also pairsDo:"
	| passArray args |
	passArray := (aBlock numArgs = 1).
	n
		to: self size
		by: n
		do: [:index | 
			args := (self copyFrom: index - n + 1 to: index) asArray.
			passArray ifTrue: [ aBlock value: args ]
				ifFalse: [ aBlock valueWithArguments: args ]].! !

!SequenceableCollection methodsFor: 'comparing' stamp: 'tk 12/6/2000 11:39'!
hasEqualElements: otherCollection
	"Answer whether the receiver's size is the same as otherCollection's
	size, and each of the receiver's elements equal the corresponding 
	element of otherCollection.
	This should probably replace the current definition of #= ."

	| size |
	(otherCollection isKindOf: SequenceableCollection) ifFalse: [^ false].
	(size := self size) = otherCollection size ifFalse: [^ false].
	1 to: size do:
		[:index |
		(self at: index) = (otherCollection at: index) ifFalse: [^ false]].
	^ true! !

!SequenceableCollection methodsFor: 'comparing' stamp: 'SqR 8/3/2000 13:39'!
hash
	| hash |

	hash := self species hash.
	1 to: self size do: [:i | hash := (hash + (self at: i) hash) hashMultiply].
	^hash! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'ar 8/14/1998 21:20'!
identityIndexOf: anElement 
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer 0."

	^self identityIndexOf: anElement ifAbsent: [0]! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'ar 8/14/1998 21:21'!
identityIndexOf: anElement ifAbsent: exceptionBlock
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer the result of evaluating the argument, 
	exceptionBlock."
	1 to: self size do:
		[:i | (self at: i) == anElement ifTrue: [^ i]].
	^ exceptionBlock value! !

!SequenceableCollection methodsFor: 'testing' stamp: 'sma 5/12/2000 14:08'!
includes: anObject
	"Answer whether anObject is one of the receiver's elements."

	^ (self indexOf: anObject) ~= 0! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:46'!
indexOf: anElement
	"Answer the index of the first occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer 0."

	^ self indexOf: anElement ifAbsent: [0]! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:47'!
indexOf: anElement ifAbsent: exceptionBlock
	"Answer the index of the first occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the 
	result of evaluating the argument, exceptionBlock."

	^ self indexOf: anElement startingAt: 1 ifAbsent: exceptionBlock! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:47'!
indexOf: anElement startingAt: start ifAbsent: exceptionBlock
	"Answer the index of the first occurence of anElement after start
	within the receiver. If the receiver does not contain anElement, 
	answer the 	result of evaluating the argument, exceptionBlock."

	start to: self size do:
		[:index |
		(self at: index) = anElement ifTrue: [^ index]].
	^ exceptionBlock value! !

!SequenceableCollection methodsFor: 'accessing' stamp: ''!
indexOfSubCollection: aSubCollection startingAt: anIndex 
	"Answer the index of the receiver's first element, such that that element 
	equals the first element of aSubCollection, and the next elements equal 
	the rest of the elements of aSubCollection. Begin the search at element 
	anIndex of the receiver. If no such match is found, answer 0."

	^self
		indexOfSubCollection: aSubCollection
		startingAt: anIndex
		ifAbsent: [0]! !

!SequenceableCollection methodsFor: 'accessing' stamp: ''!
indexOfSubCollection: sub startingAt: start ifAbsent: exceptionBlock
	"Answer the index of the receiver's first element, such that that element 
	equals the first element of sub, and the next elements equal 
	the rest of the elements of sub. Begin the search at element 
	start of the receiver. If no such match is found, answer the result of 
	evaluating argument, exceptionBlock."
	| first index |
	sub isEmpty ifTrue: [^ exceptionBlock value].
	first := sub first.
	start to: self size - sub size + 1 do:
		[:startIndex |
		(self at: startIndex) = first ifTrue:
			[index := 1.
			[(self at: startIndex+index-1) = (sub at: index)]
				whileTrue:
				[index = sub size ifTrue: [^startIndex].
				index := index+1]]].
	^ exceptionBlock value! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'ar 3/3/2001 22:43'!
integerAt: index
	"Return the integer at the given index"
	^self at: index! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'ar 3/3/2001 22:43'!
integerAt: index put: value
	"Return the integer at the given index"
	^self at: index put: value! !

!SequenceableCollection methodsFor: 'converting' stamp: 'di 11/6/1998 09:35'!
isSequenceable
	^ true! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'sma 5/12/2000 18:13'!
keysAndValuesDo: aBlock 
	"Enumerate the receiver with all the keys (aka indices) and values."

	1 to: self size do: [:index | aBlock value: index value: (self at: index)]! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'md 1/19/2006 09:56'!
last
	"Answer the last element of the receiver"

	^ self at: self size! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'sma 6/1/2000 15:30'!
last: n
	"Answer the last n elements of the receiver.  
	Raise an error if there are not enough elements."

	| size |
	size := self size.
	^ self copyFrom: size - n + 1 to: size! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'ar 12/16/2001 01:06'!
lastIndexOf: anElement
	"Answer the index of the last occurence of anElement within the 
	receiver. If the receiver does not contain anElement, answer 0."

	^ self lastIndexOf: anElement startingAt: self size ifAbsent: [0]! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'ar 12/16/2001 01:06'!
lastIndexOf: anElement ifAbsent: exceptionBlock
	"Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."
	^self lastIndexOf: anElement startingAt: self size ifAbsent: exceptionBlock! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'ar 12/16/2001 01:05'!
lastIndexOf: anElement startingAt: lastIndex ifAbsent: exceptionBlock
	"Answer the index of the last occurence of anElement within the  
	receiver. If the receiver does not contain anElement, answer the
	result of evaluating the argument, exceptionBlock."

	lastIndex to: 1 by: -1 do:
		[:index |
		(self at: index) = anElement ifTrue: [^ index]].
	^ exceptionBlock value! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'md 1/19/2006 09:56'!
middle
	"Answer the middle element of the receiver."
	^ self at: self size // 2 + 1! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'nk 8/23/2003 10:42'!
nextToLast
	^self at: self size - 1! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'JMM 10/30/2005 09:21'!
ninth
	"Answer the ninth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 9! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'nk 7/17/2003 17:55'!
overlappingPairsCollect: aBlock 
	"Answer the result of evaluating aBlock with all of the overlapping pairs of my elements."
	| retval |
	retval := self species new: self size - 1.
	1 to: self size - 1
		do: [:i | retval at: i put: (aBlock value: (self at: i) value: (self at: i + 1)) ].
	^retval! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'nk 7/17/2003 17:55'!
overlappingPairsDo: aBlock 
	"Emit overlapping pairs of my elements into aBlock"

	1 to: self size - 1
		do: [:i | aBlock value: (self at: i) value: (self at: i + 1)]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'nk 7/17/2003 17:54'!
overlappingPairsWithIndexDo: aBlock 
	"Emit overlapping pairs of my elements into aBlock, along with an index."

	1 to: self size - 1
		do: [:i | aBlock value: (self at: i) value: (self at: i + 1) value: i ]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'nk 3/30/2002 12:45'!
paddedWith: otherCollection do: twoArgBlock 
	"Evaluate twoArgBlock with corresponding elements from this collection and otherCollection.
	Missing elements from either will be passed as nil."
	1 to: (self size max: otherCollection size) do:
		[:index | twoArgBlock value: (self at: index ifAbsent: [])
				value: (otherCollection at: index ifAbsent: [])]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'di 11/12/1998 15:01'!
pairsCollect: aBlock 
	"Evaluate aBlock with my elements taken two at a time, and return an Array with the results"

	^ (1 to: self size // 2) collect:
		[:index | aBlock value: (self at: 2 * index - 1) value: (self at: 2 * index)]
"
#(1 'fred' 2 'charlie' 3 'elmer') pairsCollect:
	[:a :b | b, ' is number ', a printString]
"! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'di 11/12/1998 15:01'!
pairsDo: aBlock 
	"Evaluate aBlock with my elements taken two at a time.  If there's an odd number of items, ignore the last one.  Allows use of a flattened array for things that naturally group into pairs.  See also pairsCollect:"

	1 to: self size // 2 do:
		[:index | aBlock value: (self at: 2 * index - 1) value: (self at: 2 * index)]
"
#(1 'fred' 2 'charlie' 3 'elmer') pairsDo:
	[:a :b | Transcript cr; show: b, ' is number ', a printString]
"! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'ward 7/28/97 09:41'!
permutationsDo: aBlock
	"Repeatly value aBlock with a single copy of the receiver. Reorder the copy
	so that aBlock is presented all (self size factorial) possible permutations."
	"(1 to: 4) permutationsDo: [:each | Transcript cr; show: each printString]"

	self shallowCopy permutationsStartingAt: 1 do: aBlock! !

!SequenceableCollection methodsFor: 'private' stamp: 'ward 7/28/97 09:38'!
permutationsStartingAt: anInteger do: aBlock
	"#(1 2 3 4) permutationsDo: [:each | Transcript cr; show: each printString]"

	anInteger > self size ifTrue: [^self].
	anInteger = self size ifTrue: [^aBlock value: self].
	anInteger to: self size do:
		[:i | self swap: anInteger with: i.
		self permutationsStartingAt: anInteger + 1 do: aBlock.
		self swap: anInteger with: i]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'tk 12/27/2000 09:53'!
polynomialEval: thisX
	| sum valToPower |
	"Treat myself as the coeficients of a polynomial in X.  Evaluate it with thisX.  First element is the constant and last is the coeficient for the highest power."
	"  #(1 2 3) polynomialEval: 2   "   "is 3*X^2 + 2*X + 1 with X = 2"

	sum := self first.
	valToPower := thisX.
	2 to: self size do: [:ind | 
		sum := sum + ((self at: ind) * valToPower).
		valToPower := valToPower * thisX].
	^ sum! !

!SequenceableCollection methodsFor: 'converting' stamp: 'sma 5/12/2000 12:51'!
readStream
	^ ReadStream on: self! !

!SequenceableCollection methodsFor: 'removing' stamp: ''!
remove: oldObject ifAbsent: anExceptionBlock 
	"SequencableCollections cannot implement removing."

	self shouldNotImplement! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'ar 1/20/98 16:22'!
replaceAll: oldObject with: newObject 
	"Replace all occurences of oldObject with newObject"
	| index |
	index := self
				indexOf: oldObject
				startingAt: 1
				ifAbsent: [0].
	[index = 0]
		whileFalse: 
			[self at: index put: newObject.
			index := self
						indexOf: oldObject
						startingAt: index + 1
						ifAbsent: [0]]! !

!SequenceableCollection methodsFor: 'accessing' stamp: ''!
replaceFrom: start to: stop with: replacement 
	"This destructively replaces elements from start to stop in the receiver. 
	Answer the receiver itself. Use copyReplaceFrom:to:with: for 
	insertion/deletion which may alter the size of the result."

	replacement size = (stop - start + 1)
		ifFalse: [self error: 'Size of replacement doesnt match'].
	^self replaceFrom: start to: stop with: replacement startingAt: 1! !

!SequenceableCollection methodsFor: 'accessing' stamp: ''!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"This destructively replaces elements from start to stop in the receiver 
	starting at index, repStart, in the sequenceable collection, 
	replacementCollection. Answer the receiver. No range checks are 
	performed."

	| index repOff |
	repOff := repStart - start.
	index := start - 1.
	[(index := index + 1) <= stop]
		whileTrue: [self at: index put: (replacement at: repOff + index)]! !

!SequenceableCollection methodsFor: 'converting' stamp: 'sma 5/12/2000 17:56'!
reverse
	^ self reversed! !

!SequenceableCollection methodsFor: 'enumerating' stamp: ''!
reverseDo: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument, 
	starting with the last element and taking each in sequence up to the 
	first. For SequenceableCollections, this is the reverse of the enumeration 
	for do:."

	self size to: 1 by: -1 do: [:index | aBlock value: (self at: index)]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: ''!
reverseWith: aSequenceableCollection do: aBlock 
	"Evaluate aBlock with each of the receiver's elements, in reverse order, 
	along with the  
	corresponding element, also in reverse order, from 
	aSequencableCollection. "

	self size ~= aSequenceableCollection size ifTrue: [^ self errorNoMatch].
	self size
		to: 1
		by: -1
		do: [:index | aBlock value: (self at: index)
				value: (aSequenceableCollection at: index)]! !

!SequenceableCollection methodsFor: 'converting' stamp: 'jm 4/27/98 04:09'!
reversed
	"Answer a copy of the receiver with element order reversed."
	"Example: 'frog' reversed"

	| n result src |
	n := self size.
	result := self species new: n.
	src := n + 1.
	1 to: n do: [:i | result at: i put: (self at: (src := src - 1))].
	^ result
! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'JMM 10/30/2005 09:21'!
second
	"Answer the second element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 2! !

!SequenceableCollection methodsFor: 'enumerating' stamp: ''!
select: aBlock 
	"Refer to the comment in Collection|select:."
	| aStream |
	aStream := WriteStream on: (self species new: self size).
	1 to: self size do: 
		[:index |
		(aBlock value: (self at: index))
			ifTrue: [aStream nextPut: (self at: index)]].
	^ aStream contents! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'JMM 10/30/2005 09:21'!
seventh
	"Answer the seventh element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 7! !

!SequenceableCollection methodsFor: 'copying' stamp: ''!
shallowCopy

	^self copyFrom: 1 to: self size! !

!SequenceableCollection methodsFor: 'copying' stamp: 'sma 5/12/2000 12:36'!
shuffled
	^ self shuffledBy: Collection randomForPicking

"Examples:
	($A to: $Z) shuffled
"! !

!SequenceableCollection methodsFor: 'copying' stamp: 'djp 10/23/1999 22:12'!
shuffledBy: aRandom
	| copy | 
	copy := self shallowCopy.
	copy size to: 1 by: -1 do: 
		[:i | copy swap: i with: ((1 to: i) atRandom: aRandom)].
	^ copy! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'JMM 10/30/2005 09:22'!
sixth
	"Answer the sixth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 6! !

!SequenceableCollection methodsFor: 'copying' stamp: 'sma 4/28/2000 18:34'!
sortBy: aBlock
	"Create a copy that is sorted.  Sort criteria is the block that accepts two arguments.
	When the block is true, the first arg goes first ([:a :b | a > b] sorts in descending
	order)."

	^ (self asSortedCollection: aBlock) asOrderedCollection! !

!SequenceableCollection methodsFor: 'accessing' stamp: ''!
swap: oneIndex with: anotherIndex 
	"Move the element at oneIndex to anotherIndex, and vice-versa."

	| element |
	element := self at: oneIndex.
	self at: oneIndex put: (self at: anotherIndex).
	self at: anotherIndex put: element! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'JMM 10/30/2005 09:22'!
third
	"Answer the third element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 3! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'huma 12/11/2004 20:43'!
upTo: anObject
	"Deprecated. Use copyUpTo:"
	self deprecated: 'Use #copyUpTo:'.
	^ self copyUpTo: anObject! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'di 8/31/1999 13:13'!
with: otherCollection collect: twoArgBlock 
	"Collect and return the result of evaluating twoArgBlock with corresponding elements from this collection and otherCollection."
	| result |
	otherCollection size = self size ifFalse: [self error: 'otherCollection must be the same size'].
	result := self species new: self size.
	1 to: self size do:
		[:index | result at: index put:
		(twoArgBlock
			value: (self at: index)
			value: (otherCollection at: index))].
	^ result! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'di 8/3/1999 15:26'!
with: otherCollection do: twoArgBlock 
	"Evaluate twoArgBlock with corresponding elements from this collection and otherCollection."
	otherCollection size = self size ifFalse: [self error: 'otherCollection must be the same size'].
	1 to: self size do:
		[:index |
		twoArgBlock value: (self at: index)
				value: (otherCollection at: index)]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'di 5/17/1998 13:34'!
withIndexCollect: elementAndIndexBlock 
	"Just like with:collect: except that the iteration index supplies the second argument to the block."
	| result |
	result := self species new: self size.
	1 to: self size do:
		[:index | result at: index put:
		(elementAndIndexBlock
			value: (self at: index)
			value: index)].
	^ result! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'di 7/13/97 09:35'!
withIndexDo: elementAndIndexBlock 
	"Just like with:do: except that the iteration index supplies the second argument to the block."
	1 to: self size do:
		[:index |
		elementAndIndexBlock
			value: (self at: index)
			value: index]! !

!SequenceableCollection methodsFor: 'converting' stamp: 'sma 5/12/2000 12:52'!
writeStream
	^ WriteStream on: self! !

Collection subclass: #Set
	instanceVariableNames: 'tally array'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Unordered'!

!Set commentStamp: '<historical>' prior: 0!
I represent a set of objects without duplicates.  I can hold anything that responds to
#hash and #=, except for nil.  My instances will automatically grow, if necessary,
Note that I rely on #=, not #==.  If you want a set using #==, use IdentitySet.

Instance structure:

  array	An array whose non-nil elements are the elements of the set,
		and whose nil elements are empty slots.  There is always at least one nil.
		In fact I try to keep my "load" at 75% or less so that hashing will work well.

  tally	The number of elements in the set.  The array size is always greater than this.

The core operation is #findElementOrNil:, which either finds the position where an
object is stored in array, if it is present, or finds a suitable position holding nil, if
its argument is not present in array,!

Set subclass: #Dictionary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Unordered'!

!Dictionary commentStamp: '<historical>' prior: 0!
I represent a set of elements that can be viewed from one of two perspectives: a set of associations, or a container of values that are externally named where the name can be any object that responds to =. The external name is referred to as the key.  I inherit many operations from Set.!

!Dictionary class methodsFor: 'instance creation' stamp: ''!
newFrom: aDict 
	"Answer an instance of me containing the same associations as aDict.
	 Error if any key appears twice."
	| newDictionary |
	newDictionary := self new: aDict size.
	aDict associationsDo:
		[:x |
		(newDictionary includesKey: x key)
			ifTrue: [self error: 'Duplicate key: ', x key printString]
			ifFalse: [newDictionary add: x]].
	^ newDictionary

"	NewDictionary newFrom: {1->#a. 2->#b. 3->#c}
	{1->#a. 2->#b. 3->#c} as: NewDictionary
	NewDictionary newFrom: {1->#a. 2->#b. 1->#c}
	{1->#a. 2->#b. 1->#c} as: NewDictionary
"! !

!Dictionary methodsFor: 'comparing' stamp: 'md 10/17/2004 16:14'!
= aDictionary
	"Two dictionaries are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same set of keys.
	 (c) for each (common) key, they have the same value"

	self == aDictionary ifTrue: [ ^ true ].
	(aDictionary isKindOf: Dictionary) ifFalse: [^false].
	self size = aDictionary size ifFalse: [^false].
	self associationsDo: [:assoc|
		(aDictionary at: assoc key ifAbsent: [^false]) = assoc value
			ifFalse: [^false]].
	^true

! !

!Dictionary methodsFor: 'adding' stamp: ''!
add: anAssociation
	| index element |
	index := self findElementOrNil: anAssociation key.
	element := array at: index.
	element == nil
		ifTrue: [self atNewIndex: index put: anAssociation]
		ifFalse: [element value: anAssociation value].
	^ anAssociation! !

!Dictionary methodsFor: 'adding' stamp: 'raok 12/17/2003 16:01'!
addAll: aKeyedCollection
	aKeyedCollection == self ifFalse: [
		aKeyedCollection keysAndValuesDo: [:key :value |
			self at: key put: value]].
	^aKeyedCollection! !

!Dictionary methodsFor: 'accessing' stamp: ''!
associationAt: key 
	^ self associationAt: key ifAbsent: [self errorKeyNotFound]! !

!Dictionary methodsFor: 'accessing' stamp: ''!
associationAt: key ifAbsent: aBlock 
	"Answer the association with the given key.
	If key is not found, return the result of evaluating aBlock."

	| index assoc |
	index := self findElementOrNil: key.
	assoc := array at: index.
	nil == assoc ifTrue: [ ^ aBlock value ].
	^ assoc! !

!Dictionary methodsFor: 'accessing' stamp: 'tk 4/9/1999 10:22'!
associationDeclareAt: aKey
	"Return an existing association, or create and return a new one.  Needed as a single message by ImageSegment.prepareToBeSaved."

	| existing |
	^ self associationAt: aKey ifAbsent: [
		(Undeclared includesKey: aKey)
			ifTrue: 
				[existing := Undeclared associationAt: aKey.
				Undeclared removeKey: aKey.
				self add: existing]
			ifFalse: 
				[self add: aKey -> false]]! !

!Dictionary methodsFor: 'accessing' stamp: 'dvf 9/17/2003 16:03'!
associations
	"Answer a Collection containing the receiver's associations."
	| out |
	out := WriteStream on: (Array new: self size).
	self associationsDo: [:value | out nextPut: value].
	^ out contents! !

!Dictionary methodsFor: 'enumerating' stamp: ''!
associationsDo: aBlock 
	"Evaluate aBlock for each of the receiver's elements (key/value 
	associations)."

	super do: aBlock! !

!Dictionary methodsFor: 'enumerating' stamp: 'dtl 2/17/2003 09:40'!
associationsSelect: aBlock 
	"Evaluate aBlock with each of my associations as the argument. Collect
	into a new dictionary, only those associations for which aBlock evaluates
	to true."

	| newCollection |
	newCollection := self species new.
	self associationsDo: 
		[:each | 
		(aBlock value: each) ifTrue: [newCollection add: each]].
	^newCollection! !

!Dictionary methodsFor: 'accessing' stamp: ''!
at: key 
	"Answer the value associated with the key."

	^ self at: key ifAbsent: [self errorKeyNotFound]! !

!Dictionary methodsFor: 'accessing' stamp: 'sma 5/12/2000 14:59'!
at: key ifAbsent: aBlock 
	"Answer the value associated with the key or, if key isn't found,
	answer the result of evaluating aBlock."

	| assoc |
	assoc := array at: (self findElementOrNil: key).
	assoc ifNil: [^ aBlock value].
	^ assoc value! !

!Dictionary methodsFor: 'accessing' stamp: 'sma 5/12/2000 15:01'!
at: key ifAbsentPut: aBlock 
	"Return the value at the given key. 
	If key is not included in the receiver store the result 
	of evaluating aBlock as new value."

	^ self at: key ifAbsent: [self at: key put: aBlock value]! !

!Dictionary methodsFor: 'accessing' stamp: 'jm 5/15/1998 07:20'!
at: key ifPresent: aBlock
	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v := self at: key ifAbsent: [^ nil].
	^ aBlock value: v
! !

!Dictionary methodsFor: 'accessing' stamp: 'di 3/7/2001 15:29'!
at: key ifPresentAndInMemory: aBlock
	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v := self at: key ifAbsent: [^ nil].
	v isInMemory ifFalse: [^ nil].
	^ aBlock value: v
! !

!Dictionary methodsFor: 'accessing' stamp: 'sma 5/12/2000 15:00'!
at: key put: anObject 
	"Set the value at key to be anObject.  If key is not found, create a
	new entry for key and set is value to anObject. Answer anObject."

	| index assoc |
	index := self findElementOrNil: key.
	assoc := array at: index.
	assoc
		ifNil: [self atNewIndex: index put: (Association key: key value: anObject)]
		ifNotNil: [assoc value: anObject].
	^ anObject! !

!Dictionary methodsFor: 'enumerating' stamp: ''!
collect: aBlock 
	"Evaluate aBlock with each of my values as the argument.  Collect the
	resulting values into a collection that is like me. Answer with the new
	collection."
	| newCollection |
	newCollection := OrderedCollection new: self size.
	self do: [:each | newCollection add: (aBlock value: each)].
	^ newCollection! !

!Dictionary methodsFor: 'private' stamp: 'raok 4/22/2002 12:09'!
copy
	"Must copy the associations, or later store will affect both the
original and the copy"

	^ self shallowCopy withArray:
		(array collect: [:assoc |
			assoc ifNil: [nil]
				ifNotNil: [Association key: assoc key
value: assoc value]])! !

!Dictionary methodsFor: 'adding' stamp: ''!
declare: key from: aDictionary 
	"Add key to the receiver. If key already exists, do nothing. If aDictionary 
	includes key, then remove it from aDictionary and use its association as 
	the element of the receiver."

	(self includesKey: key) ifTrue: [^ self].
	(aDictionary includesKey: key)
		ifTrue: 
			[self add: (aDictionary associationAt: key).
			aDictionary removeKey: key]
		ifFalse: 
			[self add: key -> nil]! !

!Dictionary methodsFor: 'enumerating' stamp: ''!
do: aBlock

	super do: [:assoc | aBlock value: assoc value]! !

!Dictionary methodsFor: 'private' stamp: ''!
errorKeyNotFound

	self error: 'key not found'! !

!Dictionary methodsFor: 'private' stamp: ''!
errorValueNotFound

	self error: 'value not found'! !

!Dictionary methodsFor: 'user interface' stamp: 'hg 10/3/2001 20:47'!
explorerContents

	| contents |
	
	contents := OrderedCollection new.
	self keysSortedSafely do: [:key |
		contents add: (ObjectExplorerWrapper
			with: (self at: key)
			name: (key printString contractTo: 32)
			model: self)].
	^contents
! !

!Dictionary methodsFor: 'printing' stamp: 'MPW 1/4/1901 08:33'!
flattenOnStream:aStream
	^aStream writeDictionary:self.
! !

!Dictionary methodsFor: 'testing' stamp: 'tween 9/13/2004 10:11'!
hasBindingThatBeginsWith: aString
	"Answer true if the receiver has a key that begins with aString, false otherwise"
	
	self keysDo:[:each | 
		(each beginsWith: aString)
			ifTrue:[^true]].
	^false! !

!Dictionary methodsFor: 'testing' stamp: 'bf 8/20/1999 15:07'!
hasContentsInExplorer

	^self isEmpty not! !

!Dictionary methodsFor: 'testing' stamp: ''!
includes: anObject

	self do: [:each | anObject = each ifTrue: [^true]].
	^false! !

!Dictionary methodsFor: 'testing' stamp: 'ab 9/17/2004 00:39'!
includesAssociation: anAssociation
  ^ (self   
      associationAt: anAssociation key
      ifAbsent: [ ^ false ]) value = anAssociation value
! !

!Dictionary methodsFor: 'testing' stamp: 'sw 2/14/2000 14:34'!
includesIdentity: anObject
	"Answer whether anObject is one of the values of the receiver.  Contrast #includes: in which there is only an equality check, here there is an identity check"

	self do: [:each | anObject == each ifTrue: [^ true]].
	^ false! !

!Dictionary methodsFor: 'testing' stamp: 'RAA 8/23/2001 12:56'!
includesKey: key 
	"Answer whether the receiver has a key equal to the argument, key."
	
	self at: key ifAbsent: [^false].
	^true! !

!Dictionary methodsFor: 'testing' stamp: 'md 8/11/2005 16:49'!
isDictionary
	^true! !

!Dictionary methodsFor: 'private' stamp: ''!
keyAt: index
	"May be overridden by subclasses so that fixCollisions will work"
	| assn |
	assn := array at: index.
	assn == nil ifTrue: [^ nil]
				ifFalse: [^ assn key]! !

!Dictionary methodsFor: 'accessing' stamp: 'ar 2/13/1999 21:16'!
keyAtIdentityValue: value 
	"Answer the key that is the external name for the argument, value. If 
	there is none, answer nil.
	Note: There can be multiple keys with the same value. Only one is returned."

	^self keyAtIdentityValue: value ifAbsent: [self errorValueNotFound]! !

!Dictionary methodsFor: 'accessing' stamp: 'ar 2/13/1999 21:16'!
keyAtIdentityValue: value ifAbsent: exceptionBlock
	"Answer the key that is the external name for the argument, value. If 
	there is none, answer the result of evaluating exceptionBlock.
	Note: There can be multiple keys with the same value. Only one is returned."
 
	self associationsDo: 
		[:association | value == association value ifTrue: [^association key]].
	^exceptionBlock value! !

!Dictionary methodsFor: 'accessing' stamp: ''!
keyAtValue: value 
	"Answer the key that is the external name for the argument, value. If 
	there is none, answer nil."

	^self keyAtValue: value ifAbsent: [self errorValueNotFound]! !

!Dictionary methodsFor: 'accessing' stamp: 'tk 2/18/97'!
keyAtValue: value ifAbsent: exceptionBlock
	"Answer the key that is the external name for the argument, value. If 
	there is none, answer the result of evaluating exceptionBlock.
	: Use =, not ==, so stings like 'this' can be found.  Note that MethodDictionary continues to use == so it will be fast."
 
	self associationsDo: 
		[:association | value = association value ifTrue: [^association key]].
	^exceptionBlock value! !

!Dictionary methodsFor: 'testing' stamp: 'sw 3/23/2000 01:12'!
keyForIdentity: anObject
	"If anObject is one of the values of the receive, return its key, else return nil.  Contrast #keyAtValue: in which there is only an equality check, here there is an identity check"

	self associationsDo: [:assoc | assoc value == anObject ifTrue: [^ assoc key]].
	^ nil! !

!Dictionary methodsFor: 'accessing' stamp: ''!
keys
	"Answer a Set containing the receiver's keys."
	| aSet |
	aSet := Set new: self size.
	self keysDo: [:key | aSet add: key].
	^ aSet! !

!Dictionary methodsFor: 'enumerating' stamp: 'ar 7/11/1999 08:04'!
keysAndValuesDo: aBlock
	^self associationsDo:[:assoc|
		aBlock value: assoc key value: assoc value].! !

!Dictionary methodsFor: 'removing' stamp: 'di 4/4/2000 11:47'!
keysAndValuesRemove: keyValueBlock
	"Removes all entries for which keyValueBlock returns true."
	"When removing many items, you must not do it while iterating over the dictionary, since it may be changing.  This method takes care of tallying the removals in a first pass, and then performing all the deletions afterward.  Many places in the sytem could be simplified by using this method."

	| removals |
	removals := OrderedCollection new.
	self associationsDo:
		[:assoc | (keyValueBlock value: assoc key value: assoc value)
			ifTrue: [removals add: assoc key]].
 	removals do:
		[:aKey | self removeKey: aKey]! !

!Dictionary methodsFor: 'enumerating' stamp: ''!
keysDo: aBlock 
	"Evaluate aBlock for each of the receiver's keys."

	self associationsDo: [:association | aBlock value: association key]! !

!Dictionary methodsFor: 'accessing' stamp: 'sma 6/18/2000 12:56'!
keysSortedSafely
	"Answer a SortedCollection containing the receiver's keys."
	| sortedKeys |
	sortedKeys := SortedCollection new: self size.
	sortedKeys sortBlock:
		[:x :y |  "Should really be use <obj, string, num> compareSafely..."
		((x isString and: [y isString])
			or: [x isNumber and: [y isNumber]])
			ifTrue: [x < y]
			ifFalse: [x class == y class
				ifTrue: [x printString < y printString]
				ifFalse: [x class name < y class name]]].
	self keysDo: [:each | sortedKeys addLast: each].
	^ sortedKeys reSort! !

!Dictionary methodsFor: 'private' stamp: ''!
noCheckAdd: anObject
	"Must be defined separately for Dictionary because (self findElementOrNil:) expects a key, not an association.  9/7/96 tk"

	array at: (self findElementOrNil: anObject key) put: anObject.
	tally := tally + 1! !

!Dictionary methodsFor: 'testing' stamp: ''!
occurrencesOf: anObject 
	"Answer how many of the receiver's elements are equal to anObject."

	| count |
	count := 0.
	self do: [:each | anObject = each ifTrue: [count := count + 1]].
	^count! !

!Dictionary methodsFor: 'printing' stamp: 'apb 7/14/2004 12:48'!
printElementsOn: aStream 
	aStream nextPut: $(.
	self size > 100
		ifTrue: [aStream nextPutAll: 'size '.
			self size printOn: aStream]
		ifFalse: [self keysSortedSafely
				do: [:key | aStream print: key;
						 nextPutAll: '->';				
						 print: (self at: key);
						 space]].
	aStream nextPut: $)! !

!Dictionary methodsFor: 'private' stamp: ''!
rehash
	"Smalltalk rehash."
	| newSelf |
	newSelf := self species new: self size.
	self associationsDo: [:each | newSelf noCheckAdd: each].
	array := newSelf array! !

!Dictionary methodsFor: 'removing' stamp: ''!
remove: anObject

	self shouldNotImplement! !

!Dictionary methodsFor: 'removing' stamp: ''!
remove: anObject ifAbsent: exceptionBlock

	self shouldNotImplement! !

!Dictionary methodsFor: 'removing' stamp: ''!
removeKey: key 
	"Remove key from the receiver.
	If key is not in the receiver, notify an error."

	^ self removeKey: key ifAbsent: [self errorKeyNotFound]! !

!Dictionary methodsFor: 'removing' stamp: ''!
removeKey: key ifAbsent: aBlock 
	"Remove key (and its associated value) from the receiver. If key is not in 
	the receiver, answer the result of evaluating aBlock. Otherwise, answer 
	the value externally named by key."

	| index assoc |
	index := self findElementOrNil: key.
	assoc := array at: index.
	assoc == nil ifTrue: [ ^ aBlock value ].
	array at: index put: nil.
	tally := tally - 1.
	self fixCollisionsFrom: index.
	^ assoc value! !

!Dictionary methodsFor: 'removing' stamp: ''!
removeUnreferencedKeys   "Undeclared removeUnreferencedKeys"

	^ self unreferencedKeys do: [:key | self removeKey: key].! !

!Dictionary methodsFor: 'private' stamp: 'md 10/5/2005 15:42'!
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	finish := array size.
	start := (anObject hash \\ finish) + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [element key = anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [element key = anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !

!Dictionary methodsFor: 'enumerating' stamp: ''!
select: aBlock 
	"Evaluate aBlock with each of my values as the argument. Collect into a
	new dictionary, only those associations for which aBlock evaluates to
	true."

	| newCollection |
	newCollection := self species new.
	self associationsDo: 
		[:each | 
		(aBlock value: each value) ifTrue: [newCollection add: each]].
	^newCollection! !

!Dictionary methodsFor: 'printing' stamp: ''!
storeOn: aStream
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new)'.
	noneYet := true.
	self associationsDo: 
			[:each | 
			noneYet
				ifTrue: [noneYet := false]
				ifFalse: [aStream nextPut: $;].
			aStream nextPutAll: ' add: '.
			aStream store: each].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)! !

!Dictionary methodsFor: 'removing' stamp: 'dvf 8/23/2003 11:51'!
unreferencedKeys
	"TextConstants unreferencedKeys"

	| n |
	^'Scanning for references . . .' 
		displayProgressAt: Sensor cursorPoint
		from: 0
		to: self size
		during: 
			[:bar | 
			n := 0.
			self keys select: 
					[:key | 
					bar value: (n := n + 1).
					(self systemNavigation allCallsOn: (self associationAt: key)) isEmpty]]! !

!Dictionary methodsFor: 'private' stamp: ''!
valueAtNewKey: aKey put: anObject atIndex: index declareFrom: aDictionary 
	"Support for coordinating class variable and global declarations
	with variables that have been put in Undeclared so as to
	redirect all references to the undeclared variable."

	(aDictionary includesKey: aKey)
		ifTrue: 
			[self atNewIndex: index 
				put: ((aDictionary associationAt: aKey) value: anObject).
			aDictionary removeKey: aKey]
		ifFalse: 
			[self atNewIndex: index put: (Association key: aKey value: anObject)]! !

!Dictionary methodsFor: 'accessing' stamp: 'ar 7/11/1999 07:28'!
values
	"Answer a Collection containing the receiver's values."
	| out |
	out := WriteStream on: (Array new: self size).
	self valuesDo: [:value | out nextPut: value].
	^ out contents! !

!Dictionary methodsFor: 'enumerating' stamp: 'dtl 2/17/2003 09:48'!
valuesDo: aBlock 
	"Evaluate aBlock for each of the receiver's values."

	self associationsDo: [:association | aBlock value: association value]! !

Dictionary subclass: #IdentityDictionary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Unordered'!

!IdentityDictionary commentStamp: 'ls 06/15/02 22:35' prior: 0!
Like a Dictionary, except that keys are compared with #== instead of #= .

See the comment of IdentitySet for more information.!

!IdentityDictionary methodsFor: 'private' stamp: 'RAA 1/10/2001 14:57'!
fasterKeys
	"This was taking some time in publishing and we didn't really need a Set"
	| answer index |
	answer := Array new: self size.
	index := 0.
	self keysDo: [:key | answer at: (index := index + 1) put: key].
	^ answer! !

!IdentityDictionary methodsFor: 'private' stamp: 'di 12/1/1999 20:54'!
keyAtValue: value ifAbsent: exceptionBlock
	"Answer the key that is the external name for the argument, value. If 
	there is none, answer the result of evaluating exceptionBlock."
 
	self associationsDo: 
		[:association | value == association value ifTrue: [^ association key]].
	^ exceptionBlock value! !

!IdentityDictionary methodsFor: 'private' stamp: 'md 1/21/2006 20:04'!
keys
	"Answer a Set containing the receiver's keys."
	| aSet |
	aSet := IdentitySet new: self size.
	self keysDo: [:key | aSet add: key].
	^ aSet! !

!IdentityDictionary methodsFor: 'private' stamp: 'md 10/5/2005 15:41'!
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| finish hash start element |
	finish := array size.
	finish > 4096
		ifTrue: [hash := anObject identityHash * (finish // 4096)]
		ifFalse: [hash := anObject identityHash].
	start := (hash \\ finish) + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [element key == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [element key == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !

Dictionary subclass: #PluggableDictionary
	instanceVariableNames: 'hashBlock equalBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Unordered'!

!PluggableDictionary commentStamp: '<historical>' prior: 0!
Class PluggableDictionary allows the redefinition of hashing and equality by clients. This is in particular useful if the clients know about specific properties of the objects stored in the dictionary. See the class comment of PluggableSet for an example.

Instance variables:
	hashBlock	<BlockContext>	A one argument block used for hashing the elements.
	equalBlock	<BlockContext>	A two argument block used for comparing the elements.
!

!PluggableDictionary class methodsFor: 'as yet unclassified' stamp: 'dvf
6/10/2000 18:13'!
integerDictionary
	^ self new hashBlock: [:integer | integer hash \\ 1064164 * 1009]! !

!PluggableDictionary methodsFor: 'copying' stamp: 'ar 11/12/1998 18:48'!
copy
	^super copy postCopyBlocks! !

!PluggableDictionary methodsFor: 'accessing' stamp: 'ar 11/12/1998 18:46'!
equalBlock
	"Return the block used for comparing the elements in the receiver."
	^equalBlock! !

!PluggableDictionary methodsFor: 'accessing' stamp: 'ar 11/27/1998 23:55'!
equalBlock: aBlock
	"Set a new equality block. The block must accept two arguments and return true if the argumets are considered to be equal, false otherwise"
	equalBlock := aBlock.! !

!PluggableDictionary methodsFor: 'accessing' stamp: 'ar 11/12/1998 18:46'!
hashBlock
	"Return the block used for hashing the elements in the receiver."
	^hashBlock! !

!PluggableDictionary methodsFor: 'accessing' stamp: 'ar 11/12/1998 18:46'!
hashBlock: aBlock
	"Set a new hash block. The block must accept one argument and must return the hash value of the given argument."
	hashBlock := aBlock.! !

!PluggableDictionary methodsFor: 'accessing' stamp: 'dvf 6/10/2000
19:34'!
keys
	"Answer a Set containing the receiver's keys."
	| aSet |
	aSet := PluggableSet new: self size.
	self equalBlock ifNotNil: [aSet equalBlock: self equalBlock fixTemps].
	self hashBlock ifNotNil: [aSet hashBlock: self hashBlock fixTemps].
	self keysDo: [:key | aSet add: key].
	^ aSet! !

!PluggableDictionary methodsFor: 'copying' stamp: 'dvf 6/10/2000 19:35'!
postCopyBlocks
	hashBlock := hashBlock copy.
	equalBlock := equalBlock copy.
	"Fix temps in case we're referring to outside stuff"
	hashBlock ifNotNil: [hashBlock fixTemps].
	equalBlock ifNotNil: [equalBlock fixTemps]! !

!PluggableDictionary methodsFor: 'private' stamp: 'dvf 6/11/2000 01:33'!
scanFor: anObject 
	"Scan the key array for the first slot containing either a nil
(indicating 
	  an empty slot) or an element that matches anObject. Answer the index 
	  
	of that slot or zero if no slot is found. This  method will be
overridden   
	in various subclasses that have different interpretations for matching 
 
	elements."
	| element start finish |
	start := (hashBlock ifNil: [anObject hash]
				ifNotNil: [hashBlock value: anObject])
				\\ array size + 1.
	finish := array size.
	"Search from (hash mod size) to the end."
	start to: finish do: [:index | ((element := array at: index) == nil or:
[equalBlock ifNil: [element key = anObject]
				ifNotNil: [equalBlock value: element key value: anObject]])
			ifTrue: [^ index]].
	"Search from 1 to where we started."
	1 to: start - 1 do: [:index | ((element := array at: index) == nil or:
[equalBlock ifNil: [element key = anObject]
				ifNotNil: [equalBlock value: element key value: anObject]])
			ifTrue: [^ index]].
	^ 0"No match AND no empty slot"! !

Dictionary subclass: #WeakKeyDictionary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Weak'!

!WeakKeyDictionary commentStamp: '<historical>' prior: 0!
I am a dictionary holding only weakly on my keys. This is a bit dangerous since at any time my keys can go away. Clients are responsible to register my instances by WeakArray such that the appropriate actions can be taken upon loss of any keys.

See WeakRegistry for an example of use.
!

WeakKeyDictionary subclass: #WeakIdentityKeyDictionary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Weak'!

!WeakIdentityKeyDictionary commentStamp: '<historical>' prior: 0!
This class represents an identity dictionary with weak keys.!

!WeakIdentityKeyDictionary methodsFor: 'private' stamp: 'md 10/5/2005 15:44'!
scanFor: anObject
	"ar 10/21/2000: The method has been copied to this location to indicate that whenever #scanFor: changes #scanForNil: must be changed in the receiver as well."
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish hash |
	finish := array size.
	finish > 4096
		ifTrue: [hash := anObject identityHash * (finish // 4096)]
		ifFalse: [hash := anObject identityHash].
	start := (hash \\ finish) + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [element key == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [element key == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !

!WeakIdentityKeyDictionary methodsFor: 'private' stamp: 'ar 7/1/2003 15:15'!
scanForNil: anObject
	"Private. Scan the key array for the first slot containing nil (indicating an empty slot). Answer the index of that slot."
	| start finish hash |
	finish := array size.
	finish > 4096
		ifTrue: [hash := anObject identityHash * (finish // 4096)]
		ifFalse: [hash := anObject identityHash].
	start := (hash \\ array size) + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | (array at: index) == nil ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | (array at: index) == nil ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !

WeakIdentityKeyDictionary subclass: #WeakKeyToCollectionDictionary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Weak'!

!WeakKeyToCollectionDictionary commentStamp: '<historical>' prior: 0!
This class represents an identity dictionary with weak keys, whose values are collections. 
Keys not in the dictionary are mapped to the empty collection.  Conversely, if a collection becomes empty, the mapping can be removed to save time and space.  However, because this requires re-hashing, it does not pay to do this to eagerly.!

!WeakKeyToCollectionDictionary methodsFor: 'as yet unclassified' stamp: 'apb 7/12/2004 23:47'!
finalizeValues 
	self rehash! !

!WeakKeyToCollectionDictionary methodsFor: 'as yet unclassified' stamp: 'apb 7/13/2004 00:17'!
rehash
	"Rehash the receiver. Reimplemented to remove nils from the collections
	that appear as values, and to entirely remove associations with empty collections 
	as values."
	| oldArray assoc cleanedValue newIndex |
	oldArray := array.
	array := Array new: oldArray size.
	tally := 0.
	1 to: array size do: [:i | 
			assoc := oldArray at: i.
			(assoc notNil
					and: [(cleanedValue := assoc value copyWithout: nil) notEmpty])
				ifTrue: [newIndex := self scanForNil: assoc key.
					assoc value: cleanedValue.
					self atNewIndex: newIndex put: assoc]]! !

!WeakKeyDictionary methodsFor: 'adding' stamp: 'ar 3/21/98 16:00'!
add: anAssociation
	self at: anAssociation key put: anAssociation value.
	^ anAssociation! !

!WeakKeyDictionary methodsFor: 'accessing' stamp: 'ar 3/21/98 16:02'!
at: key put: anObject 
	"Set the value at key to be anObject.  If key is not found, create a new
	entry for key and set is value to anObject. Answer anObject."
	| index element |
	key isNil ifTrue:[^anObject].
	index := self findElementOrNil: key.
	element := array at: index.
	element == nil
		ifTrue: [self atNewIndex: index put: (WeakKeyAssociation key: key value: anObject)]
		ifFalse: [element value: anObject].
	^ anObject! !

!WeakKeyDictionary methodsFor: 'finalization' stamp: 'ar 10/21/2000 20:00'!
finalizeValues
	"remove all nil keys and rehash the receiver afterwards"
	| assoc |
	1 to: array size do:[:i|
		assoc := array at: i.
		(assoc notNil and:[assoc key == nil]) ifTrue:[array at: i put: nil].
	].
	self rehash.! !

!WeakKeyDictionary methodsFor: 'finalization' stamp: 'ar 10/21/2000 20:01'!
finalizeValues: finiObjects
	"Remove all associations with key == nil and value is in finiObjects.
	This method is folded with #rehash for efficiency."
	| oldArray assoc newIndex |
	oldArray := array.
	array := Array new: oldArray size.
	tally := 0.
	1 to: array size do:[:i|
		assoc := oldArray at: i.
		assoc ifNotNil:[
			(assoc key == nil and:[finiObjects includes: assoc value]) ifFalse:[
				newIndex := self scanForNil: assoc key.
				self atNewIndex: newIndex put: assoc].
		].
	].! !

!WeakKeyDictionary methodsFor: 'private' stamp: 'ar 10/21/2000 19:58'!
fixCollisionsFrom: oldIndex
	"The element at index has been removed and replaced by nil."
	self rehash. "Do it the hard way - we may have any number of nil keys and #rehash deals with them"! !

!WeakKeyDictionary methodsFor: 'accessing' stamp: 'ar 2/11/2001 02:21'!
keysDo: aBlock 
	"Evaluate aBlock for each of the receiver's keys."
	self associationsDo: [:association | 
		association key ifNotNil:[aBlock value: association key]].! !

!WeakKeyDictionary methodsFor: 'private' stamp: 'ar 10/21/2000 19:56'!
rehash
	"Rehash the receiver. Reimplemented to allow for multiple nil keys"
	| oldArray assoc newIndex |
	oldArray := array.
	array := Array new: oldArray size.
	tally := 0.
	1 to: array size do:[:i|
		assoc := oldArray at: i.
		assoc ifNotNil:[
			newIndex := self scanForNil: assoc key.
			self atNewIndex: newIndex put: assoc.
		].
	].! !

!WeakKeyDictionary methodsFor: 'private' stamp: 'md 10/5/2005 15:44'!
scanFor: anObject
	"ar 10/21/2000: The method has been copied to this location to indicate that whenever #scanFor: changes #scanForNil: must be changed in the receiver as well."
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	finish := array size.
	start := (anObject hash \\ finish) + 1.
	
	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [element key = anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [element key = anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !

!WeakKeyDictionary methodsFor: 'private' stamp: 'ar 10/21/2000 19:46'!
scanForNil: anObject
	"Private. Scan the key array for the first slot containing nil (indicating an empty slot). Answer the index of that slot."
	| start finish |
	start := (anObject hash \\ array size) + 1.
	finish := array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | (array at: index) == nil ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | (array at: index) == nil ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !

Dictionary subclass: #WeakValueDictionary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Weak'!

!WeakValueDictionary commentStamp: '<historical>' prior: 0!
I am a dictionary holding only weakly on my values. Clients may expect to get a nil value for any object they request.!

!WeakValueDictionary methodsFor: 'adding' stamp: 'ar 3/21/98 16:02'!
add: anAssociation
	self at: anAssociation key put: anAssociation value.
	^ anAssociation! !

!WeakValueDictionary methodsFor: 'accessing' stamp: 'ar 3/21/98 16:01'!
at: key put: anObject 
	"Set the value at key to be anObject.  If key is not found, create a new
	entry for key and set is value to anObject. Answer anObject."
	| index element |
	index := self findElementOrNil: key.
	element := array at: index.
	element == nil
		ifTrue: [self atNewIndex: index put: (WeakValueAssociation key: key value: anObject)]
		ifFalse: [element value: anObject].
	^ anObject! !

Set subclass: #IdentitySet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Unordered'!

!IdentitySet commentStamp: 'sw 1/14/2003 22:35' prior: 0!
The same as a Set, except that items are compared using #== instead of #=.

Almost any class named IdentityFoo is the same as Foo except for the way items are compared.  In Foo, #= is used, while in IdentityFoo, #== is used.  That is, identity collections will treat items as the same only if they have the same identity.

For example, note that copies of a string are equal:

	('abc' copy) = ('abc' copy)

but they are not identitcal:

	('abc' copy) == ('abc' copy)

A regular Set will only include equal objects once:

	| aSet |
	aSet := Set new.
	aSet add: 'abc' copy.
	aSet add: 'abc' copy.
	aSet


An IdentitySet will include multiple equal objects if they are not identical:

	| aSet |
	aSet := IdentitySet new.
	aSet add: 'abc' copy.
	aSet add: 'abc' copy.
	aSet
!

!IdentitySet methodsFor: 'converting' stamp: 'ar 9/22/2000 10:13'!
asIdentitySet
	^self! !

!IdentitySet methodsFor: 'private' stamp: 'md 10/5/2005 15:42'!
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| finish hash start element |
	finish := array size.
	finish > 4096
		ifTrue: [hash := anObject identityHash * (finish // 4096)]
		ifFalse: [hash := anObject identityHash].
	start := (hash \\ finish) + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [element == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [element == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !

Set subclass: #KeyedSet
	instanceVariableNames: 'keyBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Unordered'!

!KeyedSet commentStamp: '<historical>' prior: 0!
Like Set except a key of every element is used for hashing and searching instead of the element itself.  keyBlock gets the key of an element.!

KeyedSet subclass: #KeyedIdentitySet
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Unordered'!

!KeyedIdentitySet methodsFor: 'private' stamp: 'md 10/5/2005 15:43'!
scanFor: anObject
	"Same as super except change = to ==, and hash to identityHash"

	| element start finish |
	finish := array size.
	start := (anObject identityHash \\ finish) + 1.
	

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [(keyBlock value: element) == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [(keyBlock value: element) == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !

!KeyedSet class methodsFor: 'instance creation' stamp: 'ajh 10/23/2000 23:16'!
keyBlock: oneArgBlock
	"Create a new KeySet whose way to access an element's key is by executing oneArgBlock on the element"

	^ self new keyBlock: oneArgBlock! !

!KeyedSet methodsFor: 'adding' stamp: 'md 3/14/2006 12:37'!
add: newObject
	"Include newObject as one of the receiver's elements, but only if
	not already present. Answer newObject."

	| index |
	newObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
	index := self findElementOrNil: (keyBlock value: newObject).
	(array at: index) ifNil: [self atNewIndex: index put: newObject].
	^ newObject! !

!KeyedSet methodsFor: 'adding' stamp: 'ajh 12/4/2001 05:27'!
addAll: aCollection 
	"Include all the elements of aCollection as the receiver's elements"

	(aCollection respondsTo: #associationsDo:)
		ifTrue: [aCollection associationsDo: [:ass | self add: ass]]
		ifFalse: [aCollection do: [:each | self add: each]].
	^ aCollection! !

!KeyedSet methodsFor: 'accessing' stamp: 'ajh 9/5/2000 03:57'!
at: key 
	"Answer the value associated with the key."

	^ self at: key ifAbsent: [self errorKeyNotFound]! !

!KeyedSet methodsFor: 'accessing' stamp: 'ajh 10/6/2000 20:28'!
at: key ifAbsent: aBlock 
	"Answer the value associated with the key or, if key isn't found,
	answer the result of evaluating aBlock."

	| obj |
	obj := array at: (self findElementOrNil: key).
	obj ifNil: [^ aBlock value].
	^ obj! !

!KeyedSet methodsFor: 'accessing' stamp: 'ajh 12/10/2000 15:42'!
at: key ifAbsentPut: aBlock 
	"Answer the value associated with the key or, if key isn't found,
	add the result of evaluating aBlock to self"

	^ self at: key ifAbsent: [self add: aBlock value]! !

!KeyedSet methodsFor: 'accessing' stamp: 'ajh 9/5/2000 03:58'!
at: key ifPresent: aBlock
	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v := self at: key ifAbsent: [^ nil].
	^ aBlock value: v
! !

!KeyedSet methodsFor: 'copying' stamp: 'ajh 9/5/2000 03:56'!
copy
	^super copy postCopyBlocks! !

!KeyedSet methodsFor: 'private' stamp: 'ajh 3/29/2001 19:04'!
errorKeyNotFound

	self error: 'key not found'! !

!KeyedSet methodsFor: 'private' stamp: 'ajh 9/5/2000 03:44'!
fixCollisionsFrom: index
	"The element at index has been removed and replaced by nil.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one"
	| length oldIndex newIndex element |
	oldIndex := index.
	length := array size.
	[oldIndex = length
			ifTrue: [oldIndex :=  1]
			ifFalse: [oldIndex :=  oldIndex + 1].
	(element := self keyAt: oldIndex) == nil]
		whileFalse: 
			[newIndex := self findElementOrNil: (keyBlock value: element).
			oldIndex = newIndex ifFalse: [self swap: oldIndex with: newIndex]]! !

!KeyedSet methodsFor: 'testing' stamp: 'ajh 9/5/2000 03:45'!
includes: anObject 
	^ (array at: (self findElementOrNil: (keyBlock value: anObject))) ~~ nil! !

!KeyedSet methodsFor: 'testing' stamp: 'ajh 3/29/2001 23:56'!
includesKey: key

	^ (array at: (self findElementOrNil: key)) ~~ nil! !

!KeyedSet methodsFor: 'private' stamp: 'nice 4/4/2006 22:09'!
initialize: n
	super initialize: n.
	keyBlock := [:element | element key].
! !

!KeyedSet methodsFor: 'initialize' stamp: 'ajh 9/5/2000 03:36'!
keyBlock: oneArgBlock
	"When evaluated return the key of the argument which will be an element of the set"

	keyBlock := oneArgBlock! !

!KeyedSet methodsFor: 'accessing' stamp: 'ajh 7/3/2004 17:55'!
keys

	| keys |
	keys := Set new.
	self keysDo: [:key | keys add: key].
	^ keys! !

!KeyedSet methodsFor: 'accessing' stamp: 'ajh 7/3/2004 17:54'!
keysDo: block

	self do: [:item | block value: (keyBlock value: item)]! !

!KeyedSet methodsFor: 'accessing' stamp: 'ajh 5/11/2002 13:28'!
keysSorted

	| keys |
	keys := SortedCollection new.
	self do: [:item | keys add: (keyBlock value: item)].
	^ keys! !

!KeyedSet methodsFor: 'adding' stamp: 'ajh 6/3/2002 10:11'!
member: newObject
	"Include newObject as one of the receiver's elements, if already exists just return it"

	| index |
	newObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
	index := self findElementOrNil: (keyBlock value: newObject).
	(array at: index) ifNotNil: [^ array at: index].
	self atNewIndex: index put: newObject.
	^ newObject! !

!KeyedSet methodsFor: 'private' stamp: 'ajh 9/5/2000 03:46'!
noCheckAdd: anObject
	array at: (self findElementOrNil: (keyBlock value: anObject)) put: anObject.
	tally := tally + 1! !

!KeyedSet methodsFor: 'copying' stamp: 'ajh 9/5/2000 03:56'!
postCopyBlocks
	keyBlock := keyBlock copy.
	"Fix temps in case we're referring to outside stuff"
	keyBlock fixTemps.! !

!KeyedSet methodsFor: 'private' stamp: 'ajh 12/13/2001 00:17'!
rehash
	| newSelf |
	newSelf := self species new: self size.
	newSelf keyBlock: keyBlock.
	self do: [:each | newSelf noCheckAdd: each].
	array := newSelf array! !

!KeyedSet methodsFor: 'removing' stamp: 'ajh 9/5/2000 03:47'!
remove: oldObject ifAbsent: aBlock

	| index |
	index := self findElementOrNil: (keyBlock value: oldObject).
	(array at: index) == nil ifTrue: [ ^ aBlock value ].
	array at: index put: nil.
	tally := tally - 1.
	self fixCollisionsFrom: index.
	^ oldObject! !

!KeyedSet methodsFor: 'removing' stamp: 'ajh 3/29/2001 19:03'!
removeKey: key 

	^ self removeKey: key ifAbsent: [self errorKeyNotFound]! !

!KeyedSet methodsFor: 'removing' stamp: 'ajh 3/29/2001 19:03'!
removeKey: key ifAbsent: aBlock

	| index obj |
	index := self findElementOrNil: key.
	(obj := array at: index) == nil ifTrue: [ ^ aBlock value ].
	array at: index put: nil.
	tally := tally - 1.
	self fixCollisionsFrom: index.
	^ obj! !

!KeyedSet methodsFor: 'private' stamp: 'md 10/5/2005 15:43'!
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	finish := array size.
	start := (anObject hash \\ finish) + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [(keyBlock value: element) = anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [(keyBlock value: element) = anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !

Set subclass: #PluggableSet
	instanceVariableNames: 'hashBlock equalBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Unordered'!

!PluggableSet commentStamp: '<historical>' prior: 0!
PluggableSets allow the redefinition of hashing and equality by clients. This is in particular useful if the clients know about specific properties of the objects stored in the set which in turn can heavily improve the performance of sets and dictionaries.

Instance variables:
	hashBlock	<BlockContext>	A one argument block used for hashing the elements.
	equalBlock	<BlockContext>	A two argument block used for comparing the elements.

Example: Adding 1000 integer points in the range (0@0) to: (100@100) to a set.

	| rnd set max pt |
	set _ Set new: 1000.
	rnd _ Random new.
	max _ 100.
	Time millisecondsToRun:[
		1 to: 1000 do:[:i|
			pt _ (rnd next * max) truncated @ (rnd next * max) truncated.
			set add: pt.
		].
	].

The above is way slow since the default hashing function of points leads to an awful lot of collisions in the set. And now the same, with a somewhat different hash function:

	| rnd set max pt |
	set _ PluggableSet new: 1000.
	set hashBlock:[:item| (item x bitShift: 16) + item y].
	rnd _ Random new.
	max _ 100.
	Time millisecondsToRun:[
		1 to: 1000 do:[:i|
			pt _ (rnd next * max) truncated @ (rnd next * max) truncated.
			set add: pt.
		].
	].
!

!PluggableSet class methodsFor: 'as yet unclassified' stamp: 'dvf
6/10/2000 18:13'!
integerSet
	^self new hashBlock: [:integer | integer hash \\ 1064164 * 1009]! !

!PluggableSet methodsFor: 'copying' stamp: 'ar 11/12/1998 18:47'!
copy
	^super copy postCopyBlocks! !

!PluggableSet methodsFor: 'accessing' stamp: 'ar 11/12/1998 18:43'!
equalBlock
	"Return the block used for comparing the elements in the receiver."
	^equalBlock! !

!PluggableSet methodsFor: 'accessing' stamp: 'ar 11/27/1998 23:55'!
equalBlock: aBlock
	"Set a new equality block. The block must accept two arguments and return true if the argumets are considered equal, false otherwise"
	equalBlock := aBlock.! !

!PluggableSet methodsFor: 'accessing' stamp: 'ar 11/12/1998 18:43'!
hashBlock
	"Return the block used for hashing the elements in the receiver."
	^hashBlock! !

!PluggableSet methodsFor: 'accessing' stamp: 'ar 11/12/1998 19:02'!
hashBlock: aBlock
	"Set a new hash block. The block must accept one argument and return the hash value of the given argument."
	hashBlock := aBlock.! !

!PluggableSet methodsFor: 'copying' stamp: 'dvf 6/10/2000 19:34'!
postCopyBlocks
	hashBlock := hashBlock copy.
	equalBlock := equalBlock copy.
	"Fix temps in case we're referring to outside stuff"
	hashBlock ifNotNil: [hashBlock fixTemps].
	equalBlock ifNotNil: [equalBlock fixTemps]! !

!PluggableSet methodsFor: 'private' stamp: 'dvf 6/11/2000 00:54'!
scanFor: anObject 
	"Scan the key array for the first slot containing either a nil
(indicating 
	  an empty slot) or an element that matches anObject. Answer the index 
	  
	of that slot or zero if no slot is found. This  method will be
overridden   
	in various subclasses that have different interpretations for matching 
 
	elements."
	| element start finish |
	start := (hashBlock ifNil: [anObject hash]
				ifNotNil: [hashBlock value: anObject])
				\\ array size + 1.
	finish := array size.
	"Search from (hash mod size) to the end."
	start to: finish do: [:index | ((element := array at: index) == nil or:
[equalBlock ifNil: [element = anObject]
				ifNotNil: [equalBlock value: element value: anObject]])
			ifTrue: [^ index]].
	"Search from 1 to where we started."
	1 to: start - 1 do: [:index | ((element := array at: index) == nil or:
[equalBlock ifNil: [element = anObject]
				ifNotNil: [equalBlock value: element value: anObject]])
			ifTrue: [^ index]].
	^ 0"No match AND no empty slot"! !

!Set class methodsFor: 'instance creation' stamp: 'pmm 7/9/2006 11:46'!
new
	^ self basicNew initialize: 5! !

!Set class methodsFor: 'instance creation' stamp: 'nice 4/4/2006 22:10'!
new: nElements
	"Create a Set large enough to hold nElements without growing"
	^ super basicNew initialize: (self sizeFor: nElements)! !

!Set class methodsFor: 'instance creation' stamp: ''!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."
	| newCollection |
	newCollection := self new: aCollection size.
	newCollection addAll: aCollection.
	^ newCollection
"
	Set newFrom: {1. 2. 3}
	{1. 2. 3} as: Set
"! !

!Set class methodsFor: 'initialization' stamp: 'SqR 8/3/2000 13:19'!
quickRehashAllSets  "Set rehashAllSets"
	| insts |
	self withAllSubclassesDo:
		[:c |
			insts := c allInstances.
			(insts isEmpty or: [c = MethodDictionary]) ifFalse:
			['Rehashing instances of ' , c name
				displayProgressAt: Sensor cursorPoint
				from: 1 to: insts size
				during: [:bar | 1 to: insts size do: [:x | bar value: x. (insts at: x) rehash]]
			]
		]! !

!Set class methodsFor: 'initialization' stamp: 'pmm 7/9/2006 11:48'!
rehashAllSets
	"Set rehashAllSets"	
	self withAllSubclassesDo: [ :setClass |
		| instances |
		instances := setClass allInstances.
		instances isEmpty ifFalse: [
			1 to: instances size do: [ :index |
				(instances at: index) rehash ] ] ]! !

!Set class methodsFor: 'instance creation' stamp: ''!
sizeFor: nElements
	"Large enough size to hold nElements with some slop (see fullCheck)"
	nElements <= 0 ifTrue: [^ 1].
	^ nElements+1*4//3! !

!Set methodsFor: 'testing' stamp: 'tk 11/8/2001 15:35'!
= aSet
	self == aSet ifTrue: [^ true].	"stop recursion"
	(aSet isKindOf: Set) ifFalse: [^ false].
	self size = aSet size ifFalse: [^ false].
	self do: [:each | (aSet includes: each) ifFalse: [^ false]].
	^ true! !

!Set methodsFor: 'adding' stamp: 'sma 5/12/2000 17:28'!
add: newObject
	"Include newObject as one of the receiver's elements, but only if
	not already present. Answer newObject."

	| index |
	newObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
	index := self findElementOrNil: newObject.
	(array at: index) ifNil: [self atNewIndex: index put: newObject].
	^ newObject! !

!Set methodsFor: 'adding' stamp: 'sma 5/12/2000 17:29'!
add: newObject withOccurrences: anInteger
	^ self add: newObject! !

!Set methodsFor: 'private' stamp: ''!
array
	^ array! !

!Set methodsFor: 'converting' stamp: 'ar 11/20/1998 16:34'!
asSet
	^self! !

!Set methodsFor: 'private' stamp: ''!
atNewIndex: index put: anObject
	array at: index put: anObject.
	tally := tally + 1.
	self fullCheck! !

!Set methodsFor: 'accessing' stamp: 'md 7/31/2005 08:56'!
atRandom: aGenerator
	"Answer a random element of the receiver. Uses aGenerator which
    should be kept by the user in a variable and used every time. Use
    this instead of #atRandom for better uniformity of random numbers because 
	only you use the generator. Causes an error if self has no elements."
	| rand |

	self emptyCheck.
	rand := aGenerator nextInt: self size.
	self doWithIndex:[:each :ind |
		ind == rand ifTrue:[^each]].
	^ self errorEmptyCollection
! !

!Set methodsFor: 'accessing' stamp: 'sma 5/12/2000 11:40'!
capacity
	"Answer the current capacity of the receiver."

	^ array size! !

!Set methodsFor: 'enumerating' stamp: 'sma 5/12/2000 11:49'!
collect: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into a collection like the receiver. Answer  
	the new collection."

	| newSet |
	newSet := Set new: self size.
	array do: [:each | each ifNotNil: [newSet add: (aBlock value: each)]].
	^ newSet! !

!Set methodsFor: 'objects from disk' stamp: 'tk 4/8/1999 13:05'!
comeFullyUpOnReload: smartRefStream
	"Symbols have new hashes in this image."

	self rehash.
	"^ self"
! !

!Set methodsFor: 'copying' stamp: 'sma 5/12/2000 14:54'!
copy
	^ self shallowCopy withArray: array shallowCopy! !

!Set methodsFor: 'removing' stamp: 'sma 5/12/2000 14:45'!
copyWithout: oldElement 
	"Answer a copy of the receiver that does not contain any
	elements equal to oldElement."

	^ self copy
		remove: oldElement ifAbsent: [];
		yourself! !

!Set methodsFor: 'enumerating' stamp: 'sma 5/12/2000 14:36'!
do: aBlock 
	tally = 0 ifTrue: [^ self].
	1 to: array size do:
		[:index |
		| each |
		(each := array at: index) ifNotNil: [aBlock value: each]]! !

!Set methodsFor: 'enumerating' stamp: ''!
doWithIndex: aBlock2
	"Support Set enumeration with a counter, even though not ordered"
	| index |
	index := 0.
	self do: [:item | aBlock2 value: item value: (index := index+1)]! !

!Set methodsFor: 'explorer' stamp: 'hg 9/7/2001 11:51'!
explorerContents 

	^self asOrderedCollection withIndexCollect: [:each :index |
		ObjectExplorerWrapper
			with: each
			name: index printString
			model: self]! !

!Set methodsFor: 'private' stamp: 'SqR 8/23/2000 14:39'!
findElementOrNil: anObject
	"Answer the index of a first slot containing either a nil (indicating an empty slot) or an element that matches the given object. Answer the index of that slot or zero. Fail if neither a match nor an empty slot is found."

	| index |

	index := self scanFor: anObject.
	index > 0 ifTrue: [^index].

	"Bad scene.  Neither have we found a matching element
	nor even an empty slot.  No hashed set is ever supposed to get
	completely full."
	self error: 'There is no free space in this set!!'.! !

!Set methodsFor: 'private' stamp: 'SqR 8/23/2000 14:28'!
fixCollisionsFrom: index
	"The element at index has been removed and replaced by nil.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one"

	| length oldIndex newIndex element |

	oldIndex := index.
	length := array size.
	[oldIndex = length
			ifTrue: [oldIndex := 1]
			ifFalse: [oldIndex := oldIndex + 1].
	(element := self keyAt: oldIndex) == nil]
		whileFalse: 
			[newIndex := self findElementOrNil: element.
			oldIndex = newIndex ifFalse: [self swap: oldIndex with: newIndex]]! !

!Set methodsFor: 'private' stamp: 'di 11/4/97 20:11'!
fullCheck
	"Keep array at least 1/4 free for decent hash behavior"
	array size - tally < (array size // 4 max: 1)
		ifTrue: [self grow]! !

!Set methodsFor: 'private' stamp: ''!
grow
	"Grow the elements array and reinsert the old elements"
	| oldElements |
	oldElements := array.
	array := Array new: array size + self growSize.
	tally := 0.
	oldElements do:
		[:each | each == nil ifFalse: [self noCheckAdd: each]]! !

!Set methodsFor: 'private' stamp: ''!
growSize
	^ array size max: 2! !

!Set methodsFor: 'explorer' stamp: 'hg 9/7/2001 11:51'!
hasContentsInExplorer

	^self isEmpty not! !

!Set methodsFor: 'testing' stamp: ''!
includes: anObject 
	^ (array at: (self findElementOrNil: anObject)) ~~ nil! !

!Set methodsFor: 'private' stamp: 'nice 4/4/2006 22:09'!
initialize: n
	"Initialize array to an array size of n"
	array := Array new: n.
	tally := 0! !

!Set methodsFor: 'private' stamp: ''!
keyAt: index
	"May be overridden by subclasses so that fixCollisions will work"
	^ array at: index! !

!Set methodsFor: 'accessing' stamp: 'SqR 8/23/2000 13:51'!
like: anObject
	"Answer an object in the receiver that is equal to anObject,
	nil if no such object is found. Relies heavily on hash properties"

	| index |

	^(index := self scanFor: anObject) = 0
		ifFalse: [array at: index]! !

!Set methodsFor: 'private' stamp: ''!
noCheckAdd: anObject
	array at: (self findElementOrNil: anObject) put: anObject.
	tally := tally + 1! !

!Set methodsFor: 'testing' stamp: 'sma 5/12/2000 14:46'!
occurrencesOf: anObject 
	^ (self includes: anObject) ifTrue: [1] ifFalse: [0]! !

!Set methodsFor: 'private' stamp: ''!
rehash
	| newSelf |
	newSelf := self species new: self size.
	self do: [:each | newSelf noCheckAdd: each].
	array := newSelf array! !

!Set methodsFor: 'removing' stamp: ''!
remove: oldObject ifAbsent: aBlock

	| index |
	index := self findElementOrNil: oldObject.
	(array at: index) == nil ifTrue: [ ^ aBlock value ].
	array at: index put: nil.
	tally := tally - 1.
	self fixCollisionsFrom: index.
	^ oldObject! !

!Set methodsFor: 'private' stamp: 'md 10/5/2005 15:44'!
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	finish := array size.
	start := (anObject hash \\ finish) + 1.
	
	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [element = anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [element = anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !

!Set methodsFor: 'accessing' stamp: ''!
size
	^ tally! !

!Set methodsFor: 'accessing' stamp: 'sma 5/12/2000 14:34'!
someElement
	"Deprecated. Use anyOne."

	^ self anyOne! !

!Set methodsFor: 'private' stamp: ''!
swap: oneIndex with: otherIndex
	"May be overridden by subclasses so that fixCollisions will work"

	array swap: oneIndex with: otherIndex
! !

!Set methodsFor: 'enumerating' stamp: 'jcg 6/7/2003 02:01'!
union: aCollection
	"Answer the set theoretic union of the receiver and aCollection, using the receiver's notion of equality and not side effecting the receiver at all."

	^ self copy addAll: aCollection; yourself

! !

!Set methodsFor: 'private' stamp: ''!
withArray: anArray
	"private -- for use only in copy"
	array := anArray! !

Set subclass: #WeakSet
	instanceVariableNames: 'flag'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Weak'!

!WeakSet methodsFor: 'public' stamp: 'SqR 8/30/2000 13:15'!
add: newObject
	"Include newObject as one of the receiver's elements, but only if
	not already present. Answer newObject"

	| index |
	newObject ifNil: [self error: 'Sets cannot meaningfully contain nil as an element'].
	index := self findElementOrNil: newObject.
	((array at: index) == flag or: [(array at: index) isNil])
		ifTrue: [self atNewIndex: index put: newObject].
	^newObject! !

!WeakSet methodsFor: 'public' stamp: 'nk 3/11/2002 20:35'!
collect: aBlock
	| each newSet |
	newSet := self species new: self size.
	tally = 0 ifTrue: [^newSet ].
	1 to: array size do:
		[:index |
			((each := array at: index) == nil or: [each == flag])
				ifFalse: [newSet add: (aBlock value: each)]
		].
	^newSet! !

!WeakSet methodsFor: 'public' stamp: 'SqR 8/23/2000 15:46'!
do: aBlock
	| each |

	tally = 0 ifTrue: [^self].
	1 to: array size do:
		[:index |
			((each := array at: index) == nil or: [each == flag])
				ifFalse: [aBlock value: each]
		]! !

!WeakSet methodsFor: 'public' stamp: 'SqR 8/30/2000 13:13'!
do: aBlock after: anElement
	| each startIndex |

	tally = 0 ifTrue: [^self].
	startIndex := anElement ifNil: [1] ifNotNil:
		[self findElementOrNil: anElement].
	startIndex + 1 to: array size do:
		[:index |
			((each := array at: index) == nil or: [each == flag])
				ifFalse: [aBlock value: each]
		]! !

!WeakSet methodsFor: 'private' stamp: 'SqR 8/23/2000 14:30'!
fixCollisionsFrom: index
	"The element at index has been removed and replaced by nil.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one"

	| length oldIndex newIndex element |

	oldIndex := index.
	length := array size.
	[oldIndex = length
			ifTrue: [oldIndex := 1]
			ifFalse: [oldIndex := oldIndex + 1].
	(element := self keyAt: oldIndex) == flag]
		whileFalse: 
			[newIndex := self findElementOrNil: element.
			oldIndex = newIndex ifFalse: [self swap: oldIndex with: newIndex]]! !

!WeakSet methodsFor: 'private' stamp: 'SqR 8/23/2000 15:43'!
grow
	"Grow the elements array and reinsert the old elements"

	self growTo: array size + self growSize! !

!WeakSet methodsFor: 'private' stamp: 'SqR 8/23/2000 15:43'!
growTo: anInteger
	"Grow the elements array and reinsert the old elements"

	| oldElements |

	oldElements := array.
	array := WeakArray new: anInteger.
	array atAllPut: flag.
	tally := 0.
	oldElements do:
		[:each | (each == flag or: [each == nil]) ifFalse: [self noCheckAdd: each]]! !

!WeakSet methodsFor: 'public' stamp: 'SqR 8/30/2000 13:15'!
includes: anObject 
	^(array at: (self findElementOrNil: anObject)) ~~ flag! !

!WeakSet methodsFor: 'private' stamp: 'nice 4/4/2006 22:09'!
initialize: n
	"Initialize array to an array size of n"

	flag := Object new.
	array := WeakArray new: n.
	array atAllPut: flag.
	tally := 0! !

!WeakSet methodsFor: 'public' stamp: 'SqR 8/23/2000 16:02'!
like: anObject
	"Answer an object in the receiver that is equal to anObject,
	nil if no such object is found. Relies heavily on hash properties"

	| index element |

	^(index := self scanFor: anObject) = 0
		ifFalse: [(element := array at: index) == flag ifFalse: [element]]! !

!WeakSet methodsFor: 'public' stamp: 'di 2/3/2001 16:46'!
printElementsOn: aStream
	| oldPos |
	aStream nextPut: $(.
	oldPos := aStream position.
	self do: [:element | aStream print: element; space].
	aStream position > oldPos ifTrue: [aStream skip: -1 "remove the extra space"].
	aStream nextPut: $)! !

!WeakSet methodsFor: 'private' stamp: 'SqR 8/23/2000 15:43'!
rehash
	self growTo: array size! !

!WeakSet methodsFor: 'public' stamp: 'SqR 8/23/2000 15:08'!
remove: oldObject ifAbsent: aBlock

	| index |
	index := self findElementOrNil: oldObject.
	(array at: index) == flag ifTrue: [ ^ aBlock value ].
	array at: index put: flag.
	tally := tally - 1.
	self fixCollisionsFrom: index.
	^oldObject! !

!WeakSet methodsFor: 'private' stamp: 'md 10/5/2005 15:44'!
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements"

	| element start finish |

	finish := array size.
	start := (anObject hash \\ finish) + 1.
	
	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == flag or: [element = anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == flag or: [element = anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !

!WeakSet methodsFor: 'private' stamp: 'yo 11/11/2002 23:10'!
scanForLoadedSymbol: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements"

	| element start finish |

	start := (anObject hash \\ array size) + 1.
	finish := array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == flag or: [element asString = anObject asString])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == flag or: [element asString = anObject asString])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"! !

!WeakSet methodsFor: 'public' stamp: 'SqR 8/23/2000 15:12'!
size
	"Careful!! Answer the maximum amount
	of elements in the receiver, not the
	exact amount"

	^tally! !

!WeakSet methodsFor: 'public' stamp: 'SqR 8/23/2000 15:17'!
slowSize
	"Careful!! Answer the maximum amount
	of elements in the receiver, not the
	exact amount"

	tally := array inject: 0 into:
		[:total :each | (each == nil or: [each == flag])
			ifTrue: [total] ifFalse: [total + 1]].
	^tally! !

Collection subclass: #SkipList
	instanceVariableNames: 'sortBlock pointers numElements level splice'
	classVariableNames: 'Rand'
	poolDictionaries: ''
	category: 'Collections-SkipLists'!

!SkipList commentStamp: 'sac 1/23/2002 19:41' prior: 0!
A skiplist is a sorted data structure that allows one to search for any element in o(log n) time.

It also allows one to enumerate forward to the next element. Basically, its a tree-like algorithm, except it doesn't use trees.

The implementation here is similar to a Dictionary, in that it indexes (a subclass of) Associations. Thus, you can do    foo at: key put: value   You can also search for a key, if the key does not exist, it will report the first key greater than the search, or nil.
!

SkipList subclass: #IdentitySkipList
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-SkipLists'!

!IdentitySkipList commentStamp: '<historical>' prior: 0!
Like a SkipList, except that elements are compared with #== instead of #= .

See the comment of IdentitySet for more information.
!

!IdentitySkipList methodsFor: 'element comparison' stamp: 'LC 6/18/2001 20:28'!
is: element1 equalTo: element2
	^ element1 == element2! !

!SkipList class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 17:33'!
maxLevel: maxLevel
	"
	SkipList maxLevel: 5
	"
	^ super new initialize: maxLevel! !

!SkipList class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 17:34'!
maxLevel: anInteger sortBlock: aBlock
	^ (self maxLevel: anInteger) sortBlock: aBlock! !

!SkipList class methodsFor: 'instance creation' stamp: 'LC 6/17/2001 11:52'!
new
	"
	SkipList new
	"
	^ super new initialize: 10! !

!SkipList class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 17:39'!
new: anInteger
	^ self maxLevel: (anInteger log: 2) ceiling! !

!SkipList class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 18:40'!
new: anInteger sortBlock: aBlock
	^ (self new: anInteger) sortBlock: aBlock! !

!SkipList class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 18:48'!
newFrom: aCollection 
	| skipList |
	skipList := self new: aCollection size.
	skipList addAll: aCollection.
	^ skipList! !

!SkipList class methodsFor: 'instance creation' stamp: 'LC 6/18/2001 17:32'!
sortBlock: aBlock
	^ self new sortBlock: aBlock! !

!SkipList methodsFor: 'adding' stamp: 'sac 1/22/2002 18:22'!
add: element 
        "Add an association or key on to the skiplist"
        ^self add: element ifPresent: [].
        ! !

!SkipList methodsFor: 'adding' stamp: 'sac 1/25/2002 12:26'!
add: element ifPresent: aBlock
        ^self at: element put: element ifPresent: aBlock.
! !

!SkipList methodsFor: 'enumerating' stamp: 'sac 1/23/2002 20:24'!
associationsDo: aBlock
        self nodesDo: [:node | aBlock value: node]! !

!SkipList methodsFor: 'accessing' stamp: 'sac 1/22/2002 19:11'!
at: element 
        ^self at: element ifAbsent: []! !

!SkipList methodsFor: 'accessing' stamp: 'sac 1/22/2002 18:18'!
at: element ifAbsent: aBlock
        "Get the key if it exists, or if it doesn't exist, get the key just after it."
        | node forward |
        node := self.
        level to: 1 by: -1 do: [:i |
                        [forward := node forward: i.
                        self is: forward before: element] whileTrue: [node := forward]].
        node := node next.
        (self is: node theNodeFor: element) ifFalse: [^aBlock value].
        ^node value
! !

!SkipList methodsFor: 'adding' stamp: 'sac 1/22/2002 18:22'!
at: key put: value 
        "Add an association or key on to the skiplist"
        ^self at: key put: value ifPresent: [].
        ! !

!SkipList methodsFor: 'adding' stamp: 'sac 1/22/2002 18:23'!
at: key put: value ifPresent: aBlock
        | node lvl s |
        node := self search: key updating: splice.
        node ifNotNil: [^ aBlock value].
        lvl := self randomLevel.
        node := SkipListNode key: key value: value level: lvl.
        level + 1 to: lvl do: [:i | splice at: i put: self].
        1 to: lvl do: [:i |
                                s := splice at: i.
                                node atForward: i put: (s forward: i).
                                s atForward: i put: node].
        numElements := numElements + 1.
        splice atAllPut: nil.
        ^ node
! !

!SkipList methodsFor: 'private' stamp: 'LC 6/18/2001 19:26'!
atForward: i put: node
	level := node
		ifNil: [pointers findLast: [:n | n notNil]]
		ifNotNil: [level max: i].
	^ pointers at: i put: node! !

!SkipList methodsFor: 'enumerating' stamp: 'sac 1/23/2002 20:21'!
do: aBlock
        self nodesDo: [:node | aBlock value: node value]! !

!SkipList methodsFor: 'accessing' stamp: 'sac 1/22/2002 17:19'!
first
        ^pointers first.
! !

!SkipList methodsFor: 'private' stamp: 'LC 6/18/2001 11:21'!
forward: i 
	^ pointers at: i! !

!SkipList methodsFor: 'testing' stamp: 'sac 1/22/2002 17:49'!
includes: key
        ^ (self search: key updating: nil) notNil! !

!SkipList methodsFor: 'initialization' stamp: 'LC 6/18/2001 20:08'!
initialize: maxLevel
	pointers := Array new: maxLevel.
	splice := Array new: maxLevel.
	numElements := 0.
	level := 0.
	Rand ifNil: [Rand := Random new]! !

!SkipList methodsFor: 'private' stamp: 'sac 1/22/2002 17:50'!
is: node before: element 
        | key |
        node ifNil: [^ false].
        key := node key.
        ^ sortBlock
                ifNil: [key < element]
                ifNotNil: [(self is: key equalTo: element) ifTrue: [^ false].
                        sortBlock value: key value: element]! !

!SkipList methodsFor: 'element comparison' stamp: 'LC 6/18/2001 10:14'!
is: element1 equalTo: element2
	^ element1 = element2! !

!SkipList methodsFor: 'private' stamp: 'sac 1/22/2002 18:04'!
is: node theNodeFor: key 
        node ifNil: [^ false].
        node == self ifTrue: [^ false].
        
        ^ self is: node key equalTo: key! !

!SkipList methodsFor: 'testing' stamp: 'LC 6/18/2001 12:53'!
isEmpty
	^ numElements = 0! !

!SkipList methodsFor: 'enumerating' stamp: 'sac 1/25/2002 00:18'!
keysAndValuesDo: aBlock
        ^self associationsDo:[:assoc|
                aBlock value: assoc key value: assoc value].! !

!SkipList methodsFor: 'enumerating' stamp: 'sac 1/23/2002 20:22'!
keysDo: aBlock
        self nodesDo: [:node | aBlock value: node key]! !

!SkipList methodsFor: 'accessing' stamp: 'LC 6/18/2001 19:22'!
level
	^ level! !

!SkipList methodsFor: 'accessing' stamp: 'LC 6/17/2001 12:05'!
maxLevel
	^ pointers size! !

!SkipList methodsFor: 'accessing' stamp: 'LC 6/18/2001 20:18'!
maxLevel: n
	| newLevel oldPointers |
	newLevel := n max: level.
	oldPointers := pointers.
	pointers := Array new: newLevel.
	splice := Array new: newLevel.
	1 to: level do: [:i | pointers at: i put: (oldPointers at: i)]
! !

!SkipList methodsFor: 'private' stamp: 'LC 6/18/2001 16:15'!
next
	^ pointers first! !

!SkipList methodsFor: 'node enumeration' stamp: 'LC 6/18/2001 19:30'!
nodesDo: aBlock
	| node |
	node := pointers first.
	[node notNil]
		whileTrue:
			[aBlock value: node.
			node := node next]! !

!SkipList methodsFor: 'private' stamp: 'LC 6/18/2001 15:37'!
randomLevel
	| p answer max |
	p := 0.5.
	answer := 1.
	max := self maxLevel.
	[Rand next < p and: [answer < max]]
		whileTrue: [answer := answer + 1].
	^ answer! !

!SkipList methodsFor: 'removing' stamp: 'LC 6/18/2001 17:28'!
remove: element 
	^ self remove: element ifAbsent: [self errorNotFound: element]! !

!SkipList methodsFor: 'removing' stamp: 'sac 1/22/2002 16:58'!
remove: key ifAbsent: aBlock
        | node i s |
        "Remove and return th association containing key."
        node := self search: key updating: splice.
        node ifNil: [^ aBlock value].
        i := 1.
        [s := splice at: i.
        i <= level and: [(s forward: i) == node]]
                                whileTrue:
                                        [s atForward: i put: (node forward: i).
                                        i := i + 1].
        numElements := numElements - 1.
        splice atAllPut: nil.
        ^ node.
! !

!SkipList methodsFor: 'removing' stamp: 'LC 6/18/2001 20:25'!
removeAll
	pointers atAllPut: nil.
	splice atAllPut: nil.
	numElements := 0.
	level := 0.! !

!SkipList methodsFor: 'accessing' stamp: 'sac 1/22/2002 18:33'!
search: element 
        "Get the key if it exists, or if it doesn't exist, get the key just after it. If no key after it, return nil."
        | node forward |
        node := self.
        level to: 1 by: -1 do: [:i |
                        [forward := node forward: i.
                        self is: forward before: element] whileTrue: [node := forward]].
        node := node next.
        ^node! !

!SkipList methodsFor: 'private' stamp: 'sac 1/22/2002 18:01'!
search: element updating: array
        | node forward |
        node := self.
        level to: 1 by: -1 do: [:i |
                        [forward := node forward: i.
                        self is: forward before: element] whileTrue: [node := forward].
                        "At this point: node < element <= forward"
                        array ifNotNil: [array at: i put: node]].
        node := node next.
        ^ (self is: node theNodeFor: element) ifTrue: [node]! !

!SkipList methodsFor: 'accessing' stamp: 'LC 6/18/2001 15:40'!
size
	^ numElements! !

!SkipList methodsFor: 'accessing' stamp: 'LC 6/18/2001 20:19'!
sortBlock
	^ sortBlock! !

!SkipList methodsFor: 'accessing' stamp: 'LC 6/18/2001 17:30'!
sortBlock: aBlock
	sortBlock := aBlock! !

Collection subclass: #WeakRegistry
	instanceVariableNames: 'valueDictionary accessLock'
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'Collections-Weak'!

!WeakRegistry commentStamp: '<historical>' prior: 0!
I am a registry for objects needing finalization. When an object is added the object as well as its executor is stored. When the object is garbage collected, the executor can take the appropriate action for any resources associated with the object.

See also:
	Object executor
	Object actAsExecutor
	Object finalize
!

!WeakRegistry class methodsFor: 'accessing' stamp: 'ar 5/19/2003 20:10'!
default
	^Default ifNil:[Default := self new]! !

!WeakRegistry class methodsFor: 'instance creation' stamp: 'ar 3/21/98 15:32'!
new
	^self new: 5! !

!WeakRegistry class methodsFor: 'instance creation' stamp: 'ar 3/21/98 15:33'!
new: n
	| registry |
	registry := super new initialize: n.
	WeakArray addWeakDependent: registry.
	^registry! !

!WeakRegistry methodsFor: 'adding' stamp: 'ar 3/21/98 16:33'!
add: anObject
	"Add anObject to the receiver. Store the object as well as the associated executor."
	| executor |
	executor := anObject executor.
	self protected:[
		valueDictionary at: anObject put: executor.
	].
	^anObject! !

!WeakRegistry methodsFor: 'adding' stamp: 'ar 5/19/2003 20:08'!
add: anObject executor: anExecutor
	"Add anObject to the receiver. Store the object as well as the associated executor."
	self protected:[
		valueDictionary at: anObject put: anExecutor.
	].
	^anObject! !

!WeakRegistry methodsFor: 'enumerating' stamp: 'ar 3/21/98 18:36'!
do: aBlock
	^self protected:[
		valueDictionary keysDo: aBlock.
	].
! !

!WeakRegistry methodsFor: 'finalization' stamp: 'ar 10/22/2000 20:24'!
finalizeValues
	"Some of our elements may have gone away. Look for those and activate the associated executors."
	| finiObjects |
	finiObjects := nil.
	"First collect the objects."
	self protected:[
		valueDictionary associationsDo:[:assoc|
			assoc key isNil ifTrue:[
				finiObjects isNil 
					ifTrue:[finiObjects := OrderedCollection with: assoc value]
					ifFalse:[finiObjects add: assoc value]]
		].
		finiObjects isNil ifFalse:[valueDictionary finalizeValues: finiObjects asArray].
	].
	"Then do the finalization"
	finiObjects isNil ifTrue:[^self].
	finiObjects do:[:each| each finalize].
! !

!WeakRegistry methodsFor: 'initialize' stamp: 'ar 3/21/98 16:08'!
initialize: n
	valueDictionary := WeakKeyDictionary new: n.
	accessLock := Semaphore forMutualExclusion.! !

!WeakRegistry methodsFor: 'accessing' stamp: 'ar 12/12/2001 16:00'!
keys
	^self protected:[
		Array streamContents:[:s| valueDictionary keysDo:[:key| s nextPut: key]]].! !

!WeakRegistry methodsFor: 'printing' stamp: 'tk 12/5/2001 09:42'!
printElementsOn: aStream
	aStream nextPut: $(.
	accessLock 
		ifNil: [self do: [:element | aStream print: element; space]]
		ifNotNil: [aStream nextPutAll: '<this WeakRegistry is locked>; space'].
	self isEmpty ifFalse: [aStream skip: -1].
	aStream nextPut: $)! !

!WeakRegistry methodsFor: 'private' stamp: 'ar 10/8/1998 11:18'!
protected: aBlock
	"Execute aBlock protected by the accessLock"
	^accessLock isNil
		ifTrue:[aBlock value]
		ifFalse:[accessLock critical: aBlock ifError:[:msg :rcvr| rcvr error: msg]]! !

!WeakRegistry methodsFor: 'removing' stamp: 'ar 3/21/98 21:12'!
remove: oldObject ifAbsent: exceptionBlock
	"Remove oldObject as one of the receiver's elements."
	| removedObject |
	oldObject isNil ifTrue:[^oldObject].
	self protected:[
		removedObject := valueDictionary removeKey: oldObject ifAbsent:[nil].
	].
	^removedObject isNil
		ifTrue:[exceptionBlock value]
		ifFalse:[removedObject].
! !

!WeakRegistry methodsFor: 'accessing' stamp: 'sma 5/12/2000 11:40'!
size
	^ self protected: [valueDictionary size]! !

!WeakRegistry methodsFor: 'accessing' stamp: 'ar 3/20/98 19:31'!
species
	^Set! !

Collection subclass: #WideCharacterSet
	instanceVariableNames: 'map'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Support'!

!WideCharacterSet commentStamp: 'nice 5/9/2006 23:33' prior: 0!
WideCharacterSet is used to store a Set of WideCharacter with fast access and inclusion test.

Implementation should be efficient in memory if sets are sufficently sparse.

Wide Characters are at most 32bits.
We split them into 16 highBits and 16 lowBits.

map is a dictionary key: 16 highBits value: map of 16 lowBits.

Maps of lowBits  are stored as arrays of bits in a WordArray.
If a bit is set to 1, this indicate that corresponding character is present.
Only 2048 entries are necessary in each lowmap.
And only lowmap corresponding to a present high value are stored.!

!WideCharacterSet methodsFor: 'comparing' stamp: 'nice 5/9/2006 23:29'!
= anObject
	^self species == anObject species and: [
		self wideCharacterMap = anObject wideCharacterMap ]! !

!WideCharacterSet methodsFor: 'collection ops' stamp: 'nice 5/9/2006 22:45'!
add: aCharacter 
	| val high low lowmap |
	val := aCharacter asciiValue.
	high := val bitShift: -16.
	low := val bitAnd: 16rFFFF.
	lowmap := map at: high ifAbsentPut: [WordArray new: 2048].
	self setBitmap: lowmap at: low.
	^ aCharacter! !

!WideCharacterSet methodsFor: 'private' stamp: 'nice 5/9/2006 22:46'!
bitmap: aMap at: shortInteger
	"access a single bit in aMap.
	shortInteger should be between: 0 and: 16rFFFF"
	
	| collecIndex bitIndex |
	collecIndex := shortInteger bitShift: -5.
	bitIndex := shortInteger bitAnd: 16r1F.
	^(aMap at: collecIndex + 1) bitAnd: (1 bitShift: bitIndex)! !

!WideCharacterSet methodsFor: 'private' stamp: 'nice 5/10/2006 00:10'!
bitmap: aMap do: aBlock
	"Execute a block with each value (0 based) corresponding to set bits"
	
	0 to: 31 do: [:shift |
		| mask |
		mask := 1 bitShift: shift.
		1 to: aMap size do: [:i | 
			((aMap at: i) bitAnd: mask) isZero ifFalse: [aBlock value: ((i - 1 bitShift: 5) bitOr: shift)]]]! !

!WideCharacterSet methodsFor: 'comparing' stamp: 'nice 5/10/2006 00:26'!
byteArrayMap
	"return a ByteArray mapping each ascii value to a 1 if that ascii value is in the set, and a 0 if it isn't.
	Intended for use by primitives only. (and comparison)
	This version will answer a subset with only byte characters"
	
	| aMap lowmap |
	aMap := ByteArray new: 256.
	lowmap := map at: 0 ifAbsent: [^aMap].
	lowmap := lowmap copyFrom: 1 to: 8. "Keep first 8*32=256 bits..."
	self bitmap: lowmap do: [:code | aMap at: code + 1 put: 1].
	^aMap! !

!WideCharacterSet methodsFor: 'private' stamp: 'nice 5/9/2006 22:46'!
clearBitmap: aMap at: shortInteger
	"clear a single bit in aMap.
	shortInteger should be between: 0 and: 16rFFFF"
	
	| collecIndex bitIndex |
	collecIndex := shortInteger bitShift: -5.
	bitIndex := shortInteger bitAnd: 16r1F.
	^aMap at: collecIndex + 1 put: ((aMap at: collecIndex + 1) bitClear: (1 bitShift: bitIndex))! !

!WideCharacterSet methodsFor: 'converting' stamp: 'nice 3/23/2007 02:29'!
complement
	"return a character set containing precisely the characters the receiver does not"
	
	^CharacterSetComplement of: self! !

!WideCharacterSet methodsFor: 'collection ops' stamp: 'nice 5/10/2006 00:21'!
do: aBlock 
	map
		keysAndValuesDo: [:high :lowmap | self
				bitmap: lowmap
				do: [:low | aBlock
						value: (Character value: ((high bitShift: 16) bitOr: low))]]! !

!WideCharacterSet methodsFor: 'testing' stamp: 'nice 5/9/2006 23:25'!
hasWideCharacters
	"Answer true if i contain any wide character"
	
	self do: [:e | e asciiValue >= 256 ifTrue: [^true]].
	^false! !

!WideCharacterSet methodsFor: 'comparing' stamp: 'nice 5/9/2006 22:27'!
hash
	^self map hash! !

!WideCharacterSet methodsFor: 'collection ops' stamp: 'nice 5/9/2006 23:49'!
includes: aCharacter 
	| val high low |
	val := aCharacter asciiValue.
	high := val bitShift: -16.
	low := val bitAnd: 16rFFFF.
	^(self
		bitmap: (map
				at: high
				ifAbsent: [^ false])
		at: low) isZero not! !

!WideCharacterSet methodsFor: 'initialize-release' stamp: 'nice 5/9/2006 22:31'!
initialize
	map := Dictionary new.! !

!WideCharacterSet methodsFor: 'collection ops' stamp: 'nice 5/9/2006 22:45'!
remove: aCharacter 
	| val high low lowmap |
	val := aCharacter asciiValue.
	high := val bitShift: -16.
	low := val bitAnd: 16rFFFF.
	lowmap := (map
				at: high
				ifAbsent: [^ aCharacter]) self clearBitmap: lowmap at: low.
	lowmap max = 0
		ifTrue: [map removeKey: high].
	^ aCharacter! !

!WideCharacterSet methodsFor: 'private' stamp: 'nice 5/9/2006 22:47'!
setBitmap: aMap at: shortInteger
	"set a single bit in aMap.
	shortInteger should be between: 0 and: 16rFFFF"
	
	| collecIndex bitIndex |
	collecIndex := shortInteger bitShift: -5.
	bitIndex := shortInteger bitAnd: 16r1F.
	^aMap at: collecIndex + 1 put: ((aMap at: collecIndex + 1) bitOr: (1 bitShift: bitIndex))! !

!WideCharacterSet methodsFor: 'collection ops' stamp: 'nice 5/9/2006 23:00'!
size
	| size |
	size := 0.
	map
		keysAndValuesDo: [:high :lowmap | self
				bitmap: lowmap
				do: [:low | size := size + 1]].
	^ size! !

!WideCharacterSet methodsFor: 'comparing' stamp: 'nice 5/9/2006 23:29'!
species
	^self hasWideCharacters
		ifTrue: [WideCharacterSet]
		ifFalse: [CharacterSet]! !

!WideCharacterSet methodsFor: 'comparing' stamp: 'nice 5/9/2006 23:14'!
wideCharacterMap
	^map! !

Object subclass: #LimitingLineStreamWrapper
	instanceVariableNames: 'stream line limitingBlock position'
	classVariableNames: ''
	poolDictionaries: 'TextConstants'
	category: 'Collections-Streams'!

!LimitingLineStreamWrapper commentStamp: '<historical>' prior: 0!
I'm a wrapper for a stream optimized for line-by-line access using #nextLine. My instances can be nested.

I read one line ahead. Reading terminates when the stream ends, or if the limitingBlock evaluated with the line answers true. To skip the delimiting line for further reading use #skipThisLine.

Character-based reading (#next) is permitted, too. Send #updatePosition when switching from line-based reading.

See examples at the class side.

--bf 2/19/1999 12:52!

!LimitingLineStreamWrapper class methodsFor: 'examples' stamp: 'bf 2/19/1999 11:48'!
example1
	"LimitingLineStreamWrapper example1"
	"Separate chunks of text delimited by a special string"
	| inStream msgStream messages |
	inStream := self exampleStream.
	msgStream := LimitingLineStreamWrapper on: inStream delimiter: 'From '.
	messages := OrderedCollection new.
	[inStream atEnd] whileFalse: [
		msgStream skipThisLine.
		messages add: msgStream upToEnd].
	^messages
			! !

!LimitingLineStreamWrapper class methodsFor: 'examples' stamp: 'bf 2/19/1999 12:46'!
example2
	"LimitingLineStreamWrapper example2"
	"Demo nesting wrappers - get header lines from some messages"
	| inStream msgStream headers headerStream |
	inStream := self exampleStream.
	msgStream := LimitingLineStreamWrapper on: inStream delimiter: 'From '.
	headers := OrderedCollection new.
	[inStream atEnd] whileFalse: [
		msgStream skipThisLine. "Skip From"
		headerStream := LimitingLineStreamWrapper on: msgStream delimiter: ''.
		headers add: headerStream linesUpToEnd.
		[msgStream nextLine isNil] whileFalse. "Skip Body"
	].
	^headers
			! !

!LimitingLineStreamWrapper class methodsFor: 'examples' stamp: 'bf 2/19/1999 12:44'!
exampleStream
	^ReadStream on:
'From me@somewhere
From: me
To: you
Subject: Test

Test

From you@elsewhere
From: you
To: me
Subject: Re: test

okay
'! !

!LimitingLineStreamWrapper class methodsFor: 'instance creation' stamp: 'bf 11/24/1998 14:31'!
on: aStream delimiter: aString

	^self new setStream: aStream delimiter: aString
! !

!LimitingLineStreamWrapper methodsFor: 'testing' stamp: 'bf 11/13/1998 16:55'!
atEnd

	^line isNil or: [limitingBlock value: line]! !

!LimitingLineStreamWrapper methodsFor: 'stream protocol' stamp: 'bf 11/13/1998 17:00'!
close
	^stream close! !

!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 11/24/1998 14:25'!
delimiter: aString
	"Set limitBlock to check for a delimiting string. Be unlimiting if nil"

	self limitingBlock: (aString caseOf: {
		[nil] -> [[:aLine | false]].
		[''] -> [[:aLine | aLine size = 0]]
	} otherwise: [[:aLine | aLine beginsWith: aString]])
! !

!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 11/13/1998 13:08'!
lastLineRead
	"Return line last read. At stream end, this is the boundary line or nil"

	^ line! !

!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 11/24/1998 19:16'!
limitingBlock: aBlock
	"The limitingBlock is evaluated with a line to check if this line terminates the stream"

	limitingBlock := aBlock fixTemps.
	self updatePosition! !

!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 2/19/1999 11:45'!
linesUpToEnd

	| elements ln |
	elements := OrderedCollection new.
	[(ln := self nextLine) isNil] whileFalse: [ 
		elements add: ln].
	^elements! !

!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 11/24/1998 14:37'!
next
	"Provide character-based access"

	position isNil ifTrue: [^nil].
	position < line size ifTrue: [^line at: (position := position + 1)].
	line := stream nextLine.
	self updatePosition.
	^ Character cr! !

!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 11/24/1998 14:09'!
nextLine

	| thisLine |
	self atEnd ifTrue: [^nil].
	thisLine := line.
	line := stream nextLine.
	^thisLine
! !

!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 11/13/1998 13:04'!
peekLine

	self atEnd ifTrue: [^nil].
	^ line! !

!LimitingLineStreamWrapper methodsFor: 'printing' stamp: 'bf 11/24/1998 13:39'!
printOn: aStream

	super printOn: aStream.
	aStream nextPutAll: ' on '.
	stream printOn: aStream! !

!LimitingLineStreamWrapper methodsFor: 'private' stamp: 'bf 11/24/1998 14:30'!
setStream: aStream delimiter: aString

	stream := aStream.
	line := stream nextLine.
	self delimiter: aString.	"sets position"
! !

!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 11/24/1998 16:53'!
skipThisLine

	line := stream nextLine.
	self updatePosition.
! !

!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 2/19/1999 11:47'!
upToEnd

	| ln |
	^String streamContents: [:strm |
		[(ln := self nextLine) isNil] whileFalse: [ 
			strm nextPutAll: ln; cr]]! !

!LimitingLineStreamWrapper methodsFor: 'accessing' stamp: 'bf 11/24/1998 14:37'!
updatePosition
	"Call this before doing character-based access"

	position := self atEnd ifFalse: [0]! !

Object subclass: #Link
	instanceVariableNames: 'nextLink'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Support'!

!Link commentStamp: '<historical>' prior: 0!
An instance of me is a simple record of a pointer to another Link. I am an abstract class; my concrete subclasses, for example, Process, can be stored in a LinkedList structure.!

!Link class methodsFor: 'instance creation' stamp: 'apb 10/3/2000 15:55'!
nextLink: aLink 
	"Answer an instance of me referring to the argument, aLink."

	^self new nextLink: aLink; yourself! !

!Link methodsFor: 'accessing' stamp: ''!
nextLink
	"Answer the link to which the receiver points."

	^nextLink! !

!Link methodsFor: 'accessing' stamp: ''!
nextLink: aLink 
	"Store the argument, aLink, as the link to which the receiver refers. 
	Answer aLink."

	^nextLink := aLink! !

Link subclass: #StackLink
	instanceVariableNames: 'element'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Stack'!

!StackLink commentStamp: '<historical>' prior: 0!
I implement an element of a stack. I'm a container for any type of object, saved into the 'element' variable. My superclass Link allows me to be part of a LinkedList.!

!StackLink class methodsFor: 'instance creation' stamp: 'dc 7/25/2005 10:15'!
with: anObject 
	^ self new element: anObject! !

!StackLink methodsFor: 'accessing' stamp: 'dc 7/24/2005 15:34'!
element
	^element! !

!StackLink methodsFor: 'accessing' stamp: 'dc 7/25/2005 10:16'!
element: anObject 
	"Any kind of Object."
	element := anObject! !

!StackLink methodsFor: 'printing' stamp: 'dc 7/25/2005 10:15'!
printOn: aStream 
	aStream nextPutAll: self class printString;
		 nextPutAll: ' with: ';
		 nextPutAll: self element printString! !

Object subclass: #MimeConverter
	instanceVariableNames: 'dataStream mimeStream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

MimeConverter subclass: #Base64MimeConverter
	instanceVariableNames: 'data'
	classVariableNames: 'ToCharTable FromCharTable'
	poolDictionaries: ''
	category: 'Collections-Streams'!

!Base64MimeConverter commentStamp: '<historical>' prior: 0!
This class encodes and decodes data in Base64 format.  This is MIME encoding.  We translate a whole stream at once, taking a Stream as input and giving one as output.  Returns a whole stream for the caller to use.
           0 A            17 R            34 i            51 z
           1 B            18 S            35 j            52 0
           2 C            19 T            36 k            53 1
           3 D            20 U            37 l            54 2
           4 E            21 V            38 m            55 3
           5 F            22 W            39 n            56 4
           6 G            23 X            40 o            57 5
           7 H            24 Y            41 p            58 6
           8 I            25 Z            42 q            59 7
           9 J            26 a            43 r            60 8
          10 K            27 b            44 s            61 9
          11 L            28 c            45 t            62 +
          12 M            29 d            46 u            63 /
          13 N            30 e            47 v
          14 O            31 f            48 w         (pad) =
          15 P            32 g            49 x
          16 Q            33 h            50 y
Outbound: bytes are broken into 6 bit chunks, and the 0-63 value is converted to a character.  3 data bytes go into 4 characters.
Inbound: Characters are translated in to 0-63 values and shifted into 8 bit bytes.

(See: N. Borenstein, Bellcore, N. Freed, Innosoft, Network Working Group, Request for Comments: RFC 1521, September 1993, MIME (Multipurpose Internet Mail Extensions) Part One: Mechanisms for Specifying and Describing the Format of Internet Message Bodies. Sec 6.2)

By Ted Kaehler, based on Tim Olson's Base64Filter.!

!Base64MimeConverter class methodsFor: 'as yet unclassified' stamp: 'tk 2/19/2000 15:53'!
decodeInteger: mimeString
	| bytes sum |
	"Decode the MIME string into an integer of any length"

	bytes := (Base64MimeConverter mimeDecodeToBytes: 
				(ReadStream on: mimeString)) contents.
	sum := 0.
	bytes reverseDo: [:by | sum := sum * 256 + by].
	^ sum! !

!Base64MimeConverter class methodsFor: 'as yet unclassified' stamp: 'tk 2/21/2000 17:22'!
encodeInteger: int
	| strm |
	"Encode an integer of any length and return the MIME string"

	strm := ReadWriteStream on: (ByteArray new: int digitLength).
	1 to: int digitLength do: [:ii | strm nextPut: (int digitAt: ii)].
	strm reset.
	^ ((self mimeEncode: strm) contents) copyUpTo: $=	"remove padding"! !

!Base64MimeConverter class methodsFor: 'as yet unclassified' stamp: 'tk 12/9/97 13:53'!
initialize

	FromCharTable := Array new: 256.	"nils"
	ToCharTable := Array new: 64.
	($A asciiValue to: $Z asciiValue) doWithIndex: [:val :ind | 
		FromCharTable at: val+1 put: ind-1.
		ToCharTable at: ind put: val asCharacter].
	($a asciiValue to: $z asciiValue) doWithIndex: [:val :ind | 
		FromCharTable at: val+1 put: ind+25.
		ToCharTable at: ind+26 put: val asCharacter].
	($0 asciiValue to: $9 asciiValue) doWithIndex: [:val :ind | 
		FromCharTable at: val+1 put: ind+25+26.
		ToCharTable at: ind+26+26 put: val asCharacter].
	FromCharTable at: $+ asciiValue + 1 put: 62.
	ToCharTable at: 63 put: $+.
	FromCharTable at: $/ asciiValue + 1 put: 63.
	ToCharTable at: 64 put: $/.
	! !

!Base64MimeConverter class methodsFor: 'as yet unclassified' stamp: 'tk 12/12/97 11:41'!
mimeDecodeToBytes: aStream 
	"Return a RWBinaryOrTextStream of the original ByteArray.  aStream has only 65 innocuous character values.  aStream is not binary.  (See class comment). 4 bytes in aStream goes to 3 bytes in output."

	| me |
	aStream position: 0.
	me := self new mimeStream: aStream.
	me dataStream: (RWBinaryOrTextStream on: (ByteArray new: aStream size * 3 // 4)).
	me mimeDecodeToByteArray.
	me dataStream position: 0.
	^ me dataStream! !

!Base64MimeConverter class methodsFor: 'as yet unclassified' stamp: 'tk 12/9/97 13:01'!
mimeDecodeToChars: aStream 
	"Return a ReadWriteStream of the original String.  aStream has only 65 innocuous character values.  It is not binary.  (See class comment). 4 bytes in aStream goes to 3 bytes in output."

	| me |
	aStream position: 0.
	me := self new mimeStream: aStream.
	me dataStream: (ReadWriteStream on: (String new: aStream size * 3 // 4)).
	me mimeDecode.
	me dataStream position: 0.
	^ me dataStream! !

!Base64MimeConverter class methodsFor: 'as yet unclassified' stamp: 'tk 12/9/97 12:28'!
mimeEncode: aStream
	"Return a ReadWriteStream of characters.  The data of aStream is encoded as 65 innocuous characters.  (See class comment). 3 bytes in aStream goes to 4 bytes in output."

	| me |
	aStream position: 0.
	me := self new dataStream: aStream.
	me mimeStream: (ReadWriteStream on: (String new: aStream size + 20 * 4 // 3)).
	me mimeEncode.
	me mimeStream position: 0.
	^ me mimeStream! !

!Base64MimeConverter methodsFor: 'conversion' stamp: 'tk 12/9/97 13:34'!
mimeDecode
	"Convert a stream in base 64 with only a-z,A-Z,0-9,+,/ to a full byte stream of characters.  Reutrn a whole stream for the user to read."

	| nibA nibB nibC nibD |
	[mimeStream atEnd] whileFalse: [
		(nibA := self nextValue) ifNil: [^ dataStream].
		(nibB := self nextValue) ifNil: [^ dataStream].
		dataStream nextPut: ((nibA bitShift: 2) + (nibB bitShift: -4)) asCharacter.
		nibB := nibB bitAnd: 16rF.
		(nibC := self nextValue) ifNil: [^ dataStream].
		dataStream nextPut: ((nibB bitShift: 4) + (nibC bitShift: -2)) asCharacter.
		nibC := nibC bitAnd: 16r3.
		(nibD := self nextValue) ifNil: [^ dataStream].
		dataStream nextPut: ((nibC bitShift: 6) + nibD) asCharacter.
		].
	^ dataStream! !

!Base64MimeConverter methodsFor: 'conversion' stamp: 'tk 12/9/97 13:39'!
mimeDecodeToByteArray
	"Convert a stream in base 64 with only a-z,A-Z,0-9,+,/ to a full ByteArray of 0-255 values.  Reutrn a whole stream for the user to read."

	| nibA nibB nibC nibD |
	[mimeStream atEnd] whileFalse: [
		(nibA := self nextValue) ifNil: [^ dataStream].
		(nibB := self nextValue) ifNil: [^ dataStream].
		dataStream nextPut: ((nibA bitShift: 2) + (nibB bitShift: -4)).
		nibB := nibB bitAnd: 16rF.
		(nibC := self nextValue) ifNil: [^ dataStream].
		dataStream nextPut: ((nibB bitShift: 4) + (nibC bitShift: -2)).
		nibC := nibC bitAnd: 16r3.
		(nibD := self nextValue) ifNil: [^ dataStream].
		dataStream nextPut: ((nibC bitShift: 6) + nibD).
		].
	^ dataStream! !

!Base64MimeConverter methodsFor: 'conversion' stamp: 'ls 2/10/2001 13:26'!
mimeEncode
	"Convert from data to 6 bit characters."

	| phase1 phase2 raw nib lineLength |
	phase1 := phase2 := false.
	lineLength := 0.
	[dataStream atEnd] whileFalse: [
		lineLength >= 70 ifTrue: [ mimeStream cr.  lineLength := 0. ].
		data := raw := dataStream next asInteger.
		nib := (data bitAnd: 16rFC) bitShift: -2.
		mimeStream nextPut: (ToCharTable at: nib+1).
		(raw := dataStream next) ifNil: [raw := 0. phase1 := true].
		data := ((data bitAnd: 3) bitShift: 8) + raw asInteger.
		nib := (data bitAnd: 16r3F0) bitShift: -4.
		mimeStream nextPut: (ToCharTable at: nib+1).
		(raw := dataStream next) ifNil: [raw := 0. phase2 := true].
		data := ((data bitAnd: 16rF) bitShift: 8) + (raw asInteger).
		nib := (data bitAnd: 16rFC0) bitShift: -6.
		mimeStream nextPut: (ToCharTable at: nib+1).
		nib := (data bitAnd: 16r3F).
		mimeStream nextPut: (ToCharTable at: nib+1).

		lineLength := lineLength + 4.].
	phase1 ifTrue: [mimeStream skip: -2; nextPut: $=; nextPut: $=.
			^ mimeStream].
	phase2 ifTrue: [mimeStream skip: -1; nextPut: $=.
			^ mimeStream].

! !

!Base64MimeConverter methodsFor: 'conversion' stamp: 'tk 12/9/97 13:21'!
nextValue
	"The next six bits of data char from the mimeStream, or nil.  Skip all other chars"
	| raw num |
	[raw := mimeStream next.
	raw ifNil: [^ nil].	"end of stream"
	raw == $= ifTrue: [^ nil].
	num := FromCharTable at: raw asciiValue + 1.
	num ifNotNil: [^ num].
	"else ignore space, return, tab, ..."
	true] whileTrue.! !

!MimeConverter class methodsFor: 'convenience' stamp: 'bf 3/10/2000 14:47'!
forEncoding: encodingString
	"Answer a converter class for the given encoding or nil if unknown"
	encodingString ifNil: [^nil].
	^ encodingString asLowercase caseOf: 
		{ ['base64'] -> [Base64MimeConverter].
		  ['quoted-printable'] -> [QuotedPrintableMimeConverter]}
		otherwise: [].
! !

!MimeConverter class methodsFor: 'convenience' stamp: 'bf 3/10/2000 14:43'!
mimeDecode: aStringOrStream as: contentsClass
	^ contentsClass streamContents: [:out |
		self mimeDecode: aStringOrStream to: out]! !

!MimeConverter class methodsFor: 'convenience' stamp: 'bf 3/10/2000 14:40'!
mimeDecode: aStringOrStream to: outStream
	self new
		mimeStream: (aStringOrStream isStream
			ifTrue: [aStringOrStream]
			ifFalse: [ReadStream on: aStringOrStream]);
		dataStream: outStream;
		mimeDecode! !

!MimeConverter class methodsFor: 'convenience' stamp: 'bf 3/10/2000 14:40'!
mimeEncode: aCollectionOrStream
	^ String streamContents: [:out |
		self mimeEncode: aCollectionOrStream to: out]! !

!MimeConverter class methodsFor: 'convenience' stamp: 'bf 3/10/2000 14:40'!
mimeEncode: aCollectionOrStream to: outStream
	self new
		dataStream: (aCollectionOrStream isStream
			ifTrue: [aCollectionOrStream]
			ifFalse: [ReadStream on: aCollectionOrStream]);
		mimeStream: outStream;
		mimeEncode! !

!MimeConverter methodsFor: 'accessing' stamp: 'tk 12/9/97 13:55'!
dataStream
	^dataStream! !

!MimeConverter methodsFor: 'accessing' stamp: 'tk 12/9/97 13:51'!
dataStream: anObject
	dataStream := anObject! !

!MimeConverter methodsFor: 'conversion' stamp: 'bf 11/12/1998 13:30'!
mimeDecode
	"Do conversion reading from mimeStream writing to dataStream"

	self subclassResponsibility! !

!MimeConverter methodsFor: 'conversion' stamp: 'bf 11/12/1998 13:31'!
mimeEncode
	"Do conversion reading from dataStream writing to mimeStream"

	self subclassResponsibility! !

!MimeConverter methodsFor: 'accessing' stamp: 'tk 12/9/97 13:53'!
mimeStream
	^mimeStream! !

!MimeConverter methodsFor: 'accessing' stamp: 'tk 12/9/97 13:51'!
mimeStream: anObject
	mimeStream := anObject! !

MimeConverter subclass: #QuotedPrintableMimeConverter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

!QuotedPrintableMimeConverter commentStamp: '<historical>' prior: 0!
I do quoted printable MIME decoding as specified in RFC 2045 "MIME Part One: Format of Internet Message Bodies".

Short version of RFC2045, Sect. 6.7:

	(1) Any octet, except a CR or LF that is part of a CRLF line break of the canonical (standard) form of the data being encoded, may be represented by an "=" followed by a two digit hexadecimal representation of the octet's value. [...]

	(2) Octets with decimal values of 33 through 60 inclusive, and 62 through 126, inclusive, MAY be represented as the US-ASCII characters which correspond to those octets [...].

	(3) Octets with values of 9 and 32 MAY be represented as US-ASCII TAB (HT) and SPACE characters,
 respectively, but MUST NOT be so represented at the end of an encoded line.  [...]

	(4) A line break in a text body, represented as a CRLF sequence in the text canonical form, must be represented by a (RFC 822) line break, which is also a CRLF sequence, in the Quoted-Printable encoding.  [...]

	(5) The Quoted-Printable encoding REQUIRES that encoded lines be no more than 76 characters long.  If longer lines are to be encoded with the Quoted-Printable encoding, "soft" line breaks
 must be used.  An equal sign as the last character on a encoded line indicates such a non-significant ("soft") line break in the encoded text.


--bf 11/27/1998 16:50!

!QuotedPrintableMimeConverter methodsFor: 'conversion' stamp: 'bf 11/24/1998 20:33'!
mimeDecode
	"Do conversion reading from mimeStream writing to dataStream"

	| line s c1 v1 c2 v2 |
	[(line := mimeStream nextLine) isNil] whileFalse: [
		line := line withoutTrailingBlanks.
		line size = 0
			ifTrue: [dataStream cr]
			ifFalse: [
				s := ReadStream on: line.
				[dataStream nextPutAll: (s upTo: $=).
				s atEnd] whileFalse: [
					c1 := s next. v1 := c1 digitValue.
					((v1 between: 0 and: 15) and: [s atEnd not])
						ifFalse: [dataStream nextPut: $=; nextPut: c1]
						ifTrue: [c2 := s next. v2 := c2 digitValue.
							(v2 between: 0 and: 15)
								ifFalse: [dataStream nextPut: $=; nextPut: c1; nextPut: c2]
								ifTrue: [dataStream nextPut: (Character value: v1 * 16 + v2)]]].
				line last = $= ifFalse: [dataStream cr]]].
	^ dataStream! !

QuotedPrintableMimeConverter subclass: #RFC2047MimeConverter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

!RFC2047MimeConverter commentStamp: '<historical>' prior: 0!
I do quoted printable MIME decoding as specified in RFC 2047 ""MIME Part Three: Message Header Extensions for Non-ASCII Text". See String>>decodeMimeHeader!

!RFC2047MimeConverter methodsFor: 'private-encoding' stamp: 'bf 3/11/2000 23:16'!
encodeChar: aChar to: aStream

	aChar = Character space
		ifTrue: [^ aStream nextPut: $_].
	((aChar asciiValue between: 32 and: 127) and: [('?=_' includes: aChar) not])
		ifTrue: [^ aStream nextPut: aChar].
	aStream nextPut: $=;
		nextPut: (Character digitValue: aChar asciiValue // 16);
		nextPut: (Character digitValue: aChar asciiValue \\ 16)
! !

!RFC2047MimeConverter methodsFor: 'private-encoding' stamp: 'bf 3/11/2000 23:13'!
encodeWord: aString

	(aString allSatisfy: [:c | c asciiValue < 128])
		ifTrue: [^ aString].
	^ String streamContents: [:stream |
		stream nextPutAll: '=?iso-8859-1?Q?'.
		aString do: [:c | self encodeChar: c to: stream].
		stream nextPutAll: '?=']! !

!RFC2047MimeConverter methodsFor: 'private-encoding' stamp: 'bf 3/12/2000 14:36'!
isStructuredField: aString

	| fName |
	fName := aString copyUpTo: $:.
	('Resent' sameAs: (fName copyUpTo: $-))
		ifTrue: [fName := fName copyFrom: 8 to: fName size].
	^#('Sender' 'From' 'Reply-To' 'To' 'cc' 'bcc') anySatisfy: [:each | fName sameAs: each]! !

!RFC2047MimeConverter methodsFor: 'conversion' stamp: 'bf 11/27/1998 17:04'!
mimeDecode
	"Do conversion reading from mimeStream writing to dataStream. See String>>decodeMimeHeader"

	| c |
	[mimeStream atEnd] whileFalse: [
		c _ mimeStream next.
		c = $=
			ifTrue: [c _ Character value: mimeStream next digitValue * 16
				+ mimeStream next digitValue]
			ifFalse: [c = $_ ifTrue: [c _ $ ]].
		dataStream nextPut: c].
	^ dataStream! !

!RFC2047MimeConverter methodsFor: 'conversion' stamp: 'bf 3/10/2000 16:06'!
mimeEncode
	"Do conversion reading from dataStream writing to mimeStream. Break long lines and escape non-7bit chars."

	| word pos wasGood isGood max |
	true ifTrue: [mimeStream nextPutAll: dataStream upToEnd].
	pos := 0.
	max := 72.
	wasGood := true.
	[dataStream atEnd] whileFalse: [
		word := self readWord.
		isGood := word allSatisfy: [:c | c asciiValue < 128].
		wasGood & isGood ifTrue: [
			pos + word size < max
				ifTrue: [dataStream nextPutAll: word.
					pos := pos + word size]
				ifFalse: []
		]
	].
	^ mimeStream! !

!RFC2047MimeConverter methodsFor: 'private-encoding' stamp: 'bf 3/11/2000 22:30'!
readWord

	| strm |
	strm := WriteStream on: (String new: 20)
	dataStream skipSeparators.
	[dataStream atEnd] whileFalse: 
		[ | c |
		c := dataStream next.
		strm nextPut: c.
		c isSeparator ifTrue: [^ strm contents]].
	^ strm contents! !

Object subclass: #SharedQueue
	instanceVariableNames: 'contentsArray readPosition writePosition accessProtect readSynch'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Sequenceable'!

!SharedQueue commentStamp: '<historical>' prior: 0!
I provide synchronized communication of arbitrary objects between Processes. An object is sent by sending the message nextPut: and received by sending the message next. If no object has been sent when a next message is sent, the Process requesting the object will be suspended until one is sent.!

!SharedQueue class methodsFor: 'instance creation' stamp: ''!
new
	"Answer a new instance of SharedQueue that has 10 elements."

	^self new: 10! !

!SharedQueue class methodsFor: 'instance creation' stamp: 'nice 4/4/2006 22:10'!
new: anInteger 
	^super new initialize: anInteger! !

!SharedQueue methodsFor: 'accessing' stamp: 'bf 2/11/2006 15:17'!
flush
	"Throw out all pending contents"
	accessProtect critical: [
		"nil out flushed slots --bf 02/11/2006"
		contentsArray from: readPosition to: writePosition-1 put: nil.
		readPosition := 1.
		writePosition := 1.
		"Reset the read synchronization semaphore"
		readSynch initSignals].! !

!SharedQueue methodsFor: 'accessing' stamp: 'NS 6/18/2002 11:15'!
flushAllSuchThat: aBlock
	"Remove from the queue all objects that satisfy aBlock."
	| value newReadPos |
	accessProtect critical: [
		newReadPos := writePosition.
		writePosition-1 to: readPosition by: -1 do:
			[:i | value := contentsArray at: i.
			contentsArray at: i put: nil.
			(aBlock value: value) ifTrue: [
				"We take an element out of the queue, and therefore, we need to decrement 
				the readSynch signals"
				readSynch wait.
			] ifFalse: [
				newReadPos := newReadPos - 1.
				contentsArray at: newReadPos put: value]].
		readPosition := newReadPos].
	^value
! !

!SharedQueue methodsFor: 'private' stamp: 'nice 4/4/2006 22:09'!
initialize: size

	contentsArray := Array new: size.
	readPosition := 1.
	writePosition := 1.
	accessProtect := Semaphore forMutualExclusion.
	readSynch := Semaphore new! !

!SharedQueue methodsFor: 'testing' stamp: ''!
isEmpty
	"Answer whether any objects have been sent through the receiver and 
	not yet received by anyone."

	^readPosition = writePosition! !

!SharedQueue methodsFor: 'private' stamp: 'bf 10/25/2005 15:33'!
makeRoomAtEnd
	| contentsSize |
	readPosition = 1
		ifTrue: [contentsArray := contentsArray , (Array new: 10)]
		ifFalse: 
			[contentsSize := writePosition - readPosition.
			"BLT direction ok for this. Lots faster!!!!!!!!!!!! SqR!!!! 4/10/2000 10:47"
			contentsArray
				replaceFrom: 1
				to: contentsSize
				with: contentsArray
				startingAt: readPosition.
			"nil out remainder --bf 10/25/2005"
			contentsArray
				from: contentsSize+1
				to: contentsArray size
				put: nil.
			readPosition := 1.
			writePosition := contentsSize + 1]! !

!SharedQueue methodsFor: 'accessing' stamp: ''!
next
	"Answer the object that was sent through the receiver first and has not 
	yet been received by anyone. If no object has been sent, suspend the 
	requesting process until one is."

	| value |
	readSynch wait.
	accessProtect
		critical: [readPosition = writePosition
					ifTrue: 
						[self error: 'Error in SharedQueue synchronization'.
						 value := nil]
					ifFalse: 
						[value := contentsArray at: readPosition.
						 contentsArray at: readPosition put: nil.
						 readPosition := readPosition + 1]].
	^value! !

!SharedQueue methodsFor: 'accessing' stamp: 'RAA 12/14/2000 10:25'!
nextOrNil
	"Answer the object that was sent through the receiver first and has not 
	yet been received by anyone. If no object has been sent, answer <nil>."

	| value |

	accessProtect critical: [
		readPosition >= writePosition ifTrue: [
			value := nil
		] ifFalse: [
			value := contentsArray at: readPosition.
			contentsArray at: readPosition put: nil.
			readPosition := readPosition + 1
		].
		readPosition >= writePosition ifTrue: [readSynch initSignals].
	].
	^value! !

!SharedQueue methodsFor: 'accessing' stamp: 'di 10/1/2001 20:58'!
nextOrNilSuchThat: aBlock
	"Answer the next object that satisfies aBlock, skipping any intermediate objects.
	If no object has been sent, answer <nil> and leave me intact.
	NOTA BENE:  aBlock MUST NOT contain a non-local return (^)."

	| value readPos |
	accessProtect critical: [
		value := nil.
		readPos := readPosition.
		[readPos < writePosition and: [value isNil]] whileTrue: [
			value := contentsArray at: readPos.
			readPos := readPos + 1.
			(aBlock value: value) ifTrue: [
				readPosition to: readPos - 1 do: [ :j |
					contentsArray at: j put: nil.
				].
				readPosition := readPos.
			] ifFalse: [
				value := nil.
			].
		].
		readPosition >= writePosition ifTrue: [readSynch initSignals].
	].
	^value
"===
q := SharedQueue new.
1 to: 10 do: [ :i | q nextPut: i].
c := OrderedCollection new.
[
	v := q nextOrNilSuchThat: [ :e | e odd].
	v notNil
] whileTrue: [
	c add: {v. q size}
].
{c. q} explore
==="! !

!SharedQueue methodsFor: 'accessing' stamp: ''!
nextPut: value 
	"Send value through the receiver. If a Process has been suspended 
	waiting to receive a value through the receiver, allow it to proceed."

	accessProtect
		critical: [writePosition > contentsArray size
						ifTrue: [self makeRoomAtEnd].
				 contentsArray at: writePosition put: value.
				 writePosition := writePosition + 1].
	readSynch signal.
	^value! !

!SharedQueue methodsFor: 'accessing' stamp: 'tpr 1/5/2005 18:22'!
peek
	"Answer the object that was sent through the receiver first and has not 
	yet been received by anyone but do not remove it from the receiver. If 
	no object has been sent, return nil"

	| value |
	accessProtect
		critical: [readPosition >= writePosition
					ifTrue: [readPosition := 1.
							writePosition := 1.
							value := nil]
					ifFalse: [value := contentsArray at: readPosition]].
	^value! !

!SharedQueue methodsFor: 'accessing' stamp: ''!
size
	"Answer the number of objects that have been sent through the
	receiver and not yet received by anyone."

	^writePosition - readPosition! !

Object subclass: #Stack
	instanceVariableNames: 'linkedList'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Stack'!

!Stack commentStamp: 'dc 7/24/2005 15:41' prior: 0!
I implement a simple Stack. #push: adds a new object of any kind on top of the stack. #pop returns the first element and remove it from the stack. #top answer the first element of the stack without removing it.!

!Stack methodsFor: 'private' stamp: 'dc 7/25/2005 10:05'!
errorEmptyStack
	self error: 'this stack is empty'! !

!Stack methodsFor: 'initialize-release' stamp: 'dc 7/25/2005 11:39'!
initialize
	super initialize.
	linkedList := LinkedList new! !

!Stack methodsFor: 'testing' stamp: 'dc 7/25/2005 10:05'!
isEmpty
	^ self linkedList isEmpty! !

!Stack methodsFor: 'private' stamp: 'dc 7/25/2005 10:20'!
linkedList
	"The stack is implemented with a LinkedList. Do NOT call this function, it  
	is for private use !!"
	^ linkedList! !

!Stack methodsFor: 'private' stamp: 'dc 7/25/2005 10:05'!
notEmptyCheck
	"Ensure the stack is not empty."
	self isEmpty
		ifTrue: [self errorEmptyStack]! !

!Stack methodsFor: 'removing' stamp: 'dc 7/24/2005 16:16'!
pop
	"Returns the first element and remove it from the stack."

	self notEmptyCheck.
	^self linkedList removeFirst element! !

!Stack methodsFor: 'adding' stamp: 'dc 7/25/2005 10:22'!
push: anObject 
	"Adds a new object of any kind on top of the stack."
	self linkedList
		addFirst: (StackLink with: anObject).
	^ anObject.! !

!Stack methodsFor: 'accessing' stamp: 'dc 7/25/2005 10:04'!
size
	"How many objects in me ?"
	^ self linkedList size! !

!Stack methodsFor: 'accessing' stamp: 'sd 3/25/2006 15:05'!
top
	"Answer the first element of the stack without removing it."
	self notEmptyCheck.
	^ self linkedList first element! !

Object subclass: #Stream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

!Stream commentStamp: '<historical>' prior: 0!
I am an abstract class that represents an accessor for a sequence of objects. This sequence is referred to as my "contents".!

Stream subclass: #AttributedTextStream
	instanceVariableNames: 'characters attributeRuns attributeValues currentAttributes currentRun'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

!AttributedTextStream commentStamp: '<historical>' prior: 0!
a stream on Text's which keeps track of the last attribute put; new characters are added with those attributes.

instance vars:

	characters - a WriteStream of the characters in the stream
	attributeRuns - a RunArray with the attributes for the stream
	currentAttributes - the attributes to be used for new text
	attributesChanged - whether the attributes have changed since the last addition!

!AttributedTextStream class methodsFor: 'instance creation' stamp: 'gk 2/9/2004 18:50'!
new
	"For this class we override Stream class>>new since this
	class actually is created using #new, even though it is a Stream."
	
	^self basicNew initialize! !

!AttributedTextStream methodsFor: 'retrieving the text' stamp: 'ar 10/16/2001 22:39'!
contents
	| ans |
	currentRun > 0 ifTrue:[
		attributeValues nextPut: currentAttributes.
		attributeRuns nextPut: currentRun.
		currentRun := 0].
	ans := Text new: characters size.
	"this is declared private, but it's exactly what I need, and it's declared as exactly what I want it to do...."
	ans setString: characters contents  setRuns: 
		(RunArray runs: attributeRuns contents values: attributeValues contents).
	^ans! !

!AttributedTextStream methodsFor: 'access' stamp: 'ls 6/27/1998 15:09'!
currentAttributes
	"return the current attributes"
	^currentAttributes! !

!AttributedTextStream methodsFor: 'access' stamp: 'ar 10/16/2001 22:57'!
currentAttributes: newAttributes
	"set the current attributes"
	(currentRun > 0 and:[currentAttributes ~= newAttributes]) ifTrue:[
		attributeRuns nextPut: currentRun.
		attributeValues nextPut: currentAttributes.
		currentRun := 0.
	].
	currentAttributes := newAttributes.
! !

!AttributedTextStream methodsFor: 'private-initialization' stamp: 'ar 10/16/2001 22:40'!
initialize
	characters := WriteStream on: String new.
	currentAttributes := OrderedCollection new.
	currentRun := 0.
	attributeValues := WriteStream on: (Array new: 50).
	attributeRuns := WriteStream on: (Array new: 50).	! !

!AttributedTextStream methodsFor: 'stream protocol' stamp: 'ar 10/16/2001 22:38'!
nextPut: aChar
	currentRun := currentRun + 1.
	characters nextPut: aChar! !

!AttributedTextStream methodsFor: 'stream protocol' stamp: 'ar 10/16/2001 22:38'!
nextPutAll: aString
	"add an entire string with the same attributes"
	currentRun := currentRun + aString size.
	characters nextPutAll: aString.! !

!AttributedTextStream methodsFor: 'access' stamp: 'ls 9/10/1998 03:36'!
size
	"number of characters in the stream so far"
	^characters size! !

Stream subclass: #PositionableStream
	instanceVariableNames: 'collection position readLimit'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

!PositionableStream commentStamp: '<historical>' prior: 0!
I represent an accessor for a sequence of objects (a collection) that are externally named by indices so that the point of access can be repositioned. I am abstract in that I do not implement the messages next and nextPut: which are inherited from my superclass Stream.!

!PositionableStream class methodsFor: 'instance creation' stamp: ''!
on: aCollection 
	"Answer an instance of me, streaming over the elements of aCollection."

	^self basicNew on: aCollection! !

!PositionableStream class methodsFor: 'instance creation' stamp: ''!
on: aCollection from: firstIndex to: lastIndex 
	"Answer an instance of me, streaming over the elements of aCollection 
	starting with the element at firstIndex and ending with the one at 
	lastIndex."

	^self basicNew on: (aCollection copyFrom: firstIndex to: lastIndex)! !

!PositionableStream methodsFor: 'converting' stamp: 'tk 2/7/2000 11:08'!
asBinaryOrTextStream
	"Convert to a stream that can switch between bytes and characters"

	^ (RWBinaryOrTextStream with: self contentsOfEntireFile) reset! !

!PositionableStream methodsFor: 'converting' stamp: 'ar 1/2/2000 15:32'!
asZLibReadStream
	^ZLibReadStream on: collection from: position+1 to: readLimit! !

!PositionableStream methodsFor: 'testing' stamp: ''!
atEnd
	"Primitive. Answer whether the receiver can access any more objects.
	Optional. See Object documentation whatIsAPrimitive."

	<primitive: 67>
	^position >= readLimit! !

!PositionableStream methodsFor: 'accessing' stamp: 'dc 8/1/2007 16:21'!
back
	"Go back one element and return it."
	self position = 0 ifTrue: [self errorCantGoBack].
	self skip: -1.
	^ self peek! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'dc 8/1/2007 16:02'!
backChunk
	"Answer the contents of the receiver back to the previous terminator character.  Doubled terminators indicate an embedded terminator character."

	| terminator out ch |
	terminator := $!!.
	out := WriteStream on: (String new: 1000).
	[(ch := self oldBack) == nil] whileFalse: 
			[ch == terminator 
				ifTrue: 
					[self peekBack == terminator 
						ifTrue: [self oldBack	"skip doubled terminator"]
						ifFalse: [^ out contents reversed]].
			out nextPut: ch].
	^ out contents reversed! !

!PositionableStream methodsFor: 'positioning' stamp: 'tk 3/22/2002 19:33'!
backUpTo: subCollection
	"Back up the position to he subCollection.  Position must be somewhere within the stream initially.  Leave it just after it.  Return true if succeeded.  No wildcards, and case does matter."
"Example:
	| strm | strm := ReadStream on: 'zabc abdc'.
	strm setToEnd; backUpTo: 'abc'; position 
"

	| pattern startMatch |
	pattern := ReadStream on: subCollection reversed.
	startMatch := nil.
	[pattern atEnd] whileFalse: 
		[self position = 0 ifTrue: [^ false].
		self skip: -1.
		(self next) = (pattern next) 
			ifTrue: [pattern position = 1 ifTrue: [startMatch := self position]]
			ifFalse: [pattern position: 0.
					startMatch ifNotNil: [
						self position: startMatch-1.
						startMatch := nil]].
		self skip: -1].
	self position: startMatch.
	^ true

! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'yo 8/7/2003 13:04'!
basicNextChunk
	"Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character."
	| terminator out ch |
	terminator := $!!.
	out := WriteStream on: (String new: 1000).
	self skipSeparators.
	[(ch := self next) == nil] whileFalse: [
		(ch == terminator) ifTrue: [
			self peek == terminator ifTrue: [
				self next.  "skip doubled terminator"
			] ifFalse: [
				^ out contents  "terminator is not doubled; we're done!!"
			].
		].
		out nextPut: ch.
	].
	^ out contents! !

!PositionableStream methodsFor: 'data get/put' stamp: 'jm 10/5/2001 12:09'!
boolean
	"Answer the next boolean value from this (binary) stream."

	^ self next ~= 0
! !

!PositionableStream methodsFor: 'data get/put' stamp: 'jm 10/5/2001 12:11'!
boolean: aBoolean
	"Store the given boolean value on this (binary) stream."

	self nextPut: (aBoolean ifTrue: [1] ifFalse: [0]).
! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'sd 5/23/2003 14:40'!
checkForPreamble: chunk
	((chunk beginsWith: '"Change Set:') and: [ChangeSet current preambleString == nil])
		ifTrue: [ChangeSet current preambleString: chunk].
	((chunk beginsWith: '"Postscript:') and: [ChangeSet current postscriptString == nil])
		ifTrue: [ChangeSet current postscriptString: chunk].
							
! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: ''!
command: aString
	"Overridden by HtmlFileStream to append commands directly without translation.  4/5/96 tk"
	"We ignore any HTML commands.  Do nothing"! !

!PositionableStream methodsFor: 'accessing' stamp: ''!
contents
	"Answer with a copy of my collection from 1 to readLimit."

	^collection copyFrom: 1 to: readLimit! !

!PositionableStream methodsFor: 'accessing' stamp: 'sw 3/10/98 13:55'!
contentsOfEntireFile
	"For non-file streams"
	^ self contents! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'di 2/3/98 14:44'!
copyMethodChunkFrom: aStream
	"Copy the next chunk from aStream (must be different from the receiver)."
	| chunk |
	chunk := aStream nextChunkText.
	chunk runs values size = 1 "Optimize for unembellished text"
		ifTrue: [self nextChunkPut: chunk asString]
		ifFalse: [self nextChunkPutWithStyle: chunk]! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'yo 10/15/2003 19:02'!
copyMethodChunkFrom: aStream at: pos
	"Copy the next chunk from aStream (must be different from the receiver)."
	| chunk |
	aStream position: pos.
	chunk := aStream nextChunkText.
	chunk runs values size = 1 "Optimize for unembellished text"
		ifTrue: [self nextChunkPut: chunk asString]
		ifFalse: [self nextChunkPutWithStyle: chunk]! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'ar 4/11/2006 14:29'!
copyPreamble: preamble from: aStream at: pos
	"Look for a changeStamp for this method by peeking backward.
	Write a method preamble, with that stamp if found."
	| terminator last50 stamp i |
	terminator := $!!.

	"Look back to find stamp in old preamble, such as...
	Polygon methodsFor: 'private' stamp: 'di 6/25/97 21:42' prior: 34957598!! "
	aStream position: pos.
		aStream backChunk.				"to beginning of method"
		last50 := aStream backChunk.	"to get preamble"
	aStream position: pos.

	stamp := String new.
	(i := last50 findLastOccuranceOfString: 'stamp:' startingAt: 1) > 0 ifTrue:
		[stamp := (last50 copyFrom: i+8 to: last50 size) copyUpTo: $'].

	"Write the new preamble, with old stamp if any."
	self cr; nextPut: terminator.
	self nextChunkPut: (String streamContents:
		[:strm |
		strm nextPutAll: preamble.
		stamp size > 0 ifTrue:
			[strm nextPutAll: ' stamp: '; print: stamp]]).
	self cr! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'ar 4/12/2005 17:34'!
decodeString: string andRuns: runsRaw

	| strm runLength runValues newString index |
	strm := ReadStream on: runsRaw from: 1 to: runsRaw size.
	(strm peekFor: $( ) ifFalse: [^ nil].
	runLength := OrderedCollection new.
	[strm skipSeparators.
	 strm peekFor: $)] whileFalse: 
		[runLength add: (Number readFrom: strm)].

	runValues := OrderedCollection new.
	[strm atEnd not] whileTrue: 
		[runValues add: (Number readFrom: strm).
		strm next.].

	newString := WideString new: string size.
	index := 1.
	runLength with: runValues do: [:length :leadingChar |
		index to: index + length - 1 do: [:pos |
			newString at: pos put: (Character leadingChar: leadingChar code: (string at: pos) charCode).
		].
		index := index + length.
	].

	^ newString.
! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'tk 12/13/97 13:36'!
decodeStyle: runsObjData version: styleVersion
	"Decode the runs array from the ReferenceStream it is stored in."
	"Verify that the class mentioned have the same inst vars as we have now"

	| structureInfo |
	styleVersion = RemoteString currentTextAttVersion ifTrue: [
		"Matches our classes, no need for checking"
		^ (ReferenceStream on: runsObjData) next].
	structureInfo := RemoteString structureAt: styleVersion.	"or nil"
		"See SmartRefStream instVarInfo: for dfn"
	^ SmartRefStream read: runsObjData withClasses: structureInfo! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'mir 7/26/2000 13:28'!
fileIn
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimitors. The expressions are read and passed to the 
	Compiler. Answer the result of compilation."

	^ self fileInAnnouncing: 'Reading ' , self name! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'NS 1/28/2004 11:22'!
fileInAnnouncing: announcement 
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimitors. The expressions are read and passed to the 
	Compiler. Answer the result of compilation.  Put up a progress report with
     the given announcement as the title."

	| val chunk |
	announcement 
		displayProgressAt: Sensor cursorPoint
		from: 0
		to: self size
		during: 
			[:bar | 
			[self atEnd] whileFalse: 
					[bar value: self position.
					self skipSeparators.
					
					[val := (self peekFor: $!!) 
								ifTrue: [(Compiler evaluate: self nextChunk logged: false) scanFrom: self]
								ifFalse: 
									[chunk := self nextChunk.
									self checkForPreamble: chunk.
									Compiler evaluate: chunk logged: true]] 
							on: InMidstOfFileinNotification
							do: [:ex | ex resume: true].
					self skipStyleChunk].
			self close].
	"Note:  The main purpose of this banner is to flush the changes file."
	SmalltalkImage current logChange: '----End fileIn of ' , self name , '----'.
	self flag: #ThisMethodShouldNotBeThere.	"sd"
	Smalltalk forgetDoIts.
	^val! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'NS 1/28/2004 11:21'!
fileInFor: client announcing: announcement
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimitors. The expressions are read and passed to the 
	Compiler. Answer the result of compilation.  Put up a progress report with
     the given announcement as the title.
	Does NOT handle preambles or postscripts specially."
	| val chunk |
	announcement displayProgressAt: Sensor cursorPoint
		from: 0 to: self size
		during:
		[:bar |
		[self atEnd]
			whileFalse: 
				[bar value: self position.
				self skipSeparators.
				[ val := (self peekFor: $!!) ifTrue: [
						(Compiler evaluate: self nextChunk for: client logged: false) scanFrom: self
					] ifFalse: [
						chunk := self nextChunk.
						self checkForPreamble: chunk.
						Compiler evaluate: chunk for: client logged: true ].
				] on: InMidstOfFileinNotification
				  do: [ :ex | ex resume: true].
				self atEnd ifFalse: [ self skipStyleChunk ]].
		self close].
	"Note:  The main purpose of this banner is to flush the changes file."
	SmalltalkImage current logChange: '----End fileIn of ' , self name , '----'.
	Smalltalk forgetDoIts.
	^ val! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'nk 7/30/2004 17:54'!
fileInSilentlyAnnouncing: announcement 
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimitors. The expressions are read and passed to the 
	Compiler. Answer the result of compilation.  Put up a progress report with
     the given announcement as the title."

	| val chunk |
	[self atEnd] whileFalse: 
			[self skipSeparators.
			
			[val := (self peekFor: $!!) 
						ifTrue: [(Compiler evaluate: self nextChunk logged: false) scanFrom: self]
						ifFalse: 
							[chunk := self nextChunk.
							self checkForPreamble: chunk.
							Compiler evaluate: chunk logged: true]] 
					on: InMidstOfFileinNotification
					do: [:ex | ex resume: true].
			self skipStyleChunk].
	self close.
	"Note:  The main purpose of this banner is to flush the changes file."
	SmalltalkImage current  logChange: '----End fileIn of ' , self name , '----'.
	self flag: #ThisMethodShouldNotBeThere.	"sd"
	SystemNavigation new allBehaviorsDo: 
			[:cl | 
			cl
				removeSelectorSimply: #DoIt;
				removeSelectorSimply: #DoItIn:].
	^val! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: ''!
header
	"If the stream requires a standard header, override this message.  See HtmlFileStream"! !

!PositionableStream methodsFor: 'data get/put' stamp: 'jm 7/16/2001 14:43'!
int16
	"Answer the next signed, 16-bit integer from this (binary) stream."

	| n |
	n := self next.
	n := (n bitShift: 8) + (self next).
	n >= 16r8000 ifTrue: [n := n - 16r10000].
	^ n
! !

!PositionableStream methodsFor: 'data get/put' stamp: 'jm 7/16/2001 14:44'!
int16: anInteger
	"Store the given signed, 16-bit integer on this (binary) stream."

	| n |
	(anInteger < -16r8000) | (anInteger >= 16r8000)
		ifTrue: [self error: 'outside 16-bit integer range'].

	anInteger < 0
		ifTrue: [n := 16r10000 + anInteger]
		ifFalse: [n := anInteger].
	self nextPut: (n digitAt: 2).
	self nextPut: (n digitAt: 1).
! !

!PositionableStream methodsFor: 'data get/put' stamp: 'jm 7/16/2001 15:15'!
int32
	"Answer the next signed, 32-bit integer from this (binary) stream."
	"Details: As a fast check for negative number, check the high bit of the first digit"

	| n firstDigit |
	n := firstDigit := self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	firstDigit >= 128 ifTrue: [n := -16r100000000 + n].  "decode negative 32-bit integer"
	^ n
! !

!PositionableStream methodsFor: 'data get/put' stamp: 'jm 7/16/2001 14:46'!
int32: anInteger
	"Store the given signed, 32-bit integer on this (binary) stream."

	| n |
	(anInteger < -16r80000000) | (anInteger >= 16r80000000)
		ifTrue: [self error: 'outside 32-bit integer range'].

	anInteger < 0
		ifTrue: [n := 16r100000000 + anInteger]
		ifFalse: [n := anInteger].
	self nextPut: (n digitAt: 4).
	self nextPut: (n digitAt: 3).
	self nextPut: (n digitAt: 2).
	self nextPut: (n digitAt: 1).
! !

!PositionableStream methodsFor: 'testing' stamp: 'ar 1/2/2000 17:24'!
isBinary
	"Return true if the receiver is a binary byte stream"
	^collection class == ByteArray! !

!PositionableStream methodsFor: 'testing' stamp: 'dc 2/27/2007 16:16'!
isEmpty
	"Answer whether the receiver's contents has no elements."

	"Returns true if both the set of past and future sequence values of
the receiver are empty. Otherwise returns false"

	^ self atEnd and: [position = 0]! !

!PositionableStream methodsFor: 'accessing' stamp: 'tk 9/23/2001 01:14'!
last
	"Return the final element in the receiver"

	^ collection at: position! !

!PositionableStream methodsFor: 'positioning' stamp: 'hmm 10/22/1999 21:18'!
match: subCollection
	"Set the access position of the receiver to be past the next occurrence of the subCollection. Answer whether subCollection is found.  No wildcards, and case does matter."

	| pattern startMatch |
	pattern := ReadStream on: subCollection.
	startMatch := nil.
	[pattern atEnd] whileFalse: 
		[self atEnd ifTrue: [^ false].
		(self next) = (pattern next) 
			ifTrue: [pattern position = 1 ifTrue: [startMatch := self position]]
			ifFalse: [pattern position: 0.
					startMatch ifNotNil: [
						self position: startMatch.
						startMatch := nil]]].
	^ true

! !

!PositionableStream methodsFor: 'accessing' stamp: 'sw 3/10/98 13:55'!
next: anInteger 
	"Answer the next anInteger elements of my collection. Must override 
	because default uses self contents species, which might involve a large 
	collection."

	| newArray |
	newArray := collection species new: anInteger.
	1 to: anInteger do: [:index | newArray at: index put: self next].
	^newArray! !

!PositionableStream methodsFor: 'accessing' stamp: 'ar 12/23/1999 14:58'!
next: n into: aCollection
	"Read n objects into the given collection.
	Return aCollection or a partial copy if less than
	n elements have been read."
	^self next: n into: aCollection startingAt: 1! !

!PositionableStream methodsFor: 'accessing' stamp: 'ar 12/23/1999 14:54'!
next: n into: aCollection startingAt: startIndex
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."
	| obj |
	0 to: n-1 do:[:i|
		(obj := self next) == nil ifTrue:[^aCollection copyFrom: 1 to: startIndex+i-1].
		aCollection at: startIndex+i put: obj].
	^aCollection! !

!PositionableStream methodsFor: 'accessing' stamp: 'ar 1/2/2000 15:32'!
next: anInteger putAll: aCollection
	"Store the next anInteger elements from the given collection."
	^self next: anInteger putAll: aCollection startingAt: 1! !

!PositionableStream methodsFor: 'accessing' stamp: 'ar 8/12/2003 16:56'!
next: anInteger putAll: aCollection startingAt: startIndex
	"Store the next anInteger elements from the given collection."
	(startIndex = 1 and:[anInteger = aCollection size])
		ifTrue:[^self nextPutAll: aCollection].
	^self nextPutAll: (aCollection copyFrom: startIndex to: startIndex+anInteger-1)! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'yo 8/13/2003 11:59'!
nextChunk
	"Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character."
	| terminator out ch |
	terminator := $!!.
	out := WriteStream on: (String new: 1000).
	self skipSeparators.
	[(ch := self next) == nil] whileFalse: [
		(ch == terminator) ifTrue: [
			self peek == terminator ifTrue: [
				self next.  "skip doubled terminator"
			] ifFalse: [
				^ self parseLangTagFor: out contents  "terminator is not doubled; we're done!!"
			].
		].
		out nextPut: ch.
	].
	^ self parseLangTagFor: out contents.
! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'sumim 11/20/2003 18:13'!
nextChunkText
	"Deliver the next chunk as a Text.  Decode the following ]style[ chunk if present.  Position at start of next real chunk."
	| string runsRaw strm runs peek pos |
	"Read the plain text"
	string := self nextChunk.
	
	"Test for ]style[ tag"
	pos := self position.
	peek := self skipSeparatorsAndPeekNext.
	peek = $] ifFalse: [self position: pos. ^ string asText].  "no tag"
	(self upTo: $[) = ']style' ifFalse: [self position: pos. ^ string asText].  "different tag"

	"Read and decode the style chunk"
	runsRaw := self basicNextChunk.	"style encoding"
	strm := ReadStream on: runsRaw from: 1 to: runsRaw size.
	runs := RunArray scanFrom: strm.

	^ Text basicNew setString: string setRunsChecking: runs.
! !

!PositionableStream methodsFor: 'accessing' stamp: 'tk 3/27/98 08:44'!
nextDelimited: terminator
	"Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character.  For example: 'this '' was a quote'. Start postioned before the initial terminator."

	| out ch |
	out := WriteStream on: (String new: 1000).
	self atEnd ifTrue: [^ ''].
	self next == terminator ifFalse: [self skip: -1].	"absorb initial terminator"
	[(ch := self next) == nil] whileFalse: [
		(ch == terminator) ifTrue: [
			self peek == terminator ifTrue: [
				self next.  "skip doubled terminator"
			] ifFalse: [
				^ out contents  "terminator is not doubled; we're done!!"
			].
		].
		out nextPut: ch.
	].
	^ out contents! !

!PositionableStream methodsFor: 'nonhomogeneous accessing' stamp: 'sw 3/10/98 13:55'!
nextInt32
	"Read a 32-bit signed integer from the next 4 bytes"
	| s |
	s := 0.
	1 to: 4 do: [:i | s := (s bitShift: 8) + self next].
	(s bitAnd: 16r80000000) = 0
		ifTrue: [^ s]
		ifFalse: [^ -1 - s bitInvert32]! !

!PositionableStream methodsFor: 'nonhomogeneous accessing' stamp: 'sw 3/10/98 13:55'!
nextInt32Put: int32
	"Write a signed integer to the next 4 bytes"
	| pos |
	pos := int32 < 0
		ifTrue: [(0-int32) bitInvert32 + 1]
		ifFalse: [int32].
	1 to: 4 do: [:i | self nextPut: (pos digitAt: 5-i)].
	^ int32! !

!PositionableStream methodsFor: 'accessing' stamp: 'ar 12/23/1999 14:53'!
nextInto: aCollection
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."
	^self next: aCollection size into: aCollection startingAt: 1.! !

!PositionableStream methodsFor: 'accessing' stamp: 'ar 12/23/1999 14:59'!
nextInto: aCollection startingAt: startIndex
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."
	^self next: (aCollection size - startIndex+1) into: aCollection startingAt: startIndex.! !

!PositionableStream methodsFor: 'accessing' stamp: 'bf 11/24/1998 13:35'!
nextLine
	"Answer next line (may be empty), or nil if at end"

	self atEnd ifTrue: [^nil].
	^self upTo: Character cr! !

!PositionableStream methodsFor: 'nonhomogeneous accessing' stamp: 'jm 4/9/98 21:36'!
nextLittleEndianNumber: n 
	"Answer the next n bytes as a positive Integer or LargePositiveInteger, where the bytes are ordered from least significant to most significant."

	| bytes s |
	bytes := self next: n.
	s := 0.
	n to: 1 by: -1 do: [:i | s := (s bitShift: 8) bitOr: (bytes at: i)].
	^ s
! !

!PositionableStream methodsFor: 'nonhomogeneous accessing' stamp: 'di 12/6/1999 10:13'!
nextLittleEndianNumber: n put: value
	"Answer the next n bytes as a positive Integer or LargePositiveInteger, where the bytes are ordered from least significant to most significant."
	| bytes |
	bytes := ByteArray new: n.
	1 to: n do: [: i | bytes at: i put: (value digitAt: i)].
	self nextPutAll: bytes! !

!PositionableStream methodsFor: 'nonhomogeneous accessing' stamp: ''!
nextNumber: n 
	"Answer the next n bytes as a positive Integer or LargePositiveInteger."
	| s |
	s := 0.
	1 to: n do: 
		[:i | s := (s bitShift: 8) bitOr: self next asInteger].
	^ s normalize! !

!PositionableStream methodsFor: 'nonhomogeneous accessing' stamp: 'sw 3/10/98 13:55'!
nextNumber: n put: v 
	"Append to the receiver the argument, v, which is a positive 
	SmallInteger or a LargePositiveInteger, as the next n bytes.
	Possibly pad with leading zeros."

	1 to: n do: [:i | self nextPut: (v digitAt: n+1-i)].
	^ v
! !

!PositionableStream methodsFor: 'nonhomogeneous accessing' stamp: 'yo 3/1/2005 06:03'!
nextString
	"Read a string from the receiver. The first byte is the length of the string, unless it is greater than 192, in which case the first four bytes encode the length.  I expect to be in ascii mode when called (caller puts back to binary)."

	| length aByteArray |

	"read the length in binary mode"
	self binary.
	length := self next.		"first byte."
	length >= 192 ifTrue: [length := length - 192.
		1 to: 3 do: [:ii | length := length * 256 + self next]].
	aByteArray := ByteArray new: length.

	self nextInto: aByteArray.
	^aByteArray asString.
! !

!PositionableStream methodsFor: 'nonhomogeneous accessing' stamp: 'tk 6/8/1998 21:01'!
nextStringOld
	"Read a string from the receiver. The first byte is the length of the 
	string, unless it is greater than 192, in which case the first *two* bytes 
	encode the length.  Max size 16K. "

	| aString length |
	length := self next.		"first byte."
	length >= 192 ifTrue: [length := (length - 192) * 256 + self next].
	aString := String new: length.
	1 to: length do: [:ii | aString at: ii put: self next asCharacter].
	^aString! !

!PositionableStream methodsFor: 'nonhomogeneous accessing' stamp: 'yo 4/16/2001 17:56'!
nextStringPut: s 
	"Append the string, s, to the receiver.  Only used by DataStream.  Max size of 64*256*256*256."

	| length |
	(length := s size) < 192
		ifTrue: [self nextPut: length]
		ifFalse: 
			[self nextPut: (length digitAt: 4)+192.
			self nextPut: (length digitAt: 3).
			self nextPut: (length digitAt: 2).
			self nextPut: (length digitAt: 1)].
	self nextPutAll: s asByteArray.
	^s! !

!PositionableStream methodsFor: 'nonhomogeneous accessing' stamp: 'sw 3/10/98 13:55'!
nextWord
	"Answer the next two bytes from the receiver as an Integer."

	| high low |
	high := self next.
		high==nil ifTrue: [^false].
	low := self next.
		low==nil ifTrue: [^false].
	^(high asInteger bitShift: 8) + low asInteger! !

!PositionableStream methodsFor: 'nonhomogeneous accessing' stamp: 'sw 3/10/98 13:55'!
nextWordPut: aWord 
	"Append to the receiver an Integer as the next two bytes."

	self nextPut: ((aWord bitShift: -8) bitAnd: 255).
	self nextPut: (aWord bitAnd: 255).
	^aWord! !

!PositionableStream methodsFor: 'accessing' stamp: 'nk 3/18/2004 08:52'!
nextWordsInto: aBitmap 
	"Fill the word based buffer from my collection. 
	Stored on stream as Big Endian. Optimized for speed. 
	Read in BigEndian, then restoreEndianness."
	| blt pos source byteSize |
	collection class isBytes
		ifFalse: [^ self next: aBitmap size into: aBitmap startingAt: 1].

	byteSize := aBitmap byteSize.
	"is the test on collection basicSize \\ 4 necessary?"
	((self position bitAnd: 3) = 0 and: [ (collection basicSize bitAnd: 3) = 0])
		ifTrue: [source := collection.
			pos := self position.
			self skip: byteSize]
		ifFalse: ["forced to copy it into a buffer"
			source := self next: byteSize.
			pos := 0].

	"Now use BitBlt to copy the bytes to the bitmap."
	blt := (BitBlt current
				toForm: (Form new hackBits: aBitmap))
				sourceForm: (Form new hackBits: source).
	blt combinationRule: Form over. "store"
	blt sourceX: 0;
		 sourceY: pos // 4;
		 height: byteSize // 4;
		 width: 4.
	blt destX: 0;
		 destY: 0.
	blt copyBits.

	"And do whatever the bitmap needs to do to convert from big-endian order."
	aBitmap restoreEndianness.

	^ aBitmap 	"May be WordArray, ColorArray, etc"
! !

!PositionableStream methodsFor: 'accessing' stamp: 'dc 8/1/2007 16:20'!
oldBack
	"Go back one element and return it.  Use indirect messages in case I am a StandardFileStream"
	"The method is a misconception about what a stream is. A stream contains a pointer *between* elements with past and future elements. This method considers that the pointer is *on* an element. Please consider unit tests which verifies #back and #oldBack behavior. (Damien Cassou - 1 August 2007)"
	self position = 0 ifTrue: [self errorCantGoBack].
	self position = 1 ifTrue: [self position: 0.  ^ nil].
	self skip: -2.
	^ self next
! !

!PositionableStream methodsFor: 'private' stamp: ''!
on: aCollection

	collection := aCollection.
	readLimit := aCollection size.
	position := 0.
	self reset! !

!PositionableStream methodsFor: 'accessing' stamp: ''!
originalContents
	"Answer the receiver's actual contents collection, NOT a copy.  1/29/96 sw"

	^ collection! !

!PositionableStream methodsFor: 'positioning' stamp: 'di 2/15/98 14:41'!
padTo: nBytes put: aCharacter 
	"Pad using the argument, aCharacter, to the next boundary of nBytes characters."
	| rem |
	rem := nBytes - (self position \\ nBytes).
	rem = nBytes ifTrue: [^ 0].
	self next: rem put: aCharacter.! !

!PositionableStream methodsFor: 'positioning' stamp: 'di 5/25/1998 15:16'!
padToNextLongPut: char 
	"Make position be on long word boundary, writing the padding 
	character, char, if necessary."
	[self position \\ 4 = 0]
		whileFalse: [self nextPut: char]! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'sumim 11/20/2003 18:11'!
parseLangTagFor: aString

	| string peek runsRaw pos |
	string := aString.
	"Test for ]lang[ tag"
	pos := self position.
	peek := self skipSeparatorsAndPeekNext.
	peek = $] ifFalse: [self position: pos. ^ string].  "no tag"
	(self upTo: $[) = ']lang' ifTrue: [
		runsRaw := self basicNextChunk.
		string := self decodeString: aString andRuns: runsRaw
	] ifFalse: [
		self position: pos
	].
	^ string.
! !

!PositionableStream methodsFor: 'accessing' stamp: ''!
peek
	"Answer what would be returned if the message next were sent to the 
	receiver. If the receiver is at the end, answer nil."

	| nextObject |
	self atEnd ifTrue: [^nil].
	nextObject := self next.
	position := position - 1.
	^nextObject! !

!PositionableStream methodsFor: 'accessing' stamp: 'dc 8/1/2007 16:02'!
peekBack
	"Return the element at the previous position, without changing position.  Use indirect messages in case self is a StandardFileStream."

	| element |
	element := self oldBack.
	self skip: 1.
	^ element! !

!PositionableStream methodsFor: 'accessing' stamp: ''!
peekFor: anObject 
	"Answer false and do not move over the next element if it is not equal to 
	the argument, anObject, or if the receiver is at the end. Answer true 
	and increment the position for accessing elements, if the next element is 
	equal to anObject."

	| nextObject |
	self atEnd ifTrue: [^false].
	nextObject := self next.
	"peek for matching element"
	anObject = nextObject ifTrue: [^true].
	"gobble it if found"
	position := position - 1.
	^false! !

!PositionableStream methodsFor: 'positioning' stamp: ''!
position
	"Answer the current position of accessing the sequence of objects."

	^position! !

!PositionableStream methodsFor: 'positioning' stamp: ''!
position: anInteger 
	"Set the current position for accessing the objects to be anInteger, as long 
	as anInteger is within the bounds of the receiver's contents. If it is not, 
	create an error notification."

	anInteger >= 0 & (anInteger <= readLimit)
		ifTrue: [position := anInteger]
		ifFalse: [self positionError]! !

!PositionableStream methodsFor: 'private' stamp: ''!
positionError
	"Since I am not necessarily writable, it is up to my subclasses to override 
	position: if expanding the collection is preferrable to giving this error."

	self error: 'Attempt to set the position of a PositionableStream out of bounds'! !

!PositionableStream methodsFor: 'positioning' stamp: 'mir 6/29/2004 17:35'!
positionOfSubCollection: subCollection
	"Return a position such that that element at the new position equals the first element of sub, and the next elements equal the rest of the elements of sub. Begin the search at the current position.
	If no such match is found, answer 0."

	^self positionOfSubCollection: subCollection ifAbsent: [0]! !

!PositionableStream methodsFor: 'positioning' stamp: 'mir 7/5/2005 08:32'!
positionOfSubCollection: subCollection ifAbsent: exceptionBlock
	"Return a position such that that element at the new position equals the first element of sub, and the next elements equal the rest of the elements of sub. Begin the search at the current position.
	If no such match is found, answer the result of evaluating argument, exceptionBlock."

	| pattern startPosition currentPosition |
	pattern := ReadStream on: subCollection.
	startPosition := self position.
	[pattern atEnd] whileFalse: 
		[self atEnd ifTrue: [^exceptionBlock value].
		self next = pattern next
			ifFalse: [
				self position: self position-pattern position+1.
				pattern reset]].
	currentPosition := self position.
	self position: startPosition.
	^pattern atEnd
		ifTrue: [currentPosition + 1 - subCollection size]
		ifFalse: [exceptionBlock value]! !

!PositionableStream methodsFor: 'positioning' stamp: 'mir 5/14/2003 18:45'!
pushBack: aString
	"Compatibility with SocketStreams"
	self skip: aString size negated! !

!PositionableStream methodsFor: 'positioning' stamp: ''!
reset
	"Set the receiver's position to the beginning of the sequence of objects."

	position := 0! !

!PositionableStream methodsFor: 'positioning' stamp: 'sw 3/10/98 13:55'!
resetContents
	"Set the position and limits to 0."

	position := 0.
	readLimit := 0! !

!PositionableStream methodsFor: 'private' stamp: ''!
setFrom: newStart to: newStop

	position := newStart - 1.
	readLimit := newStop! !

!PositionableStream methodsFor: 'positioning' stamp: ''!
setToEnd
	"Set the position of the receiver to the end of the sequence of objects."

	position := readLimit! !

!PositionableStream methodsFor: 'positioning' stamp: ''!
skip: anInteger 
	"Set the receiver's position to be the current position+anInteger. A 
	subclass might choose to be more helpful and select the minimum of the 
	receiver's size and position+anInteger, or the maximum of 1 and 
	position+anInteger for the repositioning."

	self position: position + anInteger! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'di 6/13/97 12:00'!
skipSeparators
	[self atEnd]
		whileFalse:
		[self next isSeparator ifFalse: [^ self position: self position-1]]! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'di 1/13/98 16:08'!
skipSeparatorsAndPeekNext
	"A special function to make nextChunk fast"
	| peek |
	[self atEnd]
		whileFalse:
		[(peek := self next) isSeparator
			ifFalse: [self position: self position-1. ^ peek]]! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'tk 12/29/97 12:37'!
skipStyleChunk
	"Get to the start of the next chunk that is not a style for the previous chunk"

	| pos |
	pos := self position.
	self skipSeparators.
	self peek == $] 
		ifTrue: [(self upTo: $[) = ']text' 	"old -- no longer needed"
				"now positioned past the open bracket"
			ifFalse: [self nextChunk]]	"absorb ]style[ and its whole chunk"
				
		ifFalse: [self position: pos]	"leave untouched"
! !

!PositionableStream methodsFor: 'positioning' stamp: ''!
skipTo: anObject 
	"Set the access position of the receiver to be past the next occurrence of 
	anObject. Answer whether anObject is found."

	[self atEnd]
		whileFalse: [self next = anObject ifTrue: [^true]].
	^false! !

!PositionableStream methodsFor: 'data get/put' stamp: 'jm 9/5/2001 07:35'!
string
	"Answer the next string from this (binary) stream."

	| size |
	size := self uint16.
	^ (self next: size) asString
! !

!PositionableStream methodsFor: 'data get/put' stamp: 'jm 9/5/2001 12:09'!
string: aString
	"Store the given string on this (binary) stream. The string must contain 65535 or fewer characters."

	aString size > 16rFFFF ifTrue: [self error: 'string too long for this format'].
	self uint16: aString size.
	self nextPutAll: aString asByteArray.
! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: ''!
trailer
	"If the stream requires a standard trailer, override this message.  See HtmlFileStream"! !

!PositionableStream methodsFor: 'data get/put' stamp: 'jm 8/20/2001 07:53'!
uint16
	"Answer the next unsigned, 16-bit integer from this (binary) stream."

	| n |
	n := self next.
	n := (n bitShift: 8) + (self next).
	^ n
! !

!PositionableStream methodsFor: 'data get/put' stamp: 'jm 8/20/2001 07:53'!
uint16: anInteger
	"Store the given unsigned, 16-bit integer on this (binary) stream."

	(anInteger < 0) | (anInteger >= 16r10000)
		ifTrue: [self error: 'outside unsigned 16-bit integer range'].

	self nextPut: (anInteger digitAt: 2).
	self nextPut: (anInteger digitAt: 1).
! !

!PositionableStream methodsFor: 'data get/put' stamp: 'jm 8/20/2001 08:07'!
uint24
	"Answer the next unsigned, 24-bit integer from this (binary) stream."

	| n |
	n := self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	^ n
! !

!PositionableStream methodsFor: 'data get/put' stamp: 'jm 8/20/2001 08:07'!
uint24: anInteger
	"Store the given unsigned, 24-bit integer on this (binary) stream."

	(anInteger < 0) | (anInteger >= 16r1000000)
		ifTrue: [self error: 'outside unsigned 24-bit integer range'].

	self nextPut: (anInteger digitAt: 3).
	self nextPut: (anInteger digitAt: 2).
	self nextPut: (anInteger digitAt: 1).
! !

!PositionableStream methodsFor: 'data get/put' stamp: 'jm 8/20/2001 07:53'!
uint32
	"Answer the next unsigned, 32-bit integer from this (binary) stream."

	| n |
	n := self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	^ n
! !

!PositionableStream methodsFor: 'data get/put' stamp: 'jm 8/20/2001 07:52'!
uint32: anInteger
	"Store the given unsigned, 32-bit integer on this (binary) stream."

	(anInteger < 0) | (anInteger >= 16r100000000)
		ifTrue: [self error: 'outside unsigned 32-bit integer range'].

	self nextPut: (anInteger digitAt: 4).
	self nextPut: (anInteger digitAt: 3).
	self nextPut: (anInteger digitAt: 2).
	self nextPut: (anInteger digitAt: 1).
! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: ''!
unCommand
	"If this read stream is at a <, then skip up to just after the next >.  For removing html commands."
	| char |
	[self peek = $<] whileTrue: ["begin a block"
		[self atEnd == false and: [self next ~= $>]] whileTrue.
		"absorb characters"
		].
 ! !

!PositionableStream methodsFor: 'positioning' stamp: 'tak 8/5/2005 10:34'!
untilEndWithFork: aBlock displayingProgress: aString 
	| sem done result |
	sem := Semaphore new.
	done := false.
	[[result := aBlock value]
		ensure: [done := true.
			sem signal]] fork.
	self
		untilEnd: [done
				ifTrue: [^ result].
			(Delay forSeconds: 0.2) wait]
		displayingProgress: aString.
	sem wait.
	^ result! !

!PositionableStream methodsFor: 'accessing' stamp: ''!
upTo: anObject 
	"Answer a subcollection from the current access position to the 
	occurrence (if any, but not inclusive) of anObject in the receiver. If 
	anObject is not in the collection, answer the entire rest of the receiver."
	| newStream element |
	newStream := WriteStream on: (collection species new: 100).
	[self atEnd or: [(element := self next) = anObject]]
		whileFalse: [newStream nextPut: element].
	^newStream contents! !

!PositionableStream methodsFor: 'accessing' stamp: 'tk 7/18/1999 17:10'!
upToAll: aCollection
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aCollection. If aCollection is not in the stream, answer the entire rest of the stream."

	| startPos endMatch result |
	startPos := self position.
	(self match: aCollection) 
		ifTrue: [endMatch := self position.
			self position: startPos.
			result := self next: endMatch - startPos - aCollection size.
			self position: endMatch.
			^ result]
		ifFalse: [self position: startPos.
			^ self upToEnd]! !

!PositionableStream methodsFor: 'accessing' stamp: 'BG 2/19/2004 14:06'!
upToEnd
	"Answer a subcollection from the current access position through the last element of the receiver."

	| newStream |
	newStream := WriteStream on: (collection species new: 100).
	[self atEnd] whileFalse: [ newStream nextPut: self next ].
	^ newStream contents! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: ''!
verbatim: aString
	"Do not attempt to translate the characters.  Use to override nextPutAll:"
	^ self nextPutAll: aString! !

PositionableStream subclass: #ReadStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

!ReadStream commentStamp: '<historical>' prior: 0!
I represent an accessor for a sequence of objects that can only read objects from the sequence.!

!ReadStream class methodsFor: 'instance creation' stamp: ''!
on: aCollection from: firstIndex to: lastIndex 
	"Answer with a new instance streaming over a copy of aCollection from
	firstIndex to lastIndex."

	^self basicNew
		on: aCollection
		from: firstIndex
		to: lastIndex! !

!ReadStream methodsFor: 'accessing' stamp: ''!
ascii! !

!ReadStream methodsFor: 'accessing' stamp: ''!
binary! !

!ReadStream methodsFor: 'file stream compatibility' stamp: 'nk 12/13/2002 12:00'!
localName
	^'ReadStream'! !

!ReadStream methodsFor: 'accessing' stamp: ''!
next
	"Primitive. Answer the next object in the Stream represented by the
	receiver. Fail if the collection of this stream is not an Array or a String.
	Fail if the stream is positioned at its end, or if the position is out of
	bounds in the collection. Optional. See Object documentation
	whatIsAPrimitive."

	<primitive: 65>
	position >= readLimit
		ifTrue: [^nil]
		ifFalse: [^collection at: (position := position + 1)]! !

!ReadStream methodsFor: 'accessing' stamp: 'ls 8/16/1998 00:46'!
next: anInteger 
	"Answer the next anInteger elements of my collection.  overriden for efficiency"

	| ans endPosition |

	endPosition := position + anInteger  min:  readLimit.
	ans := collection copyFrom: position+1 to: endPosition.
	position := endPosition.
	^ans
! !

!ReadStream methodsFor: 'accessing' stamp: 'ar 12/23/1999 15:01'!
next: n into: aCollection startingAt: startIndex
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."
	| max |
	max := (readLimit - position) min: n.
	aCollection 
		replaceFrom: startIndex 
		to: startIndex+max-1
		with: collection
		startingAt: position+1.
	position := position + max.
	max = n
		ifTrue:[^aCollection]
		ifFalse:[^aCollection copyFrom: 1 to: startIndex+max-1]! !

!ReadStream methodsFor: 'accessing' stamp: ''!
nextPut: anObject

	self shouldNotImplement! !

!ReadStream methodsFor: 'private' stamp: ''!
on: aCollection from: firstIndex to: lastIndex

	| len |
	collection := aCollection.
	readLimit :=  lastIndex > (len := collection size)
						ifTrue: [len]
						ifFalse: [lastIndex].
	position := firstIndex <= 1
				ifTrue: [0]
				ifFalse: [firstIndex - 1]! !

!ReadStream methodsFor: 'file stream compatibility' stamp: 'nk 12/13/2002 12:01'!
openReadOnly! !

!ReadStream methodsFor: 'file stream compatibility' stamp: 'nk 12/13/2002 12:00'!
readOnly! !

!ReadStream methodsFor: 'accessing' stamp: 'ajh 9/5/2002 22:11'!
readStream
	"polymorphic with SequenceableCollection.  Return self"

	^ self! !

!ReadStream methodsFor: 'accessing' stamp: 'ar 11/2/1998 12:20'!
size
	"Compatibility with other streams (e.g., FileStream)"
	^readLimit! !

!ReadStream methodsFor: 'accessing' stamp: 'ls 9/12/1998 00:59'!
upTo: anObject
	"fast version using indexOf:"
	| start end |

	start := position+1.
	end := collection indexOf: anObject startingAt: start ifAbsent: [ 0 ].

	"not present--return rest of the collection"	
	end = 0 ifTrue: [ ^self upToEnd ].

	"skip to the end and return the data passed over"
	position := end.
	^collection copyFrom: start to: (end-1)! !

!ReadStream methodsFor: 'accessing' stamp: 'ls 9/12/1998 00:59'!
upToEnd
	| start |

	start := position+1.
	position := collection size.
	^collection copyFrom: start to: position! !

PositionableStream subclass: #WriteStream
	instanceVariableNames: 'writeLimit'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

!WriteStream commentStamp: '<historical>' prior: 0!
I represent an accessor for a sequence of objects that can only store objects in the sequence.!

WriteStream subclass: #LimitedWriteStream
	instanceVariableNames: 'limit limitBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

!LimitedWriteStream commentStamp: '<historical>' prior: 0!
A LimitedWriteStream is a specialized WriteStream that has a maximum size of the collection it streams over. When this limit is reached a special limitBlock is executed. This can for example be used to "bail out" of lengthy streaming operations before they have finished.  For a simple example take a look at the universal Object printString.

The message SequenceableCollection class streamContents:limitedTo: creates a LimitedWriteStream. In this case it prevents very large (or possibly recursive) object structures to "overdo" their textual representation. !

!LimitedWriteStream methodsFor: 'accessing' stamp: 'BG 3/13/2004 16:03'!
nextPut: anObject 
	"Ensure that the limit is not exceeded"

 position >= limit ifTrue: [limitBlock value]
    ifFalse: [super nextPut: anObject].
! !

!LimitedWriteStream methodsFor: 'as yet unclassified' stamp: 'BG 3/13/2004 13:18'!
nextPutAll: aCollection

	| newEnd |
	collection class == aCollection class ifFalse:
		[^ super nextPutAll: aCollection ].

	newEnd := position + aCollection size.
	newEnd > limit ifTrue: [
		super nextPutAll: (aCollection copyFrom: 1 to: (limit - position max: 0)).
		^ limitBlock value.
	].
	newEnd > writeLimit ifTrue: [
		self growTo: newEnd + 10
	].

	collection replaceFrom: position+1 to: newEnd  with: aCollection startingAt: 1.
	position := newEnd.! !

!LimitedWriteStream methodsFor: 'as yet unclassified' stamp: 'di 10/28/2001 12:49'!
pastEndPut: anObject
	collection size >= limit ifTrue: [limitBlock value].  "Exceptional return"
	^ super pastEndPut: anObject! !

!LimitedWriteStream methodsFor: 'as yet unclassified' stamp: 'di 6/20/97 09:07'!
setLimit: sizeLimit limitBlock: aBlock
	"Limit the numer of elements this stream will write..."
	limit := sizeLimit.
	"Execute this (typically ^ contents) when that limit is exceded"
	limitBlock := aBlock! !

WriteStream subclass: #ReadWriteStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

!ReadWriteStream commentStamp: '<historical>' prior: 0!
I represent an accessor for a sequence of objects. My instances can both read and store objects.!

ReadWriteStream subclass: #RWBinaryOrTextStream
	instanceVariableNames: 'isBinary'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

!RWBinaryOrTextStream commentStamp: '<historical>' prior: 0!
A simulation of a FileStream, but living totally in memory.  Hold the contents of a file or web page from the network.  Can then fileIn like a normal FileStream.

Need to be able to switch between binary and text, as a FileStream does, without recopying the whole collection.  Convert to binary upon input and output.  Always keep as text internally.!

!RWBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'tk 2/4/2000 09:15'!
asBinaryOrTextStream

	^ self! !

!RWBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'tk 6/25/97 13:22'!
ascii
	isBinary := false! !

!RWBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'tk 6/20/97 19:46'!
binary
	isBinary := true! !

!RWBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'tk 6/21/97 12:49'!
contents
	"Answer with a copy of my collection from 1 to readLimit."

	| newArray |
	isBinary ifFalse: [^ super contents].	"String"
	readLimit := readLimit max: position.
	newArray := ByteArray new: readLimit.
	^ newArray replaceFrom: 1
		to: readLimit
		with: collection
		startingAt: 1.! !

!RWBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'jm 11/4/97 08:25'!
contentsOfEntireFile
	"For compatibility with file streams."

	^ self contents! !

!RWBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'tk 6/20/97 19:47'!
isBinary
	^ isBinary! !

!RWBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'tk 12/13/97 13:07'!
next

	| byte |
	^ isBinary 
			ifTrue: [byte := super next.
				 byte ifNil: [nil] ifNotNil: [byte asciiValue]]
			ifFalse: [super next].
! !

!RWBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'ar 4/10/2005 19:26'!
next: anInteger 
	"Answer the next anInteger elements of my collection. Must override to get class right."

	| newArray |
	newArray := (isBinary ifTrue: [ByteArray] ifFalse: [ByteString]) new: anInteger.
	^ self nextInto: newArray! !

!RWBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'ls 3/27/2000 22:24'!
next: n into: aCollection startingAt: startIndex
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than n elements have been read."
	"Overriden for efficiency"
	| max |
	max := (readLimit - position) min: n.
	aCollection 
		replaceFrom: startIndex 
		to: startIndex+max-1
		with: collection
		startingAt: position+1.
	position := position + max.
	max = n
		ifTrue:[^aCollection]
		ifFalse:[^aCollection copyFrom: 1 to: startIndex+max-1]! !

!RWBinaryOrTextStream methodsFor: 'writing' stamp: 'ar 8/12/2003 16:54'!
next: anInteger putAll: aCollection startingAt: startIndex
	^super next: anInteger putAll: aCollection asString startingAt: startIndex! !

!RWBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'tk 6/20/97 07:38'!
nextPut: charOrByte

	super nextPut: charOrByte asCharacter! !

!RWBinaryOrTextStream methodsFor: 'writing' stamp: 'ar 8/12/2003 16:54'!
nextPutAll: aCollection
	^super nextPutAll: aCollection asString! !

!RWBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'tk 1/14/1999 20:16'!
padToEndWith: aChar
	"We don't have pages, so we are at the end, and don't need to pad."! !

!RWBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'tk 6/21/97 13:04'!
reset
	"Set the receiver's position to the beginning of the sequence of objects."

	super reset.
	isBinary ifNil: [isBinary := false].
	collection class == ByteArray ifTrue: ["Store as String and convert as needed."
		collection := collection asString.
		isBinary := true].
! !

!RWBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'tk 10/1/1998 11:54'!
setFileTypeToObject
	"do nothing.  We don't have a file type"! !

!RWBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'tk 6/20/97 19:47'!
text
	isBinary := false! !

!RWBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'ar 4/10/2005 19:27'!
upToEnd
	"Must override to get class right."
	| newArray |
	newArray := (isBinary ifTrue: [ByteArray] ifFalse: [ByteString]) new: self size - self position.
	^ self nextInto: newArray! !

!ReadWriteStream methodsFor: 'testing' stamp: 'tk 11/29/2001 12:47'!
= other

	(self class == ReadWriteStream and: [other class == ReadWriteStream]) ifFalse: [
		^ super = other].	"does an identity test.  Don't read contents of FileStream"
	^ self position = other position and: [self contents = other contents]! !

!ReadWriteStream methodsFor: 'converting' stamp: 'yo 7/16/2003 14:59'!
asUnZippedStream
	| isGZip outputStream first strm archive which |
	"Decompress this file if needed, and return a stream.  No file is written.  File extension may be .gz or anything else.  Also works on archives (.zip, .gZip)."

	strm := self binary.
	strm isZipArchive ifTrue: [
		archive := ZipArchive new readFrom: strm.
		which := archive members detect: [:any | any fileName asLowercase endsWith: '.ttf'] 
								ifNone: [nil].
		which ifNil: [archive close.
					^ self error: 'Can''t find .ttf file in archive'].
		strm := which contentStream.
		archive close].

	first := strm next.
	isGZip := (strm next * 256 + first) = (GZipConstants gzipMagic).
	strm skip: -2.
	isGZip 
		ifTrue: [outputStream := (MultiByteBinaryOrTextStream with:
									(GZipReadStream on: strm) upToEnd) reset.
				strm close]
		ifFalse: [outputStream := strm].
	^ outputStream! !

!ReadWriteStream methodsFor: 'file status' stamp: ''!
close
	"Presumably sets the status of the receiver to be closed. This message does 
	nothing at this level, but is included for FileStream compatibility."

	^self! !

!ReadWriteStream methodsFor: 'file status' stamp: ''!
closed
	"If you have close (for FileStream compatibility), you must respond to closed.  The result in nonsense here.  TK 29 May 96"

	^ false! !

!ReadWriteStream methodsFor: 'accessing' stamp: ''!
contents
	"Answer with a copy of my collection from 1 to readLimit."

	readLimit := readLimit max: position.
	^collection copyFrom: 1 to: readLimit! !

!ReadWriteStream methodsFor: 'fileIn/Out' stamp: 'di 7/17/97 16:12'!
fileInObjectAndCode
	"This file may contain:
1) a fileIn of code  
2) just an object in SmartReferenceStream format 
3) both code and an object.
	File it in and return the object.  Note that self must be a FileStream or RWBinaryOrTextStream.  Maybe ReadWriteStream incorporate RWBinaryOrTextStream?"
	| refStream object |
	self text.
	self peek asciiValue = 4
		ifTrue: [  "pure object file"
			refStream := SmartRefStream on: self.
			object := refStream nextAndClose]
		ifFalse: [  "objects mixed with a fileIn"
			self fileIn.  "reads code and objects, then closes the file"
			object := SmartRefStream scannedObject].	"set by side effect of one of the chunks"
	SmartRefStream scannedObject: nil.  "clear scannedObject"
	^ object! !

!ReadWriteStream methodsFor: 'fileIn/Out' stamp: ''!
fileNameEndsWith: aString
	"See comment in FileStream fileNameEndsWith:"

	^false! !

!ReadWriteStream methodsFor: 'fileIn/Out' stamp: 'RAA 4/6/2001 18:32'!
fileOutChangeSet: aChangeSetOrNil andObject: theObject
	"Write a file that has both the source code for the named class and an object as bits.  Any instance-specific object will get its class written automatically."

	"An experimental version to fileout a changeSet first so that a project can contain its own classes"


	self setFileTypeToObject.
		"Type and Creator not to be text, so can attach correctly to an email msg"
	self header; timeStamp.

	aChangeSetOrNil ifNotNil: [
		aChangeSetOrNil fileOutPreambleOn: self.
		aChangeSetOrNil fileOutOn: self.
		aChangeSetOrNil fileOutPostscriptOn: self.
	].
	self trailer.	"Does nothing for normal files.  HTML streams will have trouble with object data"

	"Append the object's raw data"
	(SmartRefStream on: self)
		nextPut: theObject;  "and all subobjects"
		close.		"also closes me"
! !

!ReadWriteStream methodsFor: 'fileIn/Out' stamp: 'sd 5/23/2003 14:41'!
fileOutChanges
	"Append to the receiver a description of all class changes."
	Cursor write showWhile:
		[self header; timeStamp.
		ChangeSet current fileOutOn: self.
		self trailer; close]! !

!ReadWriteStream methodsFor: 'fileIn/Out' stamp: 'yo 8/16/2004 13:45'!
fileOutClass: extraClass andObject: theObject
	"Write a file that has both the source code for the named class and an object as bits.  Any instance-specific object will get its class written automatically."

	| class srefStream |
	self setFileTypeToObject.
		"Type and Creator not to be text, so can attach correctly to an email msg"
	self text.
	self header; timeStamp.

	extraClass ifNotNil: [
		class := extraClass.	"A specific class the user wants written"
		class sharedPools size > 0 ifTrue:
			[class shouldFileOutPools
				ifTrue: [class fileOutSharedPoolsOn: self]].
		class fileOutOn: self moveSource: false toFile: 0].
	self trailer.	"Does nothing for normal files.  HTML streams will have trouble with object data"
	self binary.

	"Append the object's raw data"
	srefStream := SmartRefStream on: self.
	srefStream nextPut: theObject.  "and all subobjects"
	srefStream close.		"also closes me"
! !

!ReadWriteStream methodsFor: 'fileIn/Out' stamp: 'tk 3/13/98 22:25'!
fileOutClass: extraClass andObject: theObject blocking: anIdentDict
	"Write a file that has both the source code for the named class and an object as bits.  Any instance-specific object will get its class written automatically.  Accept a list of objects to map to nil or some other object (blockers).  In addition to teh choices in each class's objectToStoreOnDataStream"

	| class srefStream |
	self setFileTypeToObject.
		"Type and Creator not to be text, so can attach correctly to an email msg"
	self header; timeStamp.

	extraClass ifNotNil: [
		class := extraClass.	"A specific class the user wants written"
		class sharedPools size > 0 ifTrue:
			[class shouldFileOutPools
				ifTrue: [class fileOutSharedPoolsOn: self]].
		class fileOutOn: self moveSource: false toFile: 0].
	self trailer.	"Does nothing for normal files.  HTML streams will have trouble with object data"

	"Append the object's raw data"
	srefStream := SmartRefStream on: self.
	srefStream blockers: anIdentDict.
	srefStream nextPut: theObject.  "and all subobjects"
	srefStream close.		"also closes me"
! !

!ReadWriteStream methodsFor: 'testing' stamp: 'tk 12/2/2001 17:13'!
hash

	self class == ReadWriteStream ifFalse: [^ super hash].
	^ (self position + readLimit + 53) hash! !

!ReadWriteStream methodsFor: 'testing' stamp: 'nk 8/21/2004 15:47'!
isZipArchive
	"Determine if this appears to be a valid Zip archive"
	| sig |
	self binary.
	sig := self next: 4.
	self position: self position - 4. "rewind"
	^ZipArchive validSignatures includes: sig! !

!ReadWriteStream methodsFor: 'accessing' stamp: ''!
name
	^ 'a stream'   "for fileIn compatibility"! !

!ReadWriteStream methodsFor: 'accessing' stamp: ''!
next
	"Primitive. Return the next object in the Stream represented by the
	receiver. Fail if the collection of this stream is not an Array or a String.
	Fail if the stream is positioned at its end, or if the position is out of
	bounds in the collection. Optional. See Object documentation
	whatIsAPrimitive."

	<primitive: 65>
	"treat me as a FIFO"
	position >= readLimit
		ifTrue: [^nil]
		ifFalse: [^collection at: (position := position + 1)]! !

!ReadWriteStream methodsFor: 'accessing' stamp: 'ar 8/5/2003 02:23'!
next: anInteger 
	"Answer the next anInteger elements of my collection.  overriden for efficiency"

	| ans endPosition |
	readLimit := readLimit max: position.

	endPosition := position + anInteger  min:  readLimit.
	ans := collection copyFrom: position+1 to: endPosition.
	position := endPosition.
	^ans
! !

!ReadWriteStream methodsFor: 'converting' stamp: 'ajh 9/14/2002 20:37'!
readStream
	"polymorphic with SequenceableCollection.  Return self"

	^ self! !

ReadWriteStream subclass: #Transcripter
	instanceVariableNames: 'frame para'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

!Transcripter commentStamp: '<historical>' prior: 0!
Transcripter is a dog-simple scrolling stream with display.  It is intended to operate with no support from MVC or color in a minimal, or headless version of Squeak.  No attention has been paid to appearance or performance.!

!Transcripter class methodsFor: 'utilities' stamp: 'di 11/3/2000 18:47'!
emergencyEvaluator
	(Transcripter newInFrame: (0@0 corner: 320@200))
		show: 'Type ''revert'' to revert your last method change.
Type ''exit'' to exit the emergency evaluator.';
		readEvalPrint! !

!Transcripter class methodsFor: 'instance creation' stamp: 'di 8/14/97 12:09'!
newInFrame: frame
"
(Transcripter newInFrame: (0@0 extent: 100@200))
	nextPutAll: 'Hello there'; endEntry;
	cr; print: 355.0/113; endEntry;
	readEvalPrint.
"
	| transcript |
	transcript := self on: (String new: 100).
	transcript initInFrame: frame.
	^ transcript clear! !

!Transcripter class methodsFor: 'instance creation' stamp: 'ar 11/16/1999 20:16'!
startTranscriptProcess   "Transcripter startTranscriptProcess"
	| activeProcess |
	Transcript := self newInFrame: Display boundingBox.
	activeProcess := [Transcript readEvalPrint.
					Smalltalk processShutDownList: true; quitPrimitive]
						newProcess
					priority: Processor userSchedulingPriority.
	activeProcess resume.
	Processor terminateActive
! !

!Transcripter methodsFor: 'private' stamp: 'di 8/14/97 12:12'!
black
	Display depth = 1 ifTrue: [^ Bitmap with: 16rFFFFFFFF "Works without color support"].
	^ Color black! !

!Transcripter methodsFor: 'accessing' stamp: 'di 8/14/97 12:41'!
clear
	Display fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	self on: (String new: 100); endEntry! !

!Transcripter methodsFor: 'command line' stamp: 'di 8/12/97 22:11'!
confirm: queryString 
	| choice |
	[true]
		whileTrue: 
			[choice := self request: queryString , '
Please type yes or no followed by return'.
			choice first asUppercase = $Y ifTrue: [^ true].
			choice first asUppercase = $N ifTrue: [^ false]]! !

!Transcripter methodsFor: 'accessing' stamp: 'di 8/14/97 12:44'!
endEntry
	| c d cb |
	c := self contents.
	Display extent ~= DisplayScreen actualScreenSize ifTrue:
		["Handle case of user resizing physical window"
		DisplayScreen startUp.
		frame := frame intersect: Display boundingBox.
		^ self clear; show: c].
	para setWithText: c asText
		style: TextStyle default
		compositionRectangle: ((frame insetBy: 4) withHeight: 9999)
		clippingRectangle: frame
		foreColor: self black backColor: self white.
	d := para compositionRectangle bottom - frame bottom.
	d > 0 ifTrue:
		["Scroll up to keep all contents visible"
		cb := para characterBlockAtPoint: para compositionRectangle topLeft
											+ (0@(d+para lineGrid)).
		self on: (c copyFrom: cb stringIndex to: c size).
		readLimit:= position:= collection size.
		^ self endEntry].
	para display! !

!Transcripter methodsFor: 'initialization' stamp: 'di 8/14/97 12:44'!
initInFrame: rect
	frame := rect insetBy: 2.  "Leave room for border"
	para := Paragraph withText: self contents asText
				style: TextStyle default
				compositionRectangle: ((frame insetBy: 4) withHeight: 9999)
				clippingRectangle: frame
				foreColor: self black backColor: self white! !

!Transcripter methodsFor: 'command line' stamp: 'di 11/3/2000 18:52'!
readEvalPrint
	| line okToRevert |
	okToRevert := true.
	[#('quit' 'exit' 'done' ) includes: (line := self request: '>')]
		whileFalse:
		[line = 'revert'
		ifTrue: [okToRevert
			ifTrue: [Utilities revertLastMethodSubmission.
					self cr; show: 'reverted: ' , Utilities mostRecentlySubmittedMessage.
					okToRevert := false]
			ifFalse: [self cr; show: 'Only one level of revert currently supported']]
		ifFalse: [self cr; show: ([Compiler evaluate: line] ifError: [:err :ex | err])]]! !

!Transcripter methodsFor: 'command line' stamp: 'sma 2/26/2000 19:39'!
request: prompt
	| startPos char contents | 
	self cr; show: prompt.
	startPos := position.
	[[Sensor keyboardPressed] whileFalse.
	(char := Sensor keyboard) = Character cr]
		whileFalse:
		[char = Character backspace
			ifTrue: [readLimit := position := (position - 1 max: startPos)]
			ifFalse: [self nextPut: char].
		self endEntry].
	contents := self contents.
	^ contents copyFrom: startPos + 1 to: contents size! !

!Transcripter methodsFor: 'accessing' stamp: 'sma 2/26/2000 19:35'!
show: anObject
	self nextPutAll: anObject asString; endEntry! !

!Transcripter methodsFor: 'private' stamp: 'di 8/14/97 12:12'!
white
	Display depth = 1 ifTrue: [^ Bitmap with: 0 "Works without color support"].
	^ Color white! !

WriteStream subclass: #TextStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

!TextStream methodsFor: 'as yet unclassified' stamp: ''!
applyAttribute: att beginningAt: startPos
	collection addAttribute: att from: startPos to: self position! !

!TextStream methodsFor: 'as yet unclassified' stamp: 'dvf 10/1/2003 02:51'!
nextPutAll: aCollection 
	"Optimized access to get around Text at:Put: overhead"
	| n |
	n := aCollection size.
     position + n > writeLimit
       ifTrue:
        [self growTo: position + n + 10].
	collection 
		replaceFrom: position+1
		to: position + n
		with: aCollection
		startingAt: 1.
	position := position + n! !

!TextStream methodsFor: 'as yet unclassified' stamp: ''!
withAttribute: att do: strmBlock
	| pos1 val |
	pos1 := self position.
	val := strmBlock value.
	collection addAttribute: att from: pos1+1 to: self position.
	^ val! !

!TextStream methodsFor: 'as yet unclassified' stamp: 'djp 11/6/1999 20:30'!
withAttributes: attributes do: streamBlock 
	| pos1 val |
	pos1 := self position.
	val := streamBlock value.
	attributes do: [:attribute |
		collection
			addAttribute: attribute
			from: pos1 + 1
			to: self position].
	^ val! !

WriteStream subclass: #TranscriptStream
	instanceVariableNames: ''
	classVariableNames: 'AccessSema'
	poolDictionaries: ''
	category: 'Collections-Streams'!

!TranscriptStream commentStamp: '<historical>' prior: 0!
This class is a much simpler implementation of Transcript protocol that supports multiple views and very simple conversion to morphic.  Because it inherits from Stream, it is automatically compatible with code that is designe to write to streams.!

!TranscriptStream class methodsFor: 'toolbuilder' stamp: 'ar 2/11/2005 18:10'!
buildWith: aBuilder
	^self new buildWith: aBuilder! !

!TranscriptStream class methodsFor: 'class initialization' stamp: 'asm 4/11/2003 12:05'!
initialize

	self registerInFlapsRegistry.	! !

!TranscriptStream class methodsFor: 'as yet unclassified' stamp: 'di 5/8/1998 13:51'!
new
	^ self on: (String new: 1000)
"
INSTALLING:
TextCollector allInstances do:
	[:t | t breakDependents.
	t become: TranscriptStream new].

TESTING: (Execute this text in a workspace)
Do this first...
	tt := TranscriptStream new.
	tt openLabel: 'Transcript test 1'.
Then this will open a second view -- ooooh...
	tt openLabel: 'Transcript test 2'.
And finally make them do something...
	tt clear.
	[Sensor anyButtonPressed] whileFalse:
		[1 to: 20 do: [:i | tt print: (2 raisedTo: i-1); cr; endEntry]].
"! !

!TranscriptStream class methodsFor: 'as yet unclassified' stamp: 'di 5/8/1998 12:44'!
newTranscript: aTextCollector 
	"Store aTextCollector as the value of the system global Transcript."
	Smalltalk at: #Transcript put: aTextCollector! !

!TranscriptStream class methodsFor: 'as yet unclassified' stamp: 'sw 1/29/2002 19:56'!
openMorphicTranscript
	"Have the current project's transcript open up as a morph"

	^ Transcript openAsMorph! !

!TranscriptStream class methodsFor: 'class initialization' stamp: 'asm 4/11/2003 12:06'!
registerInFlapsRegistry
	"Register the receiver in the system's flaps registry"
	self environment
		at: #Flaps
		ifPresent: [:cl | cl registerQuad: #(TranscriptStream		openMorphicTranscript	'Transcript'			'A Transcript is a window usable for logging and debugging; browse references to #Transcript for examples of how to write to it.')
						forFlapNamed: 'Tools']
! !

!TranscriptStream class methodsFor: 'class initialization' stamp: 'asm 4/11/2003 12:41'!
unload
	"Unload the receiver from global registries"

	self environment at: #Flaps ifPresent: [:cl |
	cl unregisterQuadsWithReceiver: self] ! !

!TranscriptStream class methodsFor: 'window color' stamp: 'sw 2/26/2002 14:46'!
windowColorSpecification
	"Answer a WindowColorSpec object that declares my preference"

	^ WindowColorSpec classSymbol: self name wording: 'Transcript' brightColor: #lightOrange pastelColor: #paleOrange helpMessage: 'The system transcript'! !

!TranscriptStream methodsFor: 'stream extensions' stamp: 'sma 3/15/2000 21:28'!
bs
	self position > 0 ifTrue: [^ self skip: -1].
	self changed: #bs! !

!TranscriptStream methodsFor: 'toolbuilder' stamp: 'ar 2/11/2005 20:36'!
buildWith: builder
	| windowSpec textSpec |
	windowSpec := builder pluggableWindowSpec new.
	windowSpec model: self.
	windowSpec label: 'Transcript'.
	windowSpec children: OrderedCollection new.

	textSpec := builder pluggableTextSpec new.
	textSpec 
		model: self;
		menu: #codePaneMenu:shifted:;
		frame: (0@0corner: 1@1).
	windowSpec children add: textSpec.

	^builder build: windowSpec! !

!TranscriptStream methodsFor: 'access' stamp: 'di 3/16/1999 21:38'!
characterLimit
	"Tell the views how much to retain on screen"
	^ 20000! !

!TranscriptStream methodsFor: 'stream extensions' stamp: 'di 5/8/1998 12:35'!
clear
	"Clear all characters and redisplay the view"
	self changed: #clearText.
	self reset! !

!TranscriptStream methodsFor: 'initialization' stamp: 'gm 2/16/2003 20:39'!
closeAllViews
	"Transcript closeAllViews"

	self dependents do: 
			[:d | 
			(d isKindOf: PluggableTextView) 
				ifTrue: [d topView controller closeAndUnscheduleNoTerminate].
			(d isSystemWindow) ifTrue: [d delete]]! !

!TranscriptStream methodsFor: 'model protocol' stamp: 'di 5/27/1998 16:44'!
codePaneMenu: aMenu shifted: shifted
	"Note that unless we override perform:orSendTo:, PluggableTextController will respond to all menu items"
	^ StringHolder basicNew codePaneMenu: aMenu shifted: shifted
! !

!TranscriptStream methodsFor: 'stream extensions' stamp: 'mir 1/11/2000 11:41'!
endEntry
	"Display all the characters since the last endEntry, and reset the stream"
	self semaphore critical:[
		self changed: #appendEntry.
		self reset.
	].! !

!TranscriptStream methodsFor: 'stream extensions' stamp: 'md 8/2/2005 23:09'!
flush
	self endEntry
! !

!TranscriptStream methodsFor: 'initialization' stamp: 'di 5/8/1998 13:12'!
open
	| openCount |
	openCount := 0.
	self dependents do:
		[:d | ((d isKindOf: PluggableTextView) or:
			[d isKindOf: PluggableTextMorph]) ifTrue: [openCount := openCount + 1]].
	openCount = 0
		ifTrue: [self openLabel: 'Transcript']
		ifFalse: [self openLabel: 'Transcript #' , (openCount+1) printString]! !

!TranscriptStream methodsFor: 'initialization' stamp: 'sw 6/12/2001 13:06'!
openAsMorph
	"Answer a morph viewing this transcriptStream"

	^ (self openAsMorphLabel: 'Transcript') applyModelExtent! !

!TranscriptStream methodsFor: 'initialization' stamp: 'di 5/27/1998 16:36'!
openAsMorphLabel: labelString 
	"Build a morph viewing this transcriptStream"
	| window |
	window := (SystemWindow labelled: labelString) model: self.
	window addMorph: (PluggableTextMorph on: self text: nil accept: nil
			readSelection: nil menu: #codePaneMenu:shifted:)
		frame: (0@0 corner: 1@1).
	^ window! !

!TranscriptStream methodsFor: 'initialization' stamp: 'sma 4/30/2000 10:16'!
openLabel: aString 
	"Open a window on this transcriptStream"

	| topView codeView |
	Smalltalk isMorphic ifTrue: [^ (self openAsMorphLabel: aString) openInWorld].

	topView := (StandardSystemView new) model: self.
	topView borderWidth: 1.
	topView label: aString.
	topView minimumSize: 100 @ 50.

	codeView := PluggableTextView on: self text: nil accept: nil
					readSelection: nil menu: #codePaneMenu:shifted:.
	codeView window: (0@0 extent: 200@200).
	topView addSubView: codeView.
	topView controller open! !

!TranscriptStream methodsFor: 'stream extensions' stamp: 'di 5/8/1998 12:35'!
pastEndPut: anObject
	"If the stream reaches its limit, just output the contents and reset."
	self endEntry.
	^ self nextPut: anObject! !

!TranscriptStream methodsFor: 'model protocol' stamp: 'di 5/29/1998 17:13'!
perform: selector orSendTo: otherTarget
	"Selector was just chosen from a menu by a user.  If can respond, then
perform it on myself. If not, send it to otherTarget, presumably the
editPane from which the menu was invoked."

	(self respondsTo: selector)
		ifTrue: [^ self perform: selector]
		ifFalse: [^ otherTarget perform: selector]! !

!TranscriptStream methodsFor: 'model protocol' stamp: 'di 5/3/1999 22:49'!
release

	self dependents do:
		[:view | (view isMorph and: [view isInWorld not])
					ifTrue: [self removeDependent: view]]! !

!TranscriptStream methodsFor: 'private' stamp: 'mir 1/11/2000 11:41'!
semaphore
	^AccessSema ifNil:[AccessSema := Semaphore forMutualExclusion]! !

!TranscriptStream methodsFor: 'stream extensions' stamp: 'sma 2/26/2000 19:31'!
show: anObject  "TextCollector compatibility"
	self nextPutAll: anObject asString; endEntry! !

!TranscriptStream methodsFor: 'model protocol' stamp: 'sw 3/2/2001 10:18'!
step
	"Objects that may be models of SystemWindows need to respond to this, albeit vacuously"! !

!WriteStream class methodsFor: 'instance creation' stamp: ''!
on: aCollection from: firstIndex to: lastIndex 
	"Answer an instance of me on a copy of the argument, aCollection, 
	determined by the indices firstIndex and lastIndex. Position the instance 
	at the beginning of the collection."

	^self basicNew
		on: aCollection
		from: firstIndex
		to: lastIndex! !

!WriteStream class methodsFor: 'instance creation' stamp: ''!
with: aCollection 
	"Answer an instance of me on the argument, aCollection, positioned to 
	store objects at the end of aCollection."

	^self basicNew with: aCollection! !

!WriteStream class methodsFor: 'instance creation' stamp: ''!
with: aCollection from: firstIndex to: lastIndex 
	"Answer an instance of me on the subcollection of the argument, 
	aCollection, determined by the indices firstIndex and lastIndex. Position 
	the instance to store at the end of the subcollection."

	^self basicNew with: (aCollection copyFrom: firstIndex to: lastIndex)! !

!WriteStream methodsFor: 'private' stamp: 'di 11/18/1999 22:55'!
braceArray
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	^ collection! !

!WriteStream methodsFor: 'private' stamp: 'di 11/18/1999 22:50'!
braceArray: anArray
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	collection := anArray.
	position := 0.
	readLimit := 0.
	writeLimit := anArray size.! !

!WriteStream methodsFor: 'accessing' stamp: ''!
contents

	readLimit := readLimit max: position.
	^collection copyFrom: 1 to: position! !

!WriteStream methodsFor: 'character writing' stamp: ''!
cr
	"Append a return character to the receiver."

	self nextPut: Character cr! !

!WriteStream methodsFor: 'character writing' stamp: ''!
crtab
	"Append a return character, followed by a single tab character, to the 
	receiver."

	self nextPut: Character cr.
	self nextPut: Character tab! !

!WriteStream methodsFor: 'character writing' stamp: ''!
crtab: anInteger 
	"Append a return character, followed by anInteger tab characters, to the 
	receiver."

	self nextPut: Character cr.
	anInteger timesRepeat: [self nextPut: Character tab]! !

!WriteStream methodsFor: 'character writing' stamp: 'dc 2/11/2007 14:18'!
ensureASpace
	"Append a space character to the receiver IFF there is not one on the end."

	self ensureEndsWith: Character space! !

!WriteStream methodsFor: 'accessing' stamp: 'dc 2/11/2007 14:18'!
ensureEndsWith: anObject
	"Append anObject to the receiver IFF there is not one on the end."

	(position > 0 and: [(collection at: position) = anObject]) ifTrue: [^self].
	self nextPut: anObject! !

!WriteStream methodsFor: 'character writing' stamp: 'tk 9/23/2001 01:16'!
ensureNoSpace
	"If there is not one on the end, remove it."

	(position > 0 and: [(collection at: position) = Character space]) 
		ifTrue: [self skip: -1].! !

!WriteStream methodsFor: 'file open/close' stamp: 'mir 8/10/1999 11:56'!
flush! !

!WriteStream methodsFor: 'private' stamp: 'BG 5/24/2003 22:49'!
growTo: anInteger

   " anInteger is the required minimal new size of the collection "
	| oldSize grownCollection newSize |
	oldSize _ collection size.
     newSize := anInteger + (oldSize // 4 max: 20).
	grownCollection _ collection class new: newSize.
	collection _ grownCollection replaceFrom: 1 to: oldSize with: collection startingAt: 1.
	writeLimit _ collection size.
! !

!WriteStream methodsFor: 'accessing' stamp: ''!
next

	self shouldNotImplement! !

!WriteStream methodsFor: 'accessing' stamp: 'BG 5/26/2003 08:01'!
next: anInteger putAll: aCollection startingAt: startIndex
	"Store the next anInteger elements from the given collection."


	| newEnd |
	collection class == aCollection class ifFalse:
		[^ super next: anInteger putAll: aCollection startingAt: startIndex].

	newEnd _ position + anInteger.
	newEnd > writeLimit ifTrue:
		[self growTo: newEnd + 10].

	collection replaceFrom: position+1 to: newEnd  with: aCollection startingAt: startIndex.
	position _ newEnd.

	^aCollection! !

!WriteStream methodsFor: 'fileIn/Out' stamp: 'yo 8/13/2003 12:18'!
nextChunkPut: aString
	"Append the argument, aString, to the receiver, doubling embedded terminators."

	| i remainder terminator |
	terminator := $!!.
	remainder := aString.
	[(i := remainder indexOf: terminator) = 0] whileFalse:
		[self nextPutAll: (remainder copyFrom: 1 to: i).
		self nextPut: terminator.  "double imbedded terminators"
		remainder := remainder copyFrom: i+1 to: remainder size].
	self nextPutAll: remainder.
	aString includesUnifiedCharacter ifTrue: [
		self nextPut: terminator.
		self nextPutAll: ']lang['.
		aString writeLeadingCharRunsOn: self.
	].
	self nextPut: terminator.
! !

!WriteStream methodsFor: 'fileIn/Out' stamp: 'yo 8/28/2002 16:13'!
nextChunkPutWithStyle: aStringOrText
	"Append the argument, aText, to the receiver, doubling embedded terminators.  Put out one chunk for the string and one for the style runs.  Mark the style with ]style[."

	aStringOrText isString ifTrue: [^ self nextChunkPut: aStringOrText].
	aStringOrText runs coalesce.
	aStringOrText unembellished ifTrue: [^ self nextChunkPut: aStringOrText asString].

	self nextChunkPut: aStringOrText asString.
	self cr; nextPutAll: ']style['.
	self nextChunkPut: 
		(String streamContents: [:strm | 
			aStringOrText runs writeScanOn: strm]).
! !

!WriteStream methodsFor: 'accessing' stamp: 'ar 4/12/2005 17:34'!
nextPut: anObject 
	"Primitive. Insert the argument at the next position in the Stream
	represented by the receiver. Fail if the collection of this stream is not an
	Array or a String. Fail if the stream is positioned at its end, or if the
	position is out of bounds in the collection. Fail if the argument is not
	of the right type for the collection. Optional. See Object documentation
	whatIsAPrimitive."

	<primitive: 66>
	((collection class == ByteString) and: [
		anObject isCharacter and:[anObject isOctetCharacter not]]) ifTrue: [
			collection := (WideString from: collection).
			^self nextPut: anObject.
	].
	position >= writeLimit
		ifTrue: [^ self pastEndPut: anObject]
		ifFalse: 
			[position := position + 1.
			^collection at: position put: anObject]! !

!WriteStream methodsFor: 'accessing' stamp: 'BG 5/24/2003 20:41'!
nextPutAll: aCollection

	| newEnd |
	collection class == aCollection class ifFalse:
		[^ super nextPutAll: aCollection ].

	newEnd _ position + aCollection size.
	newEnd > writeLimit ifTrue:
		[self growTo: newEnd + 10].

	collection replaceFrom: position+1 to: newEnd  with: aCollection startingAt: 1.
	position _ newEnd.! !

!WriteStream methodsFor: 'character writing' stamp: 'di 6/7/2000 22:43'!
nextPutKeyword: keyword withArg: argValue
	"Emit a keyword/value pair in the alternate syntax"

	self nextPutAll: (keyword copyWithout: $:);
		nextPut: $(;
		store: argValue;
		nextPut: $)! !

!WriteStream methodsFor: 'private' stamp: ''!
on: aCollection

	super on: aCollection.
	readLimit := 0.
	writeLimit := aCollection size! !

!WriteStream methodsFor: 'private' stamp: ''!
on: aCollection from: firstIndex to: lastIndex

	| len |
	collection := aCollection.
	readLimit := 
		writeLimit := lastIndex > (len := collection size)
						ifTrue: [len]
						ifFalse: [lastIndex].
	position := firstIndex <= 1
				ifTrue: [0]
				ifFalse: [firstIndex - 1]! !

!WriteStream methodsFor: 'private' stamp: 'md 2/24/2006 19:50'!
pastEndPut: anObject
	"Grow the collection by creating a new bigger collection and then
	copy over the contents from the old one. We grow by doubling the size
	but the growth is kept between 20 and 1000000.
	Finally we put <anObject> at the current write position."

	| oldSize grownCollection |
	oldSize := collection size.
	grownCollection := collection class new: oldSize + ((oldSize max: 20) min: 1000000).
	collection := grownCollection replaceFrom: 1 to: oldSize with: collection startingAt: 1.
	writeLimit := collection size.
	collection at: (position := position + 1) put: anObject.
	"return the argument - added by kwl"
	^ anObject! !

!WriteStream methodsFor: 'character writing' stamp: 'tk 10/19/2001 11:12'!
peekLast
	"Return that item just put at the end of the stream"

	^ position > 0 
		ifTrue: [collection at: position]
		ifFalse: [nil]! !

!WriteStream methodsFor: 'positioning' stamp: ''!
position: anInteger 
	"Refer to the comment in PositionableStream|position:."

	readLimit := readLimit max: position.
	super position: anInteger! !

!WriteStream methodsFor: 'positioning' stamp: ''!
reset 
	"Refer to the comment in PositionableStream|reset."

	readLimit := readLimit max: position.
	position := 0! !

!WriteStream methodsFor: 'positioning' stamp: 'ar 11/12/1998 21:27'!
resetToStart
	readLimit := position := 0.! !

!WriteStream methodsFor: 'positioning' stamp: 'ajh 5/25/2001 20:19'!
setToEnd 
	"Refer to the comment in PositionableStream|setToEnd."

	readLimit := readLimit max: position.
	super setToEnd.! !

!WriteStream methodsFor: 'accessing' stamp: ''!
size

	^readLimit := readLimit max: position! !

!WriteStream methodsFor: 'character writing' stamp: ''!
space
	"Append a space character to the receiver."

	self nextPut: Character space! !

!WriteStream methodsFor: 'character writing' stamp: 'JF 7/31/2003 13:01'!
space: anInteger 
	"Append anInteger space characters to the receiver."

	anInteger timesRepeat: [self space]! !

!WriteStream methodsFor: 'printing' stamp: ''!
store: anObject 
	"Have anObject print on the receiver for purposes of rereading."

	anObject storeOn: self! !

!WriteStream methodsFor: 'character writing' stamp: ''!
tab
	"Append a tab character to the receiver."

	self nextPut: Character tab! !

!WriteStream methodsFor: 'character writing' stamp: 'JF 7/31/2003 13:00'!
tab: anInteger 
	"Append anInteger tab characters to the receiver."

	anInteger timesRepeat: [self tab]! !

!WriteStream methodsFor: 'fileIn/Out' stamp: 'nk 7/29/2004 10:11'!
timeStamp
	"Append the current time to the receiver as a String."
	self nextChunkPut:	"double string quotes and !!s"
		(String streamContents: [:s | SmalltalkImage current timeStamp: s]) printString.
	self cr! !

!WriteStream methodsFor: 'private' stamp: ''!
with: aCollection

	super on: aCollection.
	position := readLimit := writeLimit := aCollection size! !

!WriteStream methodsFor: 'private' stamp: ''!
withAttribute: att do: strmBlock 
	"No-op here is overriden in TextStream for font emphasis"
	^ strmBlock value! !

!WriteStream methodsFor: 'private' stamp: 'djp 11/6/1999 23:15'!
withAttributes: attributes do: strmBlock 
	"No-op here is overriden in TextStream for font emphasis"
	^ strmBlock value! !

Stream subclass: #SharedQueue2
	instanceVariableNames: 'monitor items'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Sequenceable'!

!SharedQueue2 commentStamp: 'ls 6/25/2005 13:48' prior: 0!
An implementation of a shared queue based on class Monitor.  Clients may may place items on the queue using nextPut: or remove them using methods like next or nextOrNil.  Items are removed in first-in first-out (FIFO) order.  It is safe for multiple threads to access the same shared queue, which is why this is a "shared" queue.

[monitor] is used to synchronize access from multiple threads.

[items] is an ordered collection holding the items that are in the queue.  New items are added  at the end, and old items are removed from the beginning.

All methods must hold the monitor while they run.
!

!SharedQueue2 class methodsFor: 'instance creation' stamp: 'ls 6/25/2005 13:53'!
new
	^self basicNew initialize! !

!SharedQueue2 methodsFor: 'initializing' stamp: 'ls 6/25/2005 13:48'!
initialize
	monitor := Monitor new.
	items := OrderedCollection new.
! !

!SharedQueue2 methodsFor: 'size' stamp: 'ls 6/25/2005 14:03'!
isEmpty
	^monitor critical: [ items isEmpty ]! !

!SharedQueue2 methodsFor: 'accessing' stamp: 'ls 6/25/2005 14:00'!
next
	^monitor critical: [
		monitor waitUntil: [ items isEmpty not ].
		items removeFirst ]
! !

!SharedQueue2 methodsFor: 'accessing' stamp: 'ls 6/25/2005 14:00'!
nextOrNil
	^monitor critical: [
		items isEmpty ifTrue: [ nil ] ifFalse: [ items removeFirst ] ]! !

!SharedQueue2 methodsFor: 'accessing' stamp: 'ls 6/25/2005 14:16'!
nextOrNilSuchThat: aBlock
	"Answer the next object that satisfies aBlock, skipping any intermediate objects.
	If no such object has been queued, answer <nil> and leave me intact."

	| index |
	^monitor critical: [
		index := items findFirst: aBlock.
		index = 0 ifTrue: [
			nil ]
		ifFalse: [
			items removeAt: index ] ].
! !

!SharedQueue2 methodsFor: 'accessing' stamp: 'ls 6/25/2005 13:58'!
nextPut: item
	monitor critical: [
		items addLast: item.
		monitor signalAll.  ]
! !

!SharedQueue2 methodsFor: 'accessing' stamp: 'ls 6/25/2005 14:00'!
peek
	"Answer the object that was sent through the receiver first and has not 
	yet been received by anyone but do not remove it from the receiver. If 
	no object has been sent, return nil"
	^monitor critical: [
		items isEmpty ifTrue: [ nil ] ifFalse: [ items first ] ]
! !

!SharedQueue2 methodsFor: 'printing' stamp: 'ls 6/25/2005 13:56'!
printOn: aStream
	monitor critical: [
		aStream 
			nextPutAll: self class name;
			nextPutAll: ' with ';
			print: items size;
		 	nextPutAll: ' items' ].! !

!SharedQueue2 methodsFor: 'size' stamp: 'ls 6/25/2005 14:04'!
size
	^monitor critical: [ items size ]! !

!Stream class methodsFor: 'instance creation' stamp: ''!
new

	self error: 'Streams are created with on: and with:'! !

!Stream methodsFor: 'testing' stamp: ''!
atEnd
	"Answer whether the receiver can access any more objects."

	self subclassResponsibility! !

!Stream methodsFor: 'accessing' stamp: 'yo 8/30/2002 17:13'!
basicNext

	^ self next.
! !

!Stream methodsFor: 'accessing' stamp: 'yo 8/30/2002 17:13'!
basicNextPut: anObject 

	^ self nextPut: anObject! !

!Stream methodsFor: 'accessing' stamp: 'yo 8/30/2002 17:13'!
basicNextPutAll: aCollection 

	^ self nextPutAll: aCollection.
! !

!Stream methodsFor: 'accessing' stamp: 'nk 2/24/2001 17:31'!
binary! !

!Stream methodsFor: 'file open/close' stamp: 'mir 8/10/1999 12:04'!
close! !

!Stream methodsFor: 'testing' stamp: 'ab 8/28/2003 18:30'!
closed
	^ false! !

!Stream methodsFor: 'accessing' stamp: ''!
contents
	"Answer all of the contents of the receiver."

	self subclassResponsibility! !

!Stream methodsFor: 'alternate syntax' stamp: 'RAA 6/20/2000 12:52'!
dialect

	^#ST80		"in case a regular stream is used to print parse nodes"! !

!Stream methodsFor: 'enumerating' stamp: ''!
do: aBlock 
	"Evaluate aBlock for each of the objects accessible by receiver."

	[self atEnd]
		whileFalse: [aBlock value: self next]! !

!Stream methodsFor: 'accessing' stamp: 'sma 4/22/2000 17:07'!
flush
	"Do nothing by default"! !

!Stream methodsFor: 'testing' stamp: 'ar 12/23/1999 15:43'!
isStream
	"Return true if the receiver responds to the stream protocol"
	^true! !

!Stream methodsFor: 'testing' stamp: 'mir 11/10/2003 18:22'!
isTypeHTTP
	^false! !

!Stream methodsFor: 'accessing' stamp: 'nk 4/29/2004 10:38'!
localName
	^'a stream'! !

!Stream methodsFor: 'accessing' stamp: ''!
next
	"Answer the next object accessible by the receiver."

	self subclassResponsibility! !

!Stream methodsFor: 'accessing' stamp: ''!
next: anInteger 
	"Answer the next anInteger number of objects accessible by the receiver."

	| aCollection |
	aCollection := OrderedCollection new.
	anInteger timesRepeat: [aCollection addLast: self next].
	^aCollection! !

!Stream methodsFor: 'accessing' stamp: ''!
next: anInteger put: anObject 
	"Make anObject be the next anInteger number of objects accessible by the 
	receiver. Answer anObject."

	anInteger timesRepeat: [self nextPut: anObject].
	^anObject! !

!Stream methodsFor: 'accessing' stamp: ''!
nextMatchAll: aColl
    "Answer true if next N objects are the ones in aColl,
     else false.  Advance stream of true, leave as was if false."
    | save |
    save := self position.
    aColl do: [:each |
       (self next) = each ifFalse: [
            self position: save.
            ^ false]
        ].
    ^ true! !

!Stream methodsFor: 'accessing' stamp: ''!
nextMatchFor: anObject 
	"Gobble the next object and answer whether it is equal to the argument, 
	anObject."

	^anObject = self next! !

!Stream methodsFor: 'accessing' stamp: ''!
nextPut: anObject 
	"Insert the argument, anObject, as the next object accessible by the 
	receiver. Answer anObject."

	self subclassResponsibility! !

!Stream methodsFor: 'accessing' stamp: ''!
nextPutAll: aCollection 
	"Append the elements of aCollection to the sequence of objects accessible 
	by the receiver. Answer aCollection."

	aCollection do: [:v | self nextPut: v].
	^aCollection! !

!Stream methodsFor: 'testing' stamp: 'ar 5/17/2001 19:07'!
nextWordsPutAll: aCollection
	"Write the argument a word-like object in big endian format on the receiver.
	May be used to write other than plain word-like objects (such as ColorArray)."
	aCollection class isPointers | aCollection class isWords not 
		ifTrue: [^self error: aCollection class name,' is not word-like'].
	1 to: aCollection basicSize do:[:i|
		self nextNumber: 4 put: (aCollection basicAt: i).
	].
	^aCollection! !

!Stream methodsFor: 'accessing' stamp: 'nk 4/29/2004 10:40'!
openReadOnly
	^self! !

!Stream methodsFor: 'printing' stamp: 'sma 6/1/2000 09:56'!
print: anObject
	"Have anObject print itself on the receiver."

	anObject printOn: self! !

!Stream methodsFor: 'printing' stamp: 'djp 7/21/1998 17:13'!
printHtml: anObject
	anObject printHtmlOn: self! !

!Stream methodsFor: 'accessing' stamp: 'ajh 7/31/2001 20:34'!
printOn: stream

	super printOn: stream.
	stream space.
	self contents printOn: stream.
! !

!Stream methodsFor: 'accessing' stamp: 'nk 4/29/2004 10:41'!
readOnly
	^self! !

!Stream methodsFor: 'as yet unclassified' stamp: 'RAA 9/11/2000 19:12'!
sleep

	"an FTP-based stream might close the connection here"! !

!Stream methodsFor: 'accessing' stamp: 'ls 9/12/1998 20:55'!
upToEnd
	"answer the remaining elements in the string"
	| elements |
	elements := OrderedCollection new.
	[ self atEnd ] whileFalse: [ 
		elements add: self next ].
	^elements! !

!Stream methodsFor: 'alternate syntax' stamp: 'RAA 6/20/2000 12:54'!
withStyleFor: elementType do: aBlock

	^aBlock value		"in case a regular stream is used to print parse nodes"
">>
(Compiler new compile: 'blah ^self' in: String notifying: nil ifFail: []) printString
<<"! !

!Stream methodsFor: 'filter streaming' stamp: 'MPW 1/1/1901 00:48'!
write:encodedObject
	^encodedObject putOn:self.
! !

Object subclass: #TextAttribute
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Text'!

!TextAttribute commentStamp: 'tk 7/22/2002 18:33' prior: 0!
Tells a piece of text to be a certain way.

Select text, press Command-6, choose a attribute.  If selected text is of the form 
	Hi There<Smalltalk beep>
the part in angle brackets is saved for action, and the Hi There appears in the paragraph.  If selection has no angle brackets, use the whole thing as both the text and the action.

TextDoIt  --  eval as a Smalltalk expression (the part in angle brackets)

TextLink -- Show a method, class comment, class hierarchy, or class defintion.
	<Point extent:>, <Point Comment>, <Point Hierarchy>, or <Point Defintion> are what you type.

TextURL -- Show the web page. <www.disney.com>

These attributes of text need to be stored on the disk in a regular file-out.  It is done in this form: 	Hi There   
	in the text, and a Run containing   dSmalltalk beep;;
	Click here to see the extent:   
	in the text, and a Run containing   method LPoint extent:;
See RunArray class scanFrom: where decoding is done.
!

TextAttribute subclass: #TextAction
	instanceVariableNames: ''
	classVariableNames: 'Purple'
	poolDictionaries: ''
	category: 'Collections-Text'!

TextAction subclass: #PluggableTextAttribute
	instanceVariableNames: 'evalBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Text'!

!PluggableTextAttribute commentStamp: '<historical>' prior: 0!
An attribute which evaluates an arbitrary block when it is selected.!

!PluggableTextAttribute class methodsFor: 'instance creation' stamp: 'ls 6/21/2001 18:09'!
evalBlock: aBlock
	^super new evalBlock: aBlock! !

!PluggableTextAttribute methodsFor: 'clicking' stamp: 'ls 6/21/2001 18:13'!
actOnClickFor: anObject
	evalBlock ifNil: [ ^self ].
	evalBlock numArgs = 0 ifTrue: [ evalBlock value.  ^true ].
	evalBlock numArgs = 1 ifTrue: [ evalBlock value: anObject.  ^true ].
	self error: 'evalBlock should have 0 or 1 arguments'! !

!PluggableTextAttribute methodsFor: 'initialization' stamp: 'ls 6/21/2001 18:06'!
evalBlock: aBlock
	evalBlock := aBlock! !

!TextAction class methodsFor: 'as yet unclassified' stamp: 'di 1/14/98 09:30'!
initialize   "TextAction initialize"
	Purple := Color r: 0.4 g: 0 b: 1.0! !

!TextAction methodsFor: 'as yet unclassified' stamp: 'nk 2/26/2005 09:49'!
analyze: aString
	"Analyze the selected text to find both the parameter to store and the text to emphesize (may be different from original selection).  Does not return self!!.  May be of the form:
3+4
<3+4>
Click Here<3+4>
<3+4>Click Here
"
	"Obtain the showing text and the instructions"
	| b1 b2 trim param show |
	b1 := aString indexOf: $<.
	b2 := aString indexOf: $>.
	(b1 < b2) & (b1 > 0) ifFalse: ["only one part"
		param := self validate: aString.
		param ifNil: [ ^{ nil. nil } ].
		^ Array with: param with: (param size = 0 ifTrue: [nil] ifFalse: [param])].
	"Two parts"
	trim := aString withBlanksTrimmed.
	(trim at: 1) == $< 
		ifTrue: [(trim last) == $>
			ifTrue: ["only instructions" 
				param := self validate: (aString copyFrom: b1+1 to: b2-1).
				show := param size = 0 ifTrue: [nil] ifFalse: [param]]
			ifFalse: ["at the front"
				param := self validate: (aString copyFrom: b1+1 to: b2-1).
				show := param size = 0 ifTrue: [nil] 
						ifFalse: [aString copyFrom: b2+1 to: aString size]]]
		ifFalse: [(trim last) == $>
			ifTrue: ["at the end"
				param := self validate: (aString copyFrom: b1+1 to: b2-1).
				show := param size = 0 ifTrue: [nil] 
						ifFalse: [aString copyFrom: 1 to: b1-1]]
			ifFalse: ["Illegal -- <> has text on both sides"
				show := nil]].
	^ Array with: param with: show
! !

!TextAction methodsFor: 'as yet unclassified' stamp: 'sw 11/9/1999 17:21'!
couldDeriveFromPrettyPrinting
	^ false! !

!TextAction methodsFor: 'as yet unclassified' stamp: 'di 10/31/97 13:11'!
dominatedByCmd0
	"Cmd-0 should turn off active text"
	^ true! !

!TextAction methodsFor: 'as yet unclassified' stamp: 'di 1/14/98 09:30'!
emphasizeScanner: scanner
	"Set the emphasis for text display"
	scanner textColor: Purple! !

!TextAction methodsFor: 'as yet unclassified' stamp: 'DSM 3/30/1999 13:15'!
info
	^ 'no hidden info'! !

!TextAction methodsFor: 'as yet unclassified' stamp: ''!
mayActOnClick

	^ true! !

!TextAction methodsFor: 'as yet unclassified' stamp: 'tk 12/16/97 16:48'!
validate: aString
	"any format is OK with me"
	^ aString! !

TextAction subclass: #TextDoIt
	instanceVariableNames: 'evalString'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Text'!

!TextDoIt class methodsFor: 'as yet unclassified' stamp: 'tk 12/6/97 20:28'!
evalString: str
	^ self new evalString: str! !

!TextDoIt class methodsFor: 'as yet unclassified' stamp: 'tk 12/16/97 09:06'!
scanFrom: strm
	"read a doit in the funny format used by Text styles on files. d10 factorial;;  end with two semicolons"

	| pos end doit |
	pos := strm position.
	[strm skipTo: $;. strm peek == $;] whileFalse.
	end := strm position - 1.
	strm position: pos.
	doit := strm next: end-pos.
	strm skip: 2.  ";;"
	^ self evalString: doit! !

!TextDoIt methodsFor: 'as yet unclassified' stamp: 'tk 12/5/97 17:01'!
actOnClickFor: anObject
	"Note: evalString gets evaluated IN THE CONTEXT OF anObject
	 -- meaning that self and all instVars are accessible"
	Compiler evaluate: evalString for: anObject logged: false.
	^ true ! !

!TextDoIt methodsFor: 'as yet unclassified' stamp: 'tk 12/16/97 16:46'!
analyze: aString

	| list |
	list := super analyze: aString.
	evalString := list at: 1.
	^ list at: 2! !

!TextDoIt methodsFor: 'as yet unclassified' stamp: 'tk 12/5/97 17:01'!
evalString: str
	evalString := str ! !

!TextDoIt methodsFor: 'as yet unclassified' stamp: 'tk 12/30/97 10:33'!
info
	^ evalString! !

!TextDoIt methodsFor: 'as yet unclassified' stamp: 'tk 12/16/97 13:46'!
writeScanOn: strm

	strm nextPut: $d; nextPutAll: evalString; nextPutAll: ';;'! !

TextDoIt subclass: #TextPrintIt
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Text'!

!TextPrintIt methodsFor: 'as yet unclassified' stamp: 'dvf 10/1/2003 13:27'!
actOnClickFor: anObject in: aParagraph at: clickPoint editor: editor
	"Note: evalString gets evaluated IN THE CONTEXT OF anObject
	 -- meaning that self and all instVars are accessible"
	| result range index |
	result := Compiler evaluate: evalString for: anObject logged: false.
	result := ' ', result printString,' '.
	"figure out where the attribute ends in aParagraph"
	index := (aParagraph characterBlockAtPoint: clickPoint) stringIndex.
	range := aParagraph text rangeOf: self startingAt: index.
	editor selectFrom: range last+1 to: range last.
	editor zapSelectionWith: result.
	editor selectFrom: range last to: range last + result size.
	^ true ! !

!TextPrintIt methodsFor: 'as yet unclassified' stamp: 'ar 9/22/2001 16:28'!
writeScanOn: strm

	strm nextPut: $P; nextPutAll: evalString; nextPutAll: ';;'! !

TextAction subclass: #TextLink
	instanceVariableNames: 'classAndMethod'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Text'!

!TextLink class methodsFor: 'as yet unclassified' stamp: 'tk 12/16/97 08:53'!
scanFrom: strm
	"read a link in the funny format used by Text styles on files. LPoint +;LPoint Comment;"

	^ self new classAndMethod: (strm upTo: $;)! !

!TextLink methodsFor: 'as yet unclassified' stamp: 'tk 5/1/2001 18:17'!
actOnClickFor: aMessageSet
	"Add to the end of the list.  'aClass selector', 'aClass Comment', 'aClass Definition', 'aClass Hierarchy' are the formats allowed."

	aMessageSet addItem: classAndMethod.
	^ true! !

!TextLink methodsFor: 'as yet unclassified' stamp: 'tk 12/16/97 16:49'!
analyze: aString

	| list |
	list := super analyze: aString.
	classAndMethod := list at: 1.
	^ list at: 2! !

!TextLink methodsFor: 'as yet unclassified' stamp: 'LC 10/8/2001 10:53'!
analyze: aString with: nonMethod
	"Initalize this attribute holder with a piece text the user typed into a paragraph.  Returns the text to emphesize (may be different from selection)  Does not return self!!.  nonMethod is what to show when clicked, i.e. the last part of specifier (Comment, Definition, or Hierarchy).  May be of the form:
Point
<Point>
Click Here<Point>
<Point>Click Here
"
	"Obtain the showing text and the instructions"
	| b1 b2 trim |
	b1 := aString indexOf: $<.
	b2 := aString indexOf: $>.
	(b1 < b2) & (b1 > 0) ifFalse: ["only one part"
		classAndMethod := self validate: aString, ' ', nonMethod.
		^ classAndMethod ifNotNil: [aString]].
	"Two parts"
	trim := aString withBlanksTrimmed.
	(trim at: 1) == $< 
		ifTrue: [(trim last) == $>
			ifTrue: ["only instructions" 
				classAndMethod := self validate: (aString copyFrom: b1+1 to: b2-1), ' ', nonMethod.
				^ classAndMethod ifNotNil: [classAndMethod]]
			ifFalse: ["at the front"
				classAndMethod := self validate: (aString copyFrom: b1+1 to: b2-1), ' ', nonMethod.
				^ classAndMethod ifNotNil: [aString copyFrom: b2+1 to: aString size]]]
		ifFalse: [(trim last) == $>
			ifTrue: ["at the end"
				classAndMethod := self validate: (aString copyFrom: b1+1 to: b2-1), ' ', nonMethod.
				^ classAndMethod ifNotNil: [aString copyFrom: 1 to: b1-1]]
			ifFalse: ["Illegal -- <> has text on both sides"
				^ nil]]
! !

!TextLink methodsFor: 'as yet unclassified' stamp: 'tk 12/5/97 17:09'!
classAndMethod: aString
	classAndMethod := aString! !

!TextLink methodsFor: 'as yet unclassified' stamp: 'tk 12/30/97 10:33'!
info
	^ classAndMethod! !

!TextLink methodsFor: 'as yet unclassified' stamp: 'nk 2/26/2005 09:50'!
validate: specString
	"Can this string be decoded to be Class space Method (or Comment, Definition, Hierarchy)? If so, return it in valid format, else nil" 

	| list first mid last |
	list := specString findTokens: ' 	.|'.
	list isEmpty ifTrue: [ ^nil ].
	last := list last.
	last first isUppercase ifTrue: [
		(#('Comment' 'Definition' 'Hierarchy') includes: last) ifFalse: [^ nil].
		"Check for 'Rectangle Comment Comment' and remove last one"
		(list at: list size - 1 ifAbsent: [^nil]) = last ifTrue: [list := list allButLast]].
	list size > 3 ifTrue: [^ nil].
	list size < 2 ifTrue: [^ nil].
	Symbol hasInterned: list first ifTrue: [:sym | first := sym].
	first ifNil: [^ nil].
	Smalltalk at: first ifAbsent: [^ nil].
	mid := list size = 3 
		ifTrue: [(list at: 2) = 'class' ifTrue: ['class '] ifFalse: [^ nil]]
		ifFalse: [''].
	"OK if method name is not interned -- may not be defined yet"
	^ first, ' ', mid, last! !

!TextLink methodsFor: 'as yet unclassified' stamp: 'tk 12/16/97 13:44'!
writeScanOn: strm

	strm nextPut: $L; nextPutAll: classAndMethod; nextPut: $;! !

TextAction subclass: #TextURL
	instanceVariableNames: 'url'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Text'!

TextURL subclass: #TextSqkPageLink
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Text'!

!TextSqkPageLink commentStamp: '<historical>' prior: 0!
I represent a link to either a SqueakPage in a BookMorph, or a regular url.  See TextMorphEditor changeEmphasis:.  
!

!TextSqkPageLink methodsFor: 'as yet unclassified' stamp: 'tk 1/12/1999 12:56'!
actOnClickFor: textMorph
	"I represent a link to either a SqueakPage in a BookMorph, or a regular url"

	| book |
	((url endsWith: '.bo') or: [url endsWith: '.sp']) ifFalse: [
		^ super actOnClickFor: textMorph].
	book := textMorph ownerThatIsA: BookMorph.
	book ifNotNil: [book goToPageUrl: url].
	"later handle case of page being in another book, not this one"
	^ true! !

!TextSqkPageLink methodsFor: 'as yet unclassified' stamp: 'tk 1/13/1999 08:14'!
writeScanOn: strm

	strm nextPut: $q; nextPutAll: url; nextPut: $;! !

TextURL subclass: #TextSqkProjectLink
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Text'!

!TextSqkProjectLink methodsFor: 'as yet unclassified' stamp: 'RAA 10/19/2000 16:27'!
actOnClickFor: textMorph

	Project enterIfThereOrFind: url.
	^ true! !

!TextSqkProjectLink methodsFor: 'as yet unclassified' stamp: 'RAA 10/19/2000 16:24'!
analyze: aString

	^url := aString! !

!TextSqkProjectLink methodsFor: 'as yet unclassified' stamp: 'RAA 10/19/2000 16:29'!
writeScanOn: strm

	strm nextPut: $p; nextPutAll: url; nextPut: $;! !

!TextURL class methodsFor: 'as yet unclassified' stamp: 'tk 12/16/97 09:24'!
scanFrom: strm
	"read a link in the funny format used by Text styles on files. Rhttp://www.disney.com;"

	^ self new url: (strm upTo: $;)! !

!TextURL methodsFor: 'as yet unclassified' stamp: 'rbb 2/18/2005 09:24'!
actOnClickFor: anObject
	"Do what you can with this URL.  Later a web browser."

	| response m |

	(url beginsWith: 'sqPr://') ifTrue: [
		ProjectLoading thumbnailFromUrl: (url copyFrom: 8 to: url size).
		^self		"should not get here, but what the heck"
	].
	"if it's a web browser, tell it to jump"
	anObject isWebBrowser
		ifTrue: [anObject jumpToUrl: url. ^ true]
		ifFalse: [((anObject respondsTo: #model) and: [anObject model isWebBrowser])
				ifTrue: [anObject model jumpToUrl: url. ^ true]].

		"if it's a morph, see if it is contained in a web browser"
		(anObject isKindOf: Morph) ifTrue: [
			m := anObject.
			[ m ~= nil ] whileTrue: [
				(m isWebBrowser) ifTrue: [
					m  jumpToUrl: url.
					^true ].
				(m hasProperty: #webBrowserView) ifTrue: [
					m model jumpToUrl: url.
					^true ].
				m := m owner. ]
		].

	"no browser in sight.  ask if we should start a new browser"
	((self confirm: 'open a browser to view this URL?' translated) and: [WebBrowser default notNil]) ifTrue: [
		WebBrowser default openOnUrl: url.
		^ true ].

	"couldn't display in a browser.  Offer to put up just the source"

	response := (UIManager default 
				chooseFrom: (Array with: 'View web page as source' translated
									with: 'Cancel' translated)
				title:  'Couldn''t find a web browser. View\page as source?' withCRs translated).
	response = 1 ifTrue: [HTTPSocket httpShowPage: url].
	^ true! !

!TextURL methodsFor: 'as yet unclassified' stamp: 'tk 12/16/97 16:47'!
analyze: aString

	| list |
	list := super analyze: aString.
	url := list at: 1.
	^ list at: 2! !

!TextURL methodsFor: 'as yet unclassified' stamp: 'tk 12/30/97 10:33'!
info
	^ url! !

!TextURL methodsFor: 'as yet unclassified' stamp: 'tk 12/16/97 08:55'!
url: aString
	url := aString! !

!TextURL methodsFor: 'as yet unclassified' stamp: 'tk 12/16/97 13:45'!
writeScanOn: strm

	strm nextPut: $R; nextPutAll: url; nextPut: $;! !

TextAttribute subclass: #TextAlignment
	instanceVariableNames: 'alignment'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Text'!

!TextAlignment class methodsFor: 'as yet unclassified' stamp: 'KR 12/5/2005 00:53'!
alignmentSymbol: alignmentCode
	^#(leftFlush rightFlush centered justified) at: (alignmentCode + 1)! !

!TextAlignment class methodsFor: 'instance creation' stamp: 'ar 12/15/2001 23:36'!
centered
	^self new alignment: 2! !

!TextAlignment class methodsFor: 'instance creation' stamp: 'ar 12/15/2001 23:36'!
justified
	^self new alignment: 3! !

!TextAlignment class methodsFor: 'instance creation' stamp: 'ar 12/15/2001 23:35'!
leftFlush
	^self new alignment: 0! !

!TextAlignment class methodsFor: 'instance creation' stamp: 'ar 12/15/2001 23:35'!
rightFlush
	^self new alignment: 1! !

!TextAlignment methodsFor: 'as yet unclassified' stamp: 'ar 12/15/2001 23:33'!
= other 
	^ (other class == self class) 
		and: [other alignment = alignment]! !

!TextAlignment methodsFor: 'as yet unclassified' stamp: 'ar 12/15/2001 23:33'!
alignment
	^alignment! !

!TextAlignment methodsFor: 'as yet unclassified' stamp: 'ar 12/15/2001 23:33'!
alignment: aNumber
	alignment := aNumber.! !

!TextAlignment methodsFor: 'as yet unclassified' stamp: 'ar 12/16/2001 00:20'!
dominates: other
	"There can be only one..."
	^self class == other class! !

!TextAlignment methodsFor: 'as yet unclassified' stamp: 'ar 12/15/2001 23:34'!
emphasizeScanner: scanner
	"Set the emphasist for text scanning"
	scanner setAlignment: alignment.! !

!TextAlignment methodsFor: 'as yet unclassified' stamp: 'ar 9/9/2003 22:03'!
hash
	"#hash is re-implemented because #= is re-implemented"
	^ alignment hash! !

!TextAlignment methodsFor: 'as yet unclassified' stamp: 'ar 12/16/2001 01:55'!
writeScanOn: strm

	strm nextPut: $a.
	alignment printOn: strm.! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: ''!
actOnClickFor: model
	"Subclasses may override to provide, eg, hot-spot actions"
	^ false! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: 'ar 9/22/2001 16:00'!
actOnClickFor: model in: aParagraph
	^self actOnClickFor: model! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: 'ar 9/22/2001 16:08'!
actOnClickFor: model in: aParagraph at: clickPoint
	^self actOnClickFor: model in: aParagraph! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: 'ar 9/22/2001 16:22'!
actOnClickFor: model in: aParagraph at: clickPoint editor: editor
	^self actOnClickFor: model in: aParagraph at: clickPoint! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: 'ar 12/16/2001 23:18'!
anchoredMorph
	"If one hides here, return it"
	^nil! !

!TextAttribute methodsFor: 'html' stamp: 'dgd 9/2/2005 12:39'!
closeHtmlOn: aStream 
	"put on the given stream the tag to close the html  
	representation of the receiver"
	self subclassResponsibility! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: 'sw 11/9/1999 17:25'!
couldDeriveFromPrettyPrinting
	"Answer whether the receiver is a kind of attribute that could have been generated by doing polychrome pretty-printing of a method without functional text attributes."

	^ true! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: 'di 10/31/97 13:09'!
dominatedByCmd0
	"Subclasses may override if cmd-0 should turn them off"
	^ false! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: ''!
dominates: another
	"Subclasses may override condense multiple attributes"
	^ false! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: 'di 4/1/1999 15:16'!
emphasisCode
	"Subclasses may override to add bold, italic, etc"
	^ 0! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: ''!
emphasizeScanner: scanner
	"Subclasses may override to set, eg, font, color, etc"! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: 'di 11/9/97 17:46'!
forFontInStyle: aTextStyle do: aBlock
	"No action is the default.  Overridden by font specs"! !

!TextAttribute methodsFor: 'testing' stamp: 'ar 9/21/2000 14:16'!
isKern
	^false! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: ''!
mayActOnClick
	"Subclasses may override to provide, eg, hot-spot actions"
	^ false! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: 'di 11/10/97 14:05'!
mayBeExtended
	"A quality that may be overridden by subclasses, such as TextAnchors, that really only apply to a single character"
	^ true! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: ''!
oldEmphasisCode: default
	"Allows running thorugh possibly multiple attributes
	and getting the emphasis out of any that has an emphasis (font number)"
	^ default! !

!TextAttribute methodsFor: 'html' stamp: 'dgd 9/2/2005 12:38'!
openHtmlOn: aStream 
	"put on the given stream the tag to open the html  
	representation of the receiver"
	self subclassResponsibility! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: 'di 10/31/97 11:03'!
reset
	"Allow subclasses to prepare themselves for merging attributes"! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: ''!
set
	"Respond true to include this attribute (as opposed to, eg, a bold
	emphasizer that is clearing the property"
	^ true! !

TextAttribute subclass: #TextColor
	instanceVariableNames: 'color'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Text'!

!TextColor commentStamp: '<historical>' prior: 0!
A TextColor encodes a text color change applicable over a given range of text.!

!TextColor class methodsFor: 'constants' stamp: ''!
black
	^ self new color: Color black! !

!TextColor class methodsFor: 'constants' stamp: ''!
blue
	^ self new color: Color blue! !

!TextColor class methodsFor: 'instance creation' stamp: ''!
color: aColor
	^ self new color: aColor! !

!TextColor class methodsFor: 'constants' stamp: ''!
cyan
	^ self new color: Color cyan! !

!TextColor class methodsFor: 'constants' stamp: 'ajh 9/10/2002 02:26'!
gray
	^ self new color: Color gray! !

!TextColor class methodsFor: 'constants' stamp: ''!
green
	^ self new color: Color green! !

!TextColor class methodsFor: 'constants' stamp: ''!
magenta
	^ self new color: Color magenta! !

!TextColor class methodsFor: 'constants' stamp: ''!
red
	^ self new color: Color red! !

!TextColor class methodsFor: 'instance creation' stamp: 'sma 3/24/2000 10:49'!
scanFrom: strm
	"read a color in the funny format used by Text styles on files. c125000255 or cblue;"

	| r g b |
	strm peek isDigit
		ifTrue:
			[r := (strm next: 3) asNumber.
			g := (strm next: 3) asNumber.
			b := (strm next: 3) asNumber.
			^ self color: (Color r: r g: g b: b range: 255)].
	"A name of a color"
	^ self color: (Color perform: (strm upTo: $;) asSymbol)! !

!TextColor class methodsFor: 'constants' stamp: 'sma 3/24/2000 10:50'!
white 
	^ self new color: Color white! !

!TextColor class methodsFor: 'constants' stamp: ''!
yellow
	^ self new color: Color yellow! !

!TextColor methodsFor: 'comparing' stamp: 'di 10/31/97 11:19'!
= other 
	^ (other class == self class) 
		and: [other color = color]! !

!TextColor methodsFor: 'html' stamp: 'dgd 9/2/2005 12:39'!
closeHtmlOn: aStream 
	"put on the given stream the tag to close the html  
	representation of the receiver"
	aStream nextPutAll: '</font>'! !

!TextColor methodsFor: 'accessing' stamp: ''!
color
	^ color! !

!TextColor methodsFor: 'accessing' stamp: ''!
color: aColor
	color := aColor! !

!TextColor methodsFor: 'scanning' stamp: 'di 10/31/97 11:20'!
dominates: other
	^ other class == self class! !

!TextColor methodsFor: 'scanning' stamp: ''!
emphasizeScanner: scanner
	"Set the emphasis for text display"
	scanner textColor: color! !

!TextColor methodsFor: 'comparing' stamp: 'sma 3/24/2000 10:51'!
hash
	^ color hash! !

!TextColor methodsFor: 'html' stamp: 'dgd 9/2/2005 12:38'!
openHtmlOn: aStream 
	"put on the given stream the tag to open the html  
	representation of the receiver"
	aStream nextPutAll: '<font color="#' , color printHtmlString , '">'! !

!TextColor methodsFor: 'printing' stamp: 'sma 3/24/2000 10:51'!
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: ' code: '; print: color! !

!TextColor methodsFor: 'scanning' stamp: 'tk 12/16/97 09:47'!
writeScanOn: strm
	"Two formats.  c125000255 or cblue;"

	| nn str |
	strm nextPut: $c.
	(nn := color name) ifNotNil: [
		(self class respondsTo: nn) ifTrue: [
			^ strm nextPutAll: nn; nextPut: $;]].
	(Array with: color red with: color green with: color blue) do: [:float |
		str := '000', (float * 255) asInteger printString.
		strm nextPutAll: (str copyFrom: str size-2 to: str size)]! !

TextAttribute subclass: #TextEmphasis
	instanceVariableNames: 'emphasisCode setMode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Text'!

!TextEmphasis commentStamp: '<historical>' prior: 0!
A TextEmphasis, encodes a characteristic applicable to all fonts.  The encoding is as follows:
	1	bold
	2	itallic
	4	underlined
	8	narrow
	16	struck out!

!TextEmphasis class methodsFor: 'as yet unclassified' stamp: ''!
bold
	^ self new emphasisCode: 1! !

!TextEmphasis class methodsFor: 'as yet unclassified' stamp: ''!
italic
	^ self new emphasisCode: 2! !

!TextEmphasis class methodsFor: 'as yet unclassified' stamp: 'di 10/31/97 13:05'!
narrow
	^ TextKern kern: -1! !

!TextEmphasis class methodsFor: 'as yet unclassified' stamp: ''!
normal
	^ self new emphasisCode: 0! !

!TextEmphasis class methodsFor: 'as yet unclassified' stamp: ''!
struckOut
	^ self new emphasisCode: 16! !

!TextEmphasis class methodsFor: 'as yet unclassified' stamp: ''!
underlined
	^ self new emphasisCode: 4! !

!TextEmphasis methodsFor: 'as yet unclassified' stamp: 'di 10/31/97 11:15'!
= other 
	^ (other class == self class) 
		and: [other emphasisCode = emphasisCode]! !

!TextEmphasis methodsFor: 'html' stamp: 'dgd 9/2/2005 12:39'!
closeHtmlOn: aStream 
	"put on the given stream the tag to close the html  
	representation of the receiver"
	emphasisCode = 1
		ifTrue: [aStream nextPutAll: '</b>'].
	emphasisCode = 2
		ifTrue: [aStream nextPutAll: '</i>'].
	emphasisCode = 4
		ifTrue: [aStream nextPutAll: '</u>']! !

!TextEmphasis methodsFor: 'as yet unclassified' stamp: 'di 10/31/97 13:11'!
dominatedByCmd0
	"Cmd-0 should turn off emphasis"
	^ true! !

!TextEmphasis methodsFor: 'as yet unclassified' stamp: 'di 10/31/97 13:13'!
dominates: other
	(emphasisCode = 0 and: [other dominatedByCmd0]) ifTrue: [^ true].
	^ (other class == self class)
		and: [emphasisCode = other emphasisCode]! !

!TextEmphasis methodsFor: 'as yet unclassified' stamp: ''!
emphasisCode
	^ emphasisCode! !

!TextEmphasis methodsFor: 'as yet unclassified' stamp: ''!
emphasisCode: int
	emphasisCode := int.
	setMode := true! !

!TextEmphasis methodsFor: 'as yet unclassified' stamp: 'di 10/29/97 11:57'!
emphasizeScanner: scanner
	"Set the emphasist for text scanning"
	scanner addEmphasis: emphasisCode! !

!TextEmphasis methodsFor: 'as yet unclassified' stamp: 'ar 9/9/2003 22:03'!
hash
	"#hash is re-implemented because #= is re-implemented"
	^emphasisCode hash
! !

!TextEmphasis methodsFor: 'html' stamp: 'dgd 9/2/2005 12:38'!
openHtmlOn: aStream 
	"put on the given stream the tag to open the html  
	representation of the receiver"
	emphasisCode = 1
		ifTrue: [aStream nextPutAll: '<b>'].
	emphasisCode = 2
		ifTrue: [aStream nextPutAll: '<i>'].
	emphasisCode = 4
		ifTrue: [aStream nextPutAll: '<u>']! !

!TextEmphasis methodsFor: 'as yet unclassified' stamp: ''!
printOn: strm
	super printOn: strm.
	strm nextPutAll: ' code: '; print: emphasisCode! !

!TextEmphasis methodsFor: 'as yet unclassified' stamp: ''!
set
	^ setMode and: [emphasisCode ~= 0]! !

!TextEmphasis methodsFor: 'as yet unclassified' stamp: ''!
turnOff
	setMode := false! !

!TextEmphasis methodsFor: 'as yet unclassified' stamp: 'tk 12/16/97 09:28'!
writeScanOn: strm

	emphasisCode = 1 ifTrue: [strm nextPut: $b].
	emphasisCode = 2 ifTrue: [strm nextPut: $i].
	emphasisCode = 0 ifTrue: [strm nextPut: $n].
	emphasisCode = 16 ifTrue: [strm nextPut: $=].
	emphasisCode = 4 ifTrue: [strm nextPut: $u].! !

TextAttribute subclass: #TextFontChange
	instanceVariableNames: 'fontNumber'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Text'!

!TextFontChange commentStamp: '<historical>' prior: 0!
A TextFontChange encodes a font change applicable over a given range of text.  The font number is interpreted relative to the textStyle governing display of this text.!

!TextFontChange class methodsFor: 'as yet unclassified' stamp: 'sw 12/6/1999 17:52'!
defaultFontChange
	"Answer a TextFontChange that represents the default font"

	^ self new fontNumber: TextStyle default defaultFontIndex! !

!TextFontChange class methodsFor: 'as yet unclassified' stamp: ''!
font1
	^ self new fontNumber: 1! !

!TextFontChange class methodsFor: 'as yet unclassified' stamp: ''!
font2
	^ self new fontNumber: 2! !

!TextFontChange class methodsFor: 'as yet unclassified' stamp: ''!
font3
	^ self new fontNumber: 3! !

!TextFontChange class methodsFor: 'as yet unclassified' stamp: ''!
font4
	^ self new fontNumber: 4! !

!TextFontChange class methodsFor: 'as yet unclassified' stamp: ''!
fontNumber: n
	^ self new fontNumber: n! !

!TextFontChange methodsFor: 'as yet unclassified' stamp: 'di 10/31/97 11:15'!
= other 
	^ (other class == self class) 
		and: [other fontNumber = fontNumber]! !

!TextFontChange methodsFor: 'html' stamp: 'dgd 9/2/2005 12:39'!
closeHtmlOn: aStream 
	"put on the given stream the tag to close the html  
	representation of the receiver"
	| font |
	font := TextStyle default fontAt: fontNumber.
	font closeHtmlOn: aStream! !

!TextFontChange methodsFor: 'as yet unclassified' stamp: 'nk 9/3/2004 15:48'!
dominates: other
	^ other isKindOf: TextFontChange! !

!TextFontChange methodsFor: 'as yet unclassified' stamp: ''!
emphasizeScanner: scanner
	"Set the font for text display"
	scanner setFont: fontNumber! !

!TextFontChange methodsFor: 'as yet unclassified' stamp: ''!
fontNumber
	^ fontNumber! !

!TextFontChange methodsFor: 'as yet unclassified' stamp: ''!
fontNumber: int
	fontNumber := int! !

!TextFontChange methodsFor: 'as yet unclassified' stamp: 'di 11/9/97 17:46'!
forFontInStyle: aTextStyle do: aBlock
	aBlock value: (aTextStyle fontAt: fontNumber)! !

!TextFontChange methodsFor: 'as yet unclassified' stamp: 'ar 9/9/2003 22:03'!
hash
	"#hash is re-implemented because #= is re-implemented"
	^fontNumber hash! !

!TextFontChange methodsFor: 'html' stamp: 'dgd 9/2/2005 12:38'!
openHtmlOn: aStream 
	"put on the given stream the tag to open the html  
	representation of the receiver"
	| font |
	font := TextStyle default fontAt: fontNumber.
	font openHtmlOn: aStream! !

!TextFontChange methodsFor: 'as yet unclassified' stamp: ''!
printOn: strm
	super printOn: strm.
	strm nextPutAll: ' font: '; print: fontNumber! !

!TextFontChange methodsFor: 'as yet unclassified' stamp: 'tk 12/16/97 09:22'!
writeScanOn: strm

	strm nextPut: $f.
	fontNumber printOn: strm.! !

TextFontChange subclass: #TextFontReference
	instanceVariableNames: 'font'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Text'!

!TextFontReference commentStamp: '<historical>' prior: 0!
A TextFontReference encodes a font change applicable over a given range of text.  The font reference is absolute:  unlike a TextFontChange, it is independent of the textStyle governing display of this text.!

!TextFontReference class methodsFor: 'as yet unclassified' stamp: ''!
toFont: aFont
	^ self new toFont: aFont! !

!TextFontReference methodsFor: 'comparing' stamp: 'nk 9/3/2004 15:43'!
= other 
	^ (other class == self class) 
		and: [other font = font]! !

!TextFontReference methodsFor: 'as yet unclassified' stamp: 'sw 11/9/1999 17:22'!
couldDeriveFromPrettyPrinting
	^ false! !

!TextFontReference methodsFor: 'as yet unclassified' stamp: ''!
emphasizeScanner: scanner
	"Set the actual font for text display"
	scanner setActualFont: font! !

!TextFontReference methodsFor: 'as yet unclassified' stamp: 'di 5/10/1999 23:47'!
font

	^ font! !

!TextFontReference methodsFor: 'as yet unclassified' stamp: 'di 11/9/97 17:47'!
forFontInStyle: aTextStyle do: aBlock
	aBlock value: font! !

!TextFontReference methodsFor: 'comparing' stamp: 'ar 9/9/2003 22:03'!
hash
	"#hash is re-implemented because #= is re-implemented"
	^font hash! !

!TextFontReference methodsFor: 'comparing' stamp: 'nk 9/3/2004 15:24'!
printOn: aStream
	aStream nextPutAll: 'a TextFontReference(';
		print: font;
		nextPut: $)! !

!TextFontReference methodsFor: 'as yet unclassified' stamp: ''!
toFont: aFont

	font := aFont! !

!TextFontReference methodsFor: 'as yet unclassified' stamp: 'tk 7/22/2002 18:39'!
writeScanOn: strm

	strm nextPut: $F.
	strm nextPutAll: font familyName; nextPut: $#.
	font height printOn: strm.! !

TextAttribute subclass: #TextIndent
	instanceVariableNames: 'amount'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Text'!

!TextIndent commentStamp: '<historical>' prior: 0!
create a hanging indent. !

!TextIndent class methodsFor: 'instance creation' stamp: 'ls 6/27/1998 15:55'!
amount: amount
	"create a TextIndent which will indent by the given amount.  Currently this is a number of tabs, but may change in the futur"
	^super new amount: amount! !

!TextIndent class methodsFor: 'example' stamp: 'ls 6/24/1998 18:06'!
example
	"TextIndent example"
	| text pg |

	"create an example text with some indentation"
	text := 'abcdao euoaeuo aeuo aeuoaeu o aeuoeauefgh bcd efghi'  asText.
	text addAttribute: (TextColor red)  from: 3 to: 8.
	text addAttribute: (TextIndent amount: 1) from: 1 to: 2.
	text addAttribute: (TextIndent amount: 2) from: 20 to: 35.

	"stick it in a paragraph and display it"
	pg := text asParagraph.
	pg compositionRectangle: (0@0 extent: 100@200).
	pg textStyle alignment: 2.
	pg displayAt: 0@0.
! !

!TextIndent class methodsFor: 'instance creation' stamp: 'ls 6/27/1998 15:54'!
tabs: numTabs
	"create an indentation by the given number of tabs"
	^self amount: numTabs! !

!TextIndent methodsFor: 'access' stamp: 'ls 6/22/1998 17:51'!
amount
	"number of tab spaces to indent by"
	^amount! !

!TextIndent methodsFor: 'access' stamp: 'ls 6/22/1998 17:51'!
amount: anInteger
	"change the number of tabs to indent by"
	amount := anInteger! !

!TextIndent methodsFor: 'condensing' stamp: 'ls 6/22/1998 19:27'!
dominates: anAttribute
	^(self class == anAttribute class)! !

!TextIndent methodsFor: 'setting indentation' stamp: 'ls 6/22/1998 18:56'!
emphasizeScanner: scanner
	scanner indentationLevel: amount! !

!TextIndent methodsFor: 'printing' stamp: 'ls 6/22/1998 18:03'!
printOn: aStream
	super printOn: aStream.
	aStream nextPutAll: ' amount: '.
	amount printOn: aStream! !

TextAttribute subclass: #TextKern
	instanceVariableNames: 'kern active'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Text'!

!TextKern commentStamp: '<historical>' prior: 0!
A TextKern encodes a kerning change applicable over a given range of text.  Positive values of kern spread letters out, negative kern will cause them to overlap more.  Note that kerns other than 0 will display somewhat slower, as kerning is not yet supported in the text scanning primitive. !

!TextKern class methodsFor: 'as yet unclassified' stamp: 'di 10/29/97 11:49'!
kern: kernValue
	^ self new kern: kernValue! !

!TextKern methodsFor: 'as yet unclassified' stamp: 'di 10/31/97 11:15'!
= other 
	^ (other class == self class) 
		and: [other kern = kern]! !

!TextKern methodsFor: 'as yet unclassified' stamp: 'sw 11/9/1999 17:21'!
couldDeriveFromPrettyPrinting
	^ false! !

!TextKern methodsFor: 'as yet unclassified' stamp: 'di 10/31/97 13:10'!
dominatedByCmd0
	"Cmd-0 should turn off kerning"
	^ true! !

!TextKern methodsFor: 'as yet unclassified' stamp: 'di 10/31/97 11:10'!
dominates: other
	"NOTE: The use of active in this code is specific to its use in the method
		Text class addAttribute: att toArray: others"
	(active and: [other class == self class and: [other kern + kern = 0]])
		ifTrue: [active := false.  ^ true].  "can only dominate once"
	^ false! !

!TextKern methodsFor: 'as yet unclassified' stamp: 'di 10/29/97 11:50'!
emphasizeScanner: scanner
	"Augment (or diminish) the kerning offset for text display"
	scanner addKern: kern! !

!TextKern methodsFor: 'as yet unclassified' stamp: 'ar 9/9/2003 22:03'!
hash
	"#hash is re-implemented because #= is re-implemented"
	^kern hash! !

!TextKern methodsFor: 'testing' stamp: 'ar 9/21/2000 14:16'!
isKern
	^true! !

!TextKern methodsFor: 'as yet unclassified' stamp: 'di 10/31/97 11:12'!
kern
	^ kern! !

!TextKern methodsFor: 'as yet unclassified' stamp: 'tk 12/30/97 09:59'!
kern: kernValue
	kern := kernValue.
	self reset.! !

!TextKern methodsFor: 'as yet unclassified' stamp: 'di 10/31/97 11:04'!
reset
	active := true! !

!TextKern methodsFor: 'as yet unclassified' stamp: 'di 10/31/97 11:11'!
set
	^ active! !

!TextKern methodsFor: 'as yet unclassified' stamp: 'tk 9/21/1999 15:57'!
writeScanOn: strm

	kern > 0 ifTrue: [
		1 to: kern do: [:kk | strm nextPut: $+]].
	kern < 0 ifTrue: [
		1 to: 0-kern do: [:kk | strm nextPut: $-]].! !
Character initialize!
Collection initialize!
WeakArray initialize!
String initialize!
Symbol initialize!
WideSymbol initialize!
Text initialize!
Base64MimeConverter initialize!
TranscriptStream initialize!
TextAction initialize!
