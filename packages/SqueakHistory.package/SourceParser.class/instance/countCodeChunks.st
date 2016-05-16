as yet unclassified
countCodeChunks
	| chunks ff |
	
	"just like FileStream>>fileIn:"
	ff := FileStream  readOnlyFileNamed: (GZipReadStream uncompressedFileName: file).
	
	"inspired by: PositionableStream>>fileInAnnouncing."
	chunks := 0.
	[ff atEnd] whileFalse: 
		[ | chunk |
		ff skipSeparators.
		chunk := ff nextChunk.
		chunks := chunks + 1.].
	
	^ chunks