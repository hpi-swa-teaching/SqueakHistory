parsing
changesFor: method class: class meta: meta category: msgCategory selector: selector
	"Inspired by VersionsBrowser browseVersionsOf:  class:  meta:  category:  selector:"
	| changeList browser |
	Cursor read showWhile:
		[changeList := (browser := VersionsBrowser new)
			scanVersionsOf: method class: class meta: meta
			category: msgCategory selector: selector].
	changeList ifNil: [
		self inform: 'No versions available'.
		^ nil].

	^ changeList changeList sortBy: [:a :b | 
		(HistoryStampParser getTimeStampFromStamp: a stamp) > 
		(HistoryStampParser getTimeStampFromStamp: b stamp)]