display text
classDisplayText: changeRecord
	| className |
	className := changeRecord methodClass name.
	 ^ className = 'nil'
			ifTrue: ['']
			ifFalse:  [className, ' ']