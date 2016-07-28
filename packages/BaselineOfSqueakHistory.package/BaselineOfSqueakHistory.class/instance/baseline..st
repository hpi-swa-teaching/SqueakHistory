baseline
baseline: spec
    <baseline>
    spec
        for: #common
        do: [ 
		spec 
			package: 'VB-Regex' with: [
				spec repository: 'http://www.squeaksource.com/Regex']. 
		spec
			package: 'SqueakHistory';
			requires: #('VB-Regex').
            spec
			group: 'default' with: #('SqueakHistory')]