baseline
baseline: spec
    <baseline>
    spec
        for: #common
        do: [ 
            spec
                package: 'SqueakHistory';
                package: 'SqueakHistory-Tests' with: [ spec requires: #('default') ].
            spec
                group: 'default' with: #('SqueakHistory');
                group: 'Tests' with: #('SqueakHistory-Tests') ]