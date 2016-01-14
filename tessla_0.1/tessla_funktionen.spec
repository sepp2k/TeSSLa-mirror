define amID := ApplicationMessageID
define amTSDef := ApplicationMessageTSDef
define amValid := ApplicationMessageValid
define amValue := ApplicationMessageValue
define bma := BranchToMessageAddress
define bmv := BranchToMessageValid
define dma := DataMessageAddress
define dms := DataMessageSize
define dmValid := DataMessageValid
define dmValue := DataMessageValue

define int1 := constant(3)
define int2 := constant(5)
define string := constant("bla")

define sum := add(int1,int2)
define diff := sub(int1,int2)
define product := multiply(int1,int2)
define shifted := shift(int1,int2)

define greater := geq(int1,int2)
define less := lessthan(int1,int2)

define negation := not(less)
define disjunction := or(less,greater)
define conjunction := and(less,greater)
define implication := implies(less,greater)

define monitor1 := monitor("always less",less)
define monitor2 := monitor("always less or greater",less,greater)
define monitor3 := monitor("always less or (greater until negation)",less,greater,negation)

define if2 := if(less,string)
define if3 := if(less,sum,diff)

out dmValue