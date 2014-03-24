
assert onlypoliceunflag {
	all st1, st2: State, ev: Event |
		(transition[st1, ev, st2] and
  		st1 != st2 and st2.stolen in st1.stolen)
		implies
		ev in EVstolen_laptop_cancel
}
check onlypoliceunflag for 3 but 1 Event, 2 State