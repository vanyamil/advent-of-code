# Order: name, speed, time on, time off
parsed_input = [
	("Dancer", 27, 5, 132),
	("Cupid", 22, 2, 41),
	("Rudolph", 11, 5, 48),
	("Donner", 28, 5, 134),
	("Dasher", 4, 16, 55),
	("Blitzen", 14, 3, 38),
	("Prancer", 3, 21, 40),
	("Comet", 18, 6, 103),
	("Vixen", 18, 5, 84),
]

def dist(idx, seconds):
	[name, speed, on, off] = parsed_input[idx]
	full_cycles = seconds // (on + off)
	d = full_cycles * speed * on
	remaining = seconds % (on + off)
	d += speed * min(remaining, on)
	return d

print("Part 1")
for i in range(len(parsed_input)):
	print(f"{parsed_input[i][0]}: {dist(i, 2503)}")

