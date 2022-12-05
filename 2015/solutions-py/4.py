from hashlib import md5

prefix = "iwrupvqb"

for i in range(1, 10000000):
	combined = prefix + str(i)
	result = md5(combined.encode())
	hex = result.hexdigest()
	if hex[:5] == "00000":
		print(f"Found hex {hex} at value {i}")
		break
	if i % 10000 == 0:
		print(f"Reached idx {i}")

print("Complete part 1")

for i in range(1, 10000000):
	combined = prefix + str(i)
	result = md5(combined.encode())
	hex = result.hexdigest()
	if hex[:6] == "000000":
		print(f"Found hex {hex} at value {i}")
		break
	if i % 10000 == 0:
		print(f"Reached idx {i}")

print("Complete part 2")