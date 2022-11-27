def enough_vowels(s):
	count = 0
	for c in s:
		if c in "aeiou":
			count += 1
			if count == 3:
				break
	return count >= 3

def has_double(s):
	for i in range(len(s) - 1):
		if s[i] == s[i+1]:
			return True
	return False

def no_baddies(s):
	for bad in ["ab", "cd", "pq", "xy"]:
		if bad in s:
			return False
	return True

def test_nice_1(s):
	return enough_vowels(s) and has_double(s) and no_baddies(s)

def repeating_pair(s):
	bigrams = dict()
	for i in range(len(s) - 1):
		bigram = s[i:i+2]
		if bigram not in bigrams:
			bigrams[bigram] = i
		elif i - bigrams[bigram] >= 2:
			return True
	return False

def repeat_split(s):
	for i in range(len(s) - 2):
		if s[i] == s[i+2]:
			return True
	return False

def test_nice_2(s):
	return repeating_pair(s) and repeat_split(s)

with open("5.txt") as f:
	strings = f.readlines()
	cnt = len(list(filter(test_nice_1, strings)))
	print(f"Part 1: Found {cnt} nice strings")
	cnt = len(list(filter(test_nice_2, strings)))
	print(f"Part 2: Found {cnt} nice strings")