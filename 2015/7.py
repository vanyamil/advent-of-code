import re

class Rule:
	def __init__(self, key, var, kw, arg = None):
		self.key = key
		self.var = var
		self.kw = kw
		self.arg = arg

		self.reqs = set()
		self.reqs.add(var)
		if arg is not None and not arg.isdigit():
			self.reqs.add(arg)

class Circuit:
	def __init__(self):
		self.found = dict() # id -> int
		self.tbd = list() # rid -> Rule
		self.keys_for_tbd = dict() # id -> rid[]

	def add_value(self, key, val):
		self.found[key] = int(val)
		# print(f"Value {val} found for key {key}")
		self.recheck_rules(key)

	def add_rule(self, rule):
		idx = len(self.tbd)
		self.tbd.append(rule)
		waiting = False
		for x in list(rule.reqs):
			if x not in self.found:
				waiting = True
				if x in self.keys_for_tbd:
					self.keys_for_tbd[x].append(idx)
				else:
					self.keys_for_tbd[x] = [idx]
				# print(f"Added wait for {x} for key {rule.key}")
			else:
				rule.reqs.remove(x)
		if not waiting:
			self.eval_rule(rule)

	def eval_rule(self, rule):
		key = rule.key
		value = None
		if rule.kw == "COPY":
			value = self.found[rule.var]
		elif rule.kw == "NOT":
			value = 65535 - self.found[rule.var]
		elif rule.kw == "AND":
			value = self.found[rule.var] & self.found[rule.arg]
		elif rule.kw == "OR":
			value = self.found[rule.var] | self.found[rule.arg]
		elif rule.kw == "LSHIFT":
			value = self.found[rule.var] << int(rule.arg)
		elif rule.kw == "RSHIFT":
			value = self.found[rule.var] >> int(rule.arg)
		else:
			raise Exception(f"Unknown operator {rule.kw}")
		self.add_value(key, value)

	def recheck_rules(self, key):
		if key in self.keys_for_tbd:
			rule_ids = self.keys_for_tbd[key]
			for id in rule_ids:
				self.tbd[id].reqs.discard(key)
				if len(self.tbd[id].reqs) == 0:
					self.eval_rule(self.tbd[id])
				else:
					rems = ",".join(self.tbd[id].reqs)
					# print(f"Rule {self.tbd[id].key} still has reqs {rems}")
			del self.keys_for_tbd[key]
		else:
			# print(f"No rules to evaluate for key {key}")
			pass

def parse_line(line, circuit):
	pattern = '([^\n]+) -> (\w+)'
	(rule, key) = re.search(pattern, line).groups()
	# Rule options
	if rule.isdigit():
		circuit.add_value(key, int(rule))
	elif rule.isalpha():
		circuit.add_rule(Rule(key, rule, "COPY"))
	elif rule[:4] == "NOT ":
		circuit.add_rule(Rule(key, rule[4:], "NOT"))
	else:
		(var, kw, arg) = rule.split(" ")
		circuit.add_rule(Rule(key, var, kw, arg))

def test():
	c = Circuit()
	with open("7test.txt") as f:
		for line in f.readlines():
			parse_line(line, c)
	for key in sorted(c.found.keys()):
		print(f"{key}: {c.found[key]}")

def part1():
	# Only b and c have starting values
	c = Circuit()
	c.add_value("1", 1)
	with open("7.txt") as f:
		for line in f.readlines():
			parse_line(line, c)
	for key in sorted(c.found.keys()):
		print(f"{key}: {c.found[key]}")
	print("Waiting for rules:")
	for idx in range(len(c.tbd)):
		rule = c.tbd[idx]
		if rule.key not in c.found:
			l = len(rule.reqs)
			print(f"Rule {idx} for key {c.tbd[idx].key} has {l} reqs remaining")

def part2():
	# Only b and c have starting values
	c = Circuit()
	c.add_value("1", 1)
	with open("7p2.txt") as f:
		for line in f.readlines():
			parse_line(line, c)
	print(f"a: {c.found['a']}")

# test()
# part1()
part2()