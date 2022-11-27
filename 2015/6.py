from enum import Enum
import re

# Solve using quadtrees
# a b
# d c

class V:
	def __init__(self, x, y = None):
		self.x = x
		self.y = x if y is None else y

	def __add__(self, v2):
		return V(self.x + v2.x, self.y + v2.y)

	def __eq__(self, v2):
		return self.x == v2.x and self.y == v2.y

	@staticmethod
	def min(v1, v2):
		return V(min(v1.x, v2.x), min(v1.y, v2.y))

	@staticmethod
	def max(v1, v2):
		return V(max(v1.x, v2.x), max(v1.y, v2.y))

	def __str__(self):
		return f"({self.x}, {self.y})"

class Quadtree:
	def __init__(self, depth, data, corner = V(0, 0)):
		self.depth = depth
		self.size = pow(2, depth)
		self.corner = corner
		self.contract(data)

	def contract(self, data):
		self.data = data
		self.contents = None

	def expand(self):
		if self.depth == 0:
			raise Exception("Too deep!")
		mid = self.midpoint()
		self.contents = (
			Quadtree(self.depth - 1, self.data, self.corner),
			Quadtree(self.depth - 1, self.data, V(mid.x, self.corner.y)),
			Quadtree(self.depth - 1, self.data, mid),
			Quadtree(self.depth - 1, self.data, V(self.corner.x, mid.y))
		)
		self.data = None

	def midpoint(self):
		return self.corner + V(self.size / 2)

	def endpoint(self):
		return self.corner + V(self.size - 1)

	def transform_data(self, va, vb, fn):
		# Restrain to dimensions of this node
		end = self.endpoint()
		va = V.max(va, self.corner)
		vb = V.min(vb, end)
		# print(f"Recoloring {self.corner}, {end} with params {va}, {vb}")
		# Base case : does not fit
		if (va.x > end.x or va.y > end.y or vb.x < self.corner.x or vb.y < self.corner.y):
			return
		# Base case : fully covered
		if va == self.corner and vb == end and self.contents is None:
			self.contract(fn(self.data))
			return
		# Recursive case
		# For each quadrant, project required values if needed
		if self.contents is None:
			self.expand()

		self.contents[0].transform_data(va, vb, fn)
		self.contents[1].transform_data(va, vb, fn)
		self.contents[2].transform_data(va, vb, fn)
		self.contents[3].transform_data(va, vb, fn)

		self.recombine()

	def recombine(self):
		s = set(map(lambda x: x.data, self.contents))
		if len(s) == 1:
			c = list(s)[0]
			if c != None:
				self.contract(c)

	def count(self, fn):
		if self.contents is None:
			return fn(self.data, self.size * self.size)

		return sum(map(lambda x: x.count(fn), self.contents))

def test_tree():
	test = Quadtree(10, 0)
	def count_white(data, tiles):
		return data * tiles
	test.transform_data(V(511, 511), V(512, 512), lambda x: 1)
	whites = test.count(count_white)
	print(f"Test tree has {whites} white tiles, expecting 4")
	test.transform_data(V(512, 512), V(513, 513), lambda x: 1)
	whites = test.count(count_white)
	print(f"Test tree has {whites} white tiles, expecting 7")
	test.transform_data(V(511, 511), V(513, 513), lambda x: 1 - x)
	whites = test.count(count_white)
	print(f"Test tree has {whites} white tiles, expecting 2")
	for x in test.contents:
		print(x.data)

def parse_instruction(instruction, tree, on_fn, off_fn, toggle_fn):
	options = ["turn off ", "turn on ", "toggle "]
	option = None
	for x in options:
		if instruction.startswith(x):
			option = x
			instruction = instruction[len(x):]
			break
	pattern = '(\d+),(\d+) through (\d+),(\d+)'
	groups = re.search(pattern, instruction).groups()
	va = V(int(groups[0]), int(groups[1]))
	vb = V(int(groups[2]), int(groups[3]))
	if option == "turn off ":
		tree.transform_data(va, vb, off_fn)
	elif option == "turn on ":
		tree.transform_data(va, vb, on_fn)
	elif option == "toggle ":
		tree.transform_data(va, vb, toggle_fn)

def parse_instruction1(instruction, tree):
	parse_instruction(instruction, tree,
		lambda x: 1,
		lambda x: 0,
		lambda x: 1 - x
	)

def part_1():
	tree = Quadtree(10, 0)
	with open("6.txt") as f:
		instructions = f.readlines()
		for x in instructions:
			parse_instruction1(x, tree)
	whites = tree.count(lambda x, y: x * y)
	print(f"Part 1: {whites} lights are lit")

def parse_instruction2(instruction, tree):
	parse_instruction(instruction, tree,
		lambda x: x + 1,
		lambda x: max(x - 1, 0),
		lambda x: x + 2
	)

def part_2():
	tree = Quadtree(10, 0)
	with open("6.txt") as f:
		instructions = f.readlines()
		for x in instructions:
			parse_instruction2(x, tree)
	whites = tree.count(lambda x, y: x * y)
	print(f"Part 2: {whites} total brightness")

part_1()
part_2()
# test_tree()