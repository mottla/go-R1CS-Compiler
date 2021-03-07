package utils

import (
	"errors"
	"fmt"
	"math/big"
	"strings"
	"sync"
)

func abs(i int) int {
	if i < 0 {
		return -i
	}
	return i
}

func maximum(a, b int) int {
	if a > b {
		return a
	}
	return b
}
func (s AvlNode) String() string {
	return fmt.Sprintf("(%v,%v)", s.Key(), s.Value().String())
}
func (t *AvlTree) String() string {
	if t == nil {
		return "empty"
	}
	t.lock.RLock()
	defer t.lock.RUnlock()
	sb := new(strings.Builder)
	t.root.print(sb)
	return sb.String()
}

// Each recursively traverses tree `tree` and collects all nodes
func (root *AvlNode) print(sb *strings.Builder) {
	if root == nil {
		return
	}

	if root.right != nil {
		root.right.print(sb)
	}
	sb.WriteString(root.String())
	if root.left != nil {
		root.left.print(sb)
	}

}

type AvlTree struct {
	root *AvlNode
	lock *sync.RWMutex
}

func NewAvlTree() *AvlTree {
	return &AvlTree{lock: &sync.RWMutex{}}
}

// Add inserts an element to tree with root `t`
func (t *AvlTree) InsertNoOverwriteAllowed(key uint, value *big.Int) (err error) {
	err = t.Put(key, value, setInsertNoOvreride)
	return
}

// Add inserts an element to tree with root `t`
func (t *AvlTree) Insert(key uint, value *big.Int) (err error) {

	err = t.Put(key, value, setInsert)

	return
}
func NewSparseArrayWith(key uint, value *big.Int) (sp *AvlTree) {
	sp = NewAvlTree()
	sp.Put(key, value, setInsertNoOvreride)
	return
}

func NewSparseArrayFromArray(in []*big.Int) *AvlTree {
	sp := NewAvlTree()
	for k, v := range in {
		if v.Cmp(bigZero) != 0 {
			sp.Put(uint(k), v, setInsertNoOvreride)
		}
	}
	return sp
}

var setInsert = func(old, new *big.Int) *big.Int {
	return new
}
var setInsertNoOvreride = func(old, new *big.Int) *big.Int {
	if old != nil && old.Cmp(bigZero) != 0 {
		panic("no override")
	}
	return new
}

// MaxNode returns the node with the maximal Key in the tree
func (t *AvlTree) MaxPower() uint {
	if k := t.root.maxNode(); k == nil {
		return 0
	} else {
		return k.key
	}
}

// MaxNode returns the node with the maximal Key in the tree
func (t *AvlTree) MaxNode() *AvlNode {
	return t.root.maxNode()
}
func (root *AvlNode) maxNode() (t *AvlNode) {
	if root == nil {
		return &AvlNode{key: 0, value: bigZero}
	}
	if root.right != nil {
		return root.right.maxNode()
	}
	return root
}

// MinNode returns the node with the minimal Key in the tree
func (t *AvlTree) MinNode() *AvlNode {

	return t.root.minNode()
}
func (root *AvlNode) minNode() (t *AvlNode) {

	if root.left != nil {
		return root.left.minNode()
	}
	return root
}

func (t *AvlTree) ToArray(length int) (collectedNodes []*big.Int) {
	if t == nil {
		return nil
	}
	if length < int(t.MaxPower()) {
		panic("")
	}
	t.lock.RLock()
	defer t.lock.RUnlock()
	entries := []Entry{}
	t.root.AllNodes(&entries)
	collectedNodes = ArrayOfBigZeros(length)
	for _, v := range entries {
		collectedNodes[int(v.Key)] = v.Value
	}
	return
}

// Each recursively traverses tree `tree` and collects all nodes
// array is filled up from highest to lowest degree
func (t *AvlTree) DecendingNodes() (collectedNodes []Entry) {
	if t == nil {
		return nil
	}
	t.lock.RLock()
	defer t.lock.RUnlock()
	t.root.AllNodes(&collectedNodes)
	return
}

// Each recursively traverses tree `tree` and collects all nodes
func (root *AvlNode) AllNodes(t *[]Entry) {
	if root == nil {
		return
	}

	if root.right != nil {
		root.right.AllNodes(t)
	}
	(*t) = append((*t), Entry{
		Key:   root.Key(),
		Value: root.Value(),
	})
	if root.left != nil {
		root.left.AllNodes(t)
	}

}

func (self *AvlTree) Root() *AvlNode {
	return self.root
}

func (self *AvlTree) Size() int {
	return self.root.Size()
}

func (self *AvlTree) Has(key uint) bool {
	return self.root.Has(key)
}

func (self *AvlTree) Put(key uint, value *big.Int, insert func(old, new *big.Int) *big.Int) (err error) {
	self.root, _ = self.root.Put(key, value, insert)
	if v, e := self.Get(key); e == nil {
		if v.Cmp(bigZero) == 0 {
			_, err = self.Remove(key)
			if err != nil {
				panic(err)
			}
		}
	}
	return nil
}

func (self *AvlTree) Get(key uint) (value *big.Int, err error) {
	return self.root.Get(key)
}

func (self *AvlTree) Remove(key uint) (value *big.Int, err error) {
	var new_root *AvlNode
	new_root, value, err = self.root.Remove(key)
	if err != nil {
		return nil, err
	}
	self.root = new_root
	return value, nil
}

// Each recursively traverses tree `tree` and collects all nodes
// array is filled up from highest to lowest degree
func (t *AvlTree) ChannelNodes(ascendingOrder bool) (collectedNodes chan Entry) {

	t.lock.RLock()

	collectedNodes = make(chan Entry)
	if ascendingOrder {
		go func() {
			t.root.channelNodesAscending(collectedNodes)
			close(collectedNodes)
			t.lock.RUnlock()
		}()
		return
	}

	go func() {
		t.root.channelNodesDescending(collectedNodes)
		close(collectedNodes)
		t.lock.RUnlock()
	}()
	return
}

// Each recursively traverses tree `tree` and collects all nodes
func (root *AvlNode) channelNodesAscending(collectedNodes chan Entry) {
	if root == nil {
		return
	}

	if root.left != nil {
		root.left.channelNodesAscending(collectedNodes)
	}
	collectedNodes <- Entry{
		Key:   root.Key(),
		Value: root.Value(),
	}
	if root.right != nil {
		root.right.channelNodesAscending(collectedNodes)
	}

}

// Each recursively traverses tree `tree` and collects all nodes
func (root *AvlNode) channelNodesDescending(collectedNodes chan Entry) {
	if root == nil {
		return
	}

	if root.right != nil {
		root.right.channelNodesDescending(collectedNodes)
	}
	collectedNodes <- Entry{
		Key:   root.Key(),
		Value: root.Value(),
	}
	if root.left != nil {
		root.left.channelNodesDescending(collectedNodes)
	}

}

type AvlNode struct {
	key    uint
	value  *big.Int
	height int
	left   *AvlNode
	right  *AvlNode
}

func (self *AvlNode) Has(key uint) (has bool) {
	if self == nil {
		return false
	}
	if self.key == key {
		return true
	} else if key < self.key {
		return self.left.Has(key)
	} else {
		return self.right.Has(key)
	}
}

func (self *AvlNode) Get(key uint) (value *big.Int, err error) {
	if self == nil {
		return bigZero, errors.New(fmt.Sprintf("not found %v", key))
	}
	if self.key == (key) {
		return self.value, nil
	} else if key < (self.key) {
		return self.left.Get(key)
	} else {
		return self.right.Get(key)
	}
}

func (self *AvlNode) pop_node(node *AvlNode) *AvlNode {
	if node == nil {
		panic("node can't be nil")
	} else if node.left != nil && node.right != nil {
		panic("node must not have both left and right")
	}

	if self == nil {
		return nil
	} else if self == node {
		var n *AvlNode
		if node.left != nil {
			n = node.left
		} else if node.right != nil {
			n = node.right
		} else {
			n = nil
		}
		node.left = nil
		node.right = nil
		return n
	}

	if node.key < (self.key) {
		self.left = self.left.pop_node(node)
	} else {
		self.right = self.right.pop_node(node)
	}

	self.height = maximum(self.left.Height(), self.right.Height()) + 1
	return self
}

func (self *AvlNode) push_node(node *AvlNode) *AvlNode {
	if node == nil {
		panic("node can't be nil")
	} else if node.left != nil || node.right != nil {
		panic("node now be a leaf")
	}

	if self == nil {
		node.height = 1
		return node
	} else if node.key < (self.key) {
		self.left = self.left.push_node(node)
	} else {
		self.right = self.right.push_node(node)
	}
	self.height = maximum(self.left.Height(), self.right.Height()) + 1
	return self
}

func (self *AvlNode) rotate_right() *AvlNode {
	if self == nil {
		return self
	}
	if self.left == nil {
		return self
	}
	new_root := self.left.rmd()
	self = self.pop_node(new_root)
	new_root.left = self.left
	new_root.right = self.right
	self.left = nil
	self.right = nil
	return new_root.push_node(self)
}

func (self *AvlNode) rotate_left() *AvlNode {
	if self == nil {
		return self
	}
	if self.right == nil {
		return self
	}
	new_root := self.right.lmd()
	self = self.pop_node(new_root)
	new_root.left = self.left
	new_root.right = self.right
	self.left = nil
	self.right = nil
	return new_root.push_node(self)
}

func (self *AvlNode) balance() *AvlNode {
	if self == nil {
		return self
	}
	for abs(self.left.Height()-self.right.Height()) > 2 {
		if self.left.Height() > self.right.Height() {
			self = self.rotate_right()
		} else {
			self = self.rotate_left()
		}
	}
	return self
}

func (self *AvlNode) Put(key uint, value *big.Int, insert func(old, new *big.Int) *big.Int) (_ *AvlNode, updated bool) {
	if self == nil {
		return &AvlNode{key: key, value: insert(big.NewInt(0), value), height: 1}, false
	}

	if self.key == key {
		self.value = insert(self.value, value)
		return self, true
	}

	if key < (self.key) {
		self.left, updated = self.left.Put(key, value, insert)
	} else {
		self.right, updated = self.right.Put(key, value, insert)
	}
	if !updated {
		self.height += 1
		return self.balance(), updated
	}
	return self, updated
}

func (self *AvlNode) Remove(key uint) (_ *AvlNode, value *big.Int, err error) {
	if self == nil {
		return nil, nil, errors.New(fmt.Sprintf("not found %v", key))
	}

	if self.key == (key) {
		if self.left != nil && self.right != nil {
			if self.left.Size() < self.right.Size() {
				lmd := self.right.lmd()
				lmd.left = self.left
				return self.right, self.value, nil
			} else {
				rmd := self.left.rmd()
				rmd.right = self.right
				return self.left, self.value, nil
			}
		} else if self.left == nil {
			return self.right, self.value, nil
		} else if self.right == nil {
			return self.left, self.value, nil
		} else {
			return nil, self.value, nil
		}
	}
	if key < (self.key) {
		self.left, value, err = self.left.Remove(key)
	} else {
		self.right, value, err = self.right.Remove(key)
	}
	if err != nil {
		return self.balance(), value, err
	}
	return self, value, err
}

func (self *AvlNode) Height() int {
	if self == nil {
		return 0
	}
	return self.height
}

func (self *AvlNode) Size() int {
	if self == nil {
		return 0
	}
	return 1 + self.left.Size() + self.right.Size()
}

func (self *AvlNode) Key() uint {
	return self.key
}

func (self *AvlNode) Value() *big.Int {
	return self.value
}

func (self *AvlNode) Left() *AvlNode {
	if self.left == nil {
		return nil
	}
	return self.left
}

func (self *AvlNode) Right() *AvlNode {
	if self.right == nil {
		return nil
	}
	return self.right
}

func (self *AvlNode) _md(side func(*AvlNode) *AvlNode) *AvlNode {
	if self == nil {
		return nil
	} else if side(self) != nil {
		return side(self)._md(side)
	} else {
		return self
	}
}

func (self *AvlNode) lmd() *AvlNode {
	return self._md(func(node *AvlNode) *AvlNode { return node.left })
}

func (self *AvlNode) rmd() *AvlNode {
	return self._md(func(node *AvlNode) *AvlNode { return node.right })
}
