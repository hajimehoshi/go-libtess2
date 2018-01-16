// SGI FREE SOFTWARE LICENSE B (Version 2.0, Sept. 18, 2008)
// Copyright (C) [dates of first publication] Silicon Graphics, Inc.
// All Rights Reserved.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice including the dates of first publication and either this
// permission notice or a reference to http://oss.sgi.com/projects/FreeB/ shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL SILICON GRAPHICS, INC.
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
// OR OTHER DEALINGS IN THE SOFTWARE.
//
// Except as contained in this notice, the name of Silicon Graphics, Inc. shall not
// be used in advertising or otherwise to promote the sale, use or other dealings in
// this Software without prior written authorization from Silicon Graphics, Inc.

package libtess2

// #include "sweep.h"
import "C"

import (
	"unsafe"
)

type dictNode struct {
	key  *C.struct_ActiveRegion
	prev *dictNode
	next *dictNode
}

type dict struct {
	head  dictNode
	frame *C.struct_TESStesselator
}

var (
	idToDict    = map[uintptr]*dict{}
	dictCounter = uintptr(1)

	idToNode    = map[uintptr]*dictNode{}
	nodeToID    = map[*dictNode]uintptr{}
	nodeCounter = uintptr(1)
)

//export dictNewDict
func dictNewDict(frame *C.struct_TESStesselator) unsafe.Pointer {
	d := &dict{
		frame: frame,
	}
	d.head.next = &d.head
	d.head.prev = &d.head

	c := nodeCounter
	nodeCounter++
	idToNode[c] = &d.head
	nodeToID[&d.head] = c

	c = dictCounter
	dictCounter++
	idToDict[c] = d
	return unsafe.Pointer(c)
}

//export dictDeleteDict
func dictDeleteDict(dictID unsafe.Pointer) {
	delete(idToDict, uintptr(dictID))
}

//export dictInsertBefore
func dictInsertBefore(dictID unsafe.Pointer, nodeID unsafe.Pointer, key *C.struct_ActiveRegion) unsafe.Pointer {
	d := idToDict[uintptr(dictID)]
	n := idToNode[uintptr(nodeID)]
	for {
		n = n.prev
		if n.key == nil || C.EdgeLeq(d.frame, n.key, key) != 0 {
			break
		}
	}

	nn := &dictNode{
		key:  key,
		next: n.next,
		prev: n,
	}
	n.next.prev = nn
	n.next = nn

	c := nodeCounter
	nodeCounter++
	idToNode[c] = nn
	nodeToID[nn] = c
	return unsafe.Pointer(c)
}

//export dictDelete
func dictDelete(nodeID unsafe.Pointer) {
	n := idToNode[uintptr(nodeID)]
	n.next.prev = n.prev
	n.prev.next = n.next
}

//export dictSearch
func dictSearch(dictID unsafe.Pointer, key *C.struct_ActiveRegion) unsafe.Pointer {
	d := idToDict[uintptr(dictID)]
	n := &d.head
	for {
		n = n.next
		if n.key == nil || C.EdgeLeq(d.frame, key, n.key) != 0 {
			break
		}
	}
	return unsafe.Pointer(nodeToID[n])
}

//export dictKey
func dictKey(nodeID unsafe.Pointer) *C.struct_ActiveRegion {
	n := idToNode[uintptr(nodeID)]
	return n.key
}

//export dictSucc
func dictSucc(nodeID unsafe.Pointer) unsafe.Pointer {
	n := idToNode[uintptr(nodeID)]
	return unsafe.Pointer(nodeToID[n.next])
}

//export dictPred
func dictPred(nodeID unsafe.Pointer) unsafe.Pointer {
	n := idToNode[uintptr(nodeID)]
	return unsafe.Pointer(nodeToID[n.prev])
}

//export dictMin
func dictMin(dictID unsafe.Pointer) unsafe.Pointer {
	d := idToDict[uintptr(dictID)]
	return unsafe.Pointer(nodeToID[d.head.next])
}

//export dictMax
func dictMax(dictID unsafe.Pointer) unsafe.Pointer {
	d := idToDict[uintptr(dictID)]
	return unsafe.Pointer(nodeToID[d.head.prev])
}

//export dictInsert
func dictInsert(dictID unsafe.Pointer, key *C.struct_ActiveRegion) unsafe.Pointer {
	d := idToDict[uintptr(dictID)]
	return dictInsertBefore(dictID, unsafe.Pointer(nodeToID[&d.head]), key)
}
