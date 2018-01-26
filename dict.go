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

// #include "mesh.h"
// #include "tess.h"
import "C"

// activeRegion:
// For each pair of adjacent edges crossing the sweep line, there is
// an ActiveRegion to represent the region between them.  The active
// regions are kept in sorted order in a dynamic dictionary.  As the
// sweep line crosses each vertex, we update the affected regions.
type activeRegion struct {
	// upper edge, directed right to left
	eUp *C.TESShalfEdge

	// dictionary node corresponding to eUp
	nodeUp *dictNode

	// used to determine which regions are
	// inside the polygon
	windingNumber int

	// is this region inside the polygon?
	inside bool

	// marks fake edges at t = +/-infinity
	sentinel bool

	// marks regions where the upper or lower
	// edge has changed, but we haven't checked
	// whether they intersect yet
	dirty bool

	// marks temporary edges introduced when
	// we process a "right vertex" (one without
	// any edges leaving to the right)
	fixUpperEdge bool
}

type dictNode struct {
	key  *activeRegion
	prev *dictNode
	next *dictNode
}

type dict struct {
	head  dictNode
	frame *C.struct_TESStesselator
}

func dictNewDict(frame *C.struct_TESStesselator) *dict {
	d := &dict{
		frame: frame,
	}
	d.head.next = &d.head
	d.head.prev = &d.head
	return d
}

func dictInsertBefore(d *dict, n *dictNode, key *activeRegion) *dictNode {
	for {
		n = n.prev
		if n.key == nil || edgeLeq(d.frame, n.key, key) {
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

	return nn
}

func dictDelete(n *dictNode) {
	n.next.prev = n.prev
	n.prev.next = n.next
}

// dictSearch returns the node with the smallest key greater than or equal
// to the given key.  If there is no such key, returns a node whose
// key is NULL.  Similarly, Succ(Max(d)) has a NULL key, etc.
func dictSearch(d *dict, key *activeRegion) *dictNode {
	n := &d.head
	for {
		n = n.next
		if n.key == nil || edgeLeq(d.frame, key, n.key) {
			break
		}
	}
	return n
}

func dictKey(n *dictNode) *activeRegion {
	return n.key
}

func dictSucc(n *dictNode) *dictNode {
	return n.next
}

func dictPred(n *dictNode) *dictNode {
	return n.prev
}

func dictMin(d *dict) *dictNode {
	return d.head.next
}

func dictMax(d *dict) *dictNode {
	return d.head.prev
}

func dictInsert(d *dict, key *activeRegion) *dictNode {
	return dictInsertBefore(d, &d.head, key)
}
