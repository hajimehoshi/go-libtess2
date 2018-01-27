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

import "C"

// The basic operations are insertion of a new key (pqInsert),
// and examination/extraction of a key whose value is minimum
// (pqMinimum/pqExtractMin).  Deletion is also allowed (pqDelete);
// for this purpose pqInsert returns a "handle" which is supplied
// as the argument.
//
// If the heap is empty, pqMinimum/pqExtractMin will return a NULL key.
// This may also be tested with pqIsEmpty.

import (
	"container/heap"
)

type pq []*vertex

func (p pq) Len() int {
	return len(p)
}

func (p pq) Less(i, j int) bool {
	return tesvertLeq(p[i], p[j])
}

func (p pq) Swap(i, j int) {
	p[i], p[j] = p[j], p[i]
}

func (p *pq) Push(x interface{}) {
	*p = append(*p, x.(*vertex))
}

func (p *pq) Pop() interface{} {
	old := *p
	x := old[len(old)-1]
	*p = old[:len(old)-1]
	return x
}

func pqNewPriorityQ(size C.int) *pq {
	p := &pq{}
	heap.Init(p)
	return p
}

func pqInsert(p *pq, key *vertex) *vertex {
	heap.Push(p, key)
	return key
}

func pqExtractMin(p *pq) *vertex {
	if len(*p) == 0 {
		return nil
	}
	return heap.Pop(p).(*vertex)
}

func pqDelete(p *pq, key *vertex) {
	idx := -1
	for i, v := range *p {
		if key == v {
			idx = i
			break
		}
	}
	heap.Remove(p, idx)
}

func pqMinimum(p *pq) *vertex {
	if len(*p) == 0 {
		return nil
	}
	return (*p)[0]
}

func pqIsEmpty(p *pq) bool {
	return len(*p) == 0
}
