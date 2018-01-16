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

// #include "priorityq.h"
//
// #include "geom.h"
import "C"

import (
	"container/heap"
	"unsafe"
)

type pq []*C.struct_TESSvertex

func (p pq) Len() int {
	return len(p)
}

func (p pq) Less(i, j int) bool {
	return C.tesvertLeq(p[i], p[j]) != 0
}

func (p pq) Swap(i, j int) {
	p[i], p[j] = p[j], p[i]
}

func (p *pq) Push(x interface{}) {
	*p = append(*p, x.(*C.struct_TESSvertex))
}

func (p *pq) Pop() interface{} {
	old := *p
	x := old[len(old)-1]
	*p = old[:len(old)-1]
	return x
}

var (
	idToPQ    = map[uintptr]*pq{}
	pqCounter = uintptr(1)
)

//export pqNewPriorityQ
func pqNewPriorityQ(size C.int) unsafe.Pointer {
	p := &pq{}
	heap.Init(p)

	c := pqCounter
	pqCounter++
	idToPQ[c] = p
	return unsafe.Pointer(c)
}

//export pqDeletePriorityQ
func pqDeletePriorityQ(pqID unsafe.Pointer) {
	delete(idToPQ, uintptr(pqID))
}

//export pqInsert
func pqInsert(pqID unsafe.Pointer, key *C.struct_TESSvertex) C.PQhandle {
	p := idToPQ[uintptr(pqID)]
	heap.Push(p, key)
	return (C.PQhandle)(unsafe.Pointer(key))
}

//export pqExtractMin
func pqExtractMin(pqID unsafe.Pointer) *C.struct_TESSvertex {
	p := idToPQ[uintptr(pqID)]
	if len(*p) == 0 {
		return nil
	}
	return heap.Pop(p).(*C.struct_TESSvertex)
}

//export pqDelete
func pqDelete(pqID unsafe.Pointer, handle C.PQhandle) {
	p := idToPQ[uintptr(pqID)]

	key := (*C.struct_TESSvertex)(unsafe.Pointer(handle))
	idx := -1
	for i, v := range *p {
		if key == v {
			idx = i
			break
		}
	}
	heap.Remove(p, idx)
}

//export pqMinimum
func pqMinimum(pqID unsafe.Pointer) *C.struct_TESSvertex {
	p := idToPQ[uintptr(pqID)]
	if len(*p) == 0 {
		return nil
	}
	return (*p)[0]
}

//export pqIsEmpty
func pqIsEmpty(pqID unsafe.Pointer) C.bool {
	p := idToPQ[uintptr(pqID)]
	return len(*p) == 0
}
