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

// #include "tesselator.h"
//
// #include <stdlib.h>
// #include <string.h>
//
// static void* golibtess2_stdAlloc(void* userData, unsigned int size) {
//   int* allocated = ( int*)userData;
//   TESS_NOTUSED(userData);
//   *allocated += (int)size;
//   return malloc(size);
// }
//
// static void golibtess2_stdFree(void* userData, void* ptr) {
//   TESS_NOTUSED(userData);
//   free(ptr);
// }
//
// static int allocated = 0;
//
// static TESStesselator* golibtess2_newTesselator() {
//   TESSalloc ma;
//   memset(&ma, 0, sizeof(ma));
//   ma.memalloc = golibtess2_stdAlloc;
//   ma.memfree = golibtess2_stdFree;
//   ma.userData = (void*)&allocated;
//   ma.extraVertices = 256; // realloc not provided, allow 256 extra vertices.
//   return tessNewTess(&ma);
// }
//
// static TESSindex golibtess2_elementAt(TESSindex* elements, int i) {
//   return elements[i];
// }
//
// static TESSreal golibtess2_vertexAt(TESSreal* vertices, TESSindex i) {
//   return vertices[i];
// }
//
// static const int sizeOfFloat = sizeof(float);
import "C"

import (
	"fmt"
	"unsafe"
)

type Vertex struct {
	X float32 // TODO: Replace this to float64 later
	Y float32
}

type Tesselator struct {
	p unsafe.Pointer
}

func NewTesselator() *Tesselator {
	return &Tesselator{
		p: unsafe.Pointer(C.golibtess2_newTesselator()),
	}
}

func (t *Tesselator) AddContour(contour []float32) {
	C.tessAddContour((*C.struct_TESStesselator)(t.p),
		2,
		unsafe.Pointer(&contour[0]),
		2*C.sizeOfFloat,
		C.int(len(contour)/2))
}

func (t *Tesselator) Tesselate() ([]int, []Vertex, error) {
	const (
		polySize   = 3
		vertexSize = 2
	)

	r := C.tessTesselate((*C.struct_TESStesselator)(t.p),
		C.TESS_WINDING_ODD,
		C.TESS_POLYGONS,
		polySize,
		vertexSize,
		nil)
	if r == 0 {
		return nil, nil, fmt.Errorf("libtess2: tessTesselate failed: error code: %d", r)
	}

	elements := []int{}
	vertices := []Vertex{}

	vc := int(C.tessGetVertexCount((*C.struct_TESStesselator)(t.p)))
	vs := C.tessGetVertices((*C.struct_TESStesselator)(t.p))
	for i := 0; i < vc; i++ {
		v := Vertex{
			X: float32(C.golibtess2_vertexAt(vs, C.TESSindex(i)*2)),
			Y: float32(C.golibtess2_vertexAt(vs, C.TESSindex(i)*2+1)),
		}
		vertices = append(vertices, v)
	}

	ec := int(C.tessGetElementCount((*C.struct_TESStesselator)(t.p)))
	es := C.tessGetElements((*C.struct_TESStesselator)(t.p))
	for i := 0; i < ec; i++ {
		for j := 0; j < polySize; j++ {
			e := int(C.golibtess2_elementAt(es, C.int(i)*polySize+C.int(j)))
			elements = append(elements, e)
		}
	}
	return elements, vertices, nil
}
