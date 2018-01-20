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

// #include "tess.h"
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
//
// void tessProjectPolygon( TESStesselator *tess );
// int tessMeshSetWindingNumber( TESSmesh *mesh, int value, int keepOnlyBoundary );
// int tessMeshTessellateInterior( TESSmesh *mesh );
// void OutputContours( TESStesselator *tess, TESSmesh *mesh, int vertexSize );
// void OutputPolymesh( TESStesselator *tess, TESSmesh *mesh, int elementType, int polySize, int vertexSize );
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

func (t *Tesselator) AddContour(contour []Vertex) {
	fs := make([]float32, len(contour)*2)
	for i, v := range contour {
		fs[2*i] = v.X
		fs[2*i+1] = v.Y
	}
	C.tessAddContour((*C.struct_TESStesselator)(t.p),
		2,
		unsafe.Pointer(&fs[0]),
		2*C.sizeOfFloat,
		C.int(len(contour)))
}

func (t *Tesselator) Tesselate() ([]int, []Vertex, error) {
	const (
		polySize   = 3
		vertexSize = 2
	)

	r := tessTesselate((*C.struct_TESStesselator)(t.p),
		C.TESS_WINDING_ODD,
		C.TESS_POLYGONS,
		polySize,
		vertexSize,
		nil)
	if !r {
		return nil, nil, fmt.Errorf("libtess2: tessTesselate failed: error code: %d", r)
	}

	elements := []int{}
	vertices := []Vertex{}

	vc := int((*C.struct_TESStesselator)(t.p).vertexCount)
	vs := (*C.struct_TESStesselator)(t.p).vertices
	for i := 0; i < vc; i++ {
		v := Vertex{
			X: float32(C.golibtess2_vertexAt(vs, C.TESSindex(i)*2)),
			Y: float32(C.golibtess2_vertexAt(vs, C.TESSindex(i)*2+1)),
		}
		vertices = append(vertices, v)
	}

	ec := int((*C.struct_TESStesselator)(t.p).elementCount)
	es := (*C.struct_TESStesselator)(t.p).elements
	for i := 0; i < ec; i++ {
		for j := 0; j < polySize; j++ {
			e := int(C.golibtess2_elementAt(es, C.int(i)*polySize+C.int(j)))
			elements = append(elements, e)
		}
	}
	return elements, vertices, nil
}

// tessTesselate: - tesselate contours.
// Parameters:
//   tess - pointer to tesselator object.
//   windingRule - winding rules used for tesselation, must be one of TessWindingRule.
//   elementType - defines the tesselation result element type, must be one of TessElementType.
//   polySize - defines maximum vertices per polygons if output is polygons.
//   vertexSize - defines the number of coordinates in tesselation result vertex, must be 2 or 3.
//   normal - defines the normal of the input contours, of null the normal is calculated automatically.
// Returns:
//   true if succeed, false if failed.
func tessTesselate(tess *C.TESStesselator, windingRule int, elementType int, polySize int, vertexSize int, normal []C.TESSreal) bool {
	/*if tess.vertices != nil {
		tess.alloc.memfree(tess.alloc.userData, tess.vertices)
		tess.vertices = 0
	}
	if tess.elements != nil {
		tess.alloc.memfree(tess.alloc.userData, tess.elements)
		tess.elements = 0
	}
	if tess.vertexIndices != nil {
		tess.alloc.memfree(tess.alloc.userData, tess.vertexIndices)
		tess.vertexIndices = 0
	}*/

	tess.vertexIndexCounter = 0

	if normal != nil {
		tess.normal[0] = normal[0]
		tess.normal[1] = normal[1]
		tess.normal[2] = normal[2]
	}

	tess.windingRule = C.int(windingRule)

	if vertexSize < 2 {
		vertexSize = 2
	}
	if vertexSize > 3 {
		vertexSize = 3
	}

	if tess.mesh == nil {
		return false
	}

	// Determine the polygon normal and project vertices onto the plane
	// of the polygon.
	C.tessProjectPolygon(tess)

	// tessComputeInterior( tess ) computes the planar arrangement specified
	// by the given contours, and further subdivides this arrangement
	// into regions.  Each region is marked "inside" if it belongs
	// to the polygon, according to the rule given by tess.windingRule.
	// Each interior region is guaranteed be monotone.
	tessComputeInterior(tess)

	mesh := tess.mesh

	// If the user wants only the boundary contours, we throw away all edges
	// except those which separate the interior from the exterior.
	// Otherwise we tessellate all the regions marked "inside".
	if elementType == C.TESS_BOUNDARY_CONTOURS {
		C.tessMeshSetWindingNumber(mesh, 1, 1 /* true */)
	} else {
		C.tessMeshTessellateInterior(mesh)
	}

	C.tessMeshCheckMesh(mesh)

	if elementType == C.TESS_BOUNDARY_CONTOURS {
		// output contours
		C.OutputContours(tess, mesh, C.int(vertexSize))
	} else {
		// output polygons
		C.OutputPolymesh(tess, mesh, C.int(elementType), C.int(polySize), C.int(vertexSize))
	}

	C.tessMeshDeleteMesh(&tess.alloc, mesh)
	tess.mesh = nil

	return true
}
