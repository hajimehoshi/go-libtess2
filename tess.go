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
// static TESSindex golibtess2_setElementAt(TESSindex* elements, int i, TESSindex v) {
//   return elements[i] = v;
// }
//
// static TESSreal golibtess2_vertexAt(TESSreal* vertices, TESSindex i) {
//   return vertices[i];
// }
//
// static TESSindex* golibtess2_incElement(TESSindex* elements) {
//   return elements + 1;
// }
//
// static TESSreal* golibtess2_incVertex(TESSreal* vertices) {
//   return vertices + 1;
// }
//
// static const int sizeOfFloat = sizeof(float);
//
// void tessProjectPolygon( TESStesselator *tess );
// int tessMeshSetWindingNumber( TESSmesh *mesh, int value, int keepOnlyBoundary );
// int tessMeshTessellateInterior( TESSmesh *mesh );
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
	tessAddContour((*C.struct_TESStesselator)(t.p), 2, fs)
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

func outputContours(tess *C.TESStesselator, mesh *C.TESSmesh, vertexSize int) {
	tess.vertexCount = 0
	tess.elementCount = 0

	for f := mesh.fHead.next; f != &mesh.fHead; f = f.next {
		if f.inside == 0 {
			continue
		}

		start := f.anEdge
		edge := f.anEdge
		for {
			tess.vertexCount++
			edge = edge.Lnext
			if edge == start {
				break
			}
		}
		tess.elementCount++
	}

	tess.elements = (*C.TESSindex)(C.golibtess2_stdAlloc(tess.alloc.userData,
		C.uint(C.sizeof_TESSindex*tess.elementCount*2)))

	tess.vertices = (*C.TESSreal)(C.golibtess2_stdAlloc(tess.alloc.userData,
		C.sizeof_TESSreal*C.uint(tess.vertexCount)*C.uint(vertexSize)))

	tess.vertexIndices = (*C.TESSindex)(C.golibtess2_stdAlloc(tess.alloc.userData,
		C.uint(C.sizeof_TESSindex*tess.vertexCount)))

	verts := tess.vertices
	elements := tess.elements
	vertInds := tess.vertexIndices

	startVert := 0

	for f := mesh.fHead.next; f != &mesh.fHead; f = f.next {
		if f.inside == 0 {
			continue
		}

		vertCount := 0
		start := f.anEdge
		edge := f.anEdge
		for {
			*verts = edge.Org.coords[0]
			verts = C.golibtess2_incVertex(verts)
			*verts = edge.Org.coords[1]
			verts = C.golibtess2_incVertex(verts)
			if vertexSize > 2 {
				*verts = edge.Org.coords[2]
				verts = C.golibtess2_incVertex(verts)
			}
			*vertInds = edge.Org.idx
			vertInds = C.golibtess2_incElement(vertInds)
			vertCount++
			edge = edge.Lnext
			if edge == start {
				break
			}
		}

		C.golibtess2_setElementAt(elements, 0, C.TESSindex(startVert))
		C.golibtess2_setElementAt(elements, 1, C.TESSindex(vertCount))
		elements = C.golibtess2_incElement(elements)
		elements = C.golibtess2_incElement(elements)

		startVert += vertCount
	}
}

// tessAddContour: - Adds a contour to be tesselated.
// The type of the vertex coordinates is assumed to be TESSreal.
// Parameters:
//   tess - pointer to tesselator object.
//   size - number of coordinates per vertex. Must be 2 or 3.
//   vertices - vertices array
func tessAddContour(tess *C.TESStesselator, size int, vertices []float32) {
	if tess.mesh == nil {
		tess.mesh = C.tessMeshNewMesh(&tess.alloc)
	}

	if size < 2 {
		size = 2
	}
	if size > 3 {
		size = 3
	}

	var e *C.TESShalfEdge
	numVertices := len(vertices) / size
	src := vertices
	for i := 0; i < numVertices; i++ {
		coords := src
		src = src[size:]

		if e == nil {
			// Make a self-loop (one vertex, one edge).
			e = C.tessMeshMakeEdge(tess.mesh)
			if e == nil {
				tess.outOfMemory = 1
				return
			}
			if C.tessMeshSplice(tess.mesh, e, e.Sym) == 0 {
				tess.outOfMemory = 1
				return
			}
		} else {
			// Create a new vertex and edge which immediately follow e
			// in the ordering around the left face.
			if C.tessMeshSplitEdge(tess.mesh, e) == nil {
				tess.outOfMemory = 1
				return
			}
			e = e.Lnext
		}

		// The new vertex is now e.Org.
		e.Org.coords[0] = C.TESSreal(coords[0])
		e.Org.coords[1] = C.TESSreal(coords[1])
		if size > 2 {
			e.Org.coords[2] = C.TESSreal(coords[2])
		} else {
			e.Org.coords[2] = 0
		}
		// Store the insertion number so that the vertex can be later recognized.
		e.Org.idx = tess.vertexIndexCounter
		tess.vertexIndexCounter++

		// The winding of an edge says how the winding number changes as we
		// cross from the edge''s right face to its left face.  We add the
		// vertices in such an order that a CCW contour will add +1 to
		// the winding number of the region inside the contour.
		e.winding = 1
		e.Sym.winding = -1
	}
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
		outputContours(tess, mesh, vertexSize)
	} else {
		// output polygons
		C.OutputPolymesh(tess, mesh, C.int(elementType), C.int(polySize), C.int(vertexSize))
	}

	C.tessMeshDeleteMesh(&tess.alloc, mesh)
	tess.mesh = nil

	return true
}
