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
// static TESSreal golibtess2_setVertexAt(TESSreal* vertices, int i, TESSreal v) {
//   return vertices[i] = v;
// }
//
// static TESSindex* golibtess2_incElement(TESSindex* elements) {
//   return elements + 1;
// }
//
// static TESSreal* golibtess2_incVertex(TESSreal* vertices) {
//   return vertices + 1;
// }
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

func abs(x C.TESSreal) C.TESSreal {
	if x < 0 {
		return -x
	}
	return x
}

func longAxis(v []C.TESSreal) int {
	i := 0
	if abs(v[1]) > abs(v[0]) {
		i = 1
	}
	if abs(v[2]) > abs(v[i]) {
		i = 2
	}
	return i
}

func shortAxis(v []C.TESSreal) int {
	i := 0
	if abs(v[1]) < abs(v[0]) {
		i = 1
	}
	if abs(v[2]) < abs(v[i]) {
		i = 2
	}
	return i
}

func computeNormal(tess *C.TESStesselator, norm []C.TESSreal) {
	maxVal := make([]C.TESSreal, 3)
	minVal := make([]C.TESSreal, 3)
	d1 := make([]C.TESSreal, 3)
	d2 := make([]C.TESSreal, 3)
	tNorm := make([]C.TESSreal, 3)
	maxVert := make([]*C.TESSvertex, 3)
	minVert := make([]*C.TESSvertex, 3)

	vHead := &tess.mesh.vHead
	v := vHead.next
	for i := 0; i < 3; i++ {
		c := v.coords[i]
		minVal[i] = c
		minVert[i] = v
		maxVal[i] = c
		maxVert[i] = v
	}

	for v := vHead.next; v != vHead; v = v.next {
		for i := 0; i < 3; i++ {
			c := v.coords[i]
			if c < minVal[i] {
				minVal[i] = c
				minVert[i] = v
			}
			if c > maxVal[i] {
				maxVal[i] = c
				maxVert[i] = v
			}
		}
	}

	// Find two vertices separated by at least 1/sqrt(3) of the maximum
	// distance between any two vertices
	i := 0
	if maxVal[1]-minVal[1] > maxVal[0]-minVal[0] {
		i = 1
	}
	if maxVal[2]-minVal[2] > maxVal[i]-minVal[i] {
		i = 2
	}
	if minVal[i] >= maxVal[i] {
		// All vertices are the same -- normal doesn't matter
		norm[0] = 0
		norm[1] = 0
		norm[2] = 1
		return
	}

	// Look for a third vertex which forms the triangle with maximum area
	// (Length of normal == twice the triangle area)
	maxLen2 := C.TESSreal(0)
	v1 := minVert[i]
	v2 := maxVert[i]
	d1[0] = v1.coords[0] - v2.coords[0]
	d1[1] = v1.coords[1] - v2.coords[1]
	d1[2] = v1.coords[2] - v2.coords[2]
	for v := vHead.next; v != vHead; v = v.next {
		d2[0] = v.coords[0] - v2.coords[0]
		d2[1] = v.coords[1] - v2.coords[1]
		d2[2] = v.coords[2] - v2.coords[2]
		tNorm[0] = d1[1]*d2[2] - d1[2]*d2[1]
		tNorm[1] = d1[2]*d2[0] - d1[0]*d2[2]
		tNorm[2] = d1[0]*d2[1] - d1[1]*d2[0]
		tLen2 := tNorm[0]*tNorm[0] + tNorm[1]*tNorm[1] + tNorm[2]*tNorm[2]
		if tLen2 > maxLen2 {
			maxLen2 = tLen2
			norm[0] = tNorm[0]
			norm[1] = tNorm[1]
			norm[2] = tNorm[2]
		}
	}

	if maxLen2 <= 0 {
		// All points lie on a single line -- any decent normal will do
		norm[0] = 0
		norm[1] = 0
		norm[2] = 0
		norm[shortAxis(d1)] = 1
	}
}

func checkOrientation(tess *C.TESStesselator) {
	fHead := &tess.mesh.fHead
	vHead := &tess.mesh.vHead

	// When we compute the normal automatically, we choose the orientation
	// so that the the sum of the signed areas of all contours is non-negative.
	area := C.TESSreal(0)
	for f := fHead.next; f != fHead; f = f.next {
		e := f.anEdge
		if e.winding <= 0 {
			continue
		}
		for {
			area += (e.Org.s - dst(e).s) * (e.Org.t + dst(e).t)
			e = e.Lnext
			if e == f.anEdge {
				break
			}
		}
	}
	if area < 0 {
		// Reverse the orientation by flipping all the t-coordinates
		for v := vHead.next; v != vHead; v = v.next {
			v.t = -v.t
		}
		tess.tUnit[0] = -tess.tUnit[0]
		tess.tUnit[1] = -tess.tUnit[1]
		tess.tUnit[2] = -tess.tUnit[2]
	}
}

func dot(u, v []C.TESSreal) C.TESSreal {
	return u[0]*v[0] + u[1]*v[1] + u[2]*v[2]
}

// Determine the polygon normal and project vertices onto the plane
// of the polygon.
func tessProjectPolygon(tess *C.TESStesselator) {
	const (
		S_UNIT_X = 1.0
		S_UNIT_Y = 0.0
	)

	vHead := &tess.mesh.vHead
	norm := make([]C.TESSreal, 3)
	computedNormal := false

	norm[0] = tess.normal[0]
	norm[1] = tess.normal[1]
	norm[2] = tess.normal[2]
	if norm[0] == 0 && norm[1] == 0 && norm[2] == 0 {
		computeNormal(tess, norm)
		computedNormal = true
	}
	sUnit := tess.sUnit[:]
	tUnit := tess.tUnit[:]
	i := longAxis(norm)

	// Project perpendicular to a coordinate axis -- better numerically
	sUnit[i] = 0
	sUnit[(i+1)%3] = S_UNIT_X
	sUnit[(i+2)%3] = S_UNIT_Y

	tUnit[i] = 0
	if norm[i] > 0 {
		tUnit[(i+1)%3] = -S_UNIT_Y
	} else {
		tUnit[(i+1)%3] = S_UNIT_Y
	}
	if norm[i] > 0 {
		tUnit[(i+2)%3] = S_UNIT_X
	} else {
		tUnit[(i+2)%3] = -S_UNIT_X
	}

	// Project the vertices onto the sweep plane
	for v := vHead.next; v != vHead; v = v.next {
		v.s = dot(v.coords[:], sUnit)
		v.t = dot(v.coords[:], tUnit)
	}
	if computedNormal {
		checkOrientation(tess)
	}

	// Compute ST bounds.
	first := true
	for v := vHead.next; v != vHead; v = v.next {
		if first {
			tess.bmin[0] = v.s
			tess.bmax[0] = v.s
			tess.bmin[1] = v.t
			tess.bmax[1] = v.t
			first = false
		} else {
			if v.s < tess.bmin[0] {
				tess.bmin[0] = v.s
			}
			if v.s > tess.bmax[0] {
				tess.bmax[0] = v.s
			}
			if v.t < tess.bmin[1] {
				tess.bmin[1] = v.t
			}
			if v.t > tess.bmax[1] {
				tess.bmax[1] = v.t
			}
		}
	}
}

// tessMeshTessellateMonoRegion tessellates a monotone region
// (what else would it do??)  The region must consist of a single
// loop of half-edges (see mesh.h) oriented CCW.  "Monotone" in this
// case means that any vertical line intersects the interior of the
// region in a single interval.
//
// Tessellation consists of adding interior edges (actually pairs of
// half-edges), to split the region into non-overlapping triangles.
//
// The basic idea is explained in Preparata and Shamos (which I don''t
// have handy right now), although their implementation is more
// complicated than this one.  The are two edge chains, an upper chain
// and a lower chain.  We process all vertices from both chains in order,
// from right to left.
//
// The algorithm ensures that the following invariant holds after each
// vertex is processed: the untessellated region consists of two
// chains, where one chain (say the upper) is a single edge, and
// the other chain is concave.  The left vertex of the single edge
// is always to the left of all vertices in the concave chain.
//
// Each step consists of adding the rightmost unprocessed vertex to one
// of the two chains, and forming a fan of triangles from the rightmost
// of two chain endpoints.  Determining whether we can add each triangle
// to the fan is a simple orientation test.  By making the fan as large
// as possible, we restore the invariant (check it yourself).
func tessMeshTessellateMonoRegion(mesh *C.TESSmesh, face *C.TESSface) {
	// All edges are oriented CCW around the boundary of the region.
	// First, find the half-edge whose origin vertex is rightmost.
	// Since the sweep goes from left to right, face.anEdge should
	// be close to the edge we want.
	up := face.anEdge
	assert(up.Lnext != up && up.Lnext.Lnext != up)

	for vertLeq(dst(up), up.Org) {
		up = lPrev(up)
	}
	for vertLeq(up.Org, dst(up)) {
		up = up.Lnext
	}
	lo := lPrev(up)

	for up.Lnext != lo {
		if vertLeq(dst(up), lo.Org) {
			// up.Dst is on the left.  It is safe to form triangles from lo.Org.
			// The edgeGoesLeft test guarantees progress even when some triangles
			// are CW, given that the upper and lower chains are truly monotone.
			for lo.Lnext != up && (edgeGoesLeft(lo.Lnext) || tesedgeSign(lo.Org, dst(lo), dst(lo.Lnext)) <= 0) {
				tempHalfEdge := tessMeshConnect(mesh, lo.Lnext, lo)
				lo = tempHalfEdge.Sym
			}
			lo = lPrev(lo)
		} else {
			// lo.Org is on the left.  We can make CCW triangles from up.Dst.
			for lo.Lnext != up && (edgeGoesRight(lPrev(up)) || tesedgeSign(dst(up), up.Org, lPrev(up).Org) >= 0) {
				tempHalfEdge := tessMeshConnect(mesh, up, lPrev(up))
				up = tempHalfEdge.Sym
			}
			up = up.Lnext
		}
	}

	// Now lo.Org == up.Dst == the leftmost vertex.  The remaining region
	// can be tessellated in a fan from this leftmost vertex.
	assert(lo.Lnext != up)
	for lo.Lnext.Lnext != up {
		tempHalfEdge := tessMeshConnect(mesh, lo.Lnext, lo)
		lo = tempHalfEdge.Sym
	}
}

// tessMeshTessellateInterior tessellates each region of
// the mesh which is marked "inside" the polygon.  Each such region
// must be monotone.
func tessMeshTessellateInterior(mesh *C.TESSmesh) {
	var next *C.TESSface
	for f := mesh.fHead.next; f != &mesh.fHead; f = next {
		// Make sure we don't try to tessellate the new triangles.
		next = f.next
		if f.inside != 0 {
			tessMeshTessellateMonoRegion(mesh, f)
		}
	}
}

// tessMeshSetWindingNumber resets the
// winding numbers on all edges so that regions marked "inside" the
// polygon have a winding number of "value", and regions outside
// have a winding number of 0.
//
// If keepOnlyBoundary is TRUE, it also deletes all edges which do not
// separate an interior region from an exterior one.
func tessMeshSetWindingNumber(mesh *C.TESSmesh, value int, keepOnlyBoundary bool) {
	var eNext *C.TESShalfEdge
	for e := mesh.eHead.next; e != &mesh.eHead; e = eNext {
		eNext = e.next
		if rFace(e).inside != e.Lface.inside {
			// This is a boundary edge (one side is interior, one is exterior).
			e.winding = 0
			if e.Lface.inside != 0 {
				e.winding = C.int(value)
			} else {
				e.winding = -C.int(value)
			}
		} else {

			// Both regions are interior, or both are exterior.
			if !keepOnlyBoundary {
				e.winding = 0
			} else {
				if C.tessMeshDelete(mesh, e) == 0 {
					return
				}
			}
		}
	}
}

//export tessNewTess
func tessNewTess(alloc *C.TESSalloc) *C.TESStesselator {
	assert(alloc != nil)

	// Only initialize fields which can be changed by the api.  Other fields
	// are initialized where they are used.

	tess := (*C.TESStesselator)(C.golibtess2_stdAlloc(alloc.userData, C.sizeof_TESStesselator))
	tess.alloc = *alloc
	// Check and set defaults.
	if tess.alloc.meshEdgeBucketSize == 0 {
		tess.alloc.meshEdgeBucketSize = 512
	}
	if tess.alloc.meshVertexBucketSize == 0 {
		tess.alloc.meshVertexBucketSize = 512
	}
	if tess.alloc.meshFaceBucketSize == 0 {
		tess.alloc.meshFaceBucketSize = 256
	}
	if tess.alloc.dictNodeBucketSize == 0 {
		tess.alloc.dictNodeBucketSize = 512
	}
	if tess.alloc.regionBucketSize == 0 {
		tess.alloc.regionBucketSize = 256
	}

	tess.normal[0] = 0
	tess.normal[1] = 0
	tess.normal[2] = 0

	tess.bmin[0] = 0
	tess.bmin[1] = 0
	tess.bmax[0] = 0
	tess.bmax[1] = 0

	tess.windingRule = C.TESS_WINDING_ODD

	if tess.alloc.regionBucketSize < 16 {
		tess.alloc.regionBucketSize = 16
	}
	if tess.alloc.regionBucketSize > 4096 {
		tess.alloc.regionBucketSize = 4096
	}
	tess.regionPool = C.createBucketAlloc(&tess.alloc, C.CString("Regions"), C.sizeof_ActiveRegion, C.uint(tess.alloc.regionBucketSize))

	// Initialize to begin polygon.
	tess.mesh = nil

	tess.outOfMemory = 0
	tess.vertexIndexCounter = 0

	tess.vertices = nil
	tess.vertexIndices = nil
	tess.vertexCount = 0
	tess.elements = nil
	tess.elementCount = 0

	return tess
}

func neighbourFace(edge *C.TESShalfEdge) C.TESSindex {
	if rFace(edge) == nil {
		return undef
	}
	if rFace(edge).inside == 0 {
		return undef
	}
	return rFace(edge).n
}

func outputPolymesh(tess *C.TESStesselator, mesh *C.TESSmesh, elementType int, polySize int, vertexSize int) {
	// Assume that the input data is triangles now.
	// Try to merge as many polygons as possible
	if polySize > 3 {
		tessMeshMergeConvexFaces(mesh, polySize)
	}

	// Mark unused
	for v := mesh.vHead.next; v != &mesh.vHead; v = v.next {
		v.n = undef
	}

	maxFaceCount := 0
	maxVertexCount := 0

	// Create unique IDs for all vertices and faces.
	for f := mesh.fHead.next; f != &mesh.fHead; f = f.next {
		f.n = undef
		if f.inside == 0 {
			continue
		}

		edge := f.anEdge
		faceVerts := 0
		for {
			v := edge.Org
			if v.n == undef {
				v.n = C.TESSindex(maxVertexCount)
				maxVertexCount++
			}
			faceVerts++
			edge = edge.Lnext
			if edge == f.anEdge {
				break
			}
		}
		assert(faceVerts <= polySize)
		f.n = C.TESSindex(maxFaceCount)
		maxFaceCount++
	}

	tess.elementCount = C.int(maxFaceCount)
	if elementType == C.TESS_CONNECTED_POLYGONS {
		maxFaceCount *= 2
	}
	tess.elements = (*C.TESSindex)(C.golibtess2_stdAlloc(tess.alloc.userData,
		C.uint(C.sizeof_TESSindex*maxFaceCount*polySize)))

	tess.vertexCount = C.int(maxVertexCount)
	tess.vertices = (*C.TESSreal)(C.golibtess2_stdAlloc(tess.alloc.userData,
		C.sizeof_TESSreal*C.uint(tess.vertexCount)*C.uint(vertexSize)))

	tess.vertexIndices = (*C.TESSindex)(C.golibtess2_stdAlloc(tess.alloc.userData,
		C.uint(C.sizeof_TESSindex*tess.vertexCount)))

	// Output vertices.
	for v := mesh.vHead.next; v != &mesh.vHead; v = v.next {
		if v.n != undef {
			// Store coordinate
			C.golibtess2_setVertexAt(tess.vertices, C.int(v.n)*C.int(vertexSize), v.coords[0])
			C.golibtess2_setVertexAt(tess.vertices, C.int(v.n)*C.int(vertexSize)+1, v.coords[1])
			if vertexSize > 2 {
				C.golibtess2_setVertexAt(tess.vertices, C.int(v.n)*C.int(vertexSize)+2, v.coords[2])
			}
			// Store vertex index.
			C.golibtess2_setElementAt(tess.vertexIndices, C.int(v.n), v.idx)
		}
	}

	// Output indices.
	elements := tess.elements
	for f := mesh.fHead.next; f != &mesh.fHead; f = f.next {
		if f.inside == 0 {
			continue
		}

		// Store polygon
		edge := f.anEdge
		faceVerts := 0
		for {
			v := edge.Org
			*elements = v.n
			elements = C.golibtess2_incElement(elements)
			faceVerts++
			edge = edge.Lnext
			if edge == f.anEdge {
				break
			}
		}
		// Fill unused.
		for i := faceVerts; i < polySize; i++ {
			*elements = undef
			elements = C.golibtess2_incElement(elements)
		}

		// Store polygon connectivity
		if elementType == C.TESS_CONNECTED_POLYGONS {
			edge = f.anEdge
			for {
				*elements = neighbourFace(edge)
				elements = C.golibtess2_incElement(elements)
				edge = edge.Lnext
				if edge == f.anEdge {
					break
				}
			}
			// Fill unused.
			for i := faceVerts; i < polySize; i++ {
				*elements = undef
				elements = C.golibtess2_incElement(elements)
			}
		}
	}
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
		tess.mesh = tessMeshNewMesh(&tess.alloc)
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
	tessProjectPolygon(tess)

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
		tessMeshSetWindingNumber(mesh, 1, true)
	} else {
		tessMeshTessellateInterior(mesh)
	}

	tessMeshCheckMesh(mesh)

	if elementType == C.TESS_BOUNDARY_CONTOURS {
		// output contours
		outputContours(tess, mesh, vertexSize)
	} else {
		// output polygons
		outputPolymesh(tess, mesh, elementType, polySize, vertexSize)
	}

	tess.mesh = nil

	return true
}
